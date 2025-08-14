# In July/August 2025 I started to add solar and storage to avertr, but then
#   I realized (due to personal time constraints) it would be best to simply
#   set aside the solar + storage for now and focus on getting the rest of
#   avertr into an R package. So this is just to store my rough code.

# You could also totally just check the commit history, but that's a bit of a
#   pain, so I'm putting this here for convenience.

# Here's the messy function I had started. See solar + storage stuff at the end.
# Generates an 8760 hourly load reduction vector to be input into avert(). For
#   Rooftop PV and distributed storage, losses used as of AVERT v4.3 are
#   hard-coded. If all you want to do is adjust an 8760 representing an onsite
#   energy program to also account for T&D losses then use adjust_reduction().
generate_reduction <- function(
    
  project_year,
  project_region,
  avert_main_module_filepath,
  avertr_rdf_filepath = NULL,
  
  # Constrain to be number 0 to 1
  apply_reduction_top_x_pct_hours = 0,
  # Constrain to be a number 0 to 1
  reduce_x_pct_in_top_hours = 0,
  
  reduce_annual_generation_by_x_gwh = 0,
  reduce_each_hour_by_x_mw = 0,
  
  onshore_wind_capacity_mw = 0,
  offshore_wind_capacity_mw = 0,
  utility_solar_pv_capacity_mw = 0,
  rooftop_solar_pv_capacity_mw = 0,
  
  pair_solar_with_storage = TRUE,
  utility_scale_storage_capacity = 0,
  distributed_storage_capacity = 0,
  duration = 4,
  charging_pattern = "Midday Charging",
  # Not yet worrying about limiting charging on selected months or weekdays/ends
  max_allowable_discharge_cylces_per_year = 150,
  round_trip_efficiency = 0.85,
  depth_of_discharge = 0.80
) {
  
  # DEFINE/LOAD OBJECTS ######
  
  bau_case_ap_region <- read_rds(avertr_rdf_filepath) |> 
    pluck(paste0("bau_case_ap_", project_region))
  
  # Vector of each hour of the year 2023
  datetime_8760 <- seq(
    from = ymd_hms("2023-01-01 00:00:00"),
    by = "1 hour",
    length.out = 8760
  )
  
  # This is the BAU load
  bau_load_8760 <- bau_case_ap_region |> 
    distinct(datetime_8760_col, load_8760_col) |> 
    pull(load_8760_col)
  
  hourly_load_reduction <- rep(0, 8760)
  
  # This is the T&D losses for the region
  t_and_d_loss_factor <- t_and_d_losses |> 
    filter(`Data year` == project_year) |> 
    pull(project_region)
  
  
  
  # APPLY BOXES ######
  # Constructs the 8760 by sequentially applying the boxes from AVERT's
  #   EnterEEREData tab. Each sub-heading of this section represents a different
  #   box. We skip Box 4 because avertr doesn't do EVs (...yet).
  ## Box 1: Enter EE based on the % reduction of regional fossil generation ======
  if (apply_reduction_top_x_pct_hours != 0 & reduce_x_pct_in_top_hours != 0) {
    # The number of hours to apply the reduction in
    # NOTE: fix rounding to be consistent w excel rounding??
    number_of_top_hours <- round(8760 * apply_reduction_top_x_pct_hours)
    
    # The indices where the top hours are located
    top_hour_indices <- bau_load_8760 |> 
      sort(decreasing = TRUE, index.return = TRUE) |> 
      pluck("ix") |> 
      (\(x) x[1:number_of_hours])()
    
    hourly_load_reduction[top_hour_indices] <- bau_load_8760[top_hour_indices] * reduce_x_pct_in_top_hours
  }
  
  
  ## Box 2: And/or enter EE distributed evenly throughout the year ======
  if (reduce_annual_generation_by_x_gwh != 0 | reduce_each_hour_by_x_mw != 0) {
    if (reduce_annual_generation_by_x_gwh != 0 & reduce_each_hour_by_x_mw != 0) {
      stop("You cannot enter a non-zero value for both reduce_annual_generation_by_x_gwh and reduce_each_hour_by_x_mw. Please enter a non-zero value for at most one of these arguments.")
    }
    reduce_annual_generation_by_x_mwh <- reduce_annual_generation_by_x_gwh * 10^3
    hourly_load_reduction <- hourly_load_reduction + (reduce_annual_generation_by_x_mwh / 8760)
    hourly_load_reduction <- hourly_load_reduction + reduce_each_hour_by_x_mw
  }
  
  
  ## Box 3: And/or enter annual capacity of RE resources ======
  
  if (
    onshore_wind_capacity_mw != 0 |
    offshore_wind_capacity_mw != 0 |
    utility_solar_pv_capacity_mw != 0 |
    rooftop_solar_pv_capacity_mw != 0
  ) {
    # Capacity factors
    cfs <- xlsx_cells(
      avert_main_module_filepath,
      sheets = "EERE_Default",
    )
    
    # Use unpivotr to clean, filtering for the appropriate region. Gets capacity
    #   factors for all four technologies.
    cfs <- cfs |> 
      behead("up-left", "Region") |> 
      behead("up", "Project Type") |> 
      behead("left", "Date") |> 
      behead("left", "Hour") |> 
      filter(
        Region == project_region &
          row <= 8786
      ) |> 
      pack() |> 
      select(row, value, `Project Type`) |> 
      unpack() |> 
      spatter(`Project Type`) |> 
      select(!row)
    
    cfs <- cfs |> 
      # NOTE!!! Pretty sure this is the wrong filter, since it leaves in Feb. 29
      #   and removes 12/31. It should be filter(!(row %in% 1419:1442)). But
      #   this is the filter AVERT uses.
      slice(1:8760) |> 
      # Re-arrange columns to do the matrix multiplication below,  keeping the
      #   four technologies in the same order as a user would input capacities for
      #   them
      relocate(`Onshore Wind`, `Offshore Wind`, `Rooftop PV`, `Utility PV`)
    
    # Vector of the four nameplate capacities the user has entered. Each has a
    #   default value of 0.
    capacity_vector <- c(
      onshore_wind_capacity_mw,
      offshore_wind_capacity_mw,
      utility_solar_pv_capacity_mw,
      rooftop_solar_pv_capacity_mw
    )
    
    # Right-multiply the 8760x4 matrix of capacity factors by the 4x1 vector of
    #   capacities input by user. Add this to whatever load reduction user has
    #   already entered.
    hourly_load_reduction <- hourly_load_reduction + (
      as.matrix(cfs) %*% capacity_vector
    )
  }
  
  
  ## Box 5: And/or enter energy storage data ======
  if (utility_scale_storage_capacity != 0 | distributed_storage_capacity != 0) {
    if (charging_pattern == "Overnight Charging") {
      charging_pattern_24h <- c(
        "Charging",
        "Charging",
        "Charging",
        "Charging",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Discharging",
        "Discharging",
        "Discharging",
        "Discharging",
        "Idle",
        "Idle",
        "Idle",
        "Idle"
      )
    } else if (charging_pattern == "Midday Charging") {
      charging_pattern_24h <- c(
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Charging",
        "Charging",
        "Charging",
        "Charging",
        "Idle",
        "Idle",
        "Idle",
        "Idle",
        "Discharging",
        "Discharging",
        "Discharging",
        "Discharging",
        "Idle",
        "Idle",
        "Idle",
        "Idle"
      )
    } else if (charging_pattern == "Manual") {
      charging_pattern_24h <- manual_charging_pattern
    }
    
    # The number of charging hours
    num_charge_hrs <- sum(charging_pattern_24h == "Charging")
    
    # The number of discharging hours
    num_discharge_hrs <- sum(charging_pattern_24h == "Discharging")
    
    charging_tibble <- tibble(
      hour = 1:24,
      charging_indicator = charging_pattern_24h
    ) |> 
      mutate(
        charging_fraction = case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ (1 / num_charge_hrs),
          charging_indicator == "Discharging" ~ (-1 * (round_trip_efficiency / num_discharge_hrs))
        ),
        daily_load_reduction_utility = case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ -1 * depth_of_discharge * utility_scale_storage_capacity,
          charging_indicator == "Discharging" ~ round_trip_efficiency * depth_of_discharge * utility_scale_storage_capacity
        ),
        daily_load_reduction_distributed = case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ -1 * depth_of_discharge * distributed_storage_capacity,
          charging_indicator == "Discharging" ~ round_trip_efficiency * depth_of_discharge * distributed_storage_capacity
        ),
        daily_load_reduction_both = daily_load_reduction_utility + (daily_load_reduction_distributed / (1 - t_and_d_loss_factor))
      )
    
    # Shouldn't T&D losses be applied to utility charging?
    
    # DO that check from Table F here
    
    # Ideally find a better way to do this
    bau_load_days <- split(bau_load_8760, ceiling(seq_along(bau_load_8760) / 24))
    
    bau_load_days <- map_dbl(bau_load_days, sum)
    
    nth_discharge_day_value <- sort(bau_load_days, decreasing = TRUE)[max_allowable_discharge_cylces_per_year]
    
    discharge_day_indicator <- bau_load_days >= nth_discharge_day_value
    
    discharge_hour_indicator <- rep(discharge_day_indicator, each = 24)
    
    
    storage_load_reduction <- charging_tibble |> 
      pull(daily_load_reduction_both) |> 
      rep(length.out = length(discharge_hour_indicator)) |> 
      (\(x) x * discharge_hour_indicator)() |> 
      unname()
    
    
    
    browser()
    # Not yet worrying about limiting charging on selected months or weekdays/ends
    
    
    if (pair_solar_with_storage) {
      # Grab the utility-scale solar profile (from above box section in this function)
      # Expand the daily pattern to be the same length as that profile
      # Add a datetime column for each hour of the year
      # mutate to create a new column with just the day of the year
      # group by day and sum solar in each day (DURING charging hrs!), put in a new col
      # and also sum utility charging needed in each day and put it in a new col
      # This is just capacity*depth_of_discharge*number_charging_hrs
      # and sum utility discharging needed in ecah day and put it in a new col
      # This is just capacity*depth_of_discharge*numedr_DIScharging_hrs*ROUND_TRIP_EFFICIENCY
      
      # case_when to get an indicator for whether each hour is within a day which
      #   has sufficient charging
      # If a day does:
      # If an hour does, then if the hour has more than enough solar, keep the
      #   exisitng profile the same and then just add the difference between the
      #   solar and the current profile as energy that goes to the grid
      # If an hour doesn't, then multiply the sum of solar capacity col in the hr by the fraction thing
      
      
      
      # I think you basically want:
      # In each charging hr set it to the lesser of max charging capacity and available solar
      # if solar capacity is too much, set the hr to max charging capacity (augmented by loss values if needed)
      # if solar capacity isn't enough, set the hr to the available solar capacity (augmented by loss values as needed)
      # In each discharge hr
      # sum the entirey charging that happened that day, multiply it by round-trip-efficiency, and divide it by the number of discharge hours
      
      
      # But I guess there could be cases where actually in AVERT charging is allowed
      #   to exceed max charging capacity? Like 
      
      
      
      
      
      
      solar_profile_utility <- pull(cfs, `Utility PV`) * utility_solar_pv_capacity_mw
      solar_profile_distributed <- pull(cfs, `Rooftop PV`) * rooftop_solar_pv_capacity_mw
      
      charging_tibble_8760 <- charging_tibble[rep(1:nrow(charging_tibble), length.out = length(solar_profile_utility)), ]
      
      charging_tibble_8760 <- cbind(charging_tibble_8760, solar_profile_utility, solar_profile_distributed)
      
      charging_tibble_8760 <- charging_tibble_8760 |> 
        mutate(
          date_time = datetime_8760,
          day = floor_date(datetime_8760, unit = "day"),
          daily_charging_both = if_else(
            charging_indicator == "Charging",
            daily_load_reduction_both,
            0
          ),
          daily_charging_min_solar_charging = if_else(
            solar_profile_utility <= (-1 * daily_charging_both),
            solar_profile_utility,
            (-1 * daily_charging_both)
          )
        )
      
      charging_tibble_8760t <- charging_tibble_8760 |> 
        group_by(day) |> 
        mutate(
          daily_charging_sum = sum(daily_charging_both),
          daily_solar_sum = sum(solar_profile_utility),
          daily_charging_min_solar_charging_cumsum = cumsum(daily_charging_min_solar_charging),
        )
      
      
      
      charging_tibble_8760t <- charging_tibble_8760t |> 
        mutate(daily_charging_sum = abs(daily_charging_sum))
      
      
      charging_tibble_8760t <- charging_tibble_8760t |> 
        mutate(
          final_profile = case_when(
            charging_indicator == "Charging" & daily_charging_sum <= (daily_charging_min_solar_charging + solar_profile_distributed), # The charging amount, but then adjust by the total amount of solar to account for excess sent to grid 
            charging_indicator == "Charging" & daily_charging_sum > daily_solar_sum & daily_charging_min_solar_charging_cumsum <= daily_charging_sum ~ daily_charging_min_solar_charging,
            charging_indicator == "Charging" & daily_charging_sum > daily_solar_sum & daily_charging_min_solar_charging_cumsum > daily_charging_sum ~ max(daily_charging_sum - daily_charging_min_solar_charging_cumsum, 0), # You'll need to finagle, but the idea is that if the daily charging sum is larger then there's still yet space to charge, so charge up to the difference; but if the cumsum is by then bigger, then there's no space, so it returns 0 
          )
        )
      
      # This setup above feels prone to edge cases...
      
      
      
      
      
      
      charging_tibble_8760 <- charging_tibble_8760 |> mutate(solar_storage_load_reduction = case_when(
        # (Or there's a simpler way to do this with just min())
        charging_indicator == "Charging" & solar_profile_utility >= (-1 * daily_load_reduction_utility) ~ daily_load_reduction_utility,
        charging_indicator == "Charging" & solar_profile_utility < (-1 * daily_load_reduction_utility) ~ solar_profile_utility,
      ))
      
      charging_tibble_8760 <- charging_tibble_8760 |> 
        group_by(day) |> 
        mutate(total_daily_charging = sum(solar_storage_load_reduction, na.rm = TRUE))
      
      charging_tibble_8760 <- charging_tibble_8760 |> 
        mutate(
          case_when(
            charging_indicator == "Discharging" ~ -1 * (total_daily_charging / num_discharge_hrs)
          )
        )
      
      
    }
    
  }
  
  
  
  
  
  # ADD the input validation section!! (As discussed in documentation)
  
}


# If you have an 8760 vector you want to use to model an onsite
#   energy-efficiency program, you'll reduce load on fossil fuel units by the
#   amount of onsite energy you save, PLUS the T&D losses associated with
#   delivering that amount of energy. So your 8760 vector of purely onsite
#   energy reductions should be adjusted up to account for this fact. That's
#   what this function does. Losses from AVERT v4.3 are hard-coded in. Note that
#   rooftop PV and distributed storage are already adjusted up for T&D losses in
#   generate_reduction().
adjust_reduction <- function(
    unadjusted_hourly_load_reduction = NULL,
    project_year = NULL,
    project_region = NULL
) {
  
  t_and_d_loss_factor <- t_and_d_losses |> 
    filter(`Data Year` == project_year) |> 
    pull(project_region)
  
  adjusted_hourly_load_reduction <- unadjusted_hourly_load_reduction / (1 - t_and_d_loss_factor)
  
  return(adjusted_hourly_load_reduction)
  
}



# TRANSMISSION DISTRIBUTION LOSS TABLE ########
t_and_d_losses <- tribble(
  ~`Data year`, ~Texas, ~`Eastern Interconnect`, ~`Western Interconnect`,
  2017, 0.0560, 0.0700, 0.0813,
  2018, 0.0483, 0.0674, 0.0854,
  2019, 0.0538, 0.0720, 0.0860,
  2020, 0.0517, 0.0758, 0.0828,
  2021, 0.0495, 0.0750, 0.0839,
  2022, 0.0458, 0.0723, 0.0867,
  2023, 0.0458, 0.0723, 0.0867
)

t_and_d_losses <- t_and_d_losses |> 
  mutate(
    Carolinas = `Eastern Interconnect`,
    Central = `Eastern Interconnect`,
    Florida = `Eastern Interconnect`,
    `Mid-Atlantic` = `Eastern Interconnect`,
    Midwest = `Eastern Interconnect`,
    `New England` = `Eastern Interconnect`,
    `New York` = `Eastern Interconnect`,
    Southeast = `Eastern Interconnect`,
    Tennessee = `Eastern Interconnect`,
    
    `California` = `Western Interconnect`,
    `Northwest` = `Western Interconnect`,
    `Rocky Mountains` = `Western Interconnect`,
    `Southwest` = `Western Interconnect`
  ) |> 
  select(!c(`Eastern Interconnect`, `Western Interconnect`))
