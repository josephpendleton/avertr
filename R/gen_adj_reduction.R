#' Generate an hourly load reduction vector
#'
#' `generate_reduction()` generates an 8760-length (or, in a leap year,
#' 8784-length) vector representing the hourly fossil-fuel generation reduction
#' associated with energy efficiency and/or renewable energy measures. The
#' resultant vector can be used with [avert()].
#'
#' The arguments specifying load change are "stacked" by default. E.g., passing
#' `reduce_each_hour_by_x_mw = 25` and `utility_solar_pv_capacity_mw = 80` models
#' the reduction in load associated with both reducing each hour by 25 MW and
#' deploying 80 MW of utility-scale solar.
#'
#' `apply_reduction_top_x_pct_hours` and `reduce_x_pct_in_top_hours` work together.
#' They must both be specified in order to have an effect. Only passing a value
#' to one of them does nothing.
#'
#' Values for `rooftop_solar_pv_capacity_mw` are automatically scaled up to
#' account for transmission and distribution losses. Similarly, values passed to
#' `apply_reduction_top_x_pct_hours`/`reduce_x_pct_in_top_hours`,
#' `reduce_annual_generation_by_x_gwh`, and/or `reduce_each_hour_by_x_mw` are
#' expected to be demand-side values, and thus are scaled up to account for
#' transmission and distribution losses. See [adjust_reduction()] for more
#' information on the arguments.
#'
#' The structure of this function largely mirrors the structure of AVERT's Main
#' Module, so see the AVERT User Manual for additional information.
#' @param project_year An integer giving the year of the run.
#' @param project_region A string giving the region of the run. It must exactly
#' match one of the 14 AVERT regions.
#' @param avert_main_module_filepath A string giving a filepath to an empty
#' version of the AVERT Main Module (v4.3) which has been saved as a .xlsx file.
#' @param avertr_rdf_filepath A string giving a filepath to the avertr regional
#' data file for the year and region.
#' @param apply_reduction_top_x_pct_hours A number from 0 to 100 giving the top
#' X% percent of hours in which to reduce load by the amount specified with
#' `reduce_x_pct_in_top_hours`.
#' @param reduce_x_pct_in_top_hours A number from 0 to 100 giving the percent
#' reduction in load that should occur in the top X% of hours, where X is
#' specified with `apply_reduction_top_x_pct_hours`.
#' @param reduce_annual_generation_by_x_gwh A number giving the total GWh by
#' which to reduce annual generation. The reduction is spread out evenly across
#' all hours of the year.
#' @param reduce_each_hour_by_x_mw A number giving the MW by which to reduce
#' load in each hour.
#' @param onshore_wind_capacity_mw A number giving the MW of onshore wind to be
#' deployed.
#' @param offshore_wind_capacity_mw A number giving the MW of offshore wind to
#' be deployed.
#' @param utility_solar_pv_capacity_mw A number giving the MW of utilty solar to
#' be deployed.
#' @param rooftop_solar_pv_capacity_mw A number giving the MW of rooftop solar
#' to be deployed.
#'
#' @returns An 8760-length (or, in a leap year, 8784-length) numeric vector
#' giving the hourly MW reduction resulting from the changes specified in the
#' arguments. Can be passed to [avert()].
#' @export
#' @examples
#' \dontrun{
#' # To model deploying 200 MW offshore wind capacity on top of reducing
#' #   generation by 10% in the top 5% of hours in 2023 in New England
#'
#' reduc_vec <- generate_reduction(
#'   offshore_wind_capacity_mw = 200,
#'   apply_reduction_top_x_pct_hours = 5,
#'   reduce_x_pct_in_top_hours = 10,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#'
#' avert(
#'   hourly_load_reduction = reduc_vec,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#' }
#'
generate_reduction <- function(
    project_year,
    project_region,
    avertr_rdf_filepath,
    avert_main_module_filepath,

    apply_reduction_top_x_pct_hours = 0,
    reduce_x_pct_in_top_hours = 0,

    reduce_annual_generation_by_x_gwh = 0,
    reduce_each_hour_by_x_mw = 0,

    onshore_wind_capacity_mw = 0,
    offshore_wind_capacity_mw = 0,
    utility_solar_pv_capacity_mw = 0,
    rooftop_solar_pv_capacity_mw = 0,

    pair_solar_with_storage = TRUE,
    utility_storage_capacity_mw = 0,
    distributed_storage_capacity_mw = 0,
    duration = 4,
    charging_pattern = "midday",
    manual_charging_vec = NULL,
    apply_profile_weekdays = TRUE,
    apply_profile_weekends = TRUE,
    apply_profile_months = 1:12,
    max_annual_discharge_cycles = 150,
    round_trip_efficiency = 85,
    depth_of_discharge = 80
) {
  # DEFINE/LOAD OBJECTS ######

  # If it's a leap year, set the number of hours in the year to 8784, else 8760.
  # (Note that "8760" is used in variable names throughout the code, but refers
  #   to either 8760 or 8784.)
  if (lubridate::leap_year(project_year)) {
    yr_hrs <- 8760 + 24
  } else {
    yr_hrs <- 8760
  }

  # Scale down the percent values entered by users, since they're assumed to be
  #   0-100, but are more practically used here are fractions from 0 to 1.
  apply_reduction_top_x_pct_hours <- apply_reduction_top_x_pct_hours / 100
  reduce_x_pct_in_top_hours <- reduce_x_pct_in_top_hours / 100
  round_trip_efficiency <- round_trip_efficiency / 100
  depth_of_discharge <- depth_of_discharge / 100

  bau_case_ap_region <- readr::read_rds(avertr_rdf_filepath) |>
    purrr::pluck(paste0("bau_case_ap_", project_region))

  # Vector of each hour of the year
  datetime_8760 <- seq(
    from = lubridate::ymd_hms(paste0(project_year, "-01-01 00:00:00")),
    by = "1 hour",
    length.out = yr_hrs
  )

  # This is the BAU load
  bau_load_8760 <- bau_case_ap_region |>
    dplyr::distinct(datetime_8760_col, load_8760_col) |>
    dplyr::pull(load_8760_col)

  hourly_load_reduction <- rep(0, yr_hrs)

  # This is the T&D losses for the region
  t_and_d_loss_factor <- t_and_d_losses |>
    dplyr::filter(`Data year` == project_year) |>
    dplyr::pull(project_region)



  # APPLY BOXES ######
  # Constructs the 8760 by sequentially applying the boxes from AVERT's
  #   EnterEEREData tab. Each sub-heading of this section represents a different
  #   box. We skip Box 4 because avertr doesn't do EVs (...yet).
  ## Box 1: Enter EE based on the % reduction of regional fossil generation ======
  if (apply_reduction_top_x_pct_hours != 0 & reduce_x_pct_in_top_hours != 0) {

    # The lowest hour in which we apply the reduction. (This calculation comes
    #   sheet CalculateEERE in AVERT.)
    lowest_hr_reduced <- quantile(bau_load_8760, 1 - apply_reduction_top_x_pct_hours)

    # The indices where the top hours are located
    top_hour_indices <- which(bau_load_8760 >= lowest_hr_reduced)

    # Get the appropriate hourly reductions for each hour
    hourly_load_reduction[top_hour_indices] <- bau_load_8760[top_hour_indices] * reduce_x_pct_in_top_hours

    # Since this box is used for modeling energy efficiency measures, where the
    #   reductions occur on-site. Thus, like AVERT, we by default adjust for
    #   T&D losses.
    hourly_load_reduction <- adjust_reduction(
      hourly_load_reduction,
      project_year = project_year,
      project_region = project_region
    )
  }


  ## Box 2: And/or enter EE distributed evenly throughout the year ======
  if (reduce_annual_generation_by_x_gwh != 0 | reduce_each_hour_by_x_mw != 0) {

    # Convert GWh to MWh
    reduce_annual_generation_by_x_mwh <- reduce_annual_generation_by_x_gwh * 10^3

    # Add the reduce_annual_generation_by_x_mwh value, divided evenly over all
    #   8760 hours of the year. Since this box is used for modeling energy
    #   efficiency measures, where the reductions occur on-site. Thus, like
    #   AVERT, we by default adjust for T&D losses.
    hourly_load_reduction <- hourly_load_reduction + adjust_reduction(
      (reduce_annual_generation_by_x_mwh / yr_hrs),
      project_year = project_year,
      project_region = project_region
    )

    # Add the reduce_each_hour_by_x_mw value, divided evenly over all
    #   8760 hours of the year. Since this box is used for modeling energy
    #   efficiency measures, where the reductions occur on-site. Thus, like
    #   AVERT, we by default adjust for T&D losses.
    hourly_load_reduction <- hourly_load_reduction + adjust_reduction(
      reduce_each_hour_by_x_mw,
      project_year = project_year,
      project_region = project_region
    )
  }


  ## Box 3: And/or enter annual capacity of RE resources ======
  if (
    onshore_wind_capacity_mw != 0 |
    offshore_wind_capacity_mw != 0 |
    utility_solar_pv_capacity_mw != 0 |
    rooftop_solar_pv_capacity_mw != 0
  ) {
    # Capacity factors
    cfs <- tidyxl::xlsx_cells(
      avert_main_module_filepath,
      sheets = "EERE_Default",
    )

    # Use unpivotr to clean, filtering for the appropriate region. Gets capacity
    #   factors for all four technologies.
    cfs <- cfs |>
      unpivotr::behead("up-left", "Region") |>
      unpivotr::behead("up", "Project Type") |>
      unpivotr::behead("left", "Date") |>
      unpivotr::behead("left", "Hour") |>
      dplyr::filter(
        Region == project_region &
          row <= 8786
      ) |>
      unpivotr::pack() |>
      dplyr::select(row, value, `Project Type`) |>
      unpivotr::unpack() |>
      unpivotr::spatter(`Project Type`) |>
      dplyr::select(!row)

    cfs <- cfs |>
      # NOTE!!! Pretty sure this is the wrong filter, since it leaves in Feb. 29
      #   and removes 12/31. It should be filter(!(row %in% 1419:1442)). But
      #   this is the filter AVERT uses.
      dplyr::slice(1:yr_hrs) |>
      # Re-arrange columns to do the matrix multiplication below,  keeping the
      #   four technologies in the same order as a user would input capacities for
      #   them
      dplyr::relocate(`Onshore Wind`, `Offshore Wind`, `Rooftop PV`, `Utility PV`)

    # Vector of the four nameplate capacities the user has entered. Each has a
    #   default value of 0.
    capacity_vector <- c(
      "onshore_wind_capacity_mw" = onshore_wind_capacity_mw,
      "offshore_wind_capacity_mw" = offshore_wind_capacity_mw,
      "rooftop_solar_pv_capacity_mw" = rooftop_solar_pv_capacity_mw,
      "utility_solar_pv_capacity_mw" = utility_solar_pv_capacity_mw
    )

    # Multiply each renewable capacity factor vector by the matching
    #   capacity input by the user, then sum them together.
    renewables_tibble <- cfs |>
      dplyr::mutate(
        `Onshore Wind` = `Onshore Wind` * capacity_vector["onshore_wind_capacity_mw"],
        `Offshore Wind` = `Offshore Wind` * capacity_vector["offshore_wind_capacity_mw"],
        `Rooftop PV` = `Rooftop PV` * capacity_vector["rooftop_solar_pv_capacity_mw"],
        `Utility PV` = `Utility PV` * capacity_vector["utility_solar_pv_capacity_mw"],

        # Offshore wind is NA for all inland regions, so replace these before
        #   summing
        `Offshore Wind` = tidyr::replace_na(`Offshore Wind`, 0),

        # The Rooftop PV capacity factor is not adjusted for T&D losses. But
        #   AVERT automatically adjusts it because Rooftop PV generation
        #   happens onsite. Similarly we adjust by default here.
        `Rooftop PV` = adjust_reduction(
          `Rooftop PV`,
          project_year = project_year,
          project_region = project_region
        ),

        summed_renewables = `Onshore Wind` + `Offshore Wind` + `Rooftop PV` + `Utility PV`
      )

    summed_renewables <- renewables_tibble |>
      dplyr::pull(summed_renewables)

    # Add this to whatever load reduction user has already entered.
    hourly_load_reduction <- hourly_load_reduction + summed_renewables
    }


  ## Box 5: And/or enter energy storage data ======
  if (utility_storage_capacity_mw != 0 |
      distributed_storage_capacity_mw != 0) {
    # Initialize a vector of "Idle"s
    charging_pattern_24h <- rep("Idle", 24)

    # Assign charging pattern based on user input and duration
    if (charging_pattern == "midday") {
      charging_pattern_24h[9:(9 + (duration - 1))] <- "Charging"
      charging_pattern_24h[17:(17 + (duration - 1))] <- "Discharging"
    } else if (charging_pattern == "overnight") {
      charging_pattern_24h[1:(1 + (duration - 1))] <- "Charging"
      charging_pattern_24h[17:(17 + (duration - 1))] <- "Discharging"
    } else if (charging_pattern == "manual") {
      charging_pattern_24h <- manual_charging_vec
    }

    # The number of charging hours
    num_charge_hrs <- sum(charging_pattern_24h == "Charging")

    # The number of discharging hours
    num_discharge_hrs <- sum(charging_pattern_24h == "Discharging")

    charging_tibble <- tibble::tibble(
      hour = 1:24,
      charging_indicator = charging_pattern_24h
    ) |>
      dplyr::mutate(
        charging_fraction = dplyr::case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ (1 / num_charge_hrs),
          charging_indicator == "Discharging" ~ (-1 * (round_trip_efficiency / num_discharge_hrs))
        ),
        daily_load_reduction_utility = dplyr::case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ -1 * depth_of_discharge * utility_storage_capacity_mw,
          charging_indicator == "Discharging" ~ round_trip_efficiency * depth_of_discharge * utility_storage_capacity_mw
        ),
        daily_load_reduction_distributed = dplyr::case_when(
          charging_indicator == "Idle" ~ 0,
          charging_indicator == "Charging" ~ -1 * depth_of_discharge * distributed_storage_capacity_mw,
          charging_indicator == "Discharging" ~ round_trip_efficiency * depth_of_discharge * distributed_storage_capacity_mw
        ),
        # We need to adjust the distributed part by T&D losses
        daily_load_reduction_distributed = daily_load_reduction_distributed / (1 - t_and_d_loss_factor),

        # Then we sum to get the total reduction
        daily_load_reduction_both = daily_load_reduction_utility + daily_load_reduction_distributed
      )

    # Now we perform the check to ensure that enough discharging hours have been
    #   specified to allow for full discharging. Note that this is only a concern
    #   if the user has specified their own manual_charging_vec.
    if (!is.null(manual_charging_vec)) {
      # The amount of utility energy to be discharged per day
      utility_discharge_amount <- utility_storage_capacity_mw *
        duration *
        round_trip_efficiency *
        depth_of_discharge

      # Number of utility discharge hours required (dividing by capacity)
      utility_discharge_hrs_required <- utility_discharge_amount /
        utility_storage_capacity_mw

      # The amount of distributed energy to be discharged per day
      distributed_discharge_amount <- distributed_storage_capacity_mw *
        duration *
        round_trip_efficiency *
        depth_of_discharge

      # Number of distributed discharge hours required (dividing by capacity)
      distributed_discharge_hrs_required <- distributed_discharge_amount /
        distributed_storage_capacity_mw

      # If distributed or utility discharge hours required exceeds discharge
      #   hours, give error
      if (
        distributed_discharge_hrs_required > num_discharge_hrs |
        utility_discharge_hrs_required > num_discharge_hrs
      ) {
        stop("With the number of discharge hours manually entered, the system will not be able to discharge correctly. Please increase the number of discharge hours using the manual_charging_vec argument.")
      }
    }

    # Creates a list of length 365 (or, in a leap year, 366) where each element
    #   is a 24-length numeric vector containing the BAU load for each hour of
    #   the given day.
    bau_load_days <- split(
      bau_load_8760,
      ceiling(
        seq_along(bau_load_8760) / 24
      )
    )

    # Creates a (named) numeric vector of length 365 (366) where each element
    #   represents the total BAU load for the given day.
    bau_load_days <- purrr::map_dbl(bau_load_days, sum)

    # Let max_annual_discharge_cycles = n. This is the the value of BAU load
    #   in the nth day (after sorting days from highest to lowest BAU load). It
    #   is the day with the lowest BAU load on which we discharge.
    nth_discharge_day_value <- sort(bau_load_days, decreasing = TRUE)[max_annual_discharge_cycles]

    # This is a 365 (366) length vector which is TRUE for all days >= the
    #   nth_discharge_day_value (i.e., for all days when we discharge) and FALSE
    #   for all other days.
    discharge_day_indicator <- bau_load_days >= nth_discharge_day_value

    # Now repeat each of those 365 (366) values 24 times. We end up with an
    #   8760 (8784)-length vector which is TRUE for hours where we discharge
    #   and FALSE for hours where we don't.
    discharge_hour_indicator <- rep(discharge_day_indicator, each = 24)

    # Finally, we need to restrict to times when charging is allowed (i.e.,
    #   remove all charging and discharging activity at times when it has been
    #   blocked)
    # This vector will be TRUE everywhere where charging is okay (unblocked) and
    #   FALSE everywhere where charging has been blocked
    unblocked_charging_vec <- rep(TRUE, yr_hrs)

    if (!apply_profile_weekdays) {
      # Vector which is TRUE on weekend hours, FALSE on weekday hours
      weekend_hour_indicator <- lubridate::wday(datetime_8760) %in% c("7", "1")

      # Block weekday hours
      unblocked_charging_vec <- unblocked_charging_vec * weekend_hour_indicator
    }

    if (!apply_profile_weekends) {
      # Vector which is TRUE on weekday hours, FALSE on weekend hours
      weekday_hour_indicator <- !(
        lubridate::wday(datetime_8760) %in% c("7", "1")
      )

      # Block weekend hours
      unblocked_charging_vec <- unblocked_charging_vec * weekday_hour_indicator
    }

    if (length(apply_profile_months) < 12) {
      # Vector which is TRUE on hours from included months, FALSE on all other
      #   hours
      month_hour_indicator <- lubridate::month(datetime_8760) %in%
        apply_profile_months

      # Block blocked months
      unblocked_charging_vec <- unblocked_charging_vec * month_hour_indicator
    }









    browser()

    charging_tibble_full <- charging_tibble |>
      dplyr::slice(rep(1:dplyr::n(), length.out = yr_hrs))

    charging_tibble_full <- dplyr::bind_cols(
      datetime_8760_col = datetime_8760,
      charging_tibble_full,
      rooftop_pv = renewables_tibble$`Rooftop PV`,
      utility_pv = renewables_tibble$`Utility PV`
    ) |>
      dplyr::mutate(year_day = lubridate::yday(datetime_8760), .before = hour)


    build_charging_tibble_full <- function(daily_load_reduction, pv, storage_capacity_mw) {

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `Charging allowed?` = as.numeric(discharge_hour_indicator),
          `ES Profile (Unpaired)` = {{daily_load_reduction}} *
            `Charging allowed?` *
            unblocked_charging_vec,
          `Solar (Unpaired)` = -1 * {{pv}},
          `Charging needed in day` = dplyr::if_else(
            (`Charging allowed?` * unblocked_charging_vec) == 1,
            {{storage_capacity_mw}} *
              depth_of_discharge *
              duration,
            0
          ),
          `Disharging needed in day` = dplyr::if_else(
            (`Charging allowed?` * unblocked_charging_vec) == 1,
            {{storage_capacity_mw}} *
              depth_of_discharge *
              round_trip_efficiency *
              duration,
            0
          )
        )

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `Available Solar in day` = sum({{pv}}) *
            `Charging allowed?`,
          .by = year_day
        )

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `Available Solar in day` = `Available Solar in day` * unblocked_charging_vec,
        )

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `Allowable Charging in day` = min(
            -1 * `Available Solar in day`,
            `Charging needed in day`
          ),
          `Allowable Disharging in day` = -1 * round_trip_efficiency,
          `HELPER - flag overloaded hour` = dplyr::if_else(
            (
              `ES Profile (Unpaired)` > 0 &
                `Charging needed in day` < (-1 * `Available Solar in day`) &
                `ES Profile (Unpaired)` > (-1 * `Solar (Unpaired)`)
            ),
            1,
            0
          )
        )

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `HELPER - flag overloaded day` = sum(`HELPER - flag overloaded hour`),
          .by = year_day
        )

      cum_av_charge_vec <- rep(NA, length.out = nrow(charging_tibble_full))

      attach(charging_tibble_full)

      for (i in 1:nrow(charging_tibble_full)) {

        if (`HELPER - flag overloaded day`[i] > 0) {
          if (`ES Profile (Unpaired)`[i] <= 0) {
            cum_av_charge_vec[i] = 0
          } else {
            cum_av_charge_vec[i] =
              (-1 * `Solar (Unpaired)`[i]) + if (i == 1) 0 else cum_av_charge_vec[i - 1]
          }
        } else {
          cum_av_charge_vec[i] = 0
        }

      }

      detach(charging_tibble_full)

      max_allow_charge_vec <- rep(NA, length.out = nrow(charging_tibble_full))

      attach(charging_tibble_full)

      for (i in 1:nrow(charging_tibble_full)) {
        if (cum_av_charge_vec[i] == 0) {
          max_allow_charge_vec[i] = 0
        } else {
          if (cum_av_charge_vec[i] < charging_tibble_full$`Allowable Charging in day`[i]) {
            max_allow_charge_vec[i] = -1 * charging_tibble_full$`Solar (Unpaired)`[i]
          } else {
            max_allow_charge_vec[i] = charging_tibble_full$`Allowable Charging in day`[i] -
              if (i == 1) 0 else cum_av_charge_vec[i - 1]
          }
        }
      }

      detach(charging_tibble_full)

      charging_tibble_full <- charging_tibble_full |>
        dplyr::bind_cols(
          `HELPER - cumulative available charge in day` = cum_av_charge_vec,
          `HELPER - max allowable charge in day` = max_allow_charge_vec
        )

      charging_tibble_full <- charging_tibble_full |>
        dplyr::mutate(
          `ES Profile (Paired)` = dplyr::case_when(
            `ES Profile (Unpaired)` == 0 ~ 0,
            `ES Profile (Unpaired)` < 0 ~ `Allowable Disharging in day` / num_discharge_hrs,
            `HELPER - flag overloaded day` > 0 ~ `HELPER - max allowable charge in day`,
            `Charging needed in day` > `Allowable Charging in day` ~ -1 * `Solar (Unpaired)`,
            .default = `ES Profile (Unpaired)`
          )
        )
    }



    TESTOUT_UTILITY <- build_charging_tibble_full(daily_load_reduction_utility, utility_pv, utility_storage_capacity_mw)
    TESTOUT_DISTRIBUTED <- build_charging_tibble_full(daily_load_reduction_distributed, rooftop_pv, distributed_storage_capacity_mw)

    # Ended here — in the process of comparing results of this function call
    #   against AVERT. Sign flip issue for first few cols, bigger issues in later
    #   calls.
    browser()


    # Eventually, combine the results of the utility and distributed calls





    # Ignore everything below here for now

################################################################################









    # # Now we take that daily load reduction (from both distributed and utility
    # #   storage) we got above and multiply it by the discharge hour indicator.
    # #   FALSEs get treated like 0s, and thus we zero out all non-discharging
    # #   hours from the vector
    # storage_load_reduction <- charging_tibble |>
    #   dplyr::pull(daily_load_reduction_both) |>
    #   rep(length.out = length(discharge_hour_indicator)) |>
    #   (\(x) x * discharge_hour_indicator)() |>
    #   unname()

    # Finally, we need to restrict to times when charging is allowed (i.e.,
    #   remove all charging and discharging activity at times when it has been
    #   blocked)
    if (!apply_profile_weekdays) {
      # Vector which is TRUE on weekend hours, FALSE on weekday hours
      weekend_hour_indicator <- lubridate::wday(datetime_8760) %in% c("7", "1")

      # Zero out all weekday hours
      storage_load_reduction <- storage_load_reduction * weekend_hour_indicator
    }

    if (!apply_profile_weekends) {
      # Vector which is TRUE on weekday hours, FALSE on weekend hours
      weekday_hour_indicator <- !(
        lubridate::wday(datetime_8760) %in% c("7", "1")
      )

      # Zero out all weekday hours
      storage_load_reduction <- storage_load_reduction * weekday_hour_indicator
    }

    if (length(apply_profile_months) < 12) {
      # Vector which is TRUE on hours from included months, FALSE on all other
      #   hours
      month_hour_indicator <- lubridate::month(datetime_8760) %in%
        apply_profile_months

      # Zero out all excluded month hours
      storage_load_reduction <- storage_load_reduction * month_hour_indicator
    }

    # Add the storage load reduction to hourly load reduction (to be returned)
    hourly_load_reduction <- hourly_load_reduction + storage_load_reduction
  }




  browser()


  # STARTING HERE: CODE FOR SOLAR

  if (pair_solar_with_storage) {

    charging_tibble_full <- charging_tibble |>
      dplyr::slice(rep(1:dplyr::n(), length.out = yr_hrs))

    charging_tibble_full <- dplyr::bind_cols(
      datetime_8760_col = datetime_8760,
      charging_tibble_full,
      rooftop_pv = renewables_tibble$`Rooftop PV`,
      utility_pv = renewables_tibble$`Utility PV`
    )

    charging_tibble_full <- charging_tibble_full |>
      dplyr::mutate(year_day = lubridate::yday(datetime_8760), .before = hour)

    # charging_day_list <- charging_tibble_full |>
    #   dplyr::group_by(year_day) |>
    #   dplyr::group_split()










    # Ended here — but I think you should take a different approach going forward:
    #   Write a function within solar_storage_day. solar_storage_day calls that
    #   inner function twice — once for utility storage/solar and once for
    #   distributed storage/solar. The inner function returns a 24-length vector
    #   with load reduction. You sum the results of the two calls of the inner
    #   function, and then solar_storage_day returns the results of that sum (a
    #   vector of length 24).

    solar_storage_day <- function(tib) {

      solar_storage_day_inner <- function(tib, daily_load_reduction, pv) {

        browser()

        # BASICALLY STARTING FROM HERE YOU NEED TO REPRODUCE CALCULATEEERE FROM
        #   AVERT


        # First column: should we run storage dispatch?




        tib_summed <- tib |>
          filter(charging_indicator == "Charging") |>
          summarize(
            total_charging_need = sum({{daily_load_reduction}}) * -1,
            total_solar_gen_charging = sum({{pv}}),
          )


















        if (
          tib_summed$total_solar_gen_charging >= tib_summed$total_charging_need
        ) {

          tib_summed_hour <- tib |>
            filter(charging_indicator == "Charging") |>
            mutate(
              charging_exceeds_solar_hour = ({{daily_load_reduction}} * -1) >= {{pv}}
            ) |>
            summarize(
              charging_exceeds_solar_hour_count = sum(charging_exceeds_solar_hour)
            )

          # The "Solar Is Less Than Charging Needs in Some Charging Hours but More
          #   Than Enough Overall" scenario
          if (
            tib_summed_hour$charging_exceeds_solar_hour_count > 0
          ) {


            # Consider, at some point, an edge case where the above condition is met
            #   but the pv_cumsum_minus_need vector looks something like this:
            #   1041.28712  -610.53026   -95.08357   0   0   0   714.67526  1048.42924
            # I'm pretty sure that what I've written would work even in that case,
            #   but be sure


            pv_cumsum_minus_need <- tib |>
              filter(charging_indicator == "Charging") |>
              mutate(
                pv_cumsum = cumsum({{pv}}),
                pv_cumsum_minus_need = pv_cumsum -
                  tib_summed$total_charging_need
              ) |>
              pull(pv_cumsum_minus_need)


            # Represents charging left over after charging at full capacity for
            #   hours
            max_neg_hr <- max(pv_cumsum_minus_need[pv_cumsum_minus_need < 0])

            # The last hour where we will set charging equal to generation
            max_neg_hr_index <- which.max(
              pv_cumsum_minus_need[pv_cumsum_minus_need < 0]
            )

            # The hour where we will set charging equal to the leftover charging
            #   (i.e., min_neg_hr * -1)
            min_pos_hr_index <- max_neg_hr_index + 1


            change_vec <- c(rep("charging_eq_gen", max_neg_hr_index), "leftover_hr", rep("zero_hr", duration - (max_neg_hr_index + 1)))

            change_vec <- tibble(hour = which(tib$charging_indicator == "Charging"), change = change_vec, pv_cumsum_minus_need = pv_cumsum_minus_need)



            tib2 <- tib

            tib2 <- tib2 |> left_join(change_vec, by = join_by(hour), unmatched = "error", relationship = "one-to-one")

            tib2 <- tib2 |>
              mutate(
                {{daily_load_reduction}} := case_when(
                  change == "charging_eq_gen" ~ {{pv}} * -1,
                  change == "leftover_hr" ~ max_neg_hr,
                  change == "zero_hr" ~ 0,
                  is.na(change) ~ {{daily_load_reduction}}
                )
              )


            # Ended hereish. Problem with column AV in CalculateEERE, includes
            #   additional negative values based on the cumulative sum. Try it
            #   with 365 allowed charging days and see the second day for an
            #   example.
            # For more representative example, probably leave everything default
            #   where you can, esp. depth of discharge and RTE should neither
            #   be 100%


            # Still need to determine if there can be non-consecutive charging
            #   hours

            # Also still need to figure out what would happen if you have a case
            #   where you meet the above conditions (more than enough overall but
            #   too little in >=1 hour) but solar available in just one hour
            #   is greater than system capacity.



























            # 1. You probably have to re-do this with tidyverse syntax — the embracing won't work (I don't think) with the $ selector
            # 2. I think you flipped smallest negative and smallest positive value so far. I think you should actually look for the
            #   smallest negative value, etc. Just test this out with a quick example. (And be careful of the 0 edge case as you
            #   recode)
            # 3. Maybe just look through the macros at this point...
            # 4. How are you sure this will stay constrained to charging hours? I think it will by default, as written...
            # 5. Is this robust to weird default charging patterns? E.g., they're interspersed, day ends with a charging hour, etc.
            #   Along those lines, in AVERT, can you enter discharging hours before charging?










              # Also, # Also, # Also, this will also remove all discharging, and you don't want to
              #   touch that. So probably an if_else() with mutate() is better here
              # tib${{daily_load_reduction}} <- rep(0, 24)
              #
              # tib${{daily_load_reduction}}[1:min_pos_hr_index] <- tib${{pv}}[[1:min_pos_hr_index]]
              # tib${{daily_load_reduction}}[min_neg_hr_index] <- min_pos_hr




              # Doesn't the code above allow the system to charge above greater than its
              #   capacity in some hours, since we're just setting charging equal to solar gen?





              # subtract the total charging need from cumsum(solar generation)
              # Find the smallest positive value
              # Set the next solar gen hour to that smallest positive hour
              # Like the "Solar Exceeds Charging Needs" scenario,

              # Think about edge cases where, e.g., the final hour perfectly meets
              #   demand, there are multiple matching, etc.

              # Also test for an edge case where, e.g., the hour before we hit
              #   sufficient charging is hour 15, but then the final hour where we
              #   actually hit sufficient charging is hour 21 (i.e., they're not
              #   adjacent).
              # And esp. what if between those two (e.g., in hour 19) there's some
              #   discharging that happens, such that in hour 21 we can actually
              #   charge to more than just the difference between hour 15 and full
              #   discharging



              #tib_summed$total_charging_need_utility






            # The "Solar Exceeds Charging Needs" scenario
          } else {
            # In this case, there's enough in each hour, so we can simply subtract
            #   required charging from the solar generation in each hour (which
            #   was already added above).

            # SO THIS RETURNS A VECTOR OF NEGATIVE VALUES REPRESENTING CHARGING,
            #   POSITIVE VALUES REPRESENTING DISCHARGING. PRETTY SURE THIS IS
            #   JUST HOW THE PREVIOUS NON-SOLAR-COUPLED VERSION WORKS.

            # I don't think you have to do anything crazy for distributed vs. utility,
            #   just make sure the distributed values you're using have already been
            #   adjusted, because you should be subtracting more than required to
            #   charge, bc some will be lost in T&D.

          }

          # The "Solar Is Less Than Charging Needs" scenario
        } else {

          # AVERT sets each hour of charging to equal the available solar PV generation (in MWh)
          # AVERT prorates the desired discharging amount by the ratio of actual total charging allowed to demanded charging.

        }



      }



      solar_storage_day_inner(tib, daily_load_reduction_utility, utility_pv)



    }




    # Expand the charging tibble out

    # Make it into a 365 (366) list

    # Write a fucntion to deal with each day, map it across the list


    storage_load_reduction
  }


  solar_storage_day(charging_tibble_full)

  return(hourly_load_reduction)
}







#' Adjust an hourly load reduction vector for T&D losses
#'
#' `adjust_reduction()` adjusts an 8760-length (or, in a leap year, 8784-length)
#' vector to account for transmission and distribution losses.
#'
#' Each element in the vector is scaled by the constant 1 / (1 - t_and_d_loss),
#' where t_and_d_loss is the proportion of electricity lost during transmission
#' and distribution in the given year and region.
#'
#' [avert()]'s `hourly_load_reduction` argument represents a reduction in
#' fossil-fuel generation at the generating units. But sometimes you want to
#' model a reduction in demand. E.g., suppose an energy efficiency program
#' decreases demand by 10 MW each hour. You can't directly pass an 8760-length
#' vector of 10s to [avert()] because a 10 MW decrease in demand will lead to an
#' even greater decrease in generation, since generators normally must supply
#' 10 MW *plus* whatever is lost in transmission and distribution. Thus, we must
#' adjust the vector to be larger.
#'
#' Note that [generate_reduction()] already scales up rooftop PV capacity to
#' account for the fact that it is distributed, so do not call this function on
#' the output of [generate_reduction()] to attempt to adjust the rooftop PV
#' capacity — that would scale the vector twice.
#' @param unadjusted_hourly_load_reduction An 8760-length (or, in a leap year,
#' 8784-length) numeric vector giving the hourly MW reduction before adjustment
#' for transmission and distribution losses.
#' @param project_year An integer giving the year of the run.
#' @param project_region A string giving the region of the run. It must exactly
#' match one of the 14 AVERT regions.
#'
#' @returns A vector with the same length as `unadjusted_hourly_load_reduction`.
#' @export
#' @examples
#' \dontrun{
#' # To model an energy efficiency program which reduces demand by 10 MW in each
#' #   hour of 2023 in Midwest
#'
#' decrease_in_demand <- rep(10, 8760)
#'
#' decrease_in_generation <- adjust_reduction(
#'   unadjusted_hourly_load_reduction = decrease_in_demand,
#'    project_year = 2023,
#'    project_region = "Midwest"
#'  )
#'
#' avert(
#'   hourly_load_reduction = decrease_in_generation,
#'   project_year = 2023,
#'   project_region = "Midwest",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_Midwest_2023.rds"
#' )
#' }
#'
adjust_reduction <- function(
    unadjusted_hourly_load_reduction,
    project_year,
    project_region
) {
  t_and_d_loss_factor <- t_and_d_losses |>
    dplyr::filter(`Data year` == project_year) |>
    dplyr::pull(project_region)

  adjusted_hourly_load_reduction <- unadjusted_hourly_load_reduction / (1 - t_and_d_loss_factor)

  return(adjusted_hourly_load_reduction)

}


