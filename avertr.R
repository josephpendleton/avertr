# A script to run an avertr scenario. project_year is a number representing the
#   the year of the project. project_region is a string representing the region
#   of the project; be sure to use the standard format of the region name
#   exactly as it appears in AVERT resources. project_capacity is a number 
#   representing the capacity of the project. avert_main_module_filepath is the 
#   string of a filepath to an empty AVERT v4.3 Main Module *which has been 
#   converted to .xlsx*.
# project_year, project_region, project_type, and project_capacity are together
#   used to generate an 8760 reduction in net load based on the default capacity
#   factors contained in the AVERT Main Module. project_type can be
#   "Onshore Wind", "Offshore Wind", "Utility PV", or "Rooftop PV".
#   project_capacity is a number in MW. If instead you want to directly use your
#   own 8760 reduction in net load, pass a numeric vector to
#   hourly_load_reduction; if you do this, project_type and project_capacity are
#   ignored. This does not currently account for T&D losses, so you'll need to
#   manually adjust for them if this is supposed to represent e.g. reduction in
#   demand. It also means results will differ from AVERT if you don't adjust.
#   The AVERT User Manual explains how T&D adjustments are made. 
# project_year and project_region are also used to find the appropriate avertr
#   RDF to use, assuming that you have downloaded a standard RDF and stored it
#   in a subdirectory of your current directory named "avertr_rdfs." However, to
#   override this behavior, simply directly enter the string of a filepath to
#   the desired avertr RDF.

library(tidyverse)
library(tidyxl)
library(unpivotr)



avert <- function(project_year, project_region, project_type = NULL,
                  project_capacity = NULL, avert_main_module_filepath,
                  hourly_load_reduction = NULL, avertr_rdf_filepath = NULL) {
  
  # DEFINE/LOAD OBJECTS ######
  
  # If no avertr rdf filepath entered, assumes it's one of the standard runs and
  #   fills in filepath based on region and year.
  if (is.null(avertr_rdf_filepath)) {
    avertr_rdf_filepath <- paste0(
      "./avertr_rdfs/avertr_rdf_",
      project_region,
      "_",
      project_year,
      ".rds"
    )
  }
  
  load_bin_data_ap_region <- read_rds(avertr_rdf_filepath) |> 
    pluck(paste0("load_bin_data_ap_", project_region))
  
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
  
  # NEI emission factors (used to calculate PM2.5, VOCs, and NH3 based on heat)
  nei_efs <- xlsx_cells(
    avert_main_module_filepath,
    sheets = "NEI_EmissionRates"
  )
  
  infrequent_so2_event_egus_raw <- xlsx_cells(
    avert_main_module_filepath,
    sheets = "Library"
  )
  
  
  ## New Hourly Load ==========
  # If an 8760 load reduction vector not passed, then derive one based on input
  #   region and capacity
  if (is.null(hourly_load_reduction)) {
    # Capacity factors
    cfs <- xlsx_cells(
      avert_main_module_filepath,
      sheets = "EERE_Default",
    )
    
    # Use unpivotr to clean, then filter for appropriate region and project type
    cfs <- cfs |> 
      behead("up-left", "Region") |> 
      behead("up", "Project Type") |> 
      filter(
        Region == project_region &
        `Project Type` == project_type &
        row <= 8786
      )
    
    capacity_factor_8760 <- cfs |> 
      # NOTE!!! Pretty sure this is the wrong filter, since it leaves in Feb. 29
      #   and removes 12/31. It should be filter(!(row %in% 1419:1442)). But
      #   this is the filter AVERT uses.
      filter(row %in% 3:8762) |>
      pull(numeric)
    
    # This is the hourly load reduction
    hourly_load_reduction <- capacity_factor_8760 * project_capacity
  }
  
  # This is the new hourly net load (i.e., ff load minus renewables)
  new_load_8760 <- bau_load_8760 - hourly_load_reduction
  
  # This is a vector containing the set of ff load bins for the region
  ff_load_bin_key <- load_bin_data_ap_region |>
    distinct(ff_load_bin_col) |> 
    pull(ff_load_bin_col)
  
 
  
  # BINNIFY #######
  # Based on the project, find the new ff load bins
  
  # NA vector for now, but will store the load bin associated with each hourly
  #  load value from load_8760s_bau
  ff_load_bin_8760 <- rep(NA, 8760)
  
  # This function binnifies the vector. Specifically, for each raw hourly load
  #   within load_8760s_bau, it finds the closest load bin which is less than or
  #   equal to the element. It does not simply find the closest load bin —
  #   it never matches to a higher load bin. This is because of the way that we
  #   do the interpolation below.
  binnify <- function(load_8760, ff_load_bin_key) {
    # For each hour of the year
    for (i in 1:8760) {
      
      # A bit of a hack, but if the load is larger than the largest ff load bin,
      #   simply assign it the highest load bin. If it's lowest than the lowest
      #   load bin, assign it the lowest load bin. These hours get
      #   entirely zeroed out in "ZERO EXTREME LOAD HOURS" below.
      if (load_8760[i] > max(ff_load_bin_key)) {
        ff_load_bin_8760[i] <- max(ff_load_bin_key)
      } else if (load_8760[i] < min(ff_load_bin_key)) {
        ff_load_bin_8760[i] <- min(ff_load_bin_key)
      } else {
        # Gives a vector containing the difference between raw load and each
        #   load bin within the region
        diff <- load_8760[i] - ff_load_bin_key
        
        # Exclude all bins where the load bin exceeds the raw load
        diff[diff < 0] <- NA
        
        # Find the smallest difference
        bin_index <- which.min(diff)
        
        # The load bin with the smallest difference is our load bin in this slot
        ff_load_bin_8760[i] <- ff_load_bin_key[bin_index]
      }
    }
    return(ff_load_bin_8760)
  }
  
  ff_load_bin_8760 <- binnify(new_load_8760, ff_load_bin_key)
  
  # Make the binned load into a tibble and bind it with a column for date time
  #   and a column for the raw load
  ff_load_bin_8760 <- bind_cols(
    ff_load_bin_8760_col = ff_load_bin_8760,
    datetime_8760_col = datetime_8760,
    load_8760_col = new_load_8760
  ) |> 
    relocate(ff_load_bin_8760_col, .after = load_8760_col)
  
  
  
  # ASSIGN DATA VALUES ##########
  # Now we take our binnified load and join on the data measures for the
  #   appropriate load bins from load_bin_data_ap_region. We expect the
  #   relationship to be many-to-many, and rely on the join to expand the size
  #   of the tibble by returning all matches. And throw an error if any given
  #   load bin from ff_load_bin_8760s_bau doesn't have a match in
  #   load_bin_data_ap.
  assigned_data <- inner_join(
    ff_load_bin_8760,
    load_bin_data_ap_region,
    by = join_by(ff_load_bin_8760_col == ff_load_bin_col),
    na_matches = "never",
    unmatched = c("error", "drop"),
    relationship = "many-to-many"
  )
  
  
  
  # OZONE SEASON SPLIT ###############
  # In each region, for each hour of the year, we have a data measure giving
  #   ozone season values and a data measure giving non-ozone season values
  #   (besides for generation, which does not depend on the ozone season). But
  #   we only want the ozone season values for hours in the ozone season, and
  #   the non-ozone season values for values not in the ozone season.
  
  ozone_split <- function(assigned_data) {
    
    # These are the rows in the ozone season
    oz <- assigned_data |> 
      filter(
        between(
          datetime_8760_col,
          ymd_hms("2023-05-01 00:00:00"),
          ymd_hms("2023-09-30 23:00:00")
        )
      )
    
    # These are the rows not in the ozone season
    non <- assigned_data |> 
      anti_join(oz, by = join_by(datetime_8760_col))
    
    # For the rows in the ozone season, deselect the non-ozone season data
    oz <- oz |> 
      select(datetime_8760_col:data_next_bin_generation, contains("ozone")) |> 
      # Rename so that we can easily bind rows below
      rename_with(~ str_replace(., "_ozone", ""))
    
    # For the rows in the non-ozone season, deselect the ozone season data
    non <- non |> 
      select(datetime_8760_col:data_next_bin_generation, contains("non")) |> 
      # Rename so that we can easily bind rows belo
      rename_with(~ str_replace(., "_non", ""))
    
    # Bind rows, re-order by time
    assigned_data_ozoned <- bind_rows(oz, non) |> 
      arrange(datetime_8760_col)
    
    return(assigned_data_ozoned)
  }
  
  assigned_data <- ozone_split(assigned_data)
  
  
  
  # INTERPOLATE ##########
  # Now we interpolate the data. For each hour of the year, for each EGU, we
  #   have its data measure for the load bin and the next load bin. Recall that
  #   the raw generation value (for the EGU, for the hour) falls between the
  #   load bin and the next load bin. Thus, based on the raw generation value,
  #   we linearly interpolate between the two load bins.
  
  interpolate <- function(assigned_data) {
    ff_load_bin_8760 <- pull(assigned_data, ff_load_bin_8760_col)
    
    ff_load_bin_8760_next <- pull(assigned_data, ff_load_bin_next_col)
    
    load_8760 <- pull(assigned_data, load_8760_col)
    
    metadata <- select(assigned_data, datetime_8760_col:full_unit_name)
    
    # Select all the load bin data
    current_data <- assigned_data |>
      select(contains("data") & !contains("next"))
    
    # Select all the load bin data for the next load bin
    next_data <- assigned_data |>
      select(contains("data") & contains("next"))
    
    # Do the interpolation. "cd" and "nd" for "current data" and "next data."
    interpolate_inner <- function(cd, nd) {
      slope <- (cd - nd) / (ff_load_bin_8760 - ff_load_bin_8760_next)
      intercept <- cd - (slope * ff_load_bin_8760)
      val <- (load_8760 * slope) + intercept
      return(val)
    }
    
    # modify2 used because it returns a tibble. interpolate_inner is a
    #   vectorized function and we apply it pairwise to each column of the
    #   current_data and next_data tables.
    interped_inner <- modify2(current_data, next_data, interpolate_inner)
    
    # Add back the "metadata"
    interped_inner <- bind_cols(metadata, interped_inner)
    
    return(interped_inner)
  }
  
  # This is the equivalent of bau_case_ap_region, except the data measure values
  #   here are based on the entered scenario
  scenario_case <- interpolate(assigned_data)
  
  
  
  # GET DIFFERENCES #######
  # Now that we have the data measures for the BAU case and the given scenario,
  #   we subtract the BAU values from the given scenario's values to get the
  #   difference in emissions, i.e., the averted emissions.
  differences_final_metadata <- scenario_case |>
    select(c(datetime_8760_col:full_unit_name))

  interped_data_regions_data <- scenario_case |>
    select(!c(datetime_8760_col:full_unit_name))

  bau_case_ap_region_data <- bau_case_ap_region |>
    select(!c(datetime_8760_col:full_unit_name))
  
  differences_final <- interped_data_regions_data |> 
    map2(bau_case_ap_region_data, ~ .x - .y) |> 
    bind_cols(differences_final_metadata) |> 
    relocate(
      data_generation:data_heat,
      .after = last_col()
    )
  
  # Round to 3 to reflect AVERT's rounding
  differences_final <- differences_final |> 
    mutate(across(c(data_generation:data_heat), ~ round(.x, 3)))
  
  
  
  # ADD PM2.5, VOCs, NH3 #######
  # PM2.5, VOCs, and NH3 are all estimated by multiplying the change in heat
  #   input by an EGU-specific emission rate
  
  # We read in emission rates above. Now we tidy them with unpivotr.
  nei_efs <- nei_efs |> 
    filter(row > 4) |> 
    behead("up", "year") |> 
    behead("up", "data_measure") |> 
    behead("left", "region") |> 
    behead("left", "state") |> 
    behead("left", "plant") |> 
    behead("left", "orspl") |> 
    behead("left", "unit") |> 
    behead("left", "full_name") |> 
    behead("left", "county") |> 
    behead("left", "orspl|unit|region")
  
  # Select the appropriate project year, region, and data measures
  nei_efs <- nei_efs |> 
    filter(
      year == project_year &
      region == project_region &
      data_measure %in% c("PM2.5", "VOCs", "NH3")
    ) |> 
    select(numeric, data_measure, orspl, unit) |> 
    pivot_wider(names_from = data_measure, values_from = numeric)
  
  differences_final <- differences_final |> 
    # Set up so that if there are rows in y that don't match, they just get
    #   dropped,but if there are rows in x which don't match, throws an error.
    #   Every unit should have a match in the NEI emission factors (but not 
    #   vice versa). And relationship is many-to-one, meaning each row from x
    #   should match with at most one row in y.
    inner_join(
      nei_efs,
      by = join_by(orispl_code == orspl, unit_code == unit),
      na_matches = "never",
      unmatched = c("error", "drop"),
      relationship = "many-to-one"
    )
  
  # Calculate NEI data
  differences_final <- differences_final |> 
    mutate(
      data_pm25 = data_heat * PM2.5,
      data_voc = data_heat * VOCs,
      data_nh3 = data_heat * NH3
    ) |> 
    select(!PM2.5:NH3)
  
  # Round to 6 to reflect AVERT's rounding
  differences_final <- differences_final |> 
    mutate(across(data_pm25:data_nh3, ~ round(.x, 6)))
  
  
  
  # ZERO INFREQUENT SO2 EGUS #######
  # Some EGUs have infrequent SO2 emission events; here we zero them
  
  # The Library sheet has multiple tables in it, so get the start row of the
  #   infrequent SO2 EGU table.
  infrequent_so2_table_start_row <- infrequent_so2_event_egus_raw |>
    filter(character == "Table 3: EGUs with infrequent SO2 emission events") |>
    pull(row) |>
    # Add four to account for header rows
    (\(x) x + 4)()
  
  infrequent_so2_table_end_row <- infrequent_so2_event_egus_raw |>
    filter(character == "Table 4: VMT assumptions") |>
    pull(row) |>
    # Subtract 1 to get to the previous row
    (\(x) x - 1)()
  
  # Filter for the correct range and remove blank cells
  infrequent_so2_event_egus <- infrequent_so2_event_egus_raw |>
    filter(
      between(
        row,
        infrequent_so2_table_start_row,
        infrequent_so2_table_end_row
      )
    ) |>
    filter(!is_blank)
  
  # Tidy with unpivotr
  infrequent_so2_event_egus <- infrequent_so2_event_egus |>
    behead("up", "Year") |>
    behead("up", "Region") |>
    behead("up", "Actual SO2 emissions (lb)") |>
    behead("up", "Regionwide SO2 % diff - RDFs") |>
    behead("up", "Regionwide SO2 % diff - corrected") |>
    behead("up", "EGU count") |>
    behead("left-up", "egu_number") |>
    behead("left", "field")
  
  # Filter for the appropriate project year and region, and the only fields we
  #   really need are the plant and unit IDs
  infrequent_so2_event_egus <- infrequent_so2_event_egus |> 
    filter(
      Region == project_region &
      project_year == Year &
      # Note that the "Unit" field indicates unit ID, and that unit IDs are
      #   entered correctly in this sheet (e.g., 001 is not represented as 1)
      (field == "ORSPL" | field == "Unit")
    )
  
  # If there are any infrequent SO2 EGUs in this region and year...
  if (nrow(infrequent_so2_event_egus) > 0) {
    
    # Tidy with unpivotr
    infrequent_so2_event_egus <- infrequent_so2_event_egus |>
      pack() |>
      select(egu_number, value, field) |>
      unpack() |>
      spatter(field)
    
    infrequent_so2_event_egus <- infrequent_so2_event_egus |>
      mutate(Unit = as.character(Unit))
    
    # Left join, so keeping all rows from differences_final. Ensure that all
    #   EGUs from infrequent_so2_event_egus get matched, and that at most one
    #   EGU gets matched to a given row of differences_final
    differences_final <- differences_final |> 
      left_join(
        infrequent_so2_event_egus,
        by = join_by(orispl_code == ORSPL, unit_code == Unit),
        na_matches = "never",
        unmatched = "error",
        relationship = "many-to-one"
      )
    
    # Replace all matches (i.e., rows where there's a successful join, s.t.
    #   egu_number is not NA) with 0
    differences_final <- differences_final |> 
      mutate(
        data_so2 = case_when(!is.na(egu_number) ~ 0, TRUE ~ data_so2)
      )
  }
  
  
  
  # ZERO EXTREME LOAD HOURS #######
  # Some load hours fall above the highest load bin or below the lowest one.
  #   Their values are replaced with 0s.
  differences_final <- differences_final |>
    mutate(
      across(
        contains("data"),
        ~ if_else(
          load_8760_col > max(ff_load_bin_key) | 
            load_8760_col < min(ff_load_bin_key),
          0,
          .x
        )
      )
    )
  
  
  
  # OTHER STATS #########
  pct_hourly_load_change <- abs(hourly_load_reduction / bau_load_8760)
  
  # Vector of total generation change in each hour
  hourly_resulting_generation_change <- differences_final |> 
    summarize(
      data_generation_summed = sum(data_generation),
      .by = datetime_8760_col
    ) |> 
    arrange(datetime_8760_col) |> 
    pull(data_generation_summed)
  
  # Signal to noise comes from regressing the generation change calculated by
  #   avertr onto the hourly load change (as input by user, either directly
  #   or through the scenario they specify).
  hourly_load_change <- (-1 * hourly_load_reduction)
  
  signal_to_noise <- lm(
    hourly_resulting_generation_change ~ hourly_load_change
  ) |> 
    summary()
  
  
  
  # WARNINGS ###########
  if (max(pct_hourly_load_change) > 0.15) {
    message("Warning: At least one hour has a load change exceeding 15% of reference scenario load.")
  }
  if (max(new_load_8760) > max(ff_load_bin_key) | 
      min(new_load_8760) < min(ff_load_bin_key)) {
    message("Warning: At least one hour is outside of the calculable range. All calculated changes in such hours are set to 0.")
  }
  
  
  
  # COMBINE AND RETURN ############
  avertr_results <- lst(
    differences_final,
    signal_to_noise,
    pct_hourly_load_change
  )
  
  return(avertr_results)
}


