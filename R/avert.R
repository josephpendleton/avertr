#' Run an AVERT scenario in R
#'
#' `avert()` runs an AVERT scenario in R given an 8760-length (or, in a leap
#' year, 8784-length) numeric vector representing the hourly MW reduction in
#' fossil-fuel generation in a given region and year.
#'
#' It's not obvious from using AVERT, but under the hood every renewable energy
#' or energy efficiency measure is ultimately translated into an hourly change
#' in fossil-fuel generation, and then that hourly generation change is used to
#' model changes in emissions. avertr explicitly divides this process into two
#' steps.
#'
#' Thus if you want to generate a vector representing deployment of renewable
#' energy or energy efficiency measures like you would in AVERT, you must first
#' run [generate_reduction()] to generate the vector, and then pass that vector
#' to `avert()`.
#'
#' For convenience, [generate_and_avert()] performs both operations in one step
#' to prevent the need to re-specify the same arguments in [generate_reduction()]
#' and `avert()`.
#'
#' @param hourly_load_reduction An 8760-length (or, in a leap year,
#' 8784-length) numeric vector giving the hourly MW reduction in fossil-fuel
#' generation This vector can have negative values to represent negative
#' reductions (i.e., increases) in fossil-fuel generation.
#' @param project_year An integer giving the year of the run.
#' @param project_region A string giving the region of the run. It must exactly
#' match one of the 14 AVERT regions.
#' @param avert_main_module_filepath A string giving a filepath to an empty
#' version of the AVERT Main Module (v4.3) which has been saved as a .xlsx file.
#' @param avertr_rdf_filepath A string giving a filepath to the avertr regional
#' data file for the year and region.
#'
#' @returns A list with three elements:
#'    1. `differences_final`, a tibble. In a region with N fossil-fuel generating
#'        units, the tibble will have N * 8760 rows. Each row contains the changes
#'        in emissions (and generation and heat input) associated with a given
#'        fossil-fuel generating unit in a given hour. Those changes appear in
#'        the columns with a "data_" prefix. The other columns give additional
#'        information on the generating unit and the fossil-fuel load bin.
#'    2. `signal_to_noise`, a two-element list containing A. the linear
#'        regression coefficients which result from regressing the generation
#'        change calculated by avertr onto the input generation change, and
#'        B. the r-squared statistic from same regression.
#'    3.  `pct_hourly_load_reduction`, an 8760-length (or, in a leap year,
#'        8784-length) numeric vector where each value represents the percent
#'        reduction in generation compared to the business-as-usual scenario.
#'        These values are presented as numbers ranging from 0 to 1. (E.g., 0.028
#'        would represent a 2.8% decrease in generation.) Negative values
#'        represent increases in generation. AVERT suggests caution when
#'        modeling a scenario where any given hour has a >15% change in
#'        generation.
#' @export
#'
#' @examples
#' \dontrun{
#' # To model a 100 MW reduction in fossil fuel generation in each hour of 2021
#' #   in Florida.
#' avert_out <- avert(
#'   hourly_load_reduction = rep(100, 8760),
#'   project_year = 2021,
#'   project_region = "Florida",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2021/avertr_rdf_Florida_2021.rds"
#'  )
#'
#'  # You can summarize across all the "data_" columns to get the annual results
#'  avert_out |>
#'    purrr::pluck("differences_final") |>
#'    dplyr::summarize(dplyr::across(dplyr::starts_with("data_"), sum))
#' }
avert <- function(hourly_load_reduction, project_year, project_region,
                  avertr_rdf_filepath, avert_main_module_filepath) {
  # DEFINE/LOAD OBJECTS ######

  # If it's a leap year, set the number of hours in the year to 8784, else 8760.
  # (Note that "8760" is used in variable names throughout the code, but refers
  #   to either 8760 or 8784.)
  if (lubridate::leap_year(project_year)) {
    yr_hrs <- 8760 + 24
  } else {
    yr_hrs <- 8760
  }

  load_bin_data_ap_region <- readr::read_rds(avertr_rdf_filepath) |>
    purrr::pluck(paste0("load_bin_data_ap_", project_region))

  bau_case_ap_region <- readr::read_rds(avertr_rdf_filepath) |>
    purrr::pluck(paste0("bau_case_ap_", project_region))

  # Vector of each hour of the year 2023
  datetime_8760 <- seq(
    from = lubridate::ymd_hms(paste0(project_year, "-01-01 00:00:00")),
    by = "1 hour",
    length.out = yr_hrs
  )

  # This is the BAU load
  bau_load_8760 <- bau_case_ap_region |>
    dplyr::distinct(datetime_8760_col, load_8760_col) |>
    dplyr::pull(load_8760_col)

  # NEI emission factors (used to calculate PM2.5, VOCs, and NH3 based on heat)
  nei_efs <- tidyxl::xlsx_cells(
    avert_main_module_filepath,
    sheets = "NEI_EmissionRates"
  )

  infrequent_so2_event_egus_raw <- tidyxl::xlsx_cells(
    avert_main_module_filepath,
    sheets = "Library"
  )


  ## New Hourly Load ==========
  # If an 8760 load reduction vector not passed, then derive one based on input
  #   region and capacity
  if (is.null(hourly_load_reduction)) {
    # Capacity factors
    cfs <- tidyxl::xlsx_cells(
      avert_main_module_filepath,
      sheets = "EERE_Default",
    )

    # Use unpivotr to clean, then filter for appropriate region and project type
    cfs <- cfs |>
      unpivotr::behead("up-left", "Region") |>
      unpivotr::behead("up", "Project Type") |>
      dplyr::filter(
        Region == project_region &
        `Project Type` == project_type &
        row <= 8786
      )

    if (lubridate::leap_year(project_year)) {
      capacity_factor_8760 <- cfs |>
        dplyr::filter(row %in% 3:8786) |>
        dplyr::pull(numeric)
    } else {
      capacity_factor_8760 <- cfs |>
        # NOTE!!! Pretty sure this is the wrong filter, since it leaves in Feb. 29
        #   and removes 12/31. It should be filter(!(row %in% 1419:1442)). But
        #   this is the filter AVERT uses.
        dplyr::filter(row %in% 3:8762) |>
        dplyr::pull(numeric)
    }


    # This is the hourly load reduction
    hourly_load_reduction <- capacity_factor_8760 * project_capacity
  }

  # This is the new hourly net load (i.e., ff load minus renewables)
  new_load_8760 <- bau_load_8760 - hourly_load_reduction

  # This is a vector containing the set of ff load bins for the region
  ff_load_bin_key <- load_bin_data_ap_region |>
    dplyr::distinct(ff_load_bin_col) |>
    dplyr::pull(ff_load_bin_col)



  # BINNIFY #######
  # Based on the project, find the new ff load bins

  # NA vector for now, but will store the load bin associated with each hourly
  #  load value from load_8760s_bau
  ff_load_bin_8760 <- rep(NA, yr_hrs)

  # This function binnifies the vector. Specifically, for each raw hourly load
  #   within load_8760s_bau, it finds the closest load bin which is less than or
  #   equal to the element. It does not simply find the closest load bin —
  #   it never matches to a higher load bin. This is because of the way that we
  #   do the interpolation below.
  binnify <- function(load_8760, ff_load_bin_key) {
    # For each hour of the year
    for (i in 1:yr_hrs) {

      # A bit of a hack, but if the load is higher than or equal to the highest
      #   ff load bin, simply assign it the highest load bin. If it's lower than
      #   or equal to the lowest load bin, assign it the lowest load bin. These
      #   hours get zeroed out in "ZERO EXTREME LOAD HOURS" below. Note that
      #   before they are zeroed out, the highest load bin hours have NA values
      #   for all their data, since in add_next_bin() from avertr_rdf_prepare.R
      #   we set the "next" load bin of the highest load bin to NA.
      if (load_8760[i] >= max(ff_load_bin_key)) {
        ff_load_bin_8760[i] <- max(ff_load_bin_key)
      } else if (load_8760[i] <= min(ff_load_bin_key)) {
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
  ff_load_bin_8760 <- dplyr::bind_cols(
    ff_load_bin_8760_col = ff_load_bin_8760,
    datetime_8760_col = datetime_8760,
    load_8760_col = new_load_8760
  ) |>
    dplyr::relocate(ff_load_bin_8760_col, .after = load_8760_col)



  # ASSIGN DATA VALUES ##########
  # Now we take our binnified load and join on the data measures for the
  #   appropriate load bins from load_bin_data_ap_region. We expect the
  #   relationship to be many-to-many, and rely on the join to expand the size
  #   of the tibble by returning all matches. And throw an error if any given
  #   load bin from ff_load_bin_8760s_bau doesn't have a match in
  #   load_bin_data_ap.
  assigned_data <- dplyr::inner_join(
    ff_load_bin_8760,
    load_bin_data_ap_region,
    by = dplyr::join_by(ff_load_bin_8760_col == ff_load_bin_col),
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
      dplyr::filter(
        dplyr::between(
          datetime_8760_col,
          lubridate::ymd_hms(paste0(project_year, "-05-01 00:00:00")),
          lubridate::ymd_hms(paste0(project_year, "-09-30 23:00:00"))
        )
      )

    # These are the rows not in the ozone season
    non <- assigned_data |>
      dplyr::anti_join(oz, by = dplyr::join_by(datetime_8760_col))

    # For the rows in the ozone season, deselect the non-ozone season data
    oz <- oz |>
      dplyr::select(datetime_8760_col:data_next_bin_generation, dplyr::contains("ozone")) |>
      # Rename so that we can easily bind rows below
      dplyr::rename_with(~ stringr::str_replace(., "_ozone", ""))

    # For the rows in the non-ozone season, deselect the ozone season data
    non <- non |>
      dplyr::select(datetime_8760_col:data_next_bin_generation, dplyr::contains("non")) |>
      # Rename so that we can easily bind rows below
      dplyr::rename_with(~ stringr::str_replace(., "_non", ""))

    # Bind rows, re-order by time
    assigned_data_ozoned <- dplyr::bind_rows(oz, non) |>
      dplyr::arrange(datetime_8760_col)

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
    ff_load_bin_8760 <- dplyr::pull(assigned_data, ff_load_bin_8760_col)

    ff_load_bin_8760_next <- dplyr::pull(assigned_data, ff_load_bin_next_col)

    load_8760 <- dplyr::pull(assigned_data, load_8760_col)

    metadata <- dplyr::select(assigned_data, datetime_8760_col:full_unit_name)

    # Select all the load bin data
    current_data <- assigned_data |>
      dplyr::select(dplyr::contains("data") & !dplyr::contains("next"))

    # Select all the load bin data for the next load bin
    next_data <- assigned_data |>
      dplyr::select(dplyr::contains("data") & dplyr::contains("next"))

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
    interped_inner <- purrr::modify2(current_data, next_data, interpolate_inner)

    # Add back the "metadata"
    interped_inner <- dplyr::bind_cols(metadata, interped_inner)

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
    dplyr::select(c(datetime_8760_col:full_unit_name))

  interped_data_regions_data <- scenario_case |>
    dplyr::select(!c(datetime_8760_col:full_unit_name))

  bau_case_ap_region_data <- bau_case_ap_region |>
    dplyr::select(!c(datetime_8760_col:full_unit_name))

  differences_final <- interped_data_regions_data |>
    purrr::map2(bau_case_ap_region_data, ~ .x - .y) |>
    dplyr::bind_cols(differences_final_metadata) |>
    dplyr::relocate(
      data_generation:data_heat,
      .after = dplyr::last_col()
    )

  # Round to 3 to reflect AVERT's rounding
  differences_final <- differences_final |>
    dplyr::mutate(dplyr::across(c(data_generation:data_heat), ~ round(.x, 3)))



  # ADD PM2.5, VOCs, NH3 #######
  # PM2.5, VOCs, and NH3 are all estimated by multiplying the change in heat
  #   input by an EGU-specific emission rate

  # We read in emission rates above. Now we tidy them with unpivotr.
  nei_efs <- nei_efs |>
    dplyr::filter(row > 4) |>
    unpivotr::behead("up", "year") |>
    unpivotr::behead("up", "data_measure") |>
    unpivotr::behead("left", "region") |>
    unpivotr::behead("left", "state") |>
    unpivotr::behead("left", "plant") |>
    unpivotr::behead("left", "orspl") |>
    unpivotr::behead("left", "unit") |>
    unpivotr::behead("left", "full_name") |>
    unpivotr::behead("left", "county") |>
    unpivotr::behead("left", "orspl|unit|region")

  # Select the appropriate project year, region, and data measures
  nei_efs <- nei_efs |>
    dplyr::filter(
      year == project_year &
      region == project_region &
      data_measure %in% c("PM2.5", "VOCs", "NH3")
    ) |>
    dplyr::select(numeric, data_measure, orspl, unit) |>
    tidyr::pivot_wider(names_from = data_measure, values_from = numeric)

  differences_final <- differences_final |>
    # Set up so that if there are rows in y that don't match, they just get
    #   dropped,but if there are rows in x which don't match, throws an error.
    #   Every unit should have a match in the NEI emission factors (but not
    #   vice versa). And relationship is many-to-one, meaning each row from x
    #   should match with at most one row in y.
    dplyr::inner_join(
      nei_efs,
      by = dplyr::join_by(orispl_code == orspl, unit_code == unit),
      na_matches = "never",
      unmatched = c("error", "drop"),
      relationship = "many-to-one"
    )

  # Calculate NEI data
  differences_final <- differences_final |>
    dplyr::mutate(
      data_pm25 = data_heat * PM2.5,
      data_voc = data_heat * VOCs,
      data_nh3 = data_heat * NH3
    ) |>
    dplyr::select(!PM2.5:NH3)

  # Round to 6 to reflect AVERT's rounding
  differences_final <- differences_final |>
    dplyr::mutate(dplyr::across(data_pm25:data_nh3, ~ round(.x, 6)))



  # ZERO INFREQUENT SO2 EGUS #######
  # Some EGUs have infrequent SO2 emission events; here we zero them

  # The Library sheet has multiple tables in it, so get the start row of the
  #   infrequent SO2 EGU table.
  infrequent_so2_table_start_row <- infrequent_so2_event_egus_raw |>
    dplyr::filter(character == "Table 3: EGUs with infrequent SO2 emission events") |>
    dplyr::pull(row) |>
    # Add four to account for header rows
    (\(x) x + 4)()

  infrequent_so2_table_end_row <- infrequent_so2_event_egus_raw |>
    dplyr::filter(character == "Table 4: VMT assumptions") |>
    dplyr::pull(row) |>
    # Subtract 1 to get to the previous row
    (\(x) x - 1)()

  # Filter for the correct range and remove blank cells
  infrequent_so2_event_egus <- infrequent_so2_event_egus_raw |>
    dplyr::filter(
      dplyr::between(
        row,
        infrequent_so2_table_start_row,
        infrequent_so2_table_end_row
      )
    ) |>
    dplyr::filter(!is_blank)

  # Tidy with unpivotr
  infrequent_so2_event_egus <- infrequent_so2_event_egus |>
    unpivotr::behead("up", "Year") |>
    unpivotr::behead("up", "Region") |>
    unpivotr::behead("up", "Actual SO2 emissions (lb)") |>
    unpivotr::behead("up", "Regionwide SO2 % diff - RDFs") |>
    unpivotr::behead("up", "Regionwide SO2 % diff - corrected") |>
    unpivotr::behead("up", "EGU count") |>
    unpivotr::behead("left-up", "egu_number") |>
    unpivotr::behead("left", "field")

  # Filter for the appropriate project year and region, and the only fields we
  #   really need are the plant and unit IDs
  infrequent_so2_event_egus <- infrequent_so2_event_egus |>
    dplyr::filter(
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
      unpivotr::pack() |>
      dplyr::select(egu_number, value, field) |>
      unpivotr::unpack() |>
      unpivotr::spatter(field)

    infrequent_so2_event_egus <- infrequent_so2_event_egus |>
      dplyr::mutate(Unit = as.character(Unit))

    # Left join, so keeping all rows from differences_final. Ensure that all
    #   EGUs from infrequent_so2_event_egus get matched, and that at most one
    #   EGU gets matched to a given row of differences_final
    differences_final <- differences_final |>
      dplyr::left_join(
        infrequent_so2_event_egus,
        by = dplyr::join_by(orispl_code == ORSPL, unit_code == Unit),
        na_matches = "never",
        unmatched = "error",
        relationship = "many-to-one"
      )

    # Replace all matches (i.e., rows where there's a successful join, s.t.
    #   egu_number is not NA) with 0
    differences_final <- differences_final |>
      dplyr::mutate(
        data_so2 = dplyr::case_when(!is.na(egu_number) ~ 0, TRUE ~ data_so2)
      )

    # Remove the egu_number column from differences_final
    differences_final <- differences_final |>
      dplyr::select(!egu_number)
  }



  # ZERO EXTREME LOAD HOURS #######
  # Some load hours fall above the highest load bin or below the lowest one.
  #   Their values are replaced with 0s. Note that we do not zero the results if
  #   load exactly equals the minimum load bin (to mirror AVERT's behavior).
  #   If load exactly equals the highest load bin, you get an error in AVERT, so
  #   we do zero those cases (not sure how else we'd deal with them since
  #   there's no next load bin to interpolate with.)

  # Add back the BAU load because we also want hours where the BAU load is
  #   outside the load bin (whether or not the new load is) to be 0.
  bau_time_load <- bau_case_ap_region |>
    dplyr::distinct(
      datetime_8760_col, load_8760_col
    )

  differences_final <- differences_final |>
    dplyr::inner_join(
      bau_time_load,
      by = dplyr::join_by(datetime_8760_col),
      na_matches = "never",
      unmatched = "error",
      relationship = "many-to-one"
    )

  # Then make all the hours outside the load bins 0
  differences_final <- differences_final |>
    dplyr::mutate(
      dplyr::across(
        dplyr::contains("data"),
        ~ dplyr::if_else(
          load_8760_col.x >= max(ff_load_bin_key) |
            load_8760_col.x < min(ff_load_bin_key) |
            load_8760_col.y >= max(ff_load_bin_key) |
            load_8760_col.y < min(ff_load_bin_key),
          0,
          .x
        )
      )
    )

  # Finally remove the BAU load column and rename the new column
  differences_final <- differences_final |>
    dplyr::select(!load_8760_col.y) |>
    dplyr::rename(load_8760_col = load_8760_col.x)

  # OTHER STATS #########
  pct_hourly_load_reduction <- hourly_load_reduction / bau_load_8760

  # Vector of total generation change in each hour
  hourly_resulting_generation_change <- differences_final |>
    dplyr::summarize(
      data_generation_summed = sum(data_generation),
      .by = datetime_8760_col
    ) |>
    dplyr::arrange(datetime_8760_col) |>
    dplyr::pull(data_generation_summed)

  # Signal to noise comes from regressing the generation change calculated by
  #   avertr onto the hourly load change (as input by user, either directly
  #   or through the scenario they specify).
  hourly_load_change <- (-1 * hourly_load_reduction)

  signal_to_noise <- stats::lm(
    hourly_resulting_generation_change ~ hourly_load_change
  ) |>
    summary()

  # Only save important parts of summary to reduce space when saving the result
  #   of avert() as a .rds
  signal_to_noise <- signal_to_noise[c("coefficients", "r.squared")]



  # WARNINGS ###########
  if (max(abs(pct_hourly_load_reduction)) > 0.15) {
    message("Warning: At least one hour has a load change exceeding 15% of reference scenario load.")
  }
  if (max(new_load_8760) > max(ff_load_bin_key) |
      min(new_load_8760) < min(ff_load_bin_key)) {
    message("Warning: At least one hour is outside of the calculable range. All results in such hours are set to 0.")
  }



  # REDUCE SIZE ##########
  # Recast to reduce size
  differences_final <- differences_final |>
    dplyr::mutate(
    load_8760_col = as.integer(load_8760_col),
    ff_load_bin_8760_col = as.integer(ff_load_bin_8760_col),
    ff_load_bin_next_col = as.integer(ff_load_bin_next_col),
    state = forcats::as_factor(state),
    county = forcats::as_factor(county),
    fuel_type = forcats::as_factor(fuel_type),
    orispl_code = as.integer(orispl_code)
  )



  # ADD UNITS ##########
  # Add units to variable names
  differences_final <- differences_final |>
    dplyr::rename(
      data_generation_mwh = data_generation,
      data_so2_lbs = data_so2,
      data_nox_lbs = data_nox,
      data_co2_short_tons = data_co2,
      data_heat_mmbtu = data_heat,
      data_pm25_lbs = data_pm25,
      data_voc_lbs = data_voc,
      data_nh3_lbs = data_nh3
    )



  # COMBINE AND RETURN ############
  avertr_results <- dplyr::lst(
    differences_final,
    signal_to_noise,
    pct_hourly_load_reduction
  )

  return(avertr_results)

}



# TRANSMISSION DISTRIBUTION LOSS TABLE ########
# Pasted in from Library sheet in AVERT
t_and_d_losses <- dplyr::tribble(
  ~`Data year`, ~Texas, ~`Eastern Interconnect`, ~`Western Interconnect`,
  2007, 0.06894193465942370000, 0.07478840899903020000, 0.08094769937585830000,
  2008,	0.05089585941649600000,	0.07672390107219970000,	0.08596212820094960000,
  2009,	0.05026463872322240000,	0.07700663620544340000,	0.08652954573857550000,
  2010,	0.06529133451403870000,	0.06918956342763710000,	0.06858678085886890000,
  2011,	0.04624397506570230000,	0.07421176625397580000,	0.10943744975004800000,
  2012,	0.04620790347500600000,	0.07737533780687570000,	0.09518905867776310000,
  2013,	0.05634150662289170000,	0.07795268358488180000,	0.09175452881193960000,
  2014,	0.05413740784579540000,	0.07431319058331550000,	0.09554333412968210000,
  2015,	0.05890145215661860000,	0.07322665131279550000,	0.08827771832684620000,
  2016,	0.06527415966044360000,	0.07004404320746770000,	0.09168606377817930000,
  2017,	0.05598760687340810000,	0.07000015177991270000,	0.08132919825498900000,
  2018,	0.04829616115318550000,	0.06743953613408460000,	0.08539288683593610000,
  2019,	0.05376767203528580000,	0.07197963803331060000,	0.08599100889763030000,
  2020,	0.05172798625659540000,	0.07583310655814570000,	0.08277487483998240000,
  2021,	0.04953529078533420000,	0.07502408315789370000,	0.08387150639006530000,
  2022,	0.04579138198800580000,	0.07230646441341050000,	0.08669949540288220000,
  2023,	0.04579138198800580000,	0.07230646441341050000,	0.08669949540288220000
)

t_and_d_losses <- t_and_d_losses |>
  dplyr::mutate(
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
  dplyr::select(!c(`Eastern Interconnect`, `Western Interconnect`))





