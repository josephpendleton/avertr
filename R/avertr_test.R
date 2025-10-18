#' Test avertr results against AVERT results (annual level)
#'
#' `test_annual()` takes an avertr run and an AVERT run and returns information
#' about the differences between the two at the annual level. It can be used to
#' verify that, with the same inputs, avertr and AVERT produce results which
#' differ only negligibly.
#'
#' `test_annual()` checks for differences at the annual level, meaning that it sums
#' changes in emissions (and generation and heat input) across all hours of
#' the year. It checks for differences both at the regional-level (across all
#' generating units) and at the generating-unit level.
#'
#' @param avertr_results The output from an [avert()] run.
#' @param avert_run_filepath A string giving a filepath to a finalized AVERT
#' run which has been saved as a .xlsx file.
#'
#' @returns A list with at least three (possibly four) elements:
#'    1. `error_summary_region`, a one-row tibble containing the difference
#'        and percent difference between the avertr results and AVERT results
#'        at the region-level (i.e., summed up across all individual generating
#'        units.)
#'    2. `error_summary_table_egu`, a table giving summaries of the difference
#'        and percent difference between the avertr results and AVERT results,
#'        where the summaries range across different generating units. E.g., if
#'        the "Mean" value for `pm25_error` is -0.0004137, that means that across
#'        all generating units in the region, the average difference between
#'        that unit's PM2.5 change in avertr and its PM2.5 change in AVERT is
#'        -0.0004137 (lbs).
#'    3.  `largest_absolute_errors_egu`, a one-row tibble where each value is
#'        the largest absolute-value error (or absolute-value percent error)
#'        across all generating units in the region. E.g., if `pct_nox_error` is
#'        0.12, that means that the largest absolute-value percent difference
#'        between avertr and AVERT's results for NOx, across all generating units,
#'        is 0.12%.
#'    4.  (Possibly) `absolute_pct_errors_above_01_egu`, a tibble containing all
#'        generating units which have an absolute-value percent error greater
#'        than 0.1% for at least one of their "`data_`" columns. If there are no
#'        such units, this tibble is not returned, and the returned list only
#'        has three elements.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' avert_out <- avert(
#'   hourly_load_reduction = rep(100, 8760),
#'   project_year = 2021,
#'   project_region = "Florida",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2021/avertr_rdf_Florida_2021.rds"
#' )
#'
#' test_annual(avert_out, "./test_scenarios/100MW_flat_reduction_Florida_2021.xlsx")
#' }
test_annual <- function(avertr_results, avert_run_filepath) {
  # Load elements of avertr_results list to environment
  list2env(avertr_results, envir = environment())

  # LOAD/PREPARE AVERT DATA ######
  avert_differences_final_egu <- tidyxl::xlsx_cells(
    avert_run_filepath,
    sheets = "Summary"
  )

  # Tidy with unpivotr
  avert_differences_final_egu <- avert_differences_final_egu |>
    dplyr::filter(row >= 7 & !is_blank) |>
    unpivotr::behead("up", "variable") |>
    unpivotr::pack() |>
    dplyr::select(row, value, variable) |>
    unpivotr::unpack() |>
    unpivotr::spatter(variable) |>
    dplyr::select(!row) |>
    dplyr::relocate(
      `Annual Change in CO2 (tons)`:`Capacity Factor (Calculated, Post-Change) (%)`,
      .after = dplyr::last_col()
    )



  # AGGREGATE TO YEAR #######
  # avertr.R gives each EGU's output for each hour of the year, but we want
  #   annual totals, so we aggregate over time. We end up with annual data for
  #   each EGU.
  differences_final_egu <- differences_final |>
    dplyr::select(!c(load_8760_col:fuel_type)) |>
    dplyr::summarize(
      dplyr::across(data_generation_mwh:data_nh3_lbs, sum),
      .by = c(orispl_code, unit_code, full_unit_name)
    )



  # TESTING #######
  ## Unit Level ===========
  ### join results ----------
  # Join the avertr results to the AVERT summary sheet by ORISPL and unit name.
  #   Join by unit name rather than unit code because the incorrect unit code is
  #   given in the AVERT results. Sometimes unit names vary, but they're the
  #   same between the  AVERT results and  avertr results. If there's not a
  #   1-to-1 relationship or if an EGU from either avertr or AVERT fails to
  #   join, it throws an error.
  test_joined_egu <- avert_differences_final_egu |>
    dplyr::inner_join(
      differences_final_egu,
      by = dplyr::join_by(
        `ORSPL (Plant ID)` == orispl_code,
        `Unit Name` == full_unit_name
      ),
      na_matches = "never",
      unmatched = "error",
      relationship = "one-to-one"
    )

  ### calculate errors ------------
  # Calculate total and percent errors between avertr and AVERT data
  test_errors_egu <- test_joined_egu |>
    dplyr::mutate(
      generation_error = data_generation_mwh - `Annual Change in Generation (MWh)`,
      pct_generation_error = (generation_error / `Annual Change in Generation (MWh)`) * 100,
      so2_error = data_so2_lbs - `Annual Change in SO2 (lb)`,
      pct_so2_error = (so2_error / `Annual Change in SO2 (lb)`) * 100,
      nox_error = data_nox_lbs - `Annual Change in NOx (lb)`,
      pct_nox_error = (nox_error / `Annual Change in NOx (lb)`) * 100,
      co2_error = data_co2_short_tons - `Annual Change in CO2 (tons)`,
      pct_co2_error = (co2_error / `Annual Change in CO2 (tons)`) * 100,
      heat_error = data_heat_mmbtu - `Annual Change in Heat Input (MMBtu)`,
      pct_heat_error = (heat_error / `Annual Change in Heat Input (MMBtu)`) * 100,
      pm25_error = data_pm25_lbs - `Annual Change in PM2.5 (lb)`,
      pct_pm25_error = (pm25_error / `Annual Change in PM2.5 (lb)`) * 100,
      voc_error = data_voc_lbs - `Annual Change in VOCs (lb)`,
      pct_voc_error = (voc_error / `Annual Change in VOCs (lb)`) * 100,
      nh3_error = data_nh3_lbs - `Annual Change in NH3 (lb)`,
      pct_nh3_error = (nh3_error / `Annual Change in NH3 (lb)`) * 100
    )

  ### summarize errors ------------
  # Summary table of the errors between avertr and AVERT.
  error_summary_table_egu <- test_errors_egu |>
    dplyr::select(dplyr::contains("error")) |>
    summary()

  # Largest absolute error from each measure. (Note that the largest absolute
  #   error for each data measure may come from different EGUs.)
  largest_absolute_errors_egu <- test_errors_egu |>
    dplyr::select(dplyr::contains("error")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), abs)) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ max(.x, na.rm = TRUE)))

  # All rows where absolute percent errors are greater than 0.1%
  absolute_pct_errors_above_01_egu <- test_errors_egu |>
    dplyr::mutate(dplyr::across(dplyr::contains("pct"), abs)) |>
    dplyr::filter(dplyr::if_any(dplyr::contains("pct"), \(x) x > 0.1))


  ## Region Level ==========
  # Now sum across all EGUs in the region to aggregate results to the region
  #   level. We do this rather than reading in the 1_Annual sheet because the
  #   results in that sheet are substantially rounded.
  ### aggregate to region --------
  test_errors_region <- test_joined_egu |>
    dplyr::summarize(
      dplyr::across(
        c(
          `Annual Change in CO2 (tons)`:`Annual Change in VOCs (lb)`,
          data_generation_mwh:data_nh3_lbs
        ),
        sum
      )
    )

  ### calculate errors --------
  test_errors_region <- test_errors_region |>
    dplyr::mutate(
      generation_error = data_generation_mwh - `Annual Change in Generation (MWh)`,
      pct_generation_error = (generation_error / `Annual Change in Generation (MWh)`) * 100,
      so2_error = data_so2_lbs - `Annual Change in SO2 (lb)`,
      pct_so2_error = (so2_error / `Annual Change in SO2 (lb)`) * 100,
      nox_error = data_nox_lbs - `Annual Change in NOx (lb)`,
      pct_nox_error = (nox_error / `Annual Change in NOx (lb)`) * 100,
      co2_error = data_co2_short_tons - `Annual Change in CO2 (tons)`,
      pct_co2_error = (co2_error / `Annual Change in CO2 (tons)`) * 100,
      heat_error = data_heat_mmbtu - `Annual Change in Heat Input (MMBtu)`,
      pct_heat_error = (heat_error / `Annual Change in Heat Input (MMBtu)`) * 100,
      pm25_error = data_pm25_lbs - `Annual Change in PM2.5 (lb)`,
      pct_pm25_error = (pm25_error / `Annual Change in PM2.5 (lb)`) * 100,
      voc_error = data_voc_lbs - `Annual Change in VOCs (lb)`,
      pct_voc_error = (voc_error / `Annual Change in VOCs (lb)`) * 100,
      nh3_error = data_nh3_lbs - `Annual Change in NH3 (lb)`,
      pct_nh3_error = (nh3_error / `Annual Change in NH3 (lb)`) * 100
    )

  ### summarize errors ---------
  error_summary_region <- test_errors_region |>
    dplyr::select(dplyr::contains("error"))

  # Get the number of data measures which had > 0.1% absolute error at the
  #   region level
  number_absolute_pct_errors_above_01_region <- error_summary_region |>
    dplyr::select(dplyr::contains("pct")) |>
    unlist() |>
    abs() |>
    (\(x) x > 0.1)() |>
    sum()



  # MESSAGE, COMBINE, AND RETURN #############
  message(paste(nrow(absolute_pct_errors_above_01_egu), "EGUs have a > 0.1% error for at least one of their data measures."))

  message(paste(number_absolute_pct_errors_above_01_region, "data measures have a regionwide percent error of > 0.1%."))

  annual_test_results <- dplyr::lst(
    error_summary_region,
    error_summary_table_egu,
    largest_absolute_errors_egu,
  )

  # If there are with EGUS > 0.1% error, add the table of such hours to the list
  #   to list to be returned
  if (nrow(absolute_pct_errors_above_01_egu) > 0) {
    annual_test_results <- append(
      annual_test_results,
      dplyr::lst(absolute_pct_errors_above_01_egu)
    )
  }

  return(annual_test_results)

}



#' Test avertr results against AVERT results (hourly level)
#'
#' `test_hourly()` takes an avertr run and an AVERT run and returns information
#' about the differences between the two at the hourly level. It can be used to
#' verify that, with the same inputs, avertr and AVERT produce results which
#' differ only negligibly.
#'
#' `test_hourly()` checks for differences at the hourly level, meaning that checks
#' for differences between avertr and AVERT in each hour of the year, for each
#' generating unit.
#'
#' @param avertr_results The output from an [avert()] run.
#' @param avert_run_filepath A string giving a filepath to a finalized AVERT
#' run which has been saved as a .xlsx file.
#'
#' @returns A list with at least two (possibly three) elements:
#'    1. `error_summary_table_hourly`, a table giving summaries of the
#'        difference and percent difference between the avertr results and AVERT
#'        results, where the summaries range across different generating-unit-hours.
#'        A generating-unit-hour is a given generating unit in a given hour. So
#'        if there are N generating units there will be 8760 * N generating-unit
#'        hours (in a non-leap year). E.g., if the "Mean" value for `pm25_error`
#'        is -0.0004137, that means that across all generating-unit-hours in the
#'        region, the average difference between that unit's PM2.5 change in
#'        avertr and its PM2.5 change in AVERT is -0.0004137 (lbs).
#'    2. `largest_absolute_errors_hourly`, a one-row tibble where each value is
#'        the largest absolute-value error (or absolute-value percent error)
#'        across all generating units-hours in the region. E.g., if `pct_nox_error`
#'        is 0.12, that means that the largest absolute-value percent difference
#'        between avertr and AVERT's results for NOx, across all generating-unit-hours,
#'        is 0.12%.
#'    3.  (Possibly) `absolute_errors_above_0001_hourly`, a tibble containing all
#'        generating-unit-hours which have an absolute-value error greater than
#'        0.001 for at least one of their "`data_`" columns. If there are no
#'        such generating-unit-hours, this tibble is not returned, and the
#'        returned list only has two elements.
#' @export
#'
#' @examples
#' \dontrun{
#' avert_out <- avert(
#'   hourly_load_reduction = rep(100, 8760),
#'   project_year = 2021,
#'   project_region = "Florida",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2021/avertr_rdf_Florida_2021.rds"
#' )
#'
#' test_hourly(avert_out, "./test_scenarios/100MW_flat_reduction_Florida_2021.xlsx")
#' }
test_hourly <- function(avertr_results, avert_run_filepath) {
  # Load elements of avertr_results list to environment
  list2env(avertr_results, envir = environment())

  # LOAD/PREPARE AVERT DATA #############
  # These are the 8 different data measures, as they appear in the names of the
  #   worksheets that result from an AVERT run
  data_measure_sheet_names <- c(
    "Generation",
    "HeatInput",
    "SO2",
    "NOx",
    "CO2",
    "PM25",
    "VOCs",
    "NH3"
  )

  # Load the 8 data measure sheets. Treat blanks or the string "Load bin out of
  #   range" as NA. This ensures that if there are any load bins outside of the
  #   range, those hours are entirely filled with NAs.
  avert_hourly_data <- data_measure_sheet_names |>
    purrr::map(
      ~ readxl::read_excel(
        avert_run_filepath,
        sheet = .x,
        col_names = FALSE,
        na = c(" ", "Load outside of bin range")
      )
    ) |>
    suppressMessages()

  # Get the value from cell B4, which should be the year
  project_year <- avert_hourly_data[[1]] |>
    dplyr::slice(4) |>
    dplyr::pull(...2) |>
    as.numeric()

  if (lubridate::leap_year(project_year)) {
    yr_hrs <- 8760 + 24
  } else {
    yr_hrs <- 8760
  }

  # The unit name, ORISPL code, and unit ID are split up across three header
  #   rows.
  avert_differences_final_hourly_headers <- avert_hourly_data |>
    # Grab these three header rows.
    purrr::map(~ dplyr::slice(.x, 1:3)) |>
    # Then select only columns 8 and 12+. 8 contains the timestamp while 12+
    #   contain all the actual EGU data.
    purrr::map(~ dplyr::select(.x, !(...1:...7) & !(...9:...11))) |>
    # Then collapse the three header rows into one
    purrr::map(purrr::map_chr, ~ stringr::str_flatten(.x, collapse = "|", na.rm = TRUE))

  # Clean the actual EGU data
  avert_differences_final_hourly <- avert_hourly_data |>
    # Grab the appropriate rows for the EGU data
    purrr::map(~ dplyr::slice(.x, 4:(yr_hrs + 3))) |>
    # Select columns 8 and 12+, like above
    purrr::map(~ dplyr::select(.x, !(...1:...7) & !(...9:...11))) |>
    # Change timestamp to datetime. Some of the values are slightly off the
    #   hour, so round them.
    purrr::map(~ dplyr::mutate(.x, ...8 = lubridate::round_date(lubridate::as_datetime(...8), unit = "hour"))) |>
    # Mutate data columns to all be numeric
    purrr::map(~ dplyr::mutate(.x, dplyr::across(!...8, as.numeric))) |>
    # As mentioned above, all hours outside of the bin range are filled with
    #   NAs, and these should be the only hours filled with NAs. Replace them
    #   all with 0s.
    purrr::map(~ dplyr::mutate(.x, dplyr::across(!...8, \(col) {dplyr::case_when(is.na(col) ~ 0, TRUE ~ col)}))) |>
    # Change the column names to align with the header column names we set up
    purrr::map2(avert_differences_final_hourly_headers, ~ purrr::set_names(.x, .y))

  # Pivot the sheets so that each EGU-hour pair is a row, and split up the
  #  column with ORISPL, unit ID, and name into three columns
  avert_differences_final_hourly <- avert_differences_final_hourly |>
    purrr::map2(
      data_measure_sheet_names,
      ~ tidyr::pivot_longer(
        .x,
        !Timestamp,
        names_to = "EGU",
        values_to = .y)
    ) |>
    purrr::map(
      ~ tidyr::separate_wider_delim(
        .x,
        EGU,
        "|",
        names = c("ORISPL Code", "Unit Code", "Unit Name")
      )
    )

  # Change ORISPL Code to numeric
  avert_differences_final_hourly <- purrr::map(
    avert_differences_final_hourly,
    ~ dplyr::mutate(.x, `ORISPL Code` = as.numeric(`ORISPL Code`))
  )

  # Take the list of the 8 data measure tables and join all of the tables to
  #   one another until you have one big table. Relationship should be one-to-
  #   one, and throw an error if any row is unmatched.
  avert_differences_final_hourly <- avert_differences_final_hourly |>
    purrr::reduce(
      dplyr::inner_join,
      by = dplyr::join_by(Timestamp, `ORISPL Code`, `Unit Code`, `Unit Name`),
      na_matches = "never",
      unmatched = "error",
      relationship = "one-to-one"
    )



  # TESTING ###########
  ## Join Results ==========
  # Join the avertr hourly results to the AVERT hourly results we prepared above
  #   by hour, ORISPL, and unit name. Join by unit name rather than unit code
  #   because the incorrect unit code is given in the hourly results. Sometimes
  #   unit names vary, but they're the same between the hourly AVERT results and
  #   avertr results. Relationship should be one-to-one and there should
  #   be no unmatched rows.
  test_joined_hourly <- dplyr::inner_join(
    differences_final,
    avert_differences_final_hourly,
    by = dplyr::join_by(
      datetime_8760_col == Timestamp,
      orispl_code == `ORISPL Code`,
      full_unit_name == `Unit Name`
    ),
    na_matches = "never",
    unmatched = "error",
    relationship = "one-to-one"
  )


  ## Calculate Errors ==========
  test_errors_hourly <- test_joined_hourly |>
    dplyr::mutate(
      generation_error = data_generation_mwh - Generation,
      pct_generation_error = (generation_error / Generation) * 100,
      so2_error = data_so2_lbs - SO2,
      pct_so2_error = (so2_error / SO2) * 100,
      nox_error = data_nox_lbs - NOx,
      pct_nox_error = (nox_error / NOx) * 100,
      co2_error = data_co2_short_tons - CO2,
      pct_co2_error = (co2_error / CO2) * 100,
      heat_error = data_heat_mmbtu - HeatInput,
      pct_heat_error = (heat_error / HeatInput) * 100,
      pm25_error = data_pm25_lbs - PM25,
      pct_pm25_error = (pm25_error / PM25) * 100,
      voc_error = data_voc_lbs - VOCs,
      pct_voc_error = (voc_error / VOCs) * 100,
      nh3_error = data_nh3_lbs - NH3,
      pct_nh3_error = (nh3_error / NH3) * 100
    )


  ## Summarize Errors ==========
  # Summary table of the errors between avertr and AVERT.
  error_summary_table_hourly <- test_errors_hourly |>
    dplyr::select(dplyr::contains("error")) |>
    summary()

  # Largest absolute error from each measure. (Note that the largest absolute
  #   error for each data measure may come from different hours and/or EGUs.)
  largest_absolute_errors_hourly <- test_errors_hourly |>
    dplyr::select(dplyr::contains("error")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), abs)) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ max(.x, na.rm = TRUE)))

  # All rows where absolute errors are greater than 0.001, which should often be
  #   the largest rounding error we get, since all data measure values in a
  #   givenhour are rounded to at least 3 decimal places in both AVERT and
  #   avertr, and there are seemingly cases where arbitrary internal rounding
  #   differences between Excel and R bumps a number up or down by 0.001 when
  #   we round to 3 decimal places.
  # Technically here we check for > 0.001000001 because in practice I've
  #   found that there are many cases where the maximum error caps out at, e.g.,
  #   0.00100000000000011, which is only slightly above the rounding error of
  #   0.001 that we'd expect.
  absolute_errors_above_0001_hourly <- test_errors_hourly |>
    dplyr::mutate(dplyr::across(dplyr::contains("error") & !dplyr::contains("pct"), abs)) |>
    dplyr::filter(dplyr::if_any(dplyr::contains("error") & !dplyr::contains("pct"), \(x) x > 0.001000001))



  # MESSAGE, COMBINE, AND RETURN #############
  message(paste("There are", nrow(absolute_errors_above_0001_hourly), "cases where an EGU has a > 0.001 absolute error in a given hour for at least one of its data measures."))

  hourly_test_results <- dplyr::lst(
    error_summary_table_hourly,
    largest_absolute_errors_hourly,
  )

  # If there are with EGUS > 0.001 absolute error, add the table of such hours
  #   to the list to be returned
  if (nrow(absolute_errors_above_0001_hourly) > 0) {
    hourly_test_results <- append(
      hourly_test_results,
      dplyr::lst(absolute_errors_above_0001_hourly)
    )
  }

  return(hourly_test_results)

}


