library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)



avertr_test_annual <- function(avertr_results, avert_run_filepath) {
  # Load elements of avertr_results list to environment
  list2env(avertr_results, envir = environment())
  
  # LOAD/PREPARE AVERT DATA ######
  avert_differences_final_egu <- xlsx_cells(
    avert_run_filepath,
    sheets = "Summary"
  )
  
  # Tidy with unpivotr
  avert_differences_final_egu <- avert_differences_final_egu |> 
    filter(row >= 7 & !is_blank) |> 
    behead("up", "variable") |>
    pack() |> 
    select(row, value, variable) |> 
    unpack() |> 
    spatter(variable) |> 
    select(!row) |> 
    relocate(
      `Annual Change in CO2 (tons)`:`Capacity Factor (Calculated, Post-Change) (%)`,
      .after = last_col()
    )
  
  
  
  # AGGREGATE TO YEAR #######
  # avertr.R gives each EGU's output for each hour of the year, but we want 
  #   annual totals, so we aggregate over time. We end up with annual data for
  #   each EGU.
  differences_final_egu <- differences_final |> 
    select(!c(load_8760_col:fuel_type)) |> 
    summarize(
      across(data_generation:data_nh3, sum),
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
    inner_join(
      differences_final_egu,
      by = join_by(
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
    mutate(
      generation_error = data_generation - `Annual Change in Generation (MWh)`,
      pct_generation_error = (generation_error / `Annual Change in Generation (MWh)`) * 100,
      so2_error = data_so2 - `Annual Change in SO2 (lb)`,
      pct_so2_error = (so2_error / `Annual Change in SO2 (lb)`) * 100,
      nox_error = data_nox - `Annual Change in NOx (lb)`,
      pct_nox_error = (nox_error / `Annual Change in NOx (lb)`) * 100,
      co2_error = data_co2 - `Annual Change in CO2 (tons)`,
      pct_co2_error = (co2_error / `Annual Change in CO2 (tons)`) * 100,
      heat_error = data_heat - `Annual Change in Heat Input (MMBtu)`,
      pct_heat_error = (heat_error / `Annual Change in Heat Input (MMBtu)`) * 100,
      pm25_error = data_pm25 - `Annual Change in PM2.5 (lb)`,
      pct_pm25_error = (pm25_error / `Annual Change in PM2.5 (lb)`) * 100,
      voc_error = data_voc - `Annual Change in VOCs (lb)`,
      pct_voc_error = (voc_error / `Annual Change in VOCs (lb)`) * 100,
      nh3_error = data_nh3 - `Annual Change in NH3 (lb)`,
      pct_nh3_error = (nh3_error / `Annual Change in NH3 (lb)`) * 100
    )
  
  ### summarize errors ------------
  # Summary table of the errors between avertr and AVERT.
  error_summary_table_egu <- test_errors_egu |> 
    select(contains("error")) |> 
    summary()
  
  # Largest absolute error from each measure. (Note that the largest absolute
  #   error for each data measure may come from different EGUs.)
  largest_absolute_errors_egu <- test_errors_egu |> 
    select(contains("error")) |> 
    mutate(across(everything(), abs)) |> 
    summarize(across(everything(), ~ max(.x, na.rm = TRUE)))
  
  # All rows where absolute percent errors are greater than 0.1%
  absolute_pct_errors_above_01_egu <- test_errors_egu |> 
    mutate(across(contains("pct"), abs)) |> 
    filter(if_any(contains("pct"), \(x) x > 0.1))
  
  
  ## Region Level ==========
  # Now sum across all EGUs in the region to aggregate results to the region
  #   level. We do this rather than reading in the 1_Annual sheet because the
  #   results in that sheet are substantially rounded.
  ### aggregate to region --------
  test_errors_region <- test_joined_egu |> 
    summarize(
      across(
        c(
          `Annual Change in CO2 (tons)`:`Annual Change in VOCs (lb)`,
          data_generation:data_nh3
        ),
        sum
      )
    )
  
  ### calculate errors --------
  test_errors_region <- test_errors_region |>
    mutate(
      generation_error = data_generation - `Annual Change in Generation (MWh)`,
      pct_generation_error = (generation_error / `Annual Change in Generation (MWh)`) * 100,
      so2_error = data_so2 - `Annual Change in SO2 (lb)`,
      pct_so2_error = (so2_error / `Annual Change in SO2 (lb)`) * 100,
      nox_error = data_nox - `Annual Change in NOx (lb)`,
      pct_nox_error = (nox_error / `Annual Change in NOx (lb)`) * 100,
      co2_error = data_co2 - `Annual Change in CO2 (tons)`,
      pct_co2_error = (co2_error / `Annual Change in CO2 (tons)`) * 100,
      heat_error = data_heat - `Annual Change in Heat Input (MMBtu)`,
      pct_heat_error = (heat_error / `Annual Change in Heat Input (MMBtu)`) * 100,
      pm25_error = data_pm25 - `Annual Change in PM2.5 (lb)`,
      pct_pm25_error = (pm25_error / `Annual Change in PM2.5 (lb)`) * 100,
      voc_error = data_voc - `Annual Change in VOCs (lb)`,
      pct_voc_error = (voc_error / `Annual Change in VOCs (lb)`) * 100,
      nh3_error = data_nh3 - `Annual Change in NH3 (lb)`,
      pct_nh3_error = (nh3_error / `Annual Change in NH3 (lb)`) * 100
    )
  
  ### summarize errors ---------
  error_summary_region <- test_errors_region |>
    select(contains("error"))
  
  # Get the number of data measures which had > 0.1% absolute error at the
  #   region level
  number_absolute_pct_errors_above_01_region <- error_summary_region |> 
    select(contains("pct")) |> 
    unlist() |> 
    abs() |> 
    (\(x) x > 0.1)() |> 
    sum()
  
  
  
  # MESSAGE, COMBINE, AND RETURN #############
  message(paste(nrow(absolute_pct_errors_above_01_egu), "EGUs have a > 0.1% error for at least one of their data measures."))
  
  message(paste(number_absolute_pct_errors_above_01_region, "data measures have a regionwide percent error of > 0.1%."))
  
  annual_test_results <- lst(
    error_summary_region,
    error_summary_table_egu,
    largest_absolute_errors_egu,
  )
  
  # If there are with EGUS > 0.1% error, add the table of such hours to the list
  #   to list to be returned
  if (nrow(absolute_pct_errors_above_01_egu) > 0) {
    annual_test_results <- append(
      annual_test_results,
      lst(absolute_pct_errors_above_01_egu)
    )
  }
  return(annual_test_results)
}



avertr_test_hourly <- function(avertr_results, avert_run_filepath) {
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
    map(
      ~ read_excel(
        avert_run_filepath,
        sheet = .x,
        col_names = FALSE,
        na = c(" ", "Load outside of bin range")
      )
    ) |> 
    suppressMessages()
  
  # The unit name, ORISPL code, and unit ID are split up across three header
  #   rows.
  avert_differences_final_hourly_headers <- avert_hourly_data |> 
    # Grab these three header rows.
    map(~ slice(.x, 1:3)) |> 
    # Then select only columns 8 and 12+. 8 contains the timestamp while 12+
    #   contain all the actual EGU data.
    map(~ select(.x, !(...1:...7) & !(...9:...11))) |> 
    # Then collapse the three header rows into one
    map(map_chr, ~ str_flatten(.x, collapse = "|", na.rm = TRUE))
  
  # Clean the actual EGU data
  avert_differences_final_hourly <- avert_hourly_data |> 
    # Grab the appropriate rows for the EGU data
    map(~ slice(.x, 4:8763)) |> 
    # Select columns 8 and 12+, like above
    map(~ select(.x, !(...1:...7) & !(...9:...11))) |> 
    # Change timestamp to datetime. Some of the values are slightly off the 
    #   hour, so round them.
    map(~ mutate(.x, ...8 = round_date(as_datetime(...8), unit = "hour"))) |> 
    # Mutate data columns to all be numeric
    map(~ mutate(.x, across(!...8, as.numeric))) |>
    # As mentioned above, all hours outside of the bin range are filled with
    #   NAs, and these should be the only hours filled with NAs. Replace them
    #   all with 0s.
    map(~ mutate(.x, across(!...8, \(col) {case_when(is.na(col) ~ 0, TRUE ~ col)}))) |> 
    # Change the column names to align with the header column names we set up
    map2(avert_differences_final_hourly_headers, ~ set_names(.x, .y))
  
  # Pivot the sheets so that each EGU-hour pair is a row, and split up the 
  #  column with ORISPL, unit ID, and name into three columns
  avert_differences_final_hourly <- avert_differences_final_hourly |> 
    map2(
      data_measure_sheet_names,
      ~ pivot_longer(
        .x,
        !Timestamp,
        names_to = "EGU",
        values_to = .y)
    ) |> 
    map(
      ~ separate_wider_delim(
        .x,
        EGU,
        "|",
        names = c("ORISPL Code", "Unit Code", "Unit Name")
      )
    )
  
  # Change ORISPL Code to numeric
  avert_differences_final_hourly <- map(
    avert_differences_final_hourly,
    ~ mutate(.x, `ORISPL Code` = as.numeric(`ORISPL Code`))
  )
  
  # Take the list of the 8 data measure tables and join all of the tables to
  #   one another until you have one big table. Relationship should be one-to-
  #   one, and throw an error if any row is unmatched.
  avert_differences_final_hourly <- avert_differences_final_hourly |>
    reduce(
      inner_join,
      by = join_by(Timestamp, `ORISPL Code`, `Unit Code`, `Unit Name`),
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
  test_joined_hourly <- inner_join(
    differences_final,
    avert_differences_final_hourly,
    by = join_by(
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
    mutate(
      generation_error = data_generation - Generation,
      pct_generation_error = (generation_error / Generation) * 100,
      so2_error = data_so2 - SO2,
      pct_so2_error = (so2_error / SO2) * 100,
      nox_error = data_nox - NOx,
      pct_nox_error = (nox_error / NOx) * 100,
      co2_error = data_co2 - CO2,
      pct_co2_error = (co2_error / CO2) * 100,
      heat_error = data_heat - HeatInput,
      pct_heat_error = (heat_error / HeatInput) * 100,
      pm25_error = data_pm25 - PM25,
      pct_pm25_error = (pm25_error / PM25) * 100,
      voc_error = data_voc - VOCs,
      pct_voc_error = (voc_error / VOCs) * 100,
      nh3_error = data_nh3 - NH3,
      pct_nh3_error = (nh3_error / NH3) * 100
    )
  
  
  ## Summarize Errors ==========
  # Summary table of the errors between avertr and AVERT.
  error_summary_table_hourly <- test_errors_hourly |> 
    select(contains("error")) |> 
    summary()
  
  # Largest absolute error from each measure. (Note that the largest absolute
  #   error for each data measure may come from different hours and/or EGUs.)
  largest_absolute_errors_hourly <- test_errors_hourly |> 
    select(contains("error")) |> 
    mutate(across(everything(), abs)) |> 
    summarize(across(everything(), ~ max(.x, na.rm = TRUE)))
  
  # All rows where absolute errors are greater than 0.001, which should be the 
  #   largest rounding error we get, since all data measure values in a given
  #   hour are rounded to at least 3 decimal places in both AVERT and avertr, 
  #   and there are seemingly cases where arbitrary internal rounding
  #   differences between Excel and R bumps a number up or down by 0.001 when
  #   we round to 3 decimal places.
  # Technically here we check for > 0.001000001 because in practice I've
  #   found that there are many cases where the maximum error caps out at, e.g.,
  #   0.00100000000000011, which is only slightly above the rounding error of
  #   0.001 that we'd expect.
  absolute_pct_errors_above_001_hourly <- test_errors_hourly |> 
    mutate(across(contains("error") & !contains("pct"), abs)) |> 
    filter(if_any(contains("error") & !contains("pct"), \(x) x > 0.001000001))
  
  
  
  # MESSAGE, COMBINE, AND RETURN #############
  message(paste("There are", nrow(absolute_pct_errors_above_001_hourly), "cases where an EGU has a > 0.001 absolute error in a given hour for at least one of its data measures."))
  
  hourly_test_results <- lst(
    error_summary_table_hourly,
    largest_absolute_errors_hourly,
  )
  
  # If there are with EGUS > 0.001 absolute error, add the table of such hours
  #   to the list to be returned
  if (nrow(absolute_pct_errors_above_001_hourly) > 0) {
    hourly_test_results <- append(
      hourly_test_results,
      lst(absolute_pct_errors_above_001_hourly)
    )
  }
  return(hourly_test_results)
}
  

