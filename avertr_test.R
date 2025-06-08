library(tidyverse)
library(readxl)
library(tidyxl)
library(unpivotr)



avertr_test_annual <- function(avertr_results, avert_run_filepath) {
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
  # avertr.R gives each EGU's output for each hour of the year, but we want annual
  #   totals, so we aggregate over time. We get annual data for each EGU.
  differences_final_egu <- differences_final |> 
    select(!c(load_8760_col:fuel_type)) |> 
    summarize(
      across(data_generation:data_nh3, sum),
      .by = c(orispl_code, unit_code, full_unit_name)
    )
  
  
  
  # TESTING #######
  ## Unit Level ===========
  ### join results ----------
  # Join the avertr results to the AVERT summary sheet by ORISPL and unit ID. If
  #   there's not a 1-to-1 relationship (i.e., a different number of EGUs between
  #   the AVERT and avertr outputs), or if an EGU from either avertr or AVERT
  #   fails to join, it throws an error
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
  absolute_pct_errors_above_0pt1_egu <- test_errors_egu |> 
    mutate(across(contains("pct"), abs)) |> 
    filter(if_any(contains("pct"), \(x) x > 0.1))
  
  
  ## Region Level ==========
  # Now sum across all EGUs in teh region to aggregate results to the region
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
  
  # All rows where absolute percent errors are greater than 0.1%
  absolute_pct_errors_above_0pt1_region <- error_summary_region |> 
    mutate(across(contains("pct"), abs)) |> 
    filter(if_any(contains("pct"), \(x) x > 0.1))
  
  
  
  # WARN, COMBINE, AND RETURN #############
  annual_test_results <- lst(
    error_summary_region,
    error_summary_table_egu,
    largest_absolute_errors_egu,
  )
  
  # If there are with EGUS > 0.1% error, warn and add to list to be returned
  if (nrow(absolute_pct_errors_above_0pt1_egu) > 0) {
    message("Warning: At least one EGU has a percent error of > 0.1% for
            at least one data measure.")
    annual_test_results <- append(
      annual_test_results,
      absolute_pct_errors_above_0pt1_egu
    )
  }
  
  # If there are any pollutants at the region level with > 0.1% error, warn and
  #   add to list to be returned
  if (nrow(absolute_pct_errors_above_0pt1_region) > 0) {
    message("Warning: At least one data measure has a regionwide percent error
    of > 0.1%.")
    annual_test_results <- append(
      annual_test_results,
      absolute_pct_errors_above_0pt1_region
    )
  }
  return(annual_test_results)
}



avertr_test_hourly <- function(avertr_results, avert_run_filepath) {
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
  
  
  
  # Try it again in readxl. Read back in the sheets from online
  # readxl in the sheets. This should automatically remove blank cols at the
  #   end. Rows that should be read in are fixed.
  # Make sure that it uses as least 8763 (or wtv) rows to guess types 
  # AND BASE the number of cols it reads on the distinct number of EGUS in the
  #   avertr test file. THis assumes regions match, but if they don't you'll
  #   end up getting an error anyway when you try to join based on ORISPL and
  #   unit id below.
  # BEFORE even combining w header rows: Mutate across cols where
  #   is.character (bc these are ones w the "Load outside
  #   bin", should really only be first col) and then case when them to replace
  #   the "Load outside bin" test w a 0 and coerce to numeric. Then mutate
  #   across cols where there's any na. These cols are the ones w missing values
  #   either from deleted SO2 events or missing bc load hour outside bin. Replace
  #   NAs w 0.
  
  
  
  
  
  browser()
  
  
  
  
  
  
  avert_hourly_data <- data_measure_sheet_names |> 
    map(
      ~ read_excel(
        avert_run_filepath,
        sheet = .x,
        col_names = FALSE,
        na = c(" ", "Load outside of bin range")
      )
    )
  
  
  
  
  
  
  # The unit name, ORISPL code, and unit ID are split up across three header rows.
  #   Read in the three and then concatenate them.
  avert_differences_final_hourly_headers <- avert_hourly_data |> 
    map(~ slice(.x, 1:3)) |> 
    map(~ select(.x, !(...1:...7) & !(...9:...11))) |> 
    map(map_chr, ~ str_flatten(.x, collapse = "|", na.rm = TRUE))
  
  # Read in all 8 data measures, assigning each sheet the column names created
  #   above
  avert_differences_final_hourly <- avert_hourly_data |> 
    map(~ slice(.x, 4:8763)) |> 
    map(~ select(.x, !(...1:...7) & !(...9:...11))) |> 
    map(~ mutate(.x, ...8 = round_date(as_datetime(...8), unit = "hour"))) |> 
    map(~ mutate(.x, across(!...8, as.numeric))) |> 
    map(~ mutate(.x, across(!...8, \(col) {case_when(is.na(col) ~ 0, TRUE ~ col)}))) |> 
    map2(avert_differences_final_hourly_headers, ~ set_names(.x, .y))
  
  # Pivot the sheets so that each EGU-hour pair is a row, and split up the column
  #   with ORISPL, unit code, and name into three columns.
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
  
  test_joined_hourly <- avert_differences_final_hourly |>
    reduce(inner_join, by = join_by(Timestamp, `ORISPL Code`, `Unit Code`, `Unit Name`), unmatched = c("error", "error"), relationship = "one-to-one")
  
  
  
  
  
  
  test_joined_hourly
  
  
  
  

  
  
  # Left off here
  
  
  
  
  
  
  
  
  
  
  # TESTING ###########
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
  
  ### summarize errors ---------
  # Summary table of the errors between AVERT and avertr. Note that percent
  #   errors may be NA where the AVERT value is 0. (To check, filter
  #   for cases where percent error is NA but AVERT value is not 0. There should
  #   be no such cases.)
  test_errors_hourly |>
    select(contains("error")) |>
    summary()
  
  # Largest absolute error from each measure. (Note that the pct error
  #   and error for each data measure may come from different rows.)
  test_errors_hourly |>
    select(contains("error")) |>
    mutate(across(everything(), abs)) |>
    summarize(across(everything(), ~ max(.x, na.rm = TRUE))) |>
    print(width = Inf)
  
  # Histograms for all the percent errors
  test_errors_hourly |>
    select(contains("pct")) |>
    iwalk(~ hist(.x, main = .y, breaks = 500))
  
  # All rows where absolute percent errors are greater than 0.1%
  test_errors_hourly |>
    mutate(across(contains("pct"), abs)) |>
    filter(if_any(contains("pct"), \(x) x > 0.1))
}






# TIDYXL VERSION

# avert_differences_final_egu_hourly <- map(
#   data_measure_sheet_names,
#   ~ xlsx_cells(avert_run_filepath, sheets = .x)
# )
# 
# browser()
# 
# 
# 
# 
# avert_differences_final_egu_hourlyALT_TEST <- map2(
#   avert_differences_final_egu_hourly,
#   data_measure_sheet_names,
#   function(data_measure_table, data_measure_name) {
#     data_measure_table <- data_measure_table |>
#       # Filters out extra rows at the end, and selects only the columns with
#       #   data (>= 12) and the column with the datetime (8).
#       filter(row <= 8763 & (col >= 12 | col == 8)) |>
#       # Datetime information doesn't appear in the date column, instead only
#       #   in the content column. I think this isn't supposed to happen. But
#       #   anyway, for those cells, change data_type to have "content." Now
#       #   when we pack() below, the data_type column tells pack() to pull
#       #   from the content column.
#       mutate(
#         data_type = if_else(
#           data_type == "date (ISO8601)",
#           "content",
#           data_type
#         )
#       ) |>
#       behead("left", "timestamp") |>
#       behead("up", "orspl") |>
#       behead("up", "unit_id") |>
#       behead("up", "unit_name") |>
#       mutate(timestamp = as_datetime(timestamp)) |>
#       mutate(
#         numeric = if_else(character == "Load outside of bin range", 0, numeric),
#         character = if_else(character == "Load outside of bin range", NA, character)
#       ) |> 
#       pack() |>
#       select(row, value, timestamp, orspl, unit_id, unit_name) |>
#       unpack() |>
#       mutate(data_measure = data_measure_name) |>
#       spatter(data_measure) |>
#       select(!row)
#     
#     return(data_measure_table)
#   }
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # See if it loads in entirely blank cols like GT. If it does you'll need to
# #   remove those blanks. And then consider just if_else to change all blanks
# #   to have is_blank false and numeric 0, and change all "load bin" ones to
# #   have NA in character and 0 in numeric. Double check the remaining cols to
# #   ensure that they'll be okay, but should be fine.
# 
# # Tidy each of the data measure tables with unpivotr
# avert_differences_final_egu_hourly <- map2(
#   avert_differences_final_egu_hourly,
#   data_measure_sheet_names,
#   function(data_measure_table, data_measure_name) {
#     data_measure_table <- data_measure_table |>
#       # Filters out extra rows at the end, and selects only the columns with
#       #   data (>= 12) and the column with the datetime (8).
#       filter(row <= 8763 & (col >= 12 | col == 8)) |>
#       # Datetime information doesn't appear in the date column, instead only
#       #   in the content column. I think this isn't supposed to happen. But
#       #   anyway, for those cells, change data_type to have "content." Now
#       #   when we pack() below, the data_type column tells pack() to pull
#       #   from the content column.
#       mutate(
#         data_type = if_else(
#           data_type == "date (ISO8601)",
#           "content",
#           data_type
#         )
#       ) |>
#       behead("left", "timestamp") |>
#       behead("up", "orspl") |>
#       behead("up", "unit_id") |>
#       behead("up", "unit_name") |>
#       mutate(timestamp = as_datetime(timestamp)) |>
#       # Filter out any cases where the load is outside bin range. These cells
#       #   either contain the below string, or are blank.
#       filter(
#         !(!is.na(character) & character == "Load outside of bin range") &
#           !is_blank
#       ) |>
#       pack() |>
#       select(row, value, timestamp, orspl, unit_id, unit_name) |>
#       unpack() |>
#       mutate(data_measure = data_measure_name) |>
#       spatter(data_measure) |>
#       select(!row)
#     
#     return(data_measure_table)
#   }
# )
# 
# 
# 
# 
# 
# # MAKE SURE you're accounting for the fact that rare SO2 plants have blanks,
# #   not 0s.
# 
# 
# 
# 
# # Add join safety conditions?
# avert_differences_final_egu_hourly <- avert_differences_final_egu_hourly |>
#   reduce(left_join, by = join_by(timestamp, orspl, unit_id, unit_name))
# 
# avert_differences_final_egu_hourly <- avert_differences_final_egu_hourly |>
#   mutate(unit_id = word(unit_name, -1))
# 
# avert_differences_final_egu_hourly <- avert_differences_final_egu_hourly |>
#   mutate(timestamp = round_date(timestamp, unit = "hour"))

