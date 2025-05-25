# A script to test outputs from avertr against outputs from AVERT. It tests at
#   the annual unit level, annual region level, and/or hourly unit level.



# SET UP ########
# Assumes you have altered avertr.R by specifying your desired region and
#   load change
source("./avertr.R")




# RUN ###########
avertred <- avert(
  project_capacity = 3154,
  project_region = "New England",
  project_year = "2023",
  project_type = "Offshore Wind"
)







# ASSIGN FILEPATH ##########
# Enter the filepath to the AVERT main module workbook (converted from its
#   default .xlsb format to either .xls or .xlsx) where you've run the scenario
#   that you wish to test avertr.R's output against
avert_run_filepath <- "/Users/joeypendleton/OtherFolders/avertr/test_scenarios/3154MW_OSW_NE_05252025.xlsx"


# LOAD OBJECTS ######

# NOTE: Replace w more robust range value
avert_differences_final <- read_excel(avert_run_filepath,
                                      sheet = "Summary",
                                      range = "C7:AA123") |> 
  mutate(`ORSPL (Plant ID)` = as.character(`ORSPL (Plant ID)`))



# AGGREGATE TO YEAR #######
# avertr.R gives each EGU's output for each hour of the year, but we want annual
#   totals, so we aggregate across time
differences_final_grouped <- differences_final |> 
  select(!c(load_8760_col, ff_load_bin_8760_col, ff_load_bin_next_col)) |> 
  summarize(
    across(data_generation:data_nh3, sum),
    .by = c(`ORISPL Code`, `Unit Code`, `Full Unit Name`)
  )



# TESTING #######
## Unit Level ===========
### join results ----------
# Join the avertr results to the AVERT summary sheet by ORISPL and unit ID. If
#   there's not a 1-to-1 relationship (i.e., a different number of EGUs between
#   the AVERT and avertr outputs), it throws an error
test_joined <- avert_differences_final |> 
  inner_join(
    differences_final_grouped,
    by = join_by(
      `ORSPL (Plant ID)` == `ORISPL Code`,
      `Unit Name` == `Full Unit Name`
    ),
    na_matches = "never",
    unmatched = "error",
    relationship = "one-to-one"
  )


### calculate errors ------------
# Calculate total and percent errors between AVERT and avertr data
test_errors <- test_joined |>
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
# Summary table of the errors between AVERT and avertr. Note that percent
#   errors may be NA where the AVERT value is 0. (To check, filter
#   for cases where percent error is NA but AVERT value is not 0. There should
#   be no such cases.)
test_errors |> 
  select(contains("error")) |> 
  summary()

# Largest absolute error from each measure. (Note that the pct error
#   and error for each data measure may come from different rows.)
test_errors |> 
  select(contains("error")) |> 
  mutate(across(everything(), abs)) |> 
  summarize(across(everything(), ~ max(.x, na.rm = TRUE))) |> 
  print(width = Inf)

# Histograms for all the percent errors
test_errors |> 
  select(contains("pct")) |> 
  iwalk(~ hist(.x, main = .y, breaks = nrow(test_errors)))

# All rows where errors are NA
test_errors |> 
  filter(if_any(contains("error"), is.na))

# All rows where absolute percent errors are greater than 0.1%
test_errors |> 
  mutate(across(contains("pct"), abs)) |> 
  filter(if_any(contains("pct"), \(x) x > 0.1))


## Region Level ==========
### aggregate to region --------
test_errors_total <- test_joined |> 
  summarize(
    across(
      c(`Annual Change in Generation (MWh)`:`Annual Change in Heat Input (MMBtu)`,
        `Annual Change in VOCs (lb)`,
        `Annual Change in NH3 (lb)`,
        data_generation:data_nh3),
      sum
    )
  )

### calculate errors --------
test_errors_total <- test_errors_total |>
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
test_errors_total |>
  select(contains("error")) |> 
  print(width = Inf)



## Hourly Unit Level =========
### wrangle hourly AVERT data --------
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

# The unit name, ORISPL code, and unit ID are split up across three header rows.
#   Read in the three and then concatenate them.
avert_unit_differences_final_plant_info <- data_measure_sheet_names |> 
  map(
    ~ read_excel(
      avert_run_filepath,
      sheet = .x,
      range = "L1:DW3", # EVENTUALLY: find more robust way to set range.
      col_names = FALSE
    )
  ) |> 
  map(map_chr, ~ str_c(.x, collapse = "|"))

# Read in all 8 data measures, assigning each sheet the column names created
#   above
avert_unit_differences_final <- data_measure_sheet_names |> 
  map2(
    avert_unit_differences_final_plant_info,
    ~ read_excel(
      avert_run_filepath,
      sheet = .x,
      range = "L4:DW8763",
      col_names = .y
    )
  )

# Pivot the sheets so that each EGU-hour pair is a row, and split up the column
#   with ORISPL, unit code, and name into three columns.
avert_unit_differences_final <- avert_unit_differences_final |> 
  map2(
    data_measure_sheet_names,
    ~ pivot_longer(
      .x,
      everything(),
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

# Bind all the tibbles in the list into one, and remove redundant columns that
#   result from the bind_cols()
avert_unit_differences_final <- avert_unit_differences_final |> 
  bind_cols() |> 
  select(`ORISPL Code...1`:`Unit Name...3` | !contains("...")) |> 
  rename(
    `ORISPL Code` = `ORISPL Code...1`,
    `Unit Code` = `Unit Code...2`,
    `Unit Name` = `Unit Name...3`
  )

# Bind avertr differences with the AVERT differences we just wrangled
test_joined_hourly <- differences_final |> 
  bind_cols(avert_unit_differences_final)

# # Check to make sure the units are the same. Unit codes won't match because the
# #   AVERT results sheets have some of the aforementioned data formatting issues
# #   (e.g., unit code "001" is "1"). So checking names instead. Should print 0.
# sum(pull(test_joined_hourly, `Full Unit Name`) !=
#       pull(test_joined_hourly, `Unit Name`))

### calculate errors ------
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


