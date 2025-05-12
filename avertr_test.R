# A script to test outputs from avertr against outputs from AVERT.



# CLEAR ENVIRONMENT #######
rm(list = ls())



# SET UP ########
# Assumes you have augmented avertr.R by specifying your desired region and
#   load change
source("./avertr.R")



# LOAD OBJECTS ######
# Enter the filepath to the AVERT main module workbook (converted from its
#   default .xlsb format to either .xls or .xlsx) that you wish to test
#   avertr.R's output against

# NOTE: Replace w more robust range value
avert_differences_final <- read_excel("./test_scenarios/500MW_OSW_NE_04012205.xls",
                                      sheet = "Summary",
                                      range = "C7:AA123") |> 
  mutate(`ORSPL (Plant ID)` = as.character(`ORSPL (Plant ID)`))



# SUMMARIZE #######
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
#   errors may be NA where the AVERT value is 0
test_errors |> 
  select(contains("error")) |> 
  summary()

# Largest absolute percent error from each measure
test_errors |> 
  select(contains("pct")) |> 
  mutate(across(everything(), abs)) |> 
  summarize(across(everything(), ~ max(.x, na.rm = TRUE)))

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
  select(contains("error"))








# Also, take another look at how exactly the rounding is done in AVERT. Is heat
#   rounded first before its used to estimate NEI emissions?
# Okay: Looks like heat gets rounded to 3, and then that rounded value is multiplied
#   by the unrounded NEI values, and then the result is immediately rounded to 6.
# I think you need to start going thru AVERT, add breakpoints, understand what the pre and post PM thing
#   is, what those values end up being, etc.


# Ok, as of 5/12, 5pm: looks like heat difference is calculated and rounded to 3, like everything else. Then, it's
#   put in the HeatInput sheet. So the HeatInput sheet is full of heat differences between the BAU and current scenario,
#   for each plant, for ecah hour (and they've all been rounded to 3 (they're rounded after the differece is taken, I think)). Then, you take each difference, and multiply it
#   by the corresponding (unrounded) NEI emissions rate, and round that to 6. This gives you change in NEI emissions.

# Independently, the pre PM (and pre NH3 and pre VOC) is calculated by multiplying the emissions rate by the original
#   (rounded to 3) heat rate, and then rounding that resulting product to 6.

# And then the post PM (and NH3, VOC) is calculated by taking the pre PM and adding the change in PM to it.





# And while you're at it, how does AVERT do ITS OWN unit matching if the data
#   is inconsistent? Look into the code





# Reminder: maybe add some of those join safety mechanisms in the join in
#   avertr_test to avertr and avertr_setup



# Can you write a more granular test to ensure that the temporal profile is
#   correct? Or at least maybe check the ozone season results? Like get the
#   output into the same output as the Summary sheet format.



