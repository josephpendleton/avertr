# TESTING #######
# Load the relevant summary sheet from AVERT
ng_avert <- read_excel("/Users/joeypendleton/OtherFolders/avertr/test_scenarios/500MW_OSW_NE_04012205.xls",
                       sheet = "Summary",
                       range = "C7:AA123")

# Join the avertr results to the AVERT summary sheet by unit name
test_time <- ng_avert |> 
  left_join(tt, by = join_by(`Unit Name` == `Full Unit Name`))

# Calculate total and percent differences between AVERT and avertr generation; plot % differences as histogram
test_time_diffs <- test_time |>
  mutate(
    generationdiff = generation.x - `Annual Change in Generation (MWh)`,
    pct_generationdiff = (generationdiff / `Annual Change in Generation (MWh)`) * 100,
    so2diff = so2.x - `Annual Change in SO2 (lb)`,
    pct_so2diff = (so2diff / `Annual Change in SO2 (lb)`) * 100,
    noxdiff = nox.x - `Annual Change in NOx (lb)`,
    pct_noxdiff = (noxdiff / `Annual Change in NOx (lb)`) * 100,
    co2diff = co2.x - `Annual Change in CO2 (tons)`,
    pct_co2diff = (co2diff / `Annual Change in CO2 (tons)`) * 100,
    heatdiff = heat.x - `Annual Change in Heat Input (MMBtu)`,
    pct_heatdiff = (heatdiff / `Annual Change in Heat Input (MMBtu)`) * 100,
    pm25diff = pm25.x - `Annual Change in PM2.5 (lb)`,
    pct_pm25diff = (pm25diff / `Annual Change in PM2.5 (lb)`) * 100,
    vocsdiff = vocs.x - `Annual Change in VOCs (lb)`,
    pct_vocsdiff = (vocsdiff / `Annual Change in VOCs (lb)`) * 100,
    nh3diff = nh3.x - `Annual Change in NH3 (lb)`,
    pct_nh3diff = (nh3diff / `Annual Change in NH3 (lb)`) * 100
  )

# Summary table of the differences between AVERT and avertr
test_time_diffs |> 
  select(generationdiff:pct_nh3diff) |> 
  summary()

# Histogram of the differences
test_time_diffs |> 
  pull(pct_generationdiff) |> # Choose which col to check here
  abs() |> 
  hist(breaks = 116)



# Was getting some differences, need to like trace them out for an individual
#   unit, see where it starts going wrong. Maybe hand calculate based on 
#   AVERT vals from various sheets a la CoBE?