# avertr, last edited 04/09

# SET UP ########
rm(list = ls())

# start.time <- Sys.time()

library(tidyverse)
library(readxl)


# USER INPUT ########
# Eventually: want to have user select variables, like region and energy type, and
#   then based on that read in appropriate sdf, other stuff, etc. Or have them
#   enter an absolute increase in energy, etc. (But also for web version you
#   don't want to overwhelm them — maybe just stick w/ simple renewables options.)
project_capacity_mw <- 500


# LOAD OBJECTS ######
# BAU fossil fuel load in the region; 8760 hours
bau_load_hourly_mw <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "F3:F8763"
) |> 
  pull(`Regional Load (MW)`)

# Hourly capacity factor; remove the 24 hours corresponding to 2/29
capacity_factor_hourly <- read_excel(
  "./avert-main-module-v4.3.xls",
  sheet = "EERE_Default",
  range = "AJ2:AJ8786"
) |> 
  #slice(c(1:1416, 1441:n())) |> 
  slice(c(1:8760)) |> # NOTE!!! This is the wrong slice, but it's the one AVERT
  #   uses. The above, commented-out slice removes the correct dates.
  #   However, using what AVERT does for now for validation.
  pull(`Offshore Wind`)

# # 8760 as datetime
# # NOTE: You should replace this w the way simpler version from avertr_setup
datetime_8760 <- read_excel(
  "./avert-main-module-v4.3.xls",
  sheet = "EERE_Default",
  range = "A2:A8786"
) |>
  mutate(
    Date = case_when(
      second(Date) != 0 ~ ceiling_date(
        Date,
        unit = "second",
        change_on_boundary = TRUE
      ),
      TRUE ~ Date
    )
  ) |>
  filter(!between(Date, ymd_hms("2012-02-29 00:00:00"), ymd_hms("2012-02-29 23:00:00"))) |>
  pull(Date)

# NEI Emission factors
nei_efs <- read_excel(
  "./avert-main-module-v4.3.xls",
  sheet = "NEI_EmissionRates",
  range = "G6:AR4636"
) |> 
  filter(str_detect(`ORSPL|Unit|Region`, "New England")) |> 
  select(`Full Name`, PM2.5...36:NH3...38)



# Eventually: neater code to read all of these in
# And maybe faster. Easier to just read everything in at once and then grab
#   values later? Woudl be hard w datatypes (altho u could just read all
#   as character and coerce later...)
generation_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H4:CD120"
) |> 
  as.data.frame()

so2_ozone_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H2004:CD2120"
) |> 
  as.data.frame()

so2_non_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H4004:CD4120"
) |> 
  as.data.frame()

nox_ozone_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H6004:CD6120"
) |> 
  as.data.frame()

nox_non_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H8004:CD8120"
) |> 
  as.data.frame()

co2_ozone_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H10004:CD10120"
) |> 
  as.data.frame()

co2_non_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H12004:CD12120"
) |> 
  as.data.frame()

heat_ozone_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H14004:CD14120"
) |> 
  as.data.frame()

heat_non_ff_load_bins <- read_excel(
  "./regional_data_files/avert-rdf-2023-epa_netgen_pmvocnh3-new-england-20240223-1642.xlsx",
  sheet = "Data",
  range = "H16004:CD16120"
) |> 
  as.data.frame()

ff_load_bins_list <- lst(
  generation_ff_load_bins,
  so2_ozone_ff_load_bins,
  so2_non_ff_load_bins,
  nox_ozone_ff_load_bins,
  nox_non_ff_load_bins,
  co2_ozone_ff_load_bins,
  co2_non_ff_load_bins,
  heat_ozone_ff_load_bins,
  heat_non_ff_load_bins,
)

ff_load_bins_list <- ff_load_bins_list |> 
  map(as_tibble)

# CALC NEW LOAD BINS #######
# Based on the project, find the new ff load bins

# This is the hourly capacity
capacity_hourly_mw <- capacity_factor_hourly * project_capacity_mw

# This is the new hourly net load
new_load_hourly_mw <- bau_load_hourly_mw - capacity_hourly_mw

# This is a vector containing the set of ff load bins for the reion
ff_load_bins_key <- colnames(generation_ff_load_bins) |> 
  as.numeric()

# Remove NAs (note: do this more cleany, it's weird to coerce all names to 
#   numeric and rely on conames being NA, better to just read all as character,
#   filter w/ regexp OR even based on location (bc you knwo which cols you
#   don't want to selec) and then convert to numeric)
ff_load_bins_key <- ff_load_bins_key[!is.na(ff_load_bins_key)]





# NA vector to store the new load bins for the net load of the region.
ff_load_bins_8760_bau <- rep(NA, 8760)

ff_load_bins_8760_bau_next <- rep(NA, 8760)

# Calculate the new load bin
for (i in 1:length(bau_load_hourly_mw)) {
  diff <- bau_load_hourly_mw[i] - ff_load_bins_key
  diff[diff < 0] <- NA # Bc of AVERT search alg, only grabs load bins BELOW the hourly load; here, diff is negative iff the given ff load bin is greater than hourly generation, but those cases are not considered matches in AVERT
  current_load_bin_index <- which.min(diff)
  ff_load_bins_8760_bau[i] <- ff_load_bins_key[current_load_bin_index]
  ff_load_bins_8760_bau_next[i] <- ff_load_bins_key[current_load_bin_index + 1] # EVENTUALLY: The better way to do this is to make an 8760x2 matrix "next load bin library". The first col is ff_load_bins_key, the second is the first, shifted by 1. Then you just save to an array or something....although idk does really help runtime? The join is the issue...
}


# NA vector to store the new load bins for the net load of the region.
ff_load_bins_8760 <- rep(NA, 8760)

ff_load_bins_8760_next <- rep(NA, 8760)

# Calculate the new load bin
for (i in 1:length(new_load_hourly_mw)) {
  diff <- new_load_hourly_mw[i] - ff_load_bins_key
  diff[diff < 0] <- NA
  current_load_bin_index <- which.min(diff)
  ff_load_bins_8760[i] <- ff_load_bins_key[current_load_bin_index]
  ff_load_bins_8760_next[i] <- ff_load_bins_key[current_load_bin_index + 1]
}



# 04/07 ISSUE INVESTIGATE ###########
# When I run the whole algorithm, I get significant differences. What's going on?
#   In this section, I want to try to investigate this.

# To start, I want to make sure that the 8760 added capacity I'm calculating
#   is roughly the same as what AVERT has, and that the resultant net load 8760
#   I'm calculating (and the net load BIN 8760) is the same as AVERT. If they
#   are, then there's something going wrong with the way I join the data on.

# 1. Here's the bau 8760 load (from RDF):
bau_load_hourly_mw

# 2. Here's the bau 8760 load BINS (which I calculated):
ff_load_bins_8760_bau

# 3. Here's the extra renewable capacity 8760 (which I calculated):
capacity_hourly_mw

# 4. Here's the new 8760 load (which I calculated):
new_load_hourly_mw

# 5. Here's the new 8760 load BINS (which I calculated):
ff_load_bins_8760


# Now I need to make sure that each of these matches up with AVERT itself

# 1. bau_load_hourly_mw I spot checked, looks good, which is expected since it comes
#   right from the RDF

# 2. Doesn't seem like they ever really give load BINS for BAU scenario

# 3. In ManualEERE Entry, they give extra renewable capacity. Reading it in here:
AVERT_capacity_hourly_mw <- read_excel(
  "./test_scenarios/500MW_OSW_NE_04012205.xls",
  sheet = "ManualEEREEntry",
  range = "I8:I8768"
)

# Values are negative, so reverse sign and then subtract
cap_diffs <- ((-1 * AVERT_capacity_hourly_mw) - capacity_hourly_mw)

cap_diffs |> summary()

# Interesting! Almost all 0, but some larger values too. Which ones?
# These are the values that don't equal 0
cap_diffs |> filter(`Total Change (MW)` != 0)

# These are the indices of those values
which(cap_diffs != 0)

# Very interesting! Looks like the values come in consecutive strings of exactly
#   24 values, before jumping to another consecutive string of 24 values

# Let's investigate the first chunk: indices 1417 to 1440 (inclusive)

# My values:
capacity_hourly_mw[1417:1440]

# AVERT's values
AVERT_capacity_hourly_mw[1417:1440, ]

# Not really a consistent value that they're off by
capacity_hourly_mw[1417:1440] - (-1 * AVERT_capacity_hourly_mw[1417:1440, ])

# Hmm. My bau ff load was read in directly. So that should be correct. Therefore,
#   there's def. a difference with the capacity values. The entered OSW capacity
#   is def. 500 in both cases, and it would be weird if it weren't. Thus, there's
#   something different about the capacity factor.




# OK! After much investigation, seems like there's an issue with AVERT. Instead
#   of cutting out Feb 29 in the non-leap year of 2023, they cut out March 3/1.
#   So the March 3/1 they give you has the values that Feb 29 would have, the
#   March 3/2 they give you has the values that March 3/1 should have, and this
#   seemingly propogates.
# I think (think!!!!) the reason this doesn't totally mess everything up is that
#   the daily capacity factor periodic thing doesn't change within a month, so
#   it's only off by one day each month, which explains my results above, where
#   I found different stretches to be off by different amounts
# Note also that this mistake seems to be made between the EERE_Default sheet
#   and the CalculateEERE sheet, since in the latter sheet, the wrong load
#   changes are already there. Indeed, CalculateEERE doesn't include 2/29 as a
#   date, and it lists the removed 24 hourly load changes at the bottom
#   with the date 1/0/00. Presumably these are intended to be from 2/9, but
#   inspection shows them to be from 12/31. This makes sense since all days after
#   that one are shifted out by one day, so 12/31, the last day of the year, is
#   the one that has to get cut

# Here's some code:

# You can verify that these capacity factors are REALLY the ones that should be
#   cut by just looking at the EERE_Default sheet and verifying that they are
#   the rows corresponding to 2/29
cfs_that_really_should_be_cut <- read_excel(
  "./test_scenarios/500MW_OSW_NE_04012205.xls",
   sheet = "EERE_Default",
  range = "AJ1419:AJ1442", col_names = FALSE
  ) 

# Here they are
cfs_that_really_should_be_cut

# Mulitply by capacity. Now compare the below value to the 24 capacity change values
#   for 3/1 (from either CalculateEERE or ManualEEREEntry, the latter sheet
#   pulls directly from the former so it should all be the same.)
cfs_that_really_should_be_cut$...1 * 500



# ASSIGN VALUES ##########
# We now have the 8760 for net load bin in each hour, after our new project.
#   But we still need to, for each hour, based on the load bin, get the
#   information on generation and pollutants. E.g., we know that net load was
#   in the 2000 MW bin in hour 2293 in the region, but we don't know what generation, 
#   so2 emissions, nox emissions, etc. each EGU has in the 2000 MW bin.

# After reading functional programming section of advanced R, clean this up
# Function takes a read-in df containing information about each EGU's activity
#   at each load bin, and tidies it
# AND RENAME this function










# 4/12, 4pm: Joey. Here are the next steps.
# First, in order to get the function below working with only a single join,
#   you should simply remove colnm argument from it so that you can determinately
#   pick out the column in order to lead() it, to get the next value. (Additionally,
#   it might be worth putting a check on that col somewhere in the code
#   to make sure it's in ascending order. It def is, but still.) Then use a
#   map() later to rename it. After all that, you can remove the second join()
#   in the loop. Oh, and change that loop to a purrr functional, like walk() at least.
# Second, AND YOU MAY WANT TO DO THIS FIRST: it's time to start re-arragning
#   and documenting. I think it makes sense to have a sheet where you load
#   a bunch of standard objects for the different scenarios and save them as
#   r data files. I'd do that now. Namely, you'll want like a list for
#   each region, maybe other info idk. Start doing that now, and then put it
#   into one script later. You should also move that concern abt the day being
#   off into another section of the script.
























eval(iris)

as.character(iris)



join_data <- function(x_ff_load_bins, colnm) {
  colnm_sym <- sym(colnm)
  x_ff_load_bins <- x_ff_load_bins |> 
    pivot_longer(
      any_of(as.character(ff_load_bins_key)),
      names_to = "ff_load_bins_8760",
      values_to = colnm) |> 
    mutate(
      ff_load_bins_8760 = as.numeric(ff_load_bins_8760)
      # nextnum = lead(!!colnm_sym),
      # nextnum = case_when( # When the load bin is as high as it can go, there's no "next" load bin. lead() pulls in the lowest load bin, but it shouldn't
      #   ff_load_bins_8760 == max(ff_load_bins_key) ~ NA,
      #   TRUE ~ nextnum
      #)
    )
  return(x_ff_load_bins) # This returns a df which has a row for each unit for each load bin.
}

# Get all the EGU names
EGU_names_key <- generation_ff_load_bins |> 
  pull(`Full Unit Name`)

# In each df, select only relevant cols
ff_load_bins_list <- ff_load_bins_list |> 
  map(~select(., !(State:`Unit Code`)))

# Apply the above tidying function
ff_load_bins_list <- ff_load_bins_list |> 
  map2(names(ff_load_bins_list), ~join_data(.x, .y))


## BAU Scenario =========
# This is the information for the BAU scenario — the one where there's no load
#   difference

# Take Cartesian product of ff_load_bins and EGU names. So we end up with
#    a row for each EGU name-load bin pair
test_bau <- expand.grid(
  ff_load_bins_8760_bau = ff_load_bins_8760_bau, # Change this ot have a name analogous to ff_load_bins, etc. OR maybe just don't change name at all?
  `Full Unit Name` = EGU_names_key
) |> 
  as_tibble() |> 
  add_column(datetime = rep(datetime_8760, length(EGU_names_key)),
             ff_load_bins_8760_bau_next = rep(ff_load_bins_8760_bau_next, length(EGU_names_key)),
             bau_load_hourly_mw = rep(bau_load_hourly_mw, length(EGU_names_key)))



# Eventually: might be best to save the ff load bin index directly, rather than
#   calculate load bin so early on
# Use the load bin col and the ff_load_bins_key to find the next load bin.
# Do the join twice: once for the current load bin, and once for the next
#   load bin. (What if you're in the max load bin? Shouldn't happen. You only get
#   into a load bin if true load is above it, and if true load is above the max
#   load bin, it shouldn't run.)
# Take the difference in the two values across the load bins, divide by the difference
#   in the two load bins. That's slope.
# Backfit for intercept
#   Calculate value.

# FOR NOW you're not worry abt cases where you have a load bin as max load, but
#   eventually you need to add that functionality, since someone could run a 
#   scenario like that.

# This is the biggest slowdown w the current algo: you join twice, even though
#   after the first join you can easily determine what the second join should be,
#   simply based on the matrix location of the values. Maybe best way is to only
#   load in index values in the above for loop, and then just literally index each
#   table in the ff_load_bins_list?
for (i in 1:length(ff_load_bins_list)) {
  test_bau <- test_bau |> 
    left_join(ff_load_bins_list[[i]], by = join_by(ff_load_bins_8760_bau == ff_load_bins_8760, `Full Unit Name`)) |> 
    left_join(ff_load_bins_list[[i]], by = join_by(ff_load_bins_8760_bau_next == ff_load_bins_8760, `Full Unit Name`))
}



# Rename columns
test_bau <- test_bau |>
  rename_with(
    .fn = ~str_remove(., "_ff_load_bins"),
    .cols = !c(ff_load_bins_8760_bau:`Full Unit Name`)
  )



## Alt Scenario =========
# Take Cartesian product of ff_load_bins and EGU names. So we end up with
#    a row for each EGU name-load bin pair
test <- expand.grid(
  ff_load_bins_8760 = ff_load_bins_8760,
  `Full Unit Name` = EGU_names_key
) |> 
  as_tibble() |> 
  add_column(datetime = rep(datetime_8760, length(EGU_names_key)),
             ff_load_bins_8760_next = rep(ff_load_bins_8760_next, length(EGU_names_key)),
             new_load_hourly_mw = rep(new_load_hourly_mw, length(EGU_names_key)))






# join on relevant values
for (i in 1:length(ff_load_bins_list)) {
  test <- test |> 
    left_join(ff_load_bins_list[[i]], by = join_by(ff_load_bins_8760, `Full Unit Name`)) |> 
    left_join(ff_load_bins_list[[i]], by = join_by(ff_load_bins_8760_next == ff_load_bins_8760, `Full Unit Name`))
}





# Rename columns
test <- test |>
  rename_with(
    .fn = ~str_remove(., "_ff_load_bins"),
    .cols = !c(ff_load_bins_8760:`Full Unit Name`)
  )

# OZONE SEASON SPLIT #############
# Split up into ozone season and not. As per AVERT documentation, ozone season is
#   May to September inclusive.
test_ozone <- test |>
  filter(between(datetime, ymd_hms("2012-05-01 00:00:00"), ymd_hms("2012-09-30 23:00:00")))

test_non <- test |>
  anti_join(test_ozone, by = join_by(datetime))

# # Check:
# nrow(test_ozone) + nrow(test_non) == nrow(test)
# intersect(test_ozone$datetime, test_non$datetime)

test_ozone <- test_ozone |> 
  select(ff_load_bins_8760:generation.y, contains("ozone")) |> 
  rename_with(~str_replace(., "_ozone", ""))

test_non <- test_non |> 
  select(ff_load_bins_8760:generation.y, !contains("ozone")) |> 
  rename_with(~str_replace(., "_non", ""))

test <- test_ozone |> 
  bind_rows(test_non)

# end.time <- Sys.time()
# time.taken <- end.time - start.time
  








test_bau_ozone <- test_bau |>
  filter(between(datetime, ymd_hms("2012-05-01 00:00:00"), ymd_hms("2012-09-30 23:00:00")))

test_bau_non <- test_bau |>
  anti_join(test_bau_ozone, by = join_by(datetime))

# # Check:
# nrow(test_bau_ozone) + nrow(test_bau_non) == nrow(test_bau)
# intersect(test_bau_ozone$datetime, test_bau_non$datetime)

test_bau_ozone <- test_bau_ozone |> 
  select(ff_load_bins_8760_bau:generation.y, contains("ozone")) |> 
  rename_with(~str_replace(., "_ozone", ""))

test_bau_non <- test_bau_non |> 
  select(ff_load_bins_8760_bau:generation.y, !contains("ozone")) |> 
  rename_with(~str_replace(., "_non", ""))

test_bau <- test_bau_ozone |> 
  bind_rows(test_bau_non)


# INTERPOLATION #########
get_val_bau <- function(current_bin, next_bin) {
  slope <- (current_bin - next_bin) / (test_bau$ff_load_bins_8760_bau - test_bau$ff_load_bins_8760_bau_next)
  intercept <- current_bin - (slope * test_bau$ff_load_bins_8760_bau)
  val <- (test_bau$bau_load_hourly_mw * slope) + intercept
  return(val)
}

current_load_bin_vals_bau <- test_bau |> 
  select(contains(".x")) |> 
  as.list()

next_load_bin_vals_bau <- test_bau |> 
  select(contains(".y")) |> 
  as.list()

res_list_bau <- map2(current_load_bin_vals_bau, next_load_bin_vals_bau, get_val_bau)

res_temp_bau <- res_list_bau |>
  c(select(test_bau, `Full Unit Name`)) |> 
  as_tibble() |> 
  left_join(nei_efs, by = join_by(`Full Unit Name` == `Full Name`))

res_temp_bau <- res_temp_bau |> 
  mutate(pm25.x = PM2.5...36 * heat.x,
         vocs.x = VOCs...37 * heat.x,
         nh3.x = NH3...38 * heat.x)

res_list_bau <- res_temp_bau |> 
  select(!c(`Full Unit Name`, PM2.5...36:NH3...38)) |> 
  as.list()



# CHECK AVERT CODE for how they do this...






get_val <- function(current_bin, next_bin) { # EVENTUALLY: seems best to just have one get_val function, with three additional arguments for the three specific cols, OR you can name the cols the same across test and test_bau, and just pass one additional argument for the df...
  slope <- (current_bin - next_bin) / (test$ff_load_bins_8760 - test$ff_load_bins_8760_next)
  intercept <- current_bin - (slope * test$ff_load_bins_8760)
  val <- (test$new_load_hourly_mw * slope) + intercept
  return(val)
}

current_load_bin_vals <- test |> 
  select(contains(".x")) |> 
  as.list()

next_load_bin_vals <- test |> 
  select(contains(".y")) |> 
  as.list()

res_list <- map2(current_load_bin_vals, next_load_bin_vals, get_val)

res_temp <- res_list |>
  c(select(test, `Full Unit Name`)) |> 
  as_tibble() |> 
  left_join(nei_efs, by = join_by(`Full Unit Name` == `Full Name`))

res_temp <- res_temp |> 
  mutate(pm25.x = PM2.5...36 * heat.x,
         vocs.x = VOCs...37 * heat.x,
         nh3.x = NH3...38 * heat.x)

res_list <- res_temp |> 
  select(!c(`Full Unit Name`, PM2.5...36:NH3...38)) |> 
  as.list()



qt <- read_rds("HIGHLYTEMPORARY_interped_data_regions.rds")

set.seed(50)
qt[[7]] |> arrange(data_generation, data_so2, data_nox) |> sample_n(100) |> view()

set.seed(50)
res_list_bau |> bind_cols() |> arrange(generation.x, so2.x, nox.x) |> sample_n(100) |> view()

# Eventually: you'll want to split this up into two steps: just find raw
#   differences, then round some to 3, some to 6
changes <- map2(res_list, res_list_bau, function(.x, .y) round(.x - .y, 6))


final_changes <- test |> 
  select(ff_load_bins_8760:new_load_hourly_mw) |> 
  add_column(bind_cols(changes))


tt <- final_changes |> summarize(
  across(generation.x:nh3.x, sum),
  .by = `Full Unit Name`
)



# TESTING #######
# Load the relevant summary sheet from AVERT
ng_avert <- read_excel("/Users/joeypendleton/Library/CloudStorage/OneDrive-BostonUniversity/BUSPH/EnBen/test_scenarios/500MW_OSW_NE_04012205.xls",
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
  pull(pct_nh3diff) |> # Choose which col to check here
  abs() |> 
  hist(breaks = 116)





# IDEAS (as of 4/8, 2pm): Looks like they exclude load hours outside of
#   the lowest load bin or highest load bin. (Note: looks like this is
#   checked against the raw load, not  the load binnified version of it)



# Maybe when you do this it would be fasteer to pre-load all these slope and
#   intercept values? Prob not, but maybe... could cut down significantly on
#   redundancy


# First of all, recall that you can open up macros and compile if you need to,
#   add breakpoints, and then hover over variable names to get the values.
#    













# NOTE: you will run up against rare so2 emission events plants (altho perhaps
#   no in New England)










# Consider fastest way to do ozone stuff. I really think it's probably
#   dividing the dataset in 2, dropping the ozone/non-ozone columns in
#   each dataset, and then summing across all hours for each plant in
#   the datasets
# (Do you want to do a monthly version?)




# Maybe think abt other JOINING algorithms, this one is still a bit slow. (I
#   think the alg. for calculating the load bins is fine, tho)

















# What you need to do for alt. pollutants: 1. grab each plant's emission rates from the main
#   module (in bottom, ~row 8766 of PM25, VOCs, and NH3 sheets). (You'll need to do this differently for each region, which kinda
#   blows, but cross that bridge later). 2. calculate heat input in both
#   ozone and non-ozone season as per above. 3. Multiply each plant's emission
#   rate by the change; should be a efficient matrix algebra way to do this.


# TO DOS #########

# ADD UNITS to all names everywhere??

# More generally: better naming conventions

# Eventaully: go back and start using a combo of unit code or ORSPL code, rather
#   than full unit name. The concatenated unit and ORSPL code seems to be what
#   EPA uses in the AVERT spreadsheet, and it seems more robust.


# Define somewhere (probably in an accompanying document) all the terms
#   you're using, like ff load bin, net load, etc.

# OH! Also check out data.table.


# But if you're still really struggling, TOTALLY just ask chatgpt. Remember, gone
#   are the days when it matters what you personally have the ability to do
#   sans AI, and here are the days when it matters what you can acutally get
#   done with the tools available to you.










# oz <- assigned_data |> 
#   filter(
#     between(
#       datetime_8760_col,
#       ymd_hms("2023-05-01 00:00:00"),
#       ymd_hms("2023-09-30 23:00:00")
#     )
#   ) |> 
#   rename_with(~ str_replace(., "_ozone", "")) # Rename so that we can easily bind rows below
# 
# non <- assigned_data |> 
#   anti_join(oz) |> 
#   rename_with(~ str_replace(., "_non", ""))
# 
# oz <- oz |> 
#   select(datetime_8760_col:data_next_bin_generation, contains("ozone")) 
# non <- non |> 
#   select(datetime_8760_col:data_next_bin_generation, contains("non"))
# 
# assigned_data_ozoned <- oz |> 
#   bind_rows(non)
# 
# return(assigned_data_ozoned)
