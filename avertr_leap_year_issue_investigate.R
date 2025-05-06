# A script to investigate an issue related to leap years in AVERT.



# START TIME ######
# For tracking runtime
start_time <- Sys.time()



# SET UP ########
rm(list = ls())

library(tidyverse)
library(readxl)



# USER INPUT ########
# Temporary
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

