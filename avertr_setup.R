# A script to generate files which get used in avertr.R. Specifically, it
#   generates ff_load_bin_data_final.rds, a list of ADD HERE!!
# Last edit: Joseph 04/30: Went back through, added comments

# Very general note: In this script, I make some moves which aren't extremely
#   "safe" — mostly assuming that all data across different files is in the 
#   same format. I do this because it makes things faster and simpler (e.g.,
#   binding columns instead of joining) and because we have a uniquely good
#   way to validate this script (namely, by comparing the output to AVERT) so
#   it makes sense to sacrifice some "safety" for performance, since we have
#   such a good QA/QC available.



# SET UP ########
rm(list = ls())

# For tracking runtime
start_time <- Sys.time()

library(tidyverse)
library(readxl)



# DEFINE/LOAD OBJECTS ############
# This is simply a vector of each hour of the year 2023
datetime_8760 <- seq(
  from = ymd_hms("2023-01-01 00:00:00"),
  by = "1 hour",
  length.out = 8760
)

# Annoyingly, the information on the year of the measurement for the NEI emission
#   factors is given not in the header row, but in the row above. We get the
#   years from that above row here, and use them as we read in the NEI EFs below.
nei_efs_years <- read_excel(
  "./avert-main-module-v4.3.xls",
  sheet = "NEI_EmissionRates",
  range = "G5:AR5",
  col_names = FALSE
) |> 
  unlist() |> 
  unname() |> 
  as.character() |> 
  replace_na("")

# NEI emission factors (used to calculate PM2.5, VOCs, and NH3 based on heat).
#   We paste in the above years that we read in, from the row above the header
#   row.
nei_efs <- read_excel(
  "./avert-main-module-v4.3.xls",
  sheet = "NEI_EmissionRates",
  range = "G6:AR4636",
  .name_repair = ~ str_c(.x, nei_efs_years),
)

# Hardcoding in the 14 region names
region_names <- c("california", "carolinas", "central", "florida", "mid-atlantic",
                  "midwest", "new-england", "new-york", "northwest", "rocky-mountains",
                  "southeast", "southwest", "tennessee", "texas")

# Hardcoding in the 9 data measures
data_measures <- c("generation", "so2_ozone", "so2_non", "nox_ozone", "nox_non",
                   "co2_ozone", "co2_non", "heat_ozone", "heat_non")

# These are the filepaths to the 14 RDFs. These are 2023 RDFs — NOT a leap year.
rdf_filepaths <- list.files(
  path = "./regional_data_files",
  full.names = TRUE
)

# These are the business-as-usual 8760 loads for each region. Note that these
#   are raw load values — they have not been put into load bins (yet).
load_8760s_bau <- rdf_filepaths |> 
  map(~ read_excel(.x, sheet = "Data", range = "F3:F8763")) |> 
  map(~ pull(.x, 1))

# These are the 9 Excel sheet ranges corresponding (in order) to each of the
#   9 data measures from above. Note that there should be the same number of
#   columns and rows for each of the 9 data measures within an RDF, but these
#   numbers may vary between RDFs. Thus, I'm grabbing a wide expanse of rows
#   and columns, to ensure that I don't accidentally miss any data. (See the
#   RDFs if this comment doesn't make sense.)
ranges <- c("H4:EA2000", "H2004:EA4000", "H4004:EA6000", "H6004:EA8000",
            "H8004:EA10000", "H10004:EA12000", "H12004:EA14000",
            "H14004:EA16000", "H16004:EA18000")

# This takes ~3 mins to run for me. expand_grid takes the Cartesian product of
#   the rdfs and data ranges, and then we map each iteration to read_excel. It
#   returns a list of length 126 (14 regions * 9 data measures = 126). The first
#   9 elements of the list are the 9 data measures for the first region. The
#   next 9 elements of the list are the 9 data measures for the second region.
#   Etc. Each element is a tibble with information on each EGU's across all
#   the different fossil-fuel load bins, for the given data measure, for the
#   given region. Let's call each of these elements a "data measure table".
ff_load_bin_data_raw <- expand_grid(rdf_filepaths, ranges) |> 
  pmap(~ read_excel(.x, sheet = "Data", range = .y)) |> 
  # To suppress messages about name repair
  suppressMessages()



# CLEAN DATA #############
## NEI Emission Factors =======
# We only want 2023 emission factors
nei_efs <- nei_efs |> 
  select(`Full Name`:`ORSPL|Unit|Region`, contains("2023")) |> 
  select(!c(County, Generation2023, `Heat Input2023`))

# Get only the region from the ORSPL|Unit|Region column
nei_efs <- nei_efs |> 
  separate_wider_delim(
    `ORSPL|Unit|Region`,
    "|",
    names = c("ORISPL Code", "Unit Code", "Region")
  )

# And we want a list containing the NEI EFs for each region
nei_efs <- nei_efs |> 
  nest(.by = `Region`) |> 
  pull(data)


## Prep Load Bin Data ======
# As mentioned above, the ranges used to select from the excel sheets are
#   unnecessarily expansive. All (and only) columns without data that we've read
#   in get assigned names containing "...", so unselect those columns.
#   Additionally, filter to ensure that the unit name is present, which
#   eliminates the extra rows.
ff_load_bin_data <- ff_load_bin_data_raw |> 
  map(filter, !is.na(`Full Unit Name`)) |> 
  map(select, !contains("..."))

# In each df, select only relevant columns
ff_load_bin_data <- ff_load_bin_data |> 
  map(~select(., !(State:FuelType)))

# Change data type of ORISPL code to character
ff_load_bin_data <- ff_load_bin_data |> 
  map(~ mutate(.x, `ORISPL Code` = as.character(`ORISPL Code`)))
  


## Prep Load Bin Keys ======
# A region's "load bin key" is the set of the names of the fossil-fuel load bins.
#   Get the column names of each data measure table, select only those with only 
#   digits in them, coerce to numeric. For each region, the results of the 9 
#   different data measure tables will look the same, so the load bins repeat 9 
#   times for each region (which is how we want this object structured, for 
#   clean_data below). Thus we get an "expanded" list, of length 126.
ff_load_bin_keys_expanded <- map(ff_load_bin_data, colnames) |> 
  map(str_subset, "^\\d+$") |> 
  map(as.numeric)

# Then get a unique version
ff_load_bin_keys <- unique(ff_load_bin_keys_expanded)


## Perform Cleaning ===========
# clean_data will be applied to each data measure table. It does 2 main things:
#   1. It makes each table longer, such that each EGU-load bin pair gets a row,
#   and 2. It creates a column containing the value of the relevant data
#    measure in the NEXT fossil-fuel load bin (which gets used later on).
clean_data <- function(ff_load_bin_data_df, ff_load_bin_key) {
  ff_load_bin_data_df <- ff_load_bin_data_df |> 
    pivot_longer(
      # Selects any columns which appear in ff_load_bin_key
      any_of(as.character(ff_load_bin_key)),
      names_to = "ff_load_bin_col",
      values_to = "data") |> 
    mutate(
      ff_load_bin_col = as.numeric(ff_load_bin_col),
      
      # lead() simply offsets data by 1. Since the df is ordered by load bin,
      #   this gives you the data measure value of the next load bin.
      data_next_bin = lead(data), 
      
      # When the load bin is as high as it can go, there's no "next" load bin.
      #   Thus, lead() pulls in the lowest load bin from the NEXT EGU, but
      #   this value should really just be NA. So if_else() to look for when
      #   the load bin is the highest in ff_load_bin_key, and assign NA to the
      #   value of data in the next load bin, since there is no next load bin.
      data_next_bin = if_else(
        ff_load_bin_col == max(ff_load_bin_key),
        NA,
        data_next_bin
      ),
      
      # Do the same thing for the load bin column itself
      ff_load_bin_next_col = lead(ff_load_bin_col),
      ff_load_bin_next_col = if_else(
        ff_load_bin_col == max(ff_load_bin_key),
        NA,
        ff_load_bin_next_col
      )
    ) |> 
    relocate(ff_load_bin_next_col, .after = ff_load_bin_col)
  
  return(ff_load_bin_data_df)
}

# Apply clean_data
ff_load_bin_data_final <- ff_load_bin_data |> 
  map2(ff_load_bin_keys_expanded, clean_data)

# Rename the two data columns to have the data measure in them.
ff_load_bin_data_final <- ff_load_bin_data_final |> 
  map2(rep(data_measures, 14),
       ~ rename_with(.data = .x, .fn = str_c, .cols = contains("data"), .y, sep = "_"))


## Prep and Save Cleaned Data ===========
# To get the final version of the data, combine each region's 9 data measure tables
#   into one.

# Split up the list of the 126 data measure tables into a list of 14 lists, each
#   representing a region and each containing 9 data tables. Then, for each of
#   those 14 lists, bind the cols together.
ff_load_bin_data_final <- ff_load_bin_data_final |> 
  split(sort(rep(1:14, 9))) |> 
  map(bind_cols) |> 
  suppressMessages()

#  Next, you have a bunch of unnecessarily duplicated columns, so remove them by
#   only selecting the first instances of those columns, plus all the data
#   columns. Also rename to remove the ...1, ...2, etc. from the column names
ff_load_bin_data_final <- ff_load_bin_data_final |> 
  map(~ select(.x, `ORISPL Code...1`:ff_load_bin_next_col...5 | contains("data"))) |> 
  map(~ rename_with(
    .data = .x,
    .fn = str_sub,
    .cols = `ORISPL Code...1`:ff_load_bin_next_col...5,
    start = 1L,
    end = -5L)
  )

# Save
write_rds(ff_load_bin_data_final, "./avertr_setup_output/ff_load_bin_data_final.rds")



# GET BAU RESULTS #############
# This section gets information on the data measures in the BAU scenario
#   for each region. The final result is a list containing a vector for each
#   data measure, where each vector contains the data measure for a given
#   EGU, for a given hour of the year.
# Note that the code for this section largely mirrors the code in avertr.R. I'm
#   doing all this here, however, since the BAU scenario doesn't depend on
#   user input, and thus only needs to be calculated once. This should help
#   avertr.R run faster.


## Binnify ==============
# NA vector for now, but will eventually store the "binnified" version of each
#   vector from load_8760s_bau
ff_load_bin_8760_bau <- rep(NA, 8760)

# This function binnifies each vector. Specifically, for each raw hourly load
#   within load_8760s_bau, it finds the closest load bin which is less than or
#   equal to the element. It does not simply find the closest load bin —
#   it never matches to a higher load bin. This is because of the way that we
#   do the interpolation below.
binnify <- function(load_8760, ff_load_bin_key) {
  
  # For each hour of the year
  for (i in 1:8760) {
    
    # Gives a vector containing the difference between raw load and each load
    #   bin within the region.
    diff <- load_8760[i] - ff_load_bin_key
    
    # Exclude all bins where the load bin exceeds the raw load
    diff[diff < 0] <- NA
    
    # Find the smallest difference
    bin_index <- which.min(diff)
    
    # The load bin with the smallest difference is our load bin in this slot
    ff_load_bin_8760_bau[i] <- ff_load_bin_key[bin_index]
  }
  return(ff_load_bin_8760_bau)
}

# Call the function and bind it with a column for the datetime and a column with the
#   raw load
ff_load_bin_8760s_bau <- load_8760s_bau |> 
  map2(ff_load_bin_keys, binnify) |> 
  map2(
    load_8760s_bau,
    ~ bind_cols(
      datetime_8760_col = datetime_8760,
      load_8760_col = .y,
      ff_load_bin_8760_col = .x
    )
  )


## Assign Data Values ==============
# Now we take our BAU ff load bins, and join on the data measures for the
#   appropriate load bins from ff_load_bin_data_final
assigned_data_regions_bau <- map2(
  ff_load_bin_8760s_bau,
  ff_load_bin_data_final,
  left_join, join_by(ff_load_bin_8760_col == ff_load_bin_col)
)


## Ozone Season Split =============
# In each region, for each hour of the year, we have a data measure giving ozone
#   season values and a data measure giving non-ozone season values (besides
#   generation, which does not depend on the ozone season). But we only want the
#   ozone season values for hours in the ozone season, and the non-ozone season
#   values for values not in the ozone season.

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
  
  assigned_data_ozoned <- bind_rows(oz, non)
  
  return(assigned_data_ozoned)
}

assigned_data_regions_selected_bau <- assigned_data_regions_bau |> 
  map(ozone_split)

# To save some memory
rm(assigned_data_regions_bau)


## Interpolate =============
# Now we interpolate the data. For each hour of the year, for each EGU, we
#   have its data measure for the load bin and the next load bin. Recall that
#   the raw generation value (for the EGU, for the hour) falls between the load
#   bin and the next load bin. Thus, based on the raw generation value, we
#   linearly interpolate between the two load bins.

interpolate <- function(assigned_data) {
  ff_load_bin_8760_bau <- pull(assigned_data, ff_load_bin_8760_col)
  
  ff_load_bin_8760_next_bau <- pull(assigned_data, ff_load_bin_next_col)
  
  load_8760_bau <- pull(assigned_data, load_8760_col)
  
  # The "metadata" here is the hour datetime, the raw load, and the load bin
  metadata <- select(assigned_data, datetime_8760_col:ff_load_bin_next_col)
  
  current_data <- assigned_data |>
    select(contains("data") & !contains("next"))

  next_data <- assigned_data |>
    select(contains("data") & contains("next"))
  
  # This process comes directly from the AVERT macros code
  interpolate_inner <- function(cd, nd) {
    slope <- (cd - nd) / (ff_load_bin_8760_bau - ff_load_bin_8760_next_bau)
    intercept <- cd - (slope * ff_load_bin_8760_bau)
    val <- (load_8760_bau * slope) + intercept
    return(val)
  }
  
  # modify2 used because it returns a tibble (map2 would return a list)
  interped_inner <- modify2(current_data, next_data, interpolate_inner)
  
  interped_inner <- bind_cols(metadata, interped_inner)
  
  return(interped_inner)
  
}

interped_data_regions <- map(assigned_data_regions_selected_bau, interpolate)

# To save some memory
rm(assigned_data_regions_selected_bau)


## Add PM2.5, VOCs, NH3 =========
# PM2.5, VOCs, and NH3 are estimated by multiplying the heat data measure by a
#   generating unit-specific emission factor from the NEI.

### fix join issue --------------
# # If we try joining by ORISPL Code and Unit Code, we see that there are some
# #   EGUs in interped_data_regions which do not have any match in the NEI EFs:
# interped_anti_join <- interped_data_regions |>
#   map2(nei_efs, anti_join, by = join_by(`ORISPL Code`, `Unit Code`))
# 
# # Inspection shows that there's a problem with how the Unit Code column data
# #   was entered into the Regional Data Files. E.g., one unit code is "3-1" but
# #   it gets read into the Regional Data File as the date March 1. And codes
# #   like "01" are simply read as "1," etc.
# 
# # Fortunately, if we take the anti-joined data, we can see that by joining on
# #   the rows that failed to join using ORISPL code and unit name, we get
# #   almost all of them to match
# interped_anti_join <- interped_anti_join |>
#   map2(
#     nei_efs,
#     anti_join,
#     by = join_by(`ORISPL Code`, `Full Unit Name` == `Full Name`)
#   )
# 
# # (Why not just join on unit name in the first place? Because some of the
# #   unit names differ between the NEI data and the RDF. ORISPL and Unit Code
# #   should be more reliable.)
# 
# # What are the stragglers? Just these units from Mid-Atlantic
# interped_anti_join |>
#   pluck(5) |>
#   distinct(`ORISPL Code`, `Unit Code`)

# Correct the stragglers by hand
interped_data_regions[[5]] <- interped_data_regions[[5]] |> 
  mutate(`Unit Code` = case_when(
    # In NEI EFs sheet, only one unit with ORISPL 50611. It's unit code is
    #   listed as 031, which makes perfect sense.
    `ORISPL Code` == "50611" & `Unit Code` == "31" ~ "031",
    # In NEI EFs sheetm exactly three units with ORISPL 55297. They have unit
    #   codes 001, 002, and 003, which again makes perfect sense.
    `ORISPL Code` == "55297" & `Unit Code` == "3" ~ "003",
    `ORISPL Code` == "55297" & `Unit Code` == "1" ~ "001",
    `ORISPL Code` == "55297" & `Unit Code` == "2" ~ "002",
     TRUE ~ `Unit Code`)
  )

# # One last check to make sure that this two-part join now works
# interped_data_regions |> 
#   map2(nei_efs, anti_join, by = join_by(`ORISPL Code`, `Unit Code`)) |> 
#   map2(
#     nei_efs,
#     anti_join,
#     by = join_by(`ORISPL Code`, `Full Unit Name` == `Full Name`)
#   )

### join -------
# Then actually do the two-part join
interped_data_regions <- interped_data_regions |> 
  map2(nei_efs, left_join, by = join_by(`ORISPL Code`, `Unit Code`)) |> 
  map2(
    nei_efs,
    left_join,
    by = join_by(`ORISPL Code`, `Full Unit Name` == `Full Name`)
  )

# We end up with duplicates of the three columns, one from the first join and
#   one from the second. The columns will either have the same value, or
#   one will be NA and the other will have the value. We combine them with
#   coalesce, which takes the first non-NA value, and then remove them.
interped_data_regions <- interped_data_regions |> 
  map(~ mutate(
    .x,
    PM2.52023 = coalesce(PM2.52023.x, PM2.52023.y),
    VOCs2023 = coalesce(VOCs2023.x, VOCs2023.y),
    NH32023 = coalesce(NH32023.y, NH32023.x)
  )) |> 
  map(select,!c(
    PM2.52023.x,
    VOCs2023.x,
    NH32023.x,
    PM2.52023.y,
    VOCs2023.y,
    NH32023.y
  ))

# # Check to ensure there are no remaining NAs
# interped_data_regions |> 
#   map(select, PM2.52023:NH32023) |> 
#   map(map, is.na) |> 
#   map(map, sum)

# We also now have two different Unit Code columns, Unit Code.x and Unit code.y.
#   The first is from the RDF (after making the four corrections I made above
#   by hand), the second is from the NEI. There are some cases where the first
#   join fails because the RDF unit code is messed up. In all of these cases,
#   the second join (using the unit name) succeeds. Thus, in these cases, the
#   Unit Code.x and Unit Code.y columns will have different values, and we want
#   to use the one from Unit Code.y. In other cases, the first join succeeds
#   but the second join (using the unit name) fails because the unit names
#   are different. In this case, Unit Code.y will be NA, but since the first
#   join already worked, we can just take the unit code from the first join.
# In sum: when neither Unit Code.x nor Unit Code.y are NA, but they have
#   conflicting values, we want Unit Code.y. Where Unit Code.y is NA, we want
#   Unit Code.x. (There should be no situations where Unit Code.x is NA, see
#   code chunk immediately below.)

# # Unit Code.x is never NA, Unit Code.y is sometimes NA
# interped_data_regions |> 
#   map(select, `Unit Code.x`, `Unit Code.y`) |>
#   map(map, is.na) |>
#   map(map, sum)










# LEFT OFF HERE!


# TRY a case_when, if it's too slow figure something else out w coalesce
interped_data_regions <- interped_data_regions |> 
  map(select, !`Unit Code.x`) |> 
  map(rename, `Unit Code` = `Unit Code.y`) |> 
  map(relocate, `Unit Code`, .after = `ORISPL Code`)









### calculate emissions ----------
interped_data_regions <- interped_data_regions |> 
  map(~ mutate(
    .x,
    data_pm25 = data_heat * PM2.52023,
    data_voc = data_heat * VOCs2023,
    data_nh3 = data_heat * NH32023
  ))

end_time <- Sys.time()
time_taken <- end_time - start_time









# Should be good! check these results against avertr.

write_rds(interped_data_regions, "HIGHLYTEMPORARY_interped_data_regions.rds")



interped_data_regions[[1]] |> head() |> view()

interped_data_regions |> 
  map(distinct, `Full Unit Name`) |> 
  map(nrow)

interped_data_regions |> 
  map(distinct, `ORISPL Code`, `Unit Code`) |> 
  map(nrow)





# Starting to worry more generally that units just aren't showing up at all...
#   is this true? You should just do some simple checks on the number
#   of units names at different points in this code, compare to number of
#   unit names, from both nei and rdf.



# AND do you join by unit name elsewhere? If so, be careful...
# It think u do this in avertr, at the end where u test
# THat's important! bc that wouldn't show in ur test, but it's a real concern.







# 4/21 NEXT STEPS: 1. Do the NEI thing immediately below to make sure it works (by checking against avertr) AND REALLY make sure that ur not introducing any NAs when you do this, that might have been happening in the other script
#   2. Do a full check where you replace the bau object in avertr.R with the imported
#   one generated in this sheet to ensure that differences are still small
#   2. Go back and comment out your code in this script, and otherwise clean it
#   up (but don't worry abt variable naming details rn), 3. Make a copy of avertr.R
#   to save it in its current form, and then start working on cleaning up avertr.R
#   and making it compatible with this, fully commenting it out, etc. 4. Add a
#   more official/standardized testing code. Maybe even make this a separate script?
#   Whatever makes most sense. 4. Look into both A. removing so2 emission events
#   units and B. how to deal with cases where we're in max or min load bins. 
#   (Currently when this happens we just silently get an NA.) Add
#   those features in while testing to ensure that you're doing it right. 5.
#   git init. 6. Add better user input stuff (like maybe make a function, load
#   in the different 8760s, etc.) 7. Consider best ways to present the output,
#   other metrics you want to add (warnings for hours > 15%, number of hours and metrics on
#   these hours, R^2, etc.) 8. Look into specifics of getting this on github, how
#   to format, etc.










