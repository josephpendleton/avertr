# A function to convert AVERT regional data files (RDFs) into avertr RDFs.
#   rdf_directory_filepath is the string of a filepath to a directory containing
#   all (and only) a set of AVERT RDFs for a given year. rdf_name_vector is a
#   vector of strings which it uses to name the avertr RDFs. This vector must
#   have the same length as the number of files in rdf_directory_filepath, and
#   the order of the names must correspond to the alphabetical order of the
#   AVERT RDFs located in rdf_directory_filepath. E.g., if
#   rdf_directory_filepath contains AVERT RDFs named B.xlsx, A.xlsx, and C.xlsx,
#   and you want to name them "rdf1", "rdf2", and "rdf3" respectively, pass
#   rdf_name_vector = c("rdf2", "rdf1", "rdf3"). rdfs_year is a number
#   representing the year of the AVERT RDFs. It is used at the very beginning
#   to generate the vector of hours in the year.
# I've currently only tested this using rdf_directory_filepath as a directory
#   containing all 14 AVERT RDFs for 2023. This script is NOT yet robust to
#   leap years. Takes ~5 mins to run on my machine.

#' Title
#'
#' @param rdf_directory_filepath
#' @param rdf_name_vector
#' @param rdfs_year
#'
#' @returns
#' @export
#'
#' @examples
prepare_rdfs <- function(rdf_directory_filepath, rdf_name_vector, rdfs_year) {

  # DEFINE/LOAD OBJECTS ############

  # If it's a leap year, set the number of hours in the year to 8784, else 8760.
  # (Note that "8760" is used in variable names throughout the code, but refers
  #   to either 8760 or 8784.)
  if (lubridate::leap_year(rdfs_year)) {
    yr_hrs <- 8760 + 24
  } else {
    yr_hrs <- 8760
  }

  # Vector of each hour of the year
  datetime_8760 <- seq(
    from = lubridate::ymd_hms(paste0(rdfs_year, "-01-01 00:00:00")),
    by = "1 hour",
    length.out = yr_hrs
  )

  # Filepaths to the AVERT regional data files being transformed into avertr rdfs
  rdf_filepaths <- list.files(
    path = rdf_directory_filepath,
    full.names = TRUE
  )

  # List of all the RDFs as tidyxl-style tibbles
  ff_load_bin_data_raw <- rdf_filepaths |>
    purrr::map(~ tidyxl::xlsx_cells(.x, sheets = "Data"))



  # load_bin_data_ap #############
  # This section creates an avertr-prepared version of the load bin data from
  #   the AVERT regional data files named load_bin_data_ap.


  ## Prepare Load Bin Data ======
  # Get the 8760 load in each region in the BAU scenario from the RDFs
  load_8760s_bau <- ff_load_bin_data_raw |>
    purrr::map(~ dplyr::filter(.x, col == 6 & row >= 4)) |>
    purrr::map(~ dplyr::pull(.x, numeric))

  # Clean the load bin data
  ff_load_bin_data <- ff_load_bin_data_raw |>
    # row > 1 to filter out first header row in each RDF, col > 8 to get rid of
    #   data related to hourly load. Also filter out a range of cells which
    #   would otherwise interfere with behead() calls below.
    purrr::map(~ dplyr::filter(.x, row > 1 & col >= 8 & !(row == 2 & col >= 15)))

  # To save some memory
  rm(ff_load_bin_data_raw)

  # These row ranges represent the different data
  row_ranges <- list(2:2000, 2002:4000, 4002:6000, 6002:8000,
                     8002:10000, 10002:12000, 12002:14000,
                     14002:16000, 16002:18000)

  # expand_grid takes the Cartesian product of the load bin data for each region
  #   and the row ranges. Then we filter for the cells in each range. If you're
  #   running all 14 regions, this returns a list of length 126 (14 regions * 9
  #   data measures = 126). The first 9 elements of the list are the 9 data
  #   measures for the first region. The next 9 elements of the list are the 9
  #   data measures for the second region. Etc. Each element is a tibble with
  #   information on each EGU's across all the different ff load bins, for the
  #   given data measure, for the given region. Let's call each of these
  #   elements a "data measure table".
  ff_load_bin_data <- tidyr::expand_grid(ff_load_bin_data, row_ranges) |>
    purrr::pmap(~ dplyr::filter(.x, row %in% .y))

  # Next we use unpivotr to (kinda) tidy the data
  ff_load_bin_data <- purrr::map(
    ff_load_bin_data,
    ~ .x |>
      unpivotr::behead("up-left", "data_measure") |>
      unpivotr::behead("up-left", "load_bin_indicator") |>
      unpivotr::behead("up", "ff_load_bin_col") |>
      unpivotr::behead("left", "state") |>
      unpivotr::behead("left", "county") |>
      unpivotr::behead("left", "lat") |>
      unpivotr::behead("left", "lon") |>
      unpivotr::behead("left", "fuel_type") |>
      unpivotr::behead("left", "orispl_code") |>
      unpivotr::behead("left", "unit_code") |>
      unpivotr::behead("left", "full_unit_name")
  )

  # Get an ordered list of the names of the data measures
  data_measures <- ff_load_bin_data |>
    purrr::map(~ unique(dplyr::pull(.x, data_measure)))

  # Set the names of each table to correspond with its data measure
  ff_load_bin_data <- purrr::set_names(ff_load_bin_data, data_measures)

  # Select a subset of columns from the tidyxl-style table, rename a column
  ff_load_bin_data <- ff_load_bin_data |>
    purrr::map(~ dplyr::select(.x, numeric, ff_load_bin_col:full_unit_name)) |>
    purrr::map(~ dplyr::rename(.x, data = numeric))

  # Overwrite the unit code with the last word in the unit name. This seems to
  #   be how it's done in AVERT code, and is necessary because the default
  #   unit codes in the RDFs are entered incorrectly (e.g., leading zeros of
  #   unit code "001" get ignored).
  ff_load_bin_data <- ff_load_bin_data |>
    purrr::map(~ dplyr::mutate(.x, unit_code = stringr::word(full_unit_name, -1)))


  ## Prep Load Bin Keys ======
  # A region's "load bin key" is the set of the names of its fossil-fuel load bins.
  #   For each table, pull and unique the column containing the ff load bings.
  #   For each region, the results of the 9 different data measure tables will
  #   look the same, so the load bins repeat 9 times for each region (which is
  #   how we want this object structured, for add_next_bin below). Thus we get
  #   an "expanded" list.
  ff_load_bin_keys_expanded <- purrr::map(
    ff_load_bin_data,
    ~ unique(dplyr::pull(.x, ff_load_bin_col))
  ) |>
    purrr::map(as.numeric)

  # Then get a non-expanded version of the list
  ff_load_bin_keys <- unique(ff_load_bin_keys_expanded)


  ## Add Next Bin ===========
  # add_next_bin will be applied to each data measure table. It creates a column
  #   containing the value of the relevant data measure in the *next*
  #   fossil-fuel load bin, which is used in the interpolation below.
  add_next_bin <- function(ff_load_bin_data_df, ff_load_bin_key) {
    ff_load_bin_data_df <- ff_load_bin_data_df |>
      dplyr::mutate(
        ff_load_bin_col = as.numeric(ff_load_bin_col),

        # lead() simply offsets data by 1. Since the df is ordered by load bin,
        #   this gives you the data measure value of the next load bin.
        data_next_bin = dplyr::lead(data),

        # When the load bin is as high as it can go, there's no "next" load bin.
        #   Thus, lead() pulls in the lowest load bin from the next EGU. But
        #   this value should really just be NA. So if_else() to look for when
        #   the load bin is the highest in ff_load_bin_key, and assigns NA to the
        #   value of data in the next load bin, since there is no next load bin.
        data_next_bin = dplyr::if_else(
          ff_load_bin_col == max(ff_load_bin_key),
          NA,
          data_next_bin
        ),

        # Do the same thing for the load bin column itself
        ff_load_bin_next_col = dplyr::lead(ff_load_bin_col),
        ff_load_bin_next_col = dplyr::if_else(
          ff_load_bin_col == max(ff_load_bin_key),
          NA,
          ff_load_bin_next_col
        )
      ) |>
      dplyr::relocate(ff_load_bin_next_col, .after = ff_load_bin_col)
    return(ff_load_bin_data_df)
  }

  # Apply add_next_bin()
  load_bin_data_ap <- purrr::map2(ff_load_bin_data, ff_load_bin_keys_expanded,
                           add_next_bin)

  # Rename the data columns to have the name of the appropriate data measure
  #   in them
  load_bin_data_ap <- load_bin_data_ap |>
    purrr::imap(
      ~ dplyr::rename_with(
        .data = .x,
        .fn = stringr::str_c,
        .cols = dplyr::contains("data"),
        .y,
        sep = "_"
      )
    )


  ## Finalize ===========
  # To get the final version of the data, combine each region's 9 data measure tables
  #   into one.

  # Split up the list of the data measure tables into a list of n lists (where
  #   n is the number of RDFs), each representing a region and each containing 9
  #   data tables. Then, for each of those 14 lists, bind the cols together.
  load_bin_data_ap <- load_bin_data_ap |>
    split(sort(rep(1:length(rdf_filepaths), 9))) |>
    purrr::map(dplyr::bind_cols) |>
    suppressMessages()

  # As a result, we have a bunch of unnecessarily duplicated columns, so remove
  #   them by only selecting the first instances of those columns, plus all the
  #   data columns.
  load_bin_data_ap <- load_bin_data_ap |>
    purrr::map(~ dplyr::select(.x, ff_load_bin_col...2:full_unit_name...11 | dplyr::contains("data"))) |>
    # And rename to remove the ...1, ...2, etc. from the column names
    purrr::map(~ dplyr::rename_with(
      .data = .x,
      .fn = stringr::str_extract,
      .cols = ff_load_bin_col...2:full_unit_name...11,
      pattern = ".+(?=\\.\\.\\.)")
    ) |>
    purrr::map(~ dplyr::relocate(.x, ff_load_bin_col:full_unit_name, .before = 1))

  # One last rename to make column names better
  load_bin_data_ap <- load_bin_data_ap |>
    purrr::map(~ dplyr::rename(.x,
                 data_generation = `data_Data: Generation (MW)`,
                 data_next_bin_generation = `data_next_bin_Data: Generation (MW)`,

                 data_so2_ozone = `data_Data: SO2 Ozone Season (lbs)`,
                 data_next_bin_so2_ozone = `data_next_bin_Data: SO2 Ozone Season (lbs)`,
                 data_so2_non = `data_Data: SO2 Not Ozone Season (lbs)`,
                 data_next_bin_so2_non = `data_next_bin_Data: SO2 Not Ozone Season (lbs)`,

                 data_nox_ozone = `data_Data: NOx Ozone Season (lbs)`,
                 data_next_bin_nox_ozone = `data_next_bin_Data: NOx Ozone Season (lbs)`,
                 data_nox_non = `data_Data: NOx Not Ozone Season (lbs)`,
                 data_next_bin_nox_non = `data_next_bin_Data: NOx Not Ozone Season (lbs)`,

                 data_co2_ozone = `data_Data: CO2 Ozone Season (Tons)`,
                 data_next_bin_co2_ozone = `data_next_bin_Data: CO2 Ozone Season (Tons)`,
                 data_co2_non = `data_Data: CO2 Not Ozone Season (Tons)`,
                 data_next_bin_co2_non = `data_next_bin_Data: CO2 Not Ozone Season (Tons)`,

                 data_heat_ozone = `data_Data: Heat Input Ozone Season (MMBtu)`,
                 data_next_bin_heat_ozone = `data_next_bin_Data: Heat Input Ozone Season (MMBtu)`,
                 data_heat_non = `data_Data: Heat Input Not Ozone Season (MMBtu)`,
                 data_next_bin_heat_non = `data_next_bin_Data: Heat Input Not Ozone Season (MMBtu)`))

  # Set names based on the rdf_name_vector argument
  load_bin_data_ap <- purrr::set_names(load_bin_data_ap, rdf_name_vector)



  # bau_case_ap #############
  # This section generates the BAU scenario scenario in each region. That is,
  #   it generates the emissions associated with the BAU load for each region.
  #   In avertr.R, the data from this BAU scenario gets subtracted from the
  #   data from whatever alternative scenario is being run, yielding averted
  #   emissions.


  ## Binnify ==============
  # NA vector for now, but will store the load bin associated with each hourly
  #   load value from load_8760s_bau
  ff_load_bin_8760_bau <- rep(NA, yr_hrs)

  # This function binnifies each vector. Specifically, for each raw hourly load
  #   within load_8760s_bau, it finds the closest load bin which is less than or
  #   equal to the element. It does not simply find the closest load bin —
  #   it never matches to a higher load bin. This is because of the way that we
  #   do the interpolation below.
  binnify <- function(load_8760, ff_load_bin_key) {
    # For each hour of the year
    for (i in 1:yr_hrs) {

      # Gives a vector containing the difference between raw load and each load
      #   bin within the region
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

  # Call the function and bind it with a column for the datetime and a column
  #   with the raw load
  ff_load_bin_8760s_bau <- load_8760s_bau |>
    purrr::map2(ff_load_bin_keys, binnify) |>
    purrr::map2(
      load_8760s_bau,
      ~ dplyr::bind_cols(
        datetime_8760_col = datetime_8760,
        load_8760_col = .y,
        ff_load_bin_8760_col = .x
      )
    )


  ## Assign Data Values ==============
  # Now we take our binnified BAU load and join on the data measures for the
  #   appropriate load bins from load_bin_data_ap. We expect the relationship
  #   to be many-to-many, and rely on the join to expand the size of the
  #   tibble by returning all matches. And throw an error if any given load
  #   bin from ff_load_bin_8760s_bau doesn't have a match in load_bin_data_ap.
  assigned_data_regions_bau <- purrr::map2(
    ff_load_bin_8760s_bau,
    load_bin_data_ap,
    ~dplyr::inner_join(
      .x,
      .y,
      dplyr::join_by(ff_load_bin_8760_col == ff_load_bin_col),
      na_matches = "never",
      unmatched = c("error", "drop"),
      relationship = "many-to-many"
    )
  )


  ## Ozone Season Split =============
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
          lubridate::ymd_hms(paste0(rdfs_year, "-05-01 00:00:00")),
          lubridate::ymd_hms(paste0(rdfs_year, "-09-30 23:00:00"))
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

  assigned_data_regions_bau <- assigned_data_regions_bau |>
    purrr::map(ozone_split)


  ## Interpolate =============
  # Now we interpolate the data. For each hour of the year, for each EGU, we
  #   have its data measure for the load bin and the next load bin. Recall that
  #   the raw generation value (for the EGU, for the hour) falls between the load
  #   bin and the next load bin. Thus, based on the raw generation value, we
  #   linearly interpolate between the two load bins.

  interpolate <- function(assigned_data) {
    ff_load_bin_8760_bau <- dplyr::pull(assigned_data, ff_load_bin_8760_col)

    ff_load_bin_8760_next_bau <- dplyr::pull(assigned_data, ff_load_bin_next_col)

    load_8760_bau <- dplyr::pull(assigned_data, load_8760_col)

    metadata <- dplyr::select(assigned_data, datetime_8760_col:full_unit_name)

    # Select all the load bin data
    current_data <- assigned_data |>
      dplyr::select(dplyr::contains("data") & !dplyr::contains("next"))

    # Select all the load bin data for the next load bin
    next_data <- assigned_data |>
      dplyr::select(dplyr::contains("data") & dplyr::contains("next"))

    # Do the interpolation. "cd" and "nd" for "current data" and "next data."
    interpolate_inner <- function(cd, nd) {
      slope <- (cd - nd) / (ff_load_bin_8760_bau - ff_load_bin_8760_next_bau)
      intercept <- cd - (slope * ff_load_bin_8760_bau)
      val <- (load_8760_bau * slope) + intercept
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

  bau_case_ap <- purrr::map(assigned_data_regions_bau, interpolate) |>
    purrr::set_names(rdf_name_vector)

  # To save some memory
  rm(assigned_data_regions_bau)



  # COMBINE AND RETURN ############
  avertr_rdfs <- purrr::map2(load_bin_data_ap, bau_case_ap, ~ dplyr::lst(.x, .y)) |>
    purrr::imap(
      ~ purrr::set_names(
        .x,
        c(stringr::str_c("load_bin_data_ap_", .y), stringr::str_c("bau_case_ap_", .y))
      )
    )

  return(avertr_rdfs)
}


