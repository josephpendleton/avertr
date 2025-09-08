# A script to generate the "standard" avertr RDFs using prepare_rdfs()
# Runs 2017 through 2023
# Assumes:
#   1. You're running from a working directory which contains a directory
#   named regional_data_files
#   2. regional_data_files contains 7 folders named 2017, 2018, . . ., 2023
#   3. Each of those 7 folders contains all 14 AVERT regional data files for the
#   corresponding year.
#   4. The file names for the AVERT regional data files within each folder are
#   such that, if you sort them alphabetically by file name, they will be in the
#   same order as the strings in the region_names vector defined below. (This
#   should be the case unless you've changed the names of the AVERT regional
#   data files. One exception: you may need to go onto Web Archive to download
#   the 2020 Southwest RDF, since EPA has (I think mistakenly) left it out of
#   the 2020 RDFs zip file, and it may download with a different name.)

library(avertr)

# Create the top-level directory for storing the rdfs, if it doesn't exist
if (!dir.exists(file.path(".", "avertr_rdfs"))) {
  dir.create(file.path(".", "avertr_rdfs"))
}

# Create all the directories for storing each year's avertr rdfs, if they don't
#   already exist
for (i in 2017:2023) {
  if (!dir.exists(file.path(".", "avertr_rdfs", i))) {
    dir.create(file.path(".", "avertr_rdfs", i))
  }
}

years <- 2017:2023

region_names <- c("California", "Carolinas", "Central", "Florida", "Mid-Atlantic",
                  "Midwest", "New England", "New York", "Northwest", "Rocky Mountains",
                  "Southeast", "Southwest", "Tennessee", "Texas")

# Make a tibble for all the scenarios. The names need to match with the
#   arguments of prepare_save_rm_rdfs below (whose arguments in turn I've
#   made to match with the arguments from prepare_rdfs()).
avertr_rdf_inputs <- tibble::tibble(
  rdf_name_vector = replicate(length(years), region_names, simplify = FALSE),
  rdfs_year = years
)

# A function to run prepare_rdfs(), save the results, and then remove the list.
#   Necessary because we'd run out of memory otherwise.
prepare_save_rm_rdfs <- function(rdf_name_vector, rdfs_year) {

  # Run prepare_rdfs()
  rdfs_out <- prepare_rdfs(
    file.path(".", "regional_data_files", rdfs_year),
    rdf_name_vector,
    rdfs_year
  )

  # Save the outputs to the appropriate directory
  purrr::iwalk(
    rdfs_out,
    ~ readr::write_rds(
      .x,
      file.path(".", "avertr_rdfs", rdfs_year, paste0("avertr_rdf_", .y, "_", rdfs_year, ".rds"))
    )
  )

  # Remove the list of avertr rdfs
  rm(rdfs_out)

}

avertr_rdf_inputs |>
  purrr::pwalk(prepare_save_rm_rdfs)


