# A script to generate the "standard" avertr RDFs. These are just the standard
#   AVERT RDFs, prepared for use with avertr, and named in a special way so that
#   you can easily run an avertr scenario without having to specify a new
#   filepath each time (see header to avertr.R for more information.) As such,
#   the naming of these files is significant, since avertr.R uses it to
#   determine which avertr RDF to read in.
# Currently only running for 2023.

library(tidyverse)



source("./avertr_rdf_prepare.R")

# 2023 ############
region_names <- c("California", "Carolinas", "Central", "Florida", "Mid-Atlantic",
                  "Midwest", "New England", "New York", "Northwest", "Rocky Mountains",
                  "Southeast", "Southwest", "Tennessee", "Texas")

# This directory contains all and only the 14 RDFs from version 4.3 of AVERT
arp_out <- prepare_rdfs("./regional_data_files", region_names, 2023)

iwalk(
  arp_out,
  ~ write_rds(.x, paste0("avertr_rdfs/avertr_rdf_", .y, "_2023", ".rds"))
)
