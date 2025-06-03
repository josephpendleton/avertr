# A script to run avertr_rdf_prepare.R and save the results for the "standard" RDFs
#   provided by EPA.


source("./avertr_rdf_prepare.R")

# 2023 ############
region_names <- c("California", "Carolinas", "Central", "Florida", "Mid-Atlantic",
                  "Midwest", "New England", "New York", "Northwest", "Rocky Mountains",
                  "Southeast", "Southwest", "Tennessee", "Texas")

arp_out <- setup_avertr("./regional_data_files", region_names, 2023)

iwalk(arp_out, ~ write_rds(.x, paste0("avertr_rdfs/avertr_rdf_", .y, "_2023", ".rds")))


