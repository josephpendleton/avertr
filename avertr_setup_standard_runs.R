# A script to run avertr_rdf_prepare.R and save the results for the "standard" RDFs
#   provided by EPA.


source("./avertr_rdf_prepare.R")

# 2023 ############
region_names <- c("California", "Carolinas", "Central", "Florida", "Mid-Atlantic",
                  "Midwest", "New England", "New York", "Northwest", "Rocky Mountains",
                  "Southeast", "Southwest", "Tennessee", "Texas")

arp_out <- setup_avertr("./regional_data_files", region_names, 2023)




iwalk(as_out$ff_load_bin_data_final, ~ write_rds(.x, paste0("ff_load_bin_data/2023/", .y, "_ff_load_bin_data_2023.rds")))

iwalk(as_out$nei_efs, ~ write_rds(.x, paste0("nei_efs/2023/", .y, "_nei_efs_2023.rds")))

iwalk(as_out$interped_data_regions, ~ write_rds(.x, paste0("bau_scenarios/2023/", .y, "_bau_scenarios_2023.rds")))


