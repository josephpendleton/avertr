# A script to run some tests on various avertr scenarios to ensure that they
#   match with their analogous AVERT scenarios. Mostly for internal use, cheking
#   the tool, etc.

source("./avertr.R")
source("./avertr_test.R")



# 6000 MW OSW NY#############
## Run avertr Scenario ===========
# osw_6000_ny <- avert(
#   project_capacity = 6000,
#   project_region = "New York",
#   project_year = "2023",
#   project_type = "Offshore Wind",
#   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx"
# )

osw_6000_ny <- generate_reduction(
  offshore_wind_capacity_mw = 6000,
  project_region = "New York",
  project_year = "2023",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/avertr_rdf_New York_2023.rds"
) |> 
  avert(
    project_region = "New York",
    project_year = "2023",
    avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
    avertr_rdf_filepath = "./avertr_rdfs/avertr_rdf_New York_2023.rds"
  )


## Test =============
### annual -----------
osw_6000_ny_test_annual <- test_annual(
  avertr_results = osw_6000_ny,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)

### hourly -----------
osw_6000_ny_test_hourly <- test_hourly(
  avertr_results = osw_6000_ny,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)



# 500 MW FLAT LOAD REDUCTION CALIFORNIA #############
## Run avertr Scenario ===========
# flat_500_ca <- avert(
#   project_year = 2023,
#   project_region = "California",
#   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#   # Applying the Western Interconnect T&D loss that AVERT applies 
#   hourly_load_reduction = rep((500 / (1 - .0867)), 8760),
# )

flat_500_ca <- adjust_reduction(
  unadjusted_hourly_load_reduction = rep(500, 8760),
  project_year = 2023,
  project_region = "California"
) |> 
  avert(
    project_region = "California",
    project_year = "2023",
    avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  )
  

## Test =============
### annual -----------
flat_500_ca_test_annual <- test_annual(
  avertr_results = flat_500_ca,
  avert_run_filepath = "./test_scenarios/500MW_flat_load_reduction_RM_06102025.xlsx"
)

### hourly -----------
flat_500_ca_test_hourly <- test_hourly(
  avertr_results = flat_500_ca,
  avert_run_filepath = "./test_scenarios/500MW_flat_load_reduction_RM_06102025.xlsx"
)



# 900 MW UTILITY PV ROCKY MOUNTAINS #############
## Run avertr Scenario ===========
# utilitypv_900_rocky <- avert(
#   project_year = 2023,
#   project_region = "Rocky Mountains",
#   project_type = "Utility PV",
#   project_capacity = 900,
#   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
# )

utilitypv_900_rocky <- generate_reduction(
  utility_solar_pv_capacity_mw = 900,
  project_region = "Rocky Mountains",
  project_year = "2023",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/avertr_rdf_Rocky Mountains_2023.rds"
) |> 
  avert(
    project_region = "Rocky Mountains",
    project_year = "2023",
    avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
    avertr_rdf_filepath = "./avertr_rdfs/avertr_rdf_Rocky Mountains_2023.rds"
  )


## Test =============
### annual -----------
utilitypv_900_rocky_test_annual <- test_annual(
  avertr_results = utilitypv_900_rocky,
  avert_run_filepath = "./test_scenarios/900MW_UPV_RM_061025.xlsx"
)


### hourly -----------
utilitypv_900_rocky_test_hourly <- test_hourly(
  avertr_results = utilitypv_900_rocky,
  avert_run_filepath = "./test_scenarios/900MW_UPV_RM_061025.xlsx"
)



# 20 MW ROOFTOP PV TEXAS #############
## Run avertr Scenario ===========
rooftoppv_20_texas <- avert(
  project_year = 2023,
  project_region = "Texas",
  project_type = "Rooftop PV",
  # Applying the Western Interconnect T&D loss that AVERT applies
  project_capacity = (20 / (1 - 0.0458)),
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
)

## Test =============
### annual -----------
rooftoppv_20_texas_annual <- test_annual(
  rooftoppv_20_texas,
  avert_run_filepath = "./test_scenarios/20MW_RPV_TX_06112025.xlsx"
)

### hourly -----------
rooftoppv_20_texas_hourly <- test_hourly(
  rooftoppv_20_texas,
  avert_run_filepath = "./test_scenarios/20MW_RPV_TX_06112025.xlsx"
)



# 2193 MW FLAT LOAD REDUCTION NEW ENGLAND #############
# This scenario is special because in the first hour of the year the new net
#   load exactly equals the lowest load bin.
## Run avertr Scenario ===========
flat_2193_ne <- avert(
  project_year = 2023,
  project_region = "New England",
  hourly_load_reduction = rep(2193, 8760),
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
)


## Test =============
### annual -----------
flat_2193_ne_annual <- test_annual(
  flat_2193_ne,
  avert_run_filepath = "./test_scenarios/2193MW_flat_load_reduction_NE.xlsx"
)

### hourly -----------
flat_2193_ne_hourly <- test_hourly(
  flat_2193_ne,
  avert_run_filepath = "./test_scenarios/2193MW_flat_load_reduction_NE.xlsx"
)



# SCEN NAME #############
## Run avertr Scenario ===========
avert(
  project_year,
  project_region,
  project_type = NULL,
  project_capacity = NULL,
  avert_main_module_filepath,
  hourly_load_reduction = NULL,
  avertr_rdf_filepath = NULL
)

## Test =============
### annual -----------

### hourly -----------


