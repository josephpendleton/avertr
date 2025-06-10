# A script to run some test on various avertr scenarios to ensure that they
#   match with their analogous AVERT scenarios. Mostly for internal use, cheking
#   the tool, etc.

source("./avertr.R")
source("./avertr_test.R")



# 6000 MW OSW NY#############
## Run avertr Scenario ===========
osw_6000_ny <- avert(
  project_capacity = 6000,
  project_region = "New York",
  project_year = "2023",
  project_type = "Offshore Wind",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx"
)


## Test =============
### annual -----------
osw_6000_ny_test_annual <- avertr_test_annual(
  avertr_results = osw_6000_ny,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)

### hourly -----------
osw_6000_ny_test_hourly <- avertr_test_hourly(
  avertr_results = osw_6000_ny,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)



# 500 MW FLAT LOAD REDUCTION CA #############
## Run avertr Scenario ===========
flat_500_ca <- avert(
  project_year = 2023,
  project_region = "California",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  hourly_load_reduction = rep((500 / (1 - .0867)), 8760),
)


## Test =============
### annual -----------
flat_500_ca_test_annual <- avertr_test_annual(
  avertr_results = flat_500_ca,
  avert_run_filepath = "./test_scenarios/500MW_flat_load_reduction_RM_06102025.xlsx"
)

### hourly -----------
flat_500_ca_test_hourly <- avertr_test_hourly(
  avertr_results = flat_500_ca,
  avert_run_filepath = "./test_scenarios/500MW_flat_load_reduction_RM_06102025.xlsx"
)



# 900 MW UTILITY PV ROCKY MOUNTAINS #############
## Run avertr Scenario ===========
utilitypv_900_rocky <- avert(
  project_year = 2023,
  project_region = "Rocky Mountains",
  project_type = "Utility PV",
  project_capacity = 900,
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
)



# TEST ALL EQUAL w directly passing filepath

## Test =============
### annual -----------

### hourly -----------










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

