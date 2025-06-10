# RUN AVERTR SCENARIO ##########
source("./avertr.R")

start_time <- Sys.time()

avertred <- avert(
  project_capacity = 6000,
  project_region = "New York",
  project_year = "2023",
  project_type = "Offshore Wind",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx"
)

end_time <- Sys.time()
end_time - start_time



# TEST ##########
source("./avertr_test.R")


## Annual =============
start_time_test_annual <- Sys.time()

test_results_annual <- avertr_test_annual(
  avertr_results = avertred,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)

end_time_test_annual <- Sys.time()
end_time_test_annual - start_time_test_annual


## Hourly ============
start_time_test_hourly <- Sys.time()

test_results_hourly <- avertr_test_hourly(
  avertr_results = avertred,
  avert_run_filepath = "./test_scenarios/6000MW_OSW_NY_06072025.xlsx"
)

end_time_test_hourly <- Sys.time()
end_time_test_hourly - start_time_test_hourly


