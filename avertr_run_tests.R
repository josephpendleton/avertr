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

start_time_test <- Sys.time()

# Enter the filepath to the AVERT main module workbook (converted from its
#   default .xlsb format to .xlsx) where you've run the scenario
#   that you wish to test avertr.R's output against
test_results_annual <- avertr_test_annual(avertred, "./test_scenarios/6000MW_OSW_NY_06072025.xlsx")

test_results_hourly <- avertr_test_hourly(avertred, "./test_scenarios/6000MW_OSW_NY_06072025.xlsx")


end_time_test <- Sys.time()
end_time_test - start_time_test

