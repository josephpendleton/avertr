source("./avertr.R")
source("./avertr_test.R")

start_time <- Sys.time()

avertred <- avert(
  project_capacity = 500,
  project_region = "New York",
  project_year = "2023",
  project_type = "Offshore Wind",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx"
)

end_time <- Sys.time()
end_time - start_time

avertr_test_annual(avertred, "./test_scenarios/500MW_OSW_NY_05222025.xlsx")



# Enter the filepath to the AVERT main module workbook (converted from its
#   default .xlsb format to .xlsx) where you've run the scenario
#   that you wish to test avertr.R's output against