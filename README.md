(Temporary version of the README. Should change later.)

1. Clone this repository.
2. Add to it a subdirectory named regional_data_files which contains all (and only) the AVERT regional data files for a given year.
3. Add to it an empty download of the most recent version (4.3 as of now) of the AVERT Main Module WHICH YOU HAVE CONVERETED TO A .xls FILE! It will download as a .xlsb, you have to convert it.
4. Run avertr_setup.R. This generates data for the business-as-usual scenario for each region.
5. Open avertr.R. Define bau_scenario_region and capacity_8760 objects appropriately, based on the region and capacity change you're modeling. (avertr.R is currently set to model 500 MW of offshore wind in New England, by default).
6. Run avertr.R. The final output is the tibble differences_final. Each row gives you the change in the prouction of each measured pollutant (plus generation and heat input) in a given hour, for a given generating unit. So the number of rows should be 8760 * (number of generating units in the region)
7. If you want to test your results against results from AVERT, use avertr_test.R.

Important notes:
Not yet robust to leap years
Doesn't yet remove plants with rare SO2 emissions events
Doesn't yet return R^2 and some other important values
Haven't tested any scenarios besides 500 MW of OSW in NE in 2023
Still working on making input and general workflow easier for the user
