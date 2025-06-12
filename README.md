Quick overview of how to use avertr. You should also consult the headers of the scripts. This is a temporary/rough version of the README file.

1. Clone this repository.
2. In the directory where you cloned the repository, create a subdirectory named regional_data_files. Add to regional_data_files all (and only) the AVERT regional data files for a given year. So far I've only tested this for the 14 2023 AVERT regional data files. So basically add all 14 2023 AVERT regional data files to regional_data_files.
3. Download the most recent version (4.3 as of now) of the AVERT Main Module to the directory where you cloned the repository. Convert the Main Module into a .xlsx file. (It will download as a .xlsb, you have to convert it.)
4. In the directory where you cloned the repository, create a subdirectory named avertr_rdfs. Run avertr_rdf_prepare_standard_runs.R, which sources avertr_rdf_prepare.R. It takes about five minutes to run on my computer. This generates and saves avertr's version of the AVERT regional data files. Again, I've currently only tested it on all 14 2023 regional data files.
5. Source avertr.R and then use the avert() function to run AVERT scenarios in R! See the header of avertr.R for more details on the function. See avertr_run_tests.R for some examples.
6. If you want to test your results against results from AVERT, use the functions defined in avertr_test.R. avertr_run_tests.R contains some such tests I've run, although of course the AVERT runs are not included in this remote, so you'll need to generate them in the AVERT Main Module and save the results as .xlsx files if you want to run the tests yourself.

Notes:
- avertr depends on four packages which must be installed: tidyverse, readxl, tidyxl, and unpivotr.
- T&D losses haven't been fully incorporated into avertr yet, so some inputs (notably Rooftop PV) will look different between avertr and AVERT.
- The 14 2023 avertr RDFs are somewhat large — together they take up 5.5 GB on my disk.
- Among many, many other things, I have yet to add the ability to "stack" different technologies — right now you can only model one technology in a given run. This should be an easy fix soon, just need to do it.
