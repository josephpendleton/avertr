Quick overview of how to use avertr. You should also consult the headers of the scripts. This is a temporary and rough version of the README file.

1. Clone this repository.
2. Add to it a subdirectory named regional_data_files which contains all (and only) the AVERT regional data files for a given year. So far I've only tested this for the 14 2023 AVERT regional data files.
3. Add to it an empty download of the most recent version (4.3 as of now) of the AVERT Main Module which you have converted to a .xlsx file. (It will download as a .xlsb, you have to convert it.)
4. Create a folder named avertr_rdfs. Run avertr_rdf_prepare_standard_runs.R, which sources avertr_rdf_prepare.R. It takes about five minutes to run on my computer. This generates and saves avertr's version of the AVERT regional data files. It currently only works for all 14 2023 regional data files.
5. Use the avert() function to run AVERT scenarios in R!
6. If you want to test your results against results from AVERT, use the functions defined in avertr_test.R. avertr_run_tests.R contains some such tests I've run, although of course the AVERT runs are not included in this remote, so you'll need to generate them yourself if you want to run the tests yourself.

Notes:
- T&D losses haven't been fully incorporated into avertr yet, so some inputs (notably Rooftop PV) will look different between avertr and AVERT.
- The 14 2023 avertr RDFs are somewhat large — together they take up 5.5 GB on my disk.
