
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avertr

<!-- badges: start -->

<!-- badges: end -->

[AVERT](https://www.epa.gov/avert) is a tool managed by the U.S.
Environmental Protection Agency (EPA) which models changes in emissions
from power plants as a result of changes in electricity generation or
demand. avertr is an R package designed to allow a user to run AVERT in
R.

avertr is directly based on the EPA’s AVERT, but it is not a product of
the EPA or affiliated with the EPA in any way.

Use of avertr assumes familiarity with AVERT. For more on AVERT, please
see the [EPA’s resources](https://www.epa.gov/avert), especially the
[AVERT User Manual](https://www.epa.gov/avert/avert-user-manual).

avertr reflects AVERT v4.3 (the most recent). It gives results which are
nearly-identical to those produced by AVERT, and only differ trivially
due to internal rounding differences.

## Installation

You can install the development version of avertr from GitHub with:

``` r
# install.packages("pak")
pak::pak("josephpendleton/avertr")
```

## Set Up

Besides installing and attaching the package, there are two setup steps
required to run avertr:

1.  Download the avertr regional data files (processed versions of
    AVERT’s regional data files). Like in AVERT, there are 14 regional
    data files (one per region) for each year. You only need the files
    for the region(s) and year(s) that you are interested in running
    scenarios for. You can download the avertr regional data files from
    [here](https://hu-my.sharepoint.com/:f:/g/personal/josephpendleton_hsph_harvard_edu/EmqeRAk_SMNJjOwcBaGF1cYBPhvXHcjbmbpopRR5Oif-7Q).
    1.  Alternatively, `prepare_rdfs()` allows you to generate the
        avertr regional data files locally from the original AVERT
        regional data files, although I expect most users won’t have
        reason to do this.
2.  Download the AVERT Main Module v4.3 from [the EPA’s
    website](https://www.epa.gov/avert/download-avert) and save it as a
    .xlsx file. It will download as a .xlsb file by default.

## Requirements

The 14 avertr regional data files for a single given year are together
about 4 GB.[^1] Your computer must have 16 GB of RAM.

## Examples

With avertr, you use `avert()` to model the emissions (and generation
and heat input) changes associated with a change in hourly generation
within a given region in a given year. avert() takes an 8760-length (or,
in a leap year, 8784-length) vector representing the hourly MW reduction
fossil-fuel generation.

``` r
# Not run

# To model a 100 MW reduction in fossil-fuel generation in each hour of 2023
#   in New England

avert(
  hourly_load_reduction = rep(100, 8760),
  project_year = 2023,
  project_region = "New England",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
)
```

Instead of manually specifying the 8760 (or 8784) vector, as above,
`generate_reduction()` can be used to it based on renewable energy or
energy efficiency projects.

``` r
# Not run

# To model deploying 200 MW offshore wind capacity on top of reducing 
#   generation by 10% in the top 5% of hours in 2023 in New England

reduc_vec <- generate_reduction(
  offshore_wind_capacity_mw = 200,
  apply_reduction_top_x_pct_hours = 5,
  reduce_x_pct_in_top_hours = 10,
  project_year = 2023,
  project_region = "New England",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
)

avert(
  hourly_load_reduction = reduc_vec,
  project_year = 2023,
  project_region = "New England",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
)
```

More compactly, `generate_and_avert()` performs both
`generate_reduction()` and `avert()` in one step. So the output of the
following chunk is identical to the output of the previous chunk.

``` r
# Not run

# To model deploying 200 MW offshore wind capacity on top of reducing 
#   generation by 10% in the top 5% of hours in 2023 in New England

generate_and_avert(
  offshore_wind_capacity_mw = 200,
  apply_reduction_top_x_pct_hours = 5,
  reduce_x_pct_in_top_hours = 10,
  project_year = 2023,
  project_region = "New England",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
)
```

You can also apply transmission and distribution losses to a vector with
`adjust_reduction()`. This is useful for, e.g., modeling an energy
efficiency program which will decrease demand by 10 MW each hour, since
a 10 MW decrease in demand will lead to an even greater decrease in
generation because generators must supply 10 MW *plus* whatever is lost
in transmission and distribution. Since the vector we pass to `avert()`
represents reduction in generation, we need to scale the 10 MW up by the
given region and year’s transmission and distribution loss.

``` r
# Not run

# To model an energy efficiency program which reduces demand by 10 MW in each
#   hour of 2023 in Midwest

decrease_in_demand <- rep(10, 8760)

decrease_in_generation <- adjust_reduction(
  unadjusted_hourly_load_reduction = decrease_in_demand,
  project_year = 2023,
  project_region = "Midwest"
)

avert(
  hourly_load_reduction = decrease_in_generation,
  project_year = 2023,
  project_region = "Midwest",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_Midwest_2023.rds"
)
```

## More on avertr

### Why use avertr?

- It’s easier to integrate into R code

- It’s faster than AVERT’s Excel Edition and (seemingly often) faster
  than the Web Edition — running a scenario in avertr generally takes
  15-45 seconds. This makes it feasible to run large numbers of
  scenarios.

### Things avertr can’t do

Here are some notable things that AVERT can do which haven’t (yet!) been
incorporated into avertr:

- Run scenarios with electric vehicles

- Run scenarios with battery storage

- Generate new regional data files for user-specified scenarios (which
  AVERT’s Statistical Module, not its Main Module, can do)

- Generate COBRA and SMOKE text files

### Other notes

An assortment of other things to keep in mind when using avertr:

- avertr works, but it is still very much a work in progress. There’s
  lots of functionality I hope to add soon which will make avertr more
  powerful and user-friendly.

- I have only tested avertr on macOS

- If you’re downloading the AVERT regional data files for 2020, note
  that the Southwest regional data file is not included in the 2020
  folder on the EPA’s current website. To generate the Southwest 2020
  avertr regional data file I downloaded the Southwest 2020 AVERT
  regional data file from [this Wayback Machine
  page](https://web.archive.org/web/20220324082548/https://www.epa.gov/avert/download-avert).
  (Note that the naming format of that file is different from the
  default names of the other 2020 AVERT regional data files on the
  current EPA website.)

[^1]: This is because, unlike the AVERT regional data files, the avertr
    regional data files store a cached run of the business-as-usual
    scenario. AVERT calculates this business-as-usual scenario on demand
    each time it is run, even though it is always the same for a given
    region and year. So the larger size of the avertr regional data
    files helps avertr run faster.
