
<!-- README.md is generated from README.Rmd. Please edit that file -->

# avertr

<!-- badges: start -->

<!-- badges: end -->

[AVERT](https://www.epa.gov/avert) is a tool managed by the U.S.
Environmental Protection Agency (EPA) which models changes in emissions
from electric generating units as a result of changes in electricity
generation or demand. avertr is an R package which allows a user to run
AVERT in R.

avertr is directly based on the EPA’s AVERT, but it is not a product of
the EPA or affiliated with the EPA in any way.

Use of avertr assumes familiarity with AVERT. For more on AVERT, please
see the [EPA’s resources](https://www.epa.gov/avert), especially the
[AVERT User Manual](https://www.epa.gov/avert/avert-user-manual).

avertr currently reflects AVERT v4.3. It produces results which are
nearly identical to those produced by AVERT, only differing trivially
due to rounding differences.

## Installation

You can install the development version of avertr from GitHub with:

``` r
# install.packages("pak")
pak::pak("josephpendleton/avertr")
```

## Set Up

Besides installing and attaching the package, there are two further
steps required to use avertr:

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
    website](https://www.epa.gov/avert/download-avert#Main) and save it
    as a .xlsx file. (It will download as a .xlsb file.)

## Requirements

The 14 avertr regional data files for a single given year are together
about 4 GB.[^1] You must have 16 GB of RAM.

## Examples

### Running AVERT scenarios

With avertr, you use `avert()` to model the emissions (and generation
and heat input) changes associated with a change in hourly generation
within a given region in a given year. `avert()` takes an 8760-length
(or, in a leap year, 8784-length) vector representing the hourly MW
reduction fossil-fuel generation.

``` r
# Not run

# To model a 100 MW reduction in fossil-fuel generation in each hour in 2023
#   in New England

avert(
  hourly_load_reduction = rep(100, 8760),
  project_year = 2023,
  project_region = "New England",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
)
```

Instead of manually specifying the 8760 (or 8784) vector, you can use
`generate_reduction()` to generate it based on renewable energy and/or
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

You can also apply transmission and distribution losses to an 8760 (or
8784) vector with `adjust_reduction()`. This is useful for, e.g.,
modeling an energy efficiency program which will decrease demand by 10
MW in each hour, since a 10 MW decrease in demand will lead to an even
greater decrease in generation because generators normally must generate
10 MW *plus* whatever is lost in transmission and distribution. Since
the vector we pass to `avert()` represents reduction in generation, not
demand, we need to scale the 10 MW up by the region-and-year-specific
transmission and distribution loss factor.

``` r
# Not run

# To model an energy efficiency program which reduces demand by 10 MW in each
#   hour in 2023 in the Midwest

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

### Testing against AVERT results

You can use `test_annual()` and `test_hourly()` to verify that results
from avertr match results from AVERT. Expect trivial differences between
avertr and AVERT results due to rounding.

``` r
# Not run

# To model deploying 33 MW utility-scale solar capacity in 2019 in Texas

avert_tx_33_upv <- generate_and_avert(
  utility_solar_pv_capacity_mw = 33,
  project_year = 2019,
  project_region = "Texas",
  avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
  avertr_rdf_filepath = "./avertr_rdfs/2019/avertr_rdf_Texas_2019.rds"
)

test_annual(avert_tx_33_upv, "./AVERT_results/33MW_UPV_2019_TX_061025.xlsx")

test_hourly(avert_tx_33_upv, "./AVERT_results/33MW_UPV_2019_TX_061025.xlsx")
```

## More on avertr

### Why use avertr?

- It’s easier to integrate into R code — no need to worry about
  importing from Excel.

- It’s faster than AVERT’s Excel Edition and (seemingly often) faster
  than the Web Edition: running a scenario in avertr generally takes
  15-45 seconds.

- It makes it feasible to batch large numbers of runs.

  - Anecdote: I recently ran 84 scenarios using avertr. It took about 40
    minutes and required no interaction after initially setting up the
    runs.

- It’s probably more extensible than AVERT’s Excel Edition or Web
  Edition for most users.

- Playing around with avertr, especially the source code, can help you
  to better understand how AVERT works (especially if you’re more
  comfortable reading R code than VBA code).

### Things avertr can’t do

Here are some notable things AVERT can do which haven’t (yet!) been
incorporated into avertr:

- Run scenarios with battery storage

- Run scenarios with electric vehicles

- Generate COBRA and SMOKE text files

### Closing note

avertr works, but it is still *very* much a work in progress. There’s
lots of functionality I hope to add soon which will make avertr more
comprehensive and user-friendly. Please [create
issues](https://github.com/josephpendleton/avertr/issues) or [ask
questions](https://github.com/josephpendleton/avertr/discussions/categories/q-a)
as needed.

[^1]: This is because, unlike the AVERT regional data files, the avertr
    regional data files store a cached run of the business-as-usual
    scenario. AVERT calculates this business-as-usual scenario on demand
    each time it is run, even though it is always the same for a given
    region and year. So the larger size of the avertr regional data
    files helps avertr run faster.
