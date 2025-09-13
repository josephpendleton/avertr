#' Generate an hourly load reduction vector and run an AVERT scenario
#'
#' `generate_and_avert()` is a wrapper function that generates an hourly
#' load reduction vector with [generate_reduction()] and then runs
#' an AVERT scenario with [avert()], all in one step. It's more convenient
#' than running the two functions separately, especially because there's no
#' need to re-specify the shared arguments (`project_year`, `project_region`,
#' `avert_main_module_filepath`, and `avertr_rdf_filepath`).
#'
#' @param ... Arguments passed on to [generate_reduction()] (and then to
#' [avert()]).
#'
#' @returns A list with three elements:
#'    1. `differences_final`, a tibble. In a region with N fossil-fuel generating
#'        units, the tibble will have N * 8760 rows. Each row contains the changes
#'        in emissions (and generation and heat input) associated with a given
#'        fossil-fuel generating unit in a given hour. Those changes appear in
#'        the columns with a "data_" prefix. The other columns given additional
#'        information on the generating unit and the fossil-fuel load bin.
#'    2. `signal_to_noise`, a two-element list containing A. the linear
#'        regression coefficients which result from regressing the generation
#'        change calculated by avertr onto the input generation change, and
#'        B. the r squared statistic from same regression.
#'    3.  `pct_hourly_load_reduction`, an 8760-length (or, in a leap year,
#'        8784-length) numeric vector where each value represents the percent
#'        reduction in generation compared to the business-as-usual scenario.
#'        Presented as numbers ranging from 0 to 1. E.g., 0.028 would represent
#'        a 2.8% decrease in generation. AVERT suggests caution when modeling
#'        a scenario where any given hour has a >15% change in generation.
#' @export
#'
#' @examples
#' \dontrun{
#' # In two steps with generate_reduction() and avert()
#' generate_reduction(
#'   offshore_wind_capacity_mw = 200,
#'   apply_reduction_top_x_pct_hours = 5,
#'   reduce_x_pct_in_top_hours = 10,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' ) |>
#' avert(
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#'
#' # In one step with generate_and_avert()
#' generate_and_avert(
#'   offshore_wind_capacity_mw = 200,
#'   apply_reduction_top_x_pct_hours = 5,
#'   reduce_x_pct_in_top_hours = 10,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#' }
generate_and_avert <- function(...) {
  # Get a list of arguments to be passed to generate_reduction()
  args_generate_reduction <- list(...)

  # Select the subset of arguments which will also get passed to avert()
  args_avert <- args_generate_reduction[
    c(
      "project_year",
      "project_region",
      "avertr_rdf_filepath",
      "avert_main_module_filepath"
    )
  ]

  # Call generate_reduction()
  gen_out <- do.call(generate_reduction, args_generate_reduction)

  # Call avert() with a list consisting of the output of generate_reduction()
  #   and the avert() arguments
  do.call(avert, c(list(gen_out), args_avert))
}


