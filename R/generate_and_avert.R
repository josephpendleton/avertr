#' Title
#'
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
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

  # call generate_reduction()
  gen_out <- do.call(generate_reduction, args_generate_reduction)

  # call avert() with a list consisting of the output of generate_reduction()
  #   and the avert() arguments
  do.call(avert, c(list(gen_out), args_avert))
}
