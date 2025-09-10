gen_avert <- function(
    project_year,
    project_region,
    avert_main_module_filepath,
    avertr_rdf_filepath,

    apply_reduction_top_x_pct_hours = 0,
    reduce_x_pct_in_top_hours = 0,

    reduce_annual_generation_by_x_gwh = 0,
    reduce_each_hour_by_x_mw = 0,

    onshore_wind_capacity_mw = 0,
    offshore_wind_capacity_mw = 0,
    utility_solar_pv_capacity_mw = 0,
    rooftop_solar_pv_capacity_mw = 0
) {
  generate_reduction(

    rooftop_solar_pv_capacity_mw = rooftop_solar_pv_capacity_mw,
    project_region = "project_region",
    project_year = project_year,
    avert_main_module_filepath = avert_main_module_filepath,
    avertr_rdf_filepath = avertr_rdf_filepath
  ) |>
    avert(
      project_region = project_region,
      project_year = project_year,
      avert_main_module_filepath = avert_main_module_filepath,
      avertr_rdf_filepath = avertr_rdf_filepath
    )
}
