#' Generate an hourly load reduction vector
#'
#' `generate_reduction()` generates an 8760-length (or, in a leap year,
#' 8784-length) vector representing the hourly fossil-fuel generation reduction
#' associated with energy efficiency and/or renewable energy measures. The
#' resultant vector can be used with [avert()].
#'
#' The arguments specifying load change are "stacked" by default. E.g., passing
#' `reduce_each_hour_by_x_mw = 25` and `utility_solar_pv_capacity_mw = 80` models
#' the reduction in load associated with both reducing each hour by 25 MW and
#' deploying 80 MW of utility-scale solar.
#'
#' `apply_reduction_top_x_pct_hours` and `reduce_x_pct_in_top_hours` work together.
#' They must both be specified in order to have an effect. Only passing a value
#' to one of them does nothing.
#'
#' Values for `rooftop_solar_pv_capacity_mw` are automatically scaled up to
#' account for transmission and distribution losses. Similarly, values passed to
#' `apply_reduction_top_x_pct_hours`/`reduce_x_pct_in_top_hours`,
#' `reduce_annual_generation_by_x_gwh`, and/or `reduce_each_hour_by_x_mw` are
#' expected to be demand-side values, and thus are scaled up to account for
#' transmission and distribution losses. See [adjust_reduction()] for more
#' information on the arguments.
#'
#' The structure of this function largely mirrors the structure of AVERT's Main
#' Module, so see the AVERT User Manual for additional information.
#' @param project_year An integer giving the year of the run.
#' @param project_region A string giving the region of the run. It must exactly
#' match one of the 14 AVERT regions.
#' @param avert_main_module_filepath A string giving a filepath to an empty
#' version of the AVERT Main Module (v4.3) which has been saved as a .xlsx file.
#' @param avertr_rdf_filepath A string giving a filepath to the avertr regional
#' data file for the year and region.
#' @param apply_reduction_top_x_pct_hours A number from 0 to 100 giving the top
#' X% percent of hours in which to reduce load by the amount specified with
#' `reduce_x_pct_in_top_hours`.
#' @param reduce_x_pct_in_top_hours A number from 0 to 100 giving the percent
#' reduction in load that should occur in the top X% of hours, where X is
#' specified with `apply_reduction_top_x_pct_hours`.
#' @param reduce_annual_generation_by_x_gwh A number giving the total GWh by
#' which to reduce annual generation. The reduction is spread out evenly across
#' all hours of the year.
#' @param reduce_each_hour_by_x_mw A number giving the MW by which to reduce
#' load in each hour.
#' @param onshore_wind_capacity_mw A number giving the MW of onshore wind to be
#' deployed.
#' @param offshore_wind_capacity_mw A number giving the MW of offshore wind to
#' be deployed.
#' @param utility_solar_pv_capacity_mw A number giving the MW of utilty solar to
#' be deployed.
#' @param rooftop_solar_pv_capacity_mw A number giving the MW of rooftop solar
#' to be deployed.
#'
#' @returns An 8760-length (or, in a leap year, 8784-length) numeric vector
#' giving the hourly MW reduction resulting from the changes specified in the
#' arguments. Can be passed to [avert()].
#' @export
#' @examples
#' \dontrun{
#' # To model deploying 200 MW offshore wind capacity on top of reducing
#' #   generation by 10% in the top 5% of hours in 2023 in New England
#'
#' reduc_vec <- generate_reduction(
#'   offshore_wind_capacity_mw = 200,
#'   apply_reduction_top_x_pct_hours = 5,
#'   reduce_x_pct_in_top_hours = 10,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#'
#' avert(
#'   hourly_load_reduction = reduc_vec,
#'   project_year = 2023,
#'   project_region = "New England",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_New England_2023.rds"
#' )
#' }
#'
generate_reduction <- function(
    project_year,
    project_region,
    avertr_rdf_filepath,
    avert_main_module_filepath,

    apply_reduction_top_x_pct_hours = 0,
    reduce_x_pct_in_top_hours = 0,

    reduce_annual_generation_by_x_gwh = 0,
    reduce_each_hour_by_x_mw = 0,

    onshore_wind_capacity_mw = 0,
    offshore_wind_capacity_mw = 0,
    utility_solar_pv_capacity_mw = 0,
    rooftop_solar_pv_capacity_mw = 0
) {
  # DEFINE/LOAD OBJECTS ######

  # If it's a leap year, set the number of hours in the year to 8784, else 8760.
  # (Note that "8760" is used in variable names throughout the code, but refers
  #   to either 8760 or 8784.)
  if (lubridate::leap_year(project_year)) {
    yr_hrs <- 8760 + 24
  } else {
    yr_hrs <- 8760
  }

  # Scale down the two percents entered by users, since they're assumed to be
  #   0-100, but are more practically used here are fractions from 0 to 1.
  apply_reduction_top_x_pct_hours <- apply_reduction_top_x_pct_hours / 100
  reduce_x_pct_in_top_hours <- reduce_x_pct_in_top_hours / 100

  bau_case_ap_region <- readr::read_rds(avertr_rdf_filepath) |>
    purrr::pluck(paste0("bau_case_ap_", project_region))

  # Vector of each hour of the year 2023
  datetime_8760 <- seq(
    from = lubridate::ymd_hms(paste0(project_year, "-01-01 00:00:00")),
    by = "1 hour",
    length.out = yr_hrs
  )

  # This is the BAU load
  bau_load_8760 <- bau_case_ap_region |>
    dplyr::distinct(datetime_8760_col, load_8760_col) |>
    dplyr::pull(load_8760_col)

  hourly_load_reduction <- rep(0, yr_hrs)

  # This is the T&D losses for the region
  t_and_d_loss_factor <- t_and_d_losses |>
    dplyr::filter(`Data year` == project_year) |>
    dplyr::pull(project_region)



  # APPLY BOXES ######
  # Constructs the 8760 by sequentially applying the boxes from AVERT's
  #   EnterEEREData tab. Each sub-heading of this section represents a different
  #   box. We skip Box 4 because avertr doesn't do EVs (...yet).
  ## Box 1: Enter EE based on the % reduction of regional fossil generation ======
  if (apply_reduction_top_x_pct_hours != 0 & reduce_x_pct_in_top_hours != 0) {
    # The number of hours to apply the reduction in
    number_of_top_hours <- round(yr_hrs * apply_reduction_top_x_pct_hours)

    # The indices where the top hours are located
    top_hour_indices <- bau_load_8760 |>
      sort(decreasing = TRUE, index.return = TRUE) |>
      purrr::pluck("ix") |>
      (\(x) x[1:number_of_top_hours])()

    # Get the appropriate hourly reductions for each hour
    hourly_load_reduction[top_hour_indices] <- bau_load_8760[top_hour_indices] * reduce_x_pct_in_top_hours

    # Since this box is used for modeling energy efficiency measures, where the
    #   reductions occur on-site. Thus, like AVERT, we by default adjust for
    #   T&D losses.
    hourly_load_reduction <- adjust_reduction(
      hourly_load_reduction,
      project_year = project_year,
      project_region = project_region
    )
  }


  ## Box 2: And/or enter EE distributed evenly throughout the year ======
  if (reduce_annual_generation_by_x_gwh != 0 | reduce_each_hour_by_x_mw != 0) {

    # Convert GWh to MWh
    reduce_annual_generation_by_x_mwh <- reduce_annual_generation_by_x_gwh * 10^3

    # Add the reduce_annual_generation_by_x_mwh value, divided evenly over all
    #   8760 hours of the year. Since this box is used for modeling energy
    #   efficiency measures, where the reductions occur on-site. Thus, like
    #   AVERT, we by default adjust for T&D losses.
    hourly_load_reduction <- hourly_load_reduction + adjust_reduction(
      (reduce_annual_generation_by_x_mwh / yr_hrs),
      project_year = project_year,
      project_region = project_region
    )

    # Add the reduce_each_hour_by_x_mw value, divided evenly over all
    #   8760 hours of the year. Since this box is used for modeling energy
    #   efficiency measures, where the reductions occur on-site. Thus, like
    #   AVERT, we by default adjust for T&D losses.
    hourly_load_reduction <- hourly_load_reduction + adjust_reduction(
      reduce_each_hour_by_x_mw,
      project_year = project_year,
      project_region = project_region
    )
  }


  ## Box 3: And/or enter annual capacity of RE resources ======
  if (
    onshore_wind_capacity_mw != 0 |
    offshore_wind_capacity_mw != 0 |
    utility_solar_pv_capacity_mw != 0 |
    rooftop_solar_pv_capacity_mw != 0
  ) {
    # Capacity factors
    cfs <- tidyxl::xlsx_cells(
      avert_main_module_filepath,
      sheets = "EERE_Default",
    )

    # Use unpivotr to clean, filtering for the appropriate region. Gets capacity
    #   factors for all four technologies.
    cfs <- cfs |>
      unpivotr::behead("up-left", "Region") |>
      unpivotr::behead("up", "Project Type") |>
      unpivotr::behead("left", "Date") |>
      unpivotr::behead("left", "Hour") |>
      dplyr::filter(
        Region == project_region &
          row <= 8786
      ) |>
      unpivotr::pack() |>
      dplyr::select(row, value, `Project Type`) |>
      unpivotr::unpack() |>
      unpivotr::spatter(`Project Type`) |>
      dplyr::select(!row)

    cfs <- cfs |>
      # NOTE!!! Pretty sure this is the wrong filter, since it leaves in Feb. 29
      #   and removes 12/31. It should be filter(!(row %in% 1419:1442)). But
      #   this is the filter AVERT uses.
      dplyr::slice(1:yr_hrs) |>
      # Re-arrange columns to do the matrix multiplication below,  keeping the
      #   four technologies in the same order as a user would input capacities for
      #   them
      dplyr::relocate(`Onshore Wind`, `Offshore Wind`, `Rooftop PV`, `Utility PV`)

    # Vector of the four nameplate capacities the user has entered. Each has a
    #   default value of 0.
    capacity_vector <- c(
      "onshore_wind_capacity_mw" = onshore_wind_capacity_mw,
      "offshore_wind_capacity_mw" = offshore_wind_capacity_mw,
      "rooftop_solar_pv_capacity_mw" = rooftop_solar_pv_capacity_mw,
      "utility_solar_pv_capacity_mw" = utility_solar_pv_capacity_mw
    )

    # Multiply each renewable capacity factor vector by the matching
    #   capacity input by the user, then sum them together.
    summed_renewables <- cfs |>
      dplyr::mutate(
        `Onshore Wind` = `Onshore Wind` * capacity_vector["onshore_wind_capacity_mw"],
        `Offshore Wind` = `Offshore Wind` * capacity_vector["offshore_wind_capacity_mw"],
        `Rooftop PV` = `Rooftop PV` * capacity_vector["rooftop_solar_pv_capacity_mw"],
        `Utility PV` = `Utility PV` * capacity_vector["utility_solar_pv_capacity_mw"],

        # Offshore wind is NA for all inland regions, so replace these before
        #   summing
        `Offshore Wind` = tidyr::replace_na(`Offshore Wind`, 0),

        # The Rooftop PV capacity factor is not adjusted for T&D losses. But
        #   AVERT automatically adjusts it because Rooftop PV generation
        #   happens onsite. Similarly we adjust by default here.
        `Rooftop PV` = adjust_reduction(
          `Rooftop PV`,
          project_year = project_year,
          project_region = project_region
        ),

        summed_renewables = `Onshore Wind` + `Offshore Wind` + `Rooftop PV` + `Utility PV`
      ) |>
      dplyr::pull(summed_renewables)

    # Add this to whatever load reduction user has already entered.
    hourly_load_reduction <- hourly_load_reduction + summed_renewables
  }
  return(hourly_load_reduction)
}



#' Adjust an hourly load reduction vector for T&D losses
#'
#' `adjust_reduction()` adjusts an 8760-length (or, in a leap year, 8784-length)
#' vector up to account for transmission and distribution losses.
#'
#' Each element in the vector is scaled up by the constant 1 / (1 - t_and_d_loss),
#' where t_and_d_loss is the proportion of electricity lost during transmission
#' and distribution in the given year and region.
#'
#' The vector passed to [avert()] represents the reduction in fossil-fuel
#' generation. But sometimes you want to model a reduction in demand. E.g.,
#' suppose an energy efficiency program decreases demand by 10 MW each hour. You
#' can't directly pass an 8760-length vector of 10s to [avert()] because there is
#' a 10 MW reduction in demand in each hour, but [avert()] expects the reduction
#' in (fossil-fuel) generation in each hour. a 10 MW decrease in demand will
#' lead to an even greater decrease in generation, since generators must supply
#' 10 MW plus whatever is lost in transmission and distribution. Thus, we
#' must adjust the vector to be larger.
#'
#' Note that [generate_reduction()] automatically scales up rooftop PV capacity,
#' so do not call this function on the output of [generate_reduction()] to attempt
#' to adjust the rooftop PV capacity — that would scale the vector twice.
#' @param unadjusted_hourly_load_reduction An 8760-length (or, in a leap year,
#' 8784-length) numeric vector giving the hourly MW reduction before adjustment
#' for transmission and distribution losses.
#' @param project_year An integer giving the year of the run.
#' @param project_region A string giving the region of the run. It must exactly
#' match one of the 14 AVERT regions.
#'
#' @returns A vector with the same length as `unadjusted_hourly_load_reduction`.
#' @export
#' @examples
#' \dontrun{
#' # To model an energy efficiency program which reduces demand by 10 MW in each
#' #   hour of 2023 in Midwest
#'
#' decrease_in_demand <- rep(10, 8760)
#'
#' decrease_in_generation <- adjust_reduction(
#'   unadjusted_hourly_load_reduction = decrease_in_demand,
#'    project_year = 2023,
#'    project_region = "Midwest"
#'  )
#'
#' avert(
#'   hourly_load_reduction = decrease_in_generation,
#'   project_year = 2023,
#'   project_region = "Midwest",
#'   avert_main_module_filepath = "./avert-main-module-v4.3.xlsx",
#'   avertr_rdf_filepath = "./avertr_rdfs/2023/avertr_rdf_Midwest_2023.rds"
#' )
#' }
#'
adjust_reduction <- function(
    unadjusted_hourly_load_reduction,
    project_year,
    project_region
) {
  t_and_d_loss_factor <- t_and_d_losses |>
    dplyr::filter(`Data year` == project_year) |>
    dplyr::pull(project_region)

  adjusted_hourly_load_reduction <- unadjusted_hourly_load_reduction / (1 - t_and_d_loss_factor)

  return(adjusted_hourly_load_reduction)

}


