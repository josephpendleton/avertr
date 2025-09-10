# Generates an 8760 hourly load reduction vector to be input into avert(). For
#   Rooftop PV and distributed storage, losses used as of AVERT v4.3 are
#   hard-coded. If all you want to do is adjust an 8760 representing an onsite
#   energy program to also account for T&D losses then use adjust_reduction().

#' Title
#'
#' @param project_year
#' @param project_region
#' @param avert_main_module_filepath
#' @param avertr_rdf_filepath
#' @param apply_reduction_top_x_pct_hours
#' @param reduce_x_pct_in_top_hours
#' @param reduce_annual_generation_by_x_gwh
#' @param reduce_each_hour_by_x_mw
#' @param onshore_wind_capacity_mw
#' @param offshore_wind_capacity_mw
#' @param utility_solar_pv_capacity_mw
#' @param rooftop_solar_pv_capacity_mw
#'
#' @returns
#' @export
#'
#' @examples
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
  #   1-100, but are more practically used here are fractions from 0 to 1.
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
    # NOTE: fix rounding to be consistent w excel rounding??
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


# If you have an 8760 vector you want to use to model an onsite
#   energy-efficiency program, you'll reduce load on fossil fuel units by the
#   amount of onsite energy you save, PLUS the T&D losses associated with
#   delivering that amount of energy. So your 8760 vector of purely onsite
#   energy reductions should be adjusted up to account for this fact. That's
#   what this function does. Losses from AVERT v4.3 are hard-coded in. Note that
#   rooftop PV and distributed storage are already adjusted up for T&D losses in
#   generate_reduction().

#' Title
#'
#' @param unadjusted_hourly_load_reduction
#' @param project_year
#' @param project_region
#'
#' @returns
#' @export
#'
#' @examples
adjust_reduction <- function(
    unadjusted_hourly_load_reduction = NULL,
    project_year = NULL,
    project_region = NULL
) {
  t_and_d_loss_factor <- t_and_d_losses |>
    dplyr::filter(`Data year` == project_year) |>
    dplyr::pull(project_region)

  adjusted_hourly_load_reduction <- unadjusted_hourly_load_reduction / (1 - t_and_d_loss_factor)

  return(adjusted_hourly_load_reduction)

}
