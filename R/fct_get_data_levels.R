#' Retrieve Organizational Data Levels
#'
#' Fetches the organizational unit levels for a given country. If the levels exist
#' locally in `org_levels`, it returns them; otherwise, it queries the API.
#'
#' @param country_iso2 A two-letter ISO country code (e.g., "KE" for Kenya).
#' @param auth Authentication credentials for API access.
#'
#' @details
#' - If the requested country's data is available in `org_levels`, it is returned.
#' - If not, the function fetches organization unit levels via the API.
#' - The API request retrieves only the `level` and `name` fields.
#'
#' @return @return A tibble with organizational unit levels
#'
#' @noRd
get_data_levels <- function(country_iso2, auth) {

  if (not_null(country_iso2) && is_scalar_character(country_iso2)) {
    dt <- org_levels %>%
      filter(iso2 == country_iso2) %>%
      arrange(level)

    if (nrow(dt) > 0) {
      return(dt)
    }
  }

  get_organisation_unit_levels(fields = 'level,name', auth = auth) %>%
    arrange(level)
}
