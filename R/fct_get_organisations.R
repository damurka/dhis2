#' Retrieve Organizations by Level
#'
#' Fetches organization units based on the specified level.
#'
#' @param level Integer. The hierarchical level of the organization.
#' @param country_iso Character.
#' @param auth Authentication object. Credentials required for API access.
#'
#' @return A tibble containing organization details.
#'
#' @noRd
get_organisations <- function(country_iso, level, auth) {
  force(level)
  check_required(country_iso)
  check_required(level)
  check_required(auth)

  get_organisations_by_level(level = level, auth = auth)
}
