#' Retrieve Organizations by Level
#'
#' Fetches organization units based on the specified level.
#'
#' @param level Integer. The hierarchical level of the organization.
#' @param auth Authentication object. Credentials required for API access.
#'
#' @return A tibble containing organization details.
#'
#' @noRd
get_organisations <- function(level, auth) {

  get_organisations_by_level(level = as.integer(level), auth = auth)
}
