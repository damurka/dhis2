#' Get List of Supported Countries
#'
#' Retrieves a named vector of supported countries based on the presence of a
#' valid `dhis2_url`.
#'
#' @return A named character vector where names are country names and values are
#'   ISO2 country codes.
#'
#' @noRd
get_supported_countries <- function() {
  dt <- country %>%
    drop_na(dhis2_url)

  set_names(dt$iso2, dt$country)
}

#' Get Country Information by ISO2 Code
#'
#' `get_country` Retrieves country details from the `country` dataset based on
#' the provided ISO2 code.
#'
#' @param country_iso2 A scalar character string representing the 2-letter ISO
#'   country code.
#'
#' @return A tibble with country details if a match is found, otherwise `NULL`.
#'
#' @noRd
get_country <- function(country_iso2) {

  check_required(country_iso2)

  if (!is_scalar_character(country_iso2)) {
    stop('country_iso2 should be a scalar string')
  }

  dt <- country %>%
    drop_na(dhis2_url) %>%
    filter(iso2 == country_iso2)

  if (nrow(dt) == 0) return(NULL)

  dt
}
