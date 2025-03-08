#' filter_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
my_summary <- function(.data, data_levels, org_level, .by, ...) {

  org_cols <- data_levels %>%
    filter(level <= org_level) %>%
    mutate(name = str_to_lower(name)) %>%
    pull(name) %>%
    make_clean_names()

  .data %>%
    arrange(across(any_of(c(org_cols, 'year', 'month')))) %>%
    summarise(
      ...,
      .by = all_of(c(org_cols, .by))
    )

}

#' @noRd
filter_by_orgs <- function(.data, orgs, data_levels, org_level) {
  if (is.null(orgs) || length(orgs) <= 0) {
    return(.data)
  }

  col <- data_levels %>%
    filter(level == org_level) %>%
    mutate(name = str_to_lower(name)) %>%
    pull(name) %>%
    make_clean_names()

  .data %>%
    filter(!!sym(col) %in% orgs)
}
