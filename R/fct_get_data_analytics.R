#' Retrieve Data Analytics
#'
#' Fetches analytical data for specified elements within a given date range and
#' organisational level.
#'
#' @param element_ids Character vector. A list of data element IDs to retrieve
#'   analytics for.
#' @param start_date Date or Character. The start date for the data query
#'   (format: "YYYY-MM-DD").
#' @param end_date Date or Character. The end date for the data query
#'   (format: "YYYY-MM-DD").
#' @param level Integer. The hierarchical level of the organisation.
#' @param auth Authentication object. Credentials required for API access.
#'
#' @return A data frame containing analytics data for the specified elements,
#'   dates, and organisation level.
#'
#' @noRd
get_data_analytics_ <- function(element_ids, start_date, end_date, level, auth) {

  get_analytics_by_level(element_ids = element_ids,
                         start_date = start_date,
                         end_date = end_date,
                         level = as.integer(level),
                         auth = auth)
}

#' Retrieve Population Data Analytics
#'
#' Fetches population-related analytical data for specified elements within a
#' given date range and organisational level.
#'
#' @param data_elements Data frame. A data frame containing data element IDs and
#'   names.
#' @param element_ids Character vector. A list of data element IDs to retrieve
#'   analytics for.
#' @param start_date Date or Character. The start date for the data query
#'   (format: "YYYY-MM-DD").
#' @param end_date Date or Character. The end date for the data query
#'   (format: "YYYY-MM-DD").
#' @param level Integer. The hierarchical level of the organisation.
#' @param auth Authentication object. Credentials required for API access.
#'
#' @return A data frame containing population analytics data, structured with
#'   years as columns and values for each data element.
#'
#' @noRd
get_population_data_analytics <- function(data_elements, element_ids, start_date, end_date, level, auth) {

  pop_periods <- format(seq(ymd(start_date), ymd(end_date), by = 'year'), '%Y')

  data <- get_analytics(
    dx %.d% element_ids,
    pe %.d% pop_periods,
    ou %.d% paste0('LEVEL-', level),
    co %.d% 'all',
    auth = auth
  )
  orgs <- get_organisations_by_level(org_ids = data$ou, level = as.integer(level), auth = auth)
  data_els <- data_elements %>%
    select(element_id, element, category_id, category) %>%
    distinct()

  data %>%
    left_join(data_els, join_by(dx == element_id, co == category_id)) %>%
    left_join(orgs, join_by(ou == id)) %>%
    mutate(year = as.integer(pe)) %>%
    select(-dx, -ou, -pe, -co)
}
