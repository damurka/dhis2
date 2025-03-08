#' get_datasets_
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_datasets_ <- function(data_elements, element_ids, start_date, end_date, level, auth) {

  dataset_ids <- data_elements %>%
    filter(element_id %in% element_ids) %>%
    distinct(dataset_id) %>%
    pull(dataset_id)

  get_data_sets_by_level(dataset_ids = dataset_ids,
                         start_date = start_date,
                         end_date = end_date,
                         level =as.integer(level),
                         auth = auth) %>%
    mutate(
      reporting_rate = actual_reports / expected_reports,
      year = factor(year)
    )
}
