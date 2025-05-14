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

#' Retrieve Reporting Completeness Data from DHIS2
#'
#' `get_completeness_data` retrieves completeness data from the DHIS2 API for a
#' specified country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing completeness data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{month}{Month of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{hfd_subtitle}{Subtitle for the data column.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   completeness_data <- get_completeness_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_completeness_data_ <- function(dataset_ids,
                                   start_date,
                                   end_date,
                                   level,
                                   orgs,
                                   completeness_data,
                                   auth,
                                   timeout = 3600) {

  periods <- format(seq(ymd(start_date), ymd(end_date), by = "month"),"%Y%m")

  completeness_values <- c("REPORTING_RATE", "ACTUAL_REPORTS", "EXPECTED_REPORTS")
  completeness_values <- expand.grid(dataset_ids, completeness_values) %>%
    mutate(combined = paste(Var1, Var2, sep = ".")) %>%
    pull(combined)

  # Limit orgs to only those relevant to our data
  ou_count <- nrow(orgs)
  pe_count <- length(periods)

  # Adjust batch size dynamically based on orgs & periods
  base_batch_size <- 50  # Default batch size for small requests
  max_requests <- 5000   # Avoid hitting API limits

  effective_element_batch_size <- min(base_batch_size, max(4, floor(max_requests / (ou_count * pe_count))))
  effective_pe_batch_size <- min(10, max(1, floor(max_requests / (ou_count * length(completeness_values)))))

  # Split `element_ids` dynamically
  element_batches <- split(completeness_values, ceiling(seq_along(completeness_values) / effective_element_batch_size))

  # Split `pe` dynamically
  pe_batches <- split(periods, ceiling(seq_along(periods) / effective_pe_batch_size))

  # Create batch combinations using `tidyr::expand_grid()`
  batch_combinations <- expand_grid(element_batch = element_batches, pe_batch = pe_batches)

  data <- map2(batch_combinations$element_batch, batch_combinations$pe_batch, ~ {
    print(.x)
    print(.y)
    get_analytics(
      dx %.d% paste(.x, collapse = ";"),
      pe %.d% paste(.y, collapse = ";"),
      ou %.d% paste0('LEVEL-', level),
      auth = auth
    )
  }) %>%
    bind_rows() %>%
    distinct()

  data <- data %>%
    separate_wider_delim(dx, delim = '.', names = c('dx', 'dataset')) %>%
    left_join(completeness_data, by = c('dx' = 'element_id'), relationship = 'many-to-many') %>%
    left_join(orgs, by = c('ou' = 'id')) %>%
    mutate(
      pe = ym(pe),
      year = as.integer(year(pe)),
      month = factor(month(pe, label = TRUE, abbr = FALSE), levels = month.name),
    ) %>%
    select(-dx, -ou, -pe) %>%
    mutate(
      hfd_id = case_match(dataset,
                          'REPORTING_RATE' ~ paste0(hfd_id, '_reporting_rate'),
                          'ACTUAL_REPORTS' ~ paste0(hfd_id, '_reporting_received'),
                          'EXPECTED_REPORTS' ~ paste0(hfd_id, '_reporting_expected')),
      hfd_subtitle = case_match(dataset,
                                'REPORTING_RATE' ~ 'Reporting completeness rate (%)',
                                'ACTUAL_REPORTS' ~ 'Received number (#)',
                                'EXPECTED_REPORTS' ~ 'Expected number (#)')
    ) %>%
    select(-dataset)

  return(data)
}
