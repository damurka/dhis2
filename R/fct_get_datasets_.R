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
get_completeness_data_ <- function(mapped_data, start_date, end_date, level, auth, timeout = 60) {

  completeness_data <- hfd %>%
    select(-ends_with('french'), -contains('desc')) %>%
    rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
    filter(str_detect(hfd_sheet, '^Reporting_completeness')) %>%
    left_join(mapped_data, join_by(hfd_id, hfd_title)) %>%
    select(-category_id, -category)

  # completeness_data <- data_elements %>%
  #   filter(hfd_sheet == 'Reporting_completeness')

  # dt_element_ids <- hfd %>%
  #   filter(!is.na(element_id)) %>%
  #   pull(element_id)

  dt_element_ids <- completeness_data %>%
    drop_na(element_id) %>%
    distinct(element_id) %>%
    pull(element_id)

  completeness_values <- c("REPORTING_RATE", "ACTUAL_REPORTS", "EXPECTED_REPORTS")
  completeness_values <- expand.grid(dt_element_ids, completeness_values) %>%
    mutate(combined = paste(Var1, Var2, sep = ".")) %>%
    pull(combined)

  periods <- format(seq(ymd(start_date), ymd(end_date), by = "month"),"%Y%m")

  data <- get_analytics(
    dx %.d% completeness_values,
    pe %.d% periods,
    ou %.d% paste0('LEVEL-', level),
    auth = auth
  )

  # organisations <- get_organisations_by_level(org_ids = data$ou, level = as.integer(level))


  data <- data %>%
    separate_wider_delim(dx, delim = '.', names = c('dx', 'dataset')) %>%
    left_join(completeness_data, by = c('dx' = 'element_id'), relationship = 'many-to-many') %>%
    # left_join(organisations, by = c('ou' = 'id')) #%>%
    mutate(
      pe = ym(pe),
      year = year(pe),
      month = month(pe, label = TRUE, abbr = FALSE),
      year = as.integer(year),
      month = factor(month, levels = month.name)
    ) %>%
    select(-dx, -ou, -pe) %>%
    # relocate(district, year, month, hfd_id, hfd_title, hfd_sheet) %>%
    # arrange(district, year, month) %>%
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
