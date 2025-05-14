#' Retrieve DHIS2 Health Facility Data
#'
#' `get_dhis2_hfd` retrieves health facility data (HFD) from the DHIS2 API for a
#' specific country and date range. It collects completeness, service, population,
#' and administrative data.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A list of class `cd_dhis2_hfd` containing:
#'
#' - **`completeness`**: Data frame of completeness data.
#' - **`service`**: Data frame of service data.
#' - **`population`**: Data frame of population data.
#' - **`admin`**: Data frame of administrative data.
#'
#' @examples
#' \dontrun{
#' # Retrieve data for Kenya from January 1, 2020, to January 1, 2024
#' hfd_data <- get_dhis2_hfd("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @export
get_dhis2_hfd <- function(country_iso3, start_date, end_date, level = 3, timeout = 3600) {

  hfd_sheet = iso3 = NULL

  org_units_headers <- get_organisation_unit_levels(fields = c('level', 'name')) %>%
    filter(level <= !!level) %>%
    arrange(level) %>%
    pull(name) %>%
    make_clean_names()

  org_units <- get_organisations_by_level(level = level) %>%
    mutate(iso3 = country_iso3)

  completeness_data <- get_completeness_data(country_iso3 = country_iso3,
                                             start_date = start_date,
                                             end_date = end_date,
                                             org_units = org_units,
                                             org_units_headers = org_units_headers,
                                             level = level,
                                             timeout = timeout)
  service_data <- get_service_data(country_iso3 = country_iso3,
                                   start_date = start_date,
                                   end_date = end_date,
                                   org_units = org_units,
                                   org_units_headers = org_units_headers,
                                   level = level,
                                   timeout = timeout)
  population_data <- get_population_data(country_iso3 = country_iso3,
                                         start_date = start_date,
                                         end_date = end_date,
                                         org_units = org_units,
                                         org_units_headers = org_units_headers,
                                         level = level,
                                         timeout = timeout)

  # admin_data <- data_elements %>%
  #   filter(hfd_sheet == 'Admin_data', iso3 == country_iso3)

  structure(
    list(
      completeness = completeness_data,
      service = service_data,
      population = population_data,
      # admin = admin_data
    ),
    class = 'cd_dhis2_hfd'
  )
}

#' Retrieve Service Data from DHIS2
#'
#' `get_service_data` retrieves service data from the DHIS2 API for a specified
#' country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing service data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{month}{Month of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   service_data <- get_service_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_service_data <- function(wb, mapped_data, start_date, end_date, org_units, org_units_headers, credentials, level = 3, timeout = 3600) {

  hfd_sheet = iso3 = element_id = dx = pe = ou = year = month = element_name =
    district = hfd_id = hfd_title = NULL

  last_col <- org_units_headers[length(org_units_headers)]

  service_data <- hfd %>%
    select(-ends_with('french'), -contains('desc')) %>%
    rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
    filter(str_detect(hfd_sheet, '^Service_data')) %>%
    left_join(mapped_data, join_by(hfd_id, hfd_title))

  dt_element_ids <- service_data %>%
    drop_na(element_id) %>%
    distinct(element_id) %>%
    pull(element_id)

  if (length(dt_element_ids) > 0) {

    data <- get_data_analytics_(element_ids = dt_element_ids,
                        start_date = start_date,
                        end_date = end_date,
                        level = level,
                        orgs = org_units,
                        data_els = service_data,
                        auth = credentials$auth) %>%
      my_summary2(
        org_cols = org_units_headers,
        .by = c('year', 'month', 'hfd_id', 'hfd_title', 'hfd_sheet'),
        value = sum(value)
      ) %>%
      select(-element_name) %>%
      relocate(all_of(org_units_headers), year, month, hfd_id, hfd_title, hfd_sheet) %>%
      arrange(across(all_of(c(org_units_headers, 'year', 'month')))) %>%
      append_missing_columns(service_data, org_units_headers)

    service_instruction <- 'Enter the reported number of individuals that received the specific service for each month and by subnational unit (district).
    CAUTION: The name and order of districts SHOULD BE the same across all the sheets. Do not create a new column or displace an existing column. Do not enter any data beyond the total number of health subnational units in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'

    create_sheets(
      wb = wb, .data = data, header_rows = c(org_units_headers, 'year', 'month'),
      instruction = service_instruction, instruction_row_height = 140
    )
  }
}

#' Retrieve Population Data from DHIS2
#'
#' `get_population_data` retrieves population data from the DHIS2 API for a
#' specified country and date range.
#'
#' @param country_iso3 Character. ISO3 code of the country.
#' @param start_date Character. Start date in "YYYY-MM-DD" format.
#' @param end_date Character. End date in "YYYY-MM-DD" format.
#' @param timeout Numeric. Timeout for API calls in seconds. Default is 60.
#'
#' @return A data frame containing population data, with columns:
#' \describe{
#'   \item{district}{District name.}
#'   \item{year}{Year of the data.}
#'   \item{hfd_id}{Health facility data identifier.}
#'   \item{hfd_title}{Title or description of the data element.}
#'   \item{hfd_sheet}{Category or sheet name for the data.}
#'   \item{value}{Value of the data element.}
#' }
#'
#' @examples
#' \dontrun{
#'   population_data <- get_population_data("KEN", "2020-01-01", "2024-01-01")
#' }
#'
#' @noRd
get_population_data <- function(wb, mapped_data, start_date, end_date, org_units, org_units_headers, credentials, level = 3, timeout = 3600) {

  hfd_sheet = iso3 = element_id = dx = pe = ou = element_name = district = year =
    hfd_id = hfd_title = NULL

  population_data <- hfd %>%
    select(-ends_with('french'), -contains('desc')) %>%
    rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
    filter(str_detect(hfd_sheet, '^Population_data')) %>%
    left_join(mapped_data, join_by(hfd_id, hfd_title))

  dt_element_ids <- population_data %>%
    drop_na(element_id) %>%
    distinct(element_id) %>%
    pull(element_id)

  if (length(dt_element_ids) > 0) {

    data <- get_data_analytics_(element_ids = dt_element_ids,
                                start_date = start_date,
                                end_date = end_date,
                                level = level,
                                is_population = TRUE,
                                orgs = org_units,
                                data_els = population_data,
                                auth = credentials$auth) %>%
      select(-element_name) %>%
      relocate(all_of(org_units_headers), year, hfd_id, hfd_title, hfd_sheet) %>%
      arrange(across(all_of(c(org_units_headers, 'year')))) %>%
      append_missing_columns(population_data, org_units_headers)

    population_instruction <- 'In this sheet, you will compile official population data used to produce denominators by the routine health information system in your country. These data are integrated into DHIS2 or other routine health data collection systems, and are usually derived from projections by the country\'s institute of statistics and/or demographic. Please enter data by district and year from year 2019 to 2023.
    WARNING: The order of the health districts MUST BE the same in all other sheets of this tool. Do not create new columns or move an existing column. Do not enter unnecessary data below the number of districts that exist in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'

    create_sheets(
      wb = wb, .data = data, header_rows = c(org_units_headers, 'year'),
      freeze_col = 3, instruction = population_instruction, instruction_row_height = 120
    )
  }
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
get_completeness_data <- function(wb,
                                  mapped_data,
                                  start_date,
                                  end_date,
                                  org_units,
                                  org_units_headers,
                                  credentials,
                                  level = 3,
                                  timeout = 3600) {

  hfd_sheet = iso3 = element_id = Var1 = Var2 = combined = dx = pe = ou = year =
    month  = element_name = district = hfd_id = hfd_title = dataset = NULL

  completeness_data <- hfd %>%
    select(-ends_with('french'), -contains('desc')) %>%
    rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
    filter(str_detect(hfd_sheet, '^Reporting_completeness')) %>%
    left_join(mapped_data, join_by(hfd_id, hfd_title)) %>%
    select(-category_id, -category)

  completeness_els <- completeness_data %>%
    drop_na(element_id) %>%
    distinct(element_id) %>%
    pull(element_id)

  if (length(completeness_els) > 0) {

    data <- get_completeness_data_(dataset_ids = completeness_els,
                                   start_date = start_date,
                                   end_date = end_date,
                                   level = level,
                                   orgs = org_units,
                                   completeness_data = completeness_data,
                                   auth = credentials$auth) %>%
      select(-element_name) %>%
      relocate(all_of(org_units_headers), year, month, hfd_id, hfd_title, hfd_sheet) %>%
      arrange(across(all_of(c(org_units_headers, 'year', 'month')))) # %>%
      # append_missing_columns(population_data, org_units_headers)

    create_sheets(
      wb = wb, .data = data, header_rows = c(org_units_headers, 'year', 'month')
    )
  }
}

#' Append Missing Columns to a Dataset
#'
#' Adds missing columns to the dataset by aligning it with a reference dataset of
#' elements. This function ensures that all required health facility data (`hfd_id`)
#' columns are included in the provided data, even if they are missing. Missing
#' columns are filled with `NA` values.
#'
#' @param .data A data frame containing `hfd_id`, `district`, `year`, and `month`.
#' @param element_data A reference data frame containing all potential `hfd_id` values,
#'        along with their corresponding `hfd_title` and `hfd_sheet`.
#'
#' @details
#' The function performs the following steps:
#' 1. Identifies missing `hfd_id` columns by comparing `element_data` with `.data`.
#' 2. Creates new rows for each missing `hfd_id` value, preserving the structure of `.data`
#'    (e.g., `district`, `year`, `month`) and filling `value` with `NA`.
#' 3. Appends the missing rows to the original dataset.
#'
#' @return A data frame that includes all original rows from `.data` plus additional rows
#' for missing `hfd_id` values.
#'
#' @examples
#' \dontrun{
#'   # Example input data
#'   main_data <- tibble(
#'     district = c("District A", "District B"),
#'     year = c(2021, 2021),
#'     month = c("January", "January"),
#'     hfd_id = c("ANC", "ANC"),
#'     value = c(100, 200)
#'   )
#'
#'   reference_data <- tibble(
#'     hfd_id = c("ANC", "PNC", "Csection"),
#'     hfd_title = c("Antenatal Care", "Postnatal Care", "Caesarian Sections"),
#'     hfd_sheet = c("Service_data_1", "Service_data_1", "Service_data_1")
#'   )
#'
#'   # Use the function to append missing columns
#'   extended_data <- append_missing_columns(main_data, reference_data)
#' }
#'
#' @note
#' - The function assumes that the columns `district`, `year`, `month`, and `value`
#'   exist in `.data`.
#' - Ensures that the output dataset aligns with the full set of elements specified
#'   in `element_data`.
#'
#' @noRd
append_missing_columns <- function(.data, element_data, org_cols) {

  hfd_id = hfd_title = hfd_sheet = district = year = month = data = NULL

  dt_hfd_ids <- element_data %>%
    pull(hfd_id)
  missing_columns <- unique(.data$hfd_id)
  missing_columns <- dt_hfd_ids[!(dt_hfd_ids %in% missing_columns)]
  if (length(missing_columns) > 0) {
    columns_to_add <- element_data %>%
      filter(hfd_id %in% missing_columns) %>%
      select(hfd_id, hfd_title, hfd_sheet)

    data_items <- .data %>%
      distinct(across(any_of(c(org_cols, "year", "month"))))
      # distinct(any_of(org_cols), year, month)

    extended_data_items <- columns_to_add %>%
      rowwise() %>%
      mutate(
        data = list(
          data_items %>%
            mutate(value = NA)
        )
      ) %>%
      unnest(data) %>%
      ungroup()

    bind_rows(.data, extended_data_items)
  }
}
