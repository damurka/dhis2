box::use(
  dplyr[...],
  here[here],
  janitor[make_clean_names],
  khisr[get_analytics_by_level, get_data_elements, get_data_sets_by_level, get_organisation_unit_levels],
  rlang[sym, `!!`, check_required, is_scalar_character, set_names],
  stringr[str_to_lower],
  tidyr[hoist, unnest_longer],
  readr[read_csv],
  vroom[vroom]
)

#' @export
get_supported_countries <- function() {
  dt <- vroom('app/data/country.csv.xz', show_col_types = FALSE) %>%
    filter(!is.na(dhis2_url))

  set_names(dt$iso3, dt$country)
}

#' @export
get_base_url <- function(country_iso3) {

  check_required(country_iso3)

  if (!is_scalar_character(country_iso3)) {
    stop('country_iso3 should be a scalar string')
  }

  dt <- vroom('app/data/country.csv.xz', show_col_types = FALSE) %>%
    filter(!is.na(dhis2_url), iso3 == country_iso3)

  if (NROW(dt) == 0) {
    return(NULL)
  } else {
    return(dt)
  }
}


#' @export
my_data_elements <- function(iso3, auth) {

  file <- paste0('app/data/data_elements_', tolower(iso3), '.csv.xz')
  if (file.exists(file)) {
    return(vroom(file, show_col_types = FALSE))
  }

  get_data_elements(fields = c('id','name','categoryCombo[categoryOptionCombos[id,name]]', 'dataSetElements[dataSet[id,name]], dataElementGroups[id,name]'),
                            auth = auth) %>%
    rename(
      element_id = id,
      element = name
    ) %>%
    hoist(categoryCombo, 'categoryOptionCombos') %>%
    unnest_longer(categoryOptionCombos) %>%
    hoist(categoryOptionCombos,
          category = c('name'),
          category_id = c('id')) %>%
    unnest_longer(dataSetElements) %>%
    hoist(dataSetElements, 'dataSet') %>%
    hoist(dataSet,
          dataset_id = c('id'),
          dataset = c('name'),
    ) %>%
    unnest_longer(dataElementGroups) %>%
    hoist(dataElementGroups,
          element_group = c('name'),
          element_group_id = c('id')) %>%
    select(element_id, element, element_group_id, element_group, dataset_id, dataset, category_id, category)
}

#' @export
my_data_levels <- function(iso3, auth) {
  file <- paste0('app/data/organization_unit_levels_', tolower(iso3), '.csv.xz')
  if (file.exists(file)) {
    return(vroom(file, show_col_types = FALSE) %>% arrange(level))
  }

  get_organisation_unit_levels(fields = 'level,name', auth = auth) %>%
    arrange(level)
}

#' @export
my_data_analytics <- function(element_ids,
                              start_date,
                              end_date,
                              level,
                              auth) {

  get_analytics_by_level(element_ids = element_ids,
                         start_date = start_date,
                         end_date = end_date,
                         level = as.integer(level),
                         auth = auth)
}

#' @export
my_datasets <- function(data_elements,
                         element_ids,
                         start_date,
                         end_date,
                         level,
                         auth) {

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

#' @export
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

#' @export
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

#' @export
get_sheet_data <- function() {
  read_csv('app/data/sheet_data.csv', show_col_types = FALSE)
}
