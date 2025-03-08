#' get_data_elements_
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_data_elements_ <- function(country_iso2, auth) {

  if (not_null(country_iso2) && is_scalar_character(country_iso2)) {
    dt <- data_elements %>%
      filter(iso2 == country_iso2)

    if (nrow(dt) > 0) {
      return(dt)
    }
  }

  get_data_elements(fields = c('id','name','categoryCombo[categoryOptionCombos[id,name]]', 'dataSetElements[dataSet[id,name]], dataElementGroups[id,name]'),
                    auth = auth) %>%
    rename(element_id = id, element = name) %>%
    hoist(categoryCombo, 'categoryOptionCombos') %>%
    unnest_longer(categoryOptionCombos) %>%
    hoist(categoryOptionCombos, category = 'name', category_id = 'id') %>%
    unnest_longer(dataSetElements) %>%
    hoist(dataSetElements, 'dataSet') %>%
    hoist(dataSet, dataset_id = 'id', dataset = 'name') %>%
    unnest_longer(dataElementGroups) %>%
    hoist(dataElementGroups, element_group = 'name', element_group_id = 'id') %>%
    select(element_id, element, element_group_id, element_group, dataset_id, dataset, category_id, category)
}
