#' Organisation Selection Input
#'
#' Creates a select input for filtering organisations based on data levels.
#'
#' @param id Character. The module ID for namespacing UI elements.
#'
#' @return A Shiny UI element (`selectizeInput`) for selecting organisation levels.
#'
#' @noRd
organisations_select <- function(id) {
  ns <- NS(id)
  selectizeInput(
    ns("level_subgroup"),
    'Filter by Data Level',
    choices = NULL,
    multiple = TRUE,
    options = list(placeholder = 'Select the organisation to Filter (Leave blank to select all')
  )
}

#' Organisation Selection Server
#'
#' Server logic for dynamically updating organisation selection based on data levels.
#'
#' @param id Character. The module ID for namespacing.
#' @param data_levels Reactive. A reactive expression providing available data levels.
#' @param credentials List. Authentication credentials containing access tokens.
#'
#' @return A reactive expression returning selected organisation.
#'
#' @noRd
organisations_select_server <- function(id, data_levels, credentials) {
  stopifnot(is.reactive(data_levels))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      orgs <- reactive({
        req(credentials$auth, data_levels(), data_levels()$selected)
        iso2 <- session$userData$iso2
        get_cached_org_units(country_iso = iso2,
                             level = as.integer(data_levels()$selected),
                             auth = credentials$auth)
      })

      observe({
        req(orgs(), data_levels(), data_levels()$items, data_levels()$selected)

        level_name <- data_levels()$items %>%
          filter(level == data_levels()$selected) %>%
          mutate(name = str_to_lower(name) %>% make_clean_names()) %>%
          pull(name)

        level_subgroups <- orgs() %>%
          rename(name = any_of(level_name)) %>%
          select(id, name)

        choices <- NULL
        if (!is.null(level_subgroups)) {
          choices <- level_subgroups$name
          #names(choices) <- level_subgroups$name
        }

        updateSelectizeInput(session, "level_subgroup", choices = choices, selected = NULL, server = TRUE)
      })

      return(reactive(input$level_subgroup))
    }
  )
}
