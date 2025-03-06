box::use(
  shiny[is.reactive, moduleServer, NS, observe, reactive, req, selectizeInput, updateSelectizeInput],
  dplyr[any_of, filter, mutate, pull, rename, select, `%>%`],
  janitor[make_clean_names],
  stringr[str_to_lower],
  khisr[get_organisations_by_level]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  selectizeInput(ns("level_subgroup"),
                 'Filter by Data Level',
                 choices = NULL,
                 options = list(placeholder = 'Select the organisation to Filter (Leave blank to select all'),
                 multiple = TRUE)
}

#' @export
server <- function(id, data_levels, selected_level, credentials) {
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_level))

  moduleServer(id = id, module = function(input, output, session) {

    orgs <- reactive({
      req(selected_level())
      get_organisations_by_level(level = as.integer(selected_level()), auth = credentials$auth)
    })

    observe({
      req(credentials$auth)

      level_name <- data_levels() %>%
        filter(level == selected_level()) %>%
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
  })
}
