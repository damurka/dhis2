#' data_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_data_page_ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    title = 'CD2030 Data Extractor',
    theme = bs_theme(version = 5),
    sidebar = sidebar(
      title = 'Data Controls',
      data_levels_select(ns('data_levels')),
      data_elements_select(ns('data_elements')),
      dateRangeInput(
        ns("date"),
        "Data Period",
        start = Sys.Date() - 90,
        end = Sys.Date(),
        startview = 'year',
        format = 'M dd, yyyy'
      ),
      actionButton(ns("btn"), "Retrieve Data")
    ),
    nav_spacer(),
    nav_panel(
      title = 'Data Filters',
      mod_filter_page_ui(ns('filter_page_1'))
    ),
    nav_panel(
      title = 'Service Data',
      mod_service_page_ui(ns('service_page_1'))
    ),
    nav_panel(
      title = 'Reporting Completeness',
      mod_completeness_page_ui(ns('completeness_page_1'))
    ),
    nav_panel(
      title = 'Population Data',
      mod_population_page_ui(ns('population_page_1'))
    ),
    nav_panel(
      title = 'Countdown Data',
      mod_countdown_page_ui(ns('countdown_page_1'))
    ),
    nav_spacer(),
    nav_menu(
      title = uiOutput(ns("profile_menu")),
      nav_item(
        actionLink(ns("logout"), "Logout")
      )
    )
  )
}

#' data_page Server Functions
#'
#' @noRd
mod_data_page_server <- function(id, credentials){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    data_levels <- data_levels_select_server('data_levels', credentials)
    data_elements <- data_elements_select_server('data_elements', credentials)
    selected_data_elements <- mod_filter_page_server('filter_page_1', data_elements, data_levels, credentials)

    data_analytics <- eventReactive(c(input$btn, input$logout), {
      req(credentials$auth, data_elements()$selected, data_levels()$selected, input$date)

      tryCatch(
        get_data_analytics_(
          element_ids = data_elements()$selected,
          level = data_levels()$selected,
          start_date = input$date[1],
          end_date = input$date[2],
          auth = credentials$auth
        ),
        error = function(e) e
      )
    })

    datasets <- eventReactive(c(input$btn, input$logout), {
      req(credentials$auth, data_elements()$items, data_elements()$selected, data_levels()$selected, input$date)

      tryCatch(
        get_datasets_(
          data_elements = data_elements()$items,
          element_id = data_elements()$selected,
          level = data_levels()$selected,
          start_date = input$date[1],
          end_date = input$date[2],
          auth = credentials$auth
        ),
        error = function(e) e
      )
    })

    mod_service_page_server('service_page_1', data_analytics, data_levels, selected_data_elements)
    mod_completeness_page_server("completeness_page_1", datasets, data_levels, selected_data_elements)
    mod_population_page_server('population_page_1', data_levels, reactive(input$date), credentials)
    mod_countdown_page_server("countdown_page_1")

    observeEvent(input$btn, {
      req(credentials$auth)

      if (is.null(data_elements()$selected)) {
        showNotification("No data elements are selected", type = "error", duration=5)
      }
    })

    observeEvent(input$logout, {
      khis_cred_clear(credentials$auth)
      credentials$auth <- NULL
    })

    output$profile_menu <- renderUI({
      username <- NULL
      if (not_null(credentials$auth)) {
        username <- credentials$auth$get_profile()$get_display_name()
      }

      HTML(paste(
        "<span style='font-size:14px;'>", username, "</span><br>",
        "<span style='font-size:12px;'>", session$userData$country, "</span>"
      ))
    })
  })
}
