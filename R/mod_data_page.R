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
      organisations_select(ns('orgs_filter')),
      dateRangeInput(
        ns("date"),
        "Data Period",
        start = Sys.Date() - 90,
        end = Sys.Date(),
        startview = 'year',
        format = 'M dd, yyyy'
      )
    ),
    # header = tagList(
    #   useWaiter(),  # only needed once per app, but safe here if reused
    #   useHostess(), # same as above
    #
    #   waiterShowOnLoad(
    #     color = '#f2f8ee',
    #     html = tagList(
    #       hostess_loader(
    #         id = ns('loader'),  # important: use namespaced id!
    #         preset = 'bubble',
    #         text_color = '#7bc148',
    #         class = 'label-center',
    #         center_page = TRUE,
    #         stroke_color = "#7bc148"
    #       )
    #     )
    #   )
    # ),
    nav_spacer(),
    nav_panel(
      title = 'Service Data',
      mod_service_group_page_ui(ns('filter_page_1'))
    ),
    nav_panel(
      title = 'Population Data',
      mod_population_group_page_ui(ns('population_group_page_1'))
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
    selected_orgs <- organisations_select_server('orgs_filter', data_levels, credentials)
    # selected_orgs <- reactive(NULL)

    mod_service_group_page_server('filter_page_1', data_levels, selected_orgs, reactive(input$logout), reactive(input$date), credentials)
    mod_population_group_page_server('population_group_page_1', data_levels, selected_orgs, reactive(input$logout), reactive(input$date), credentials)
    mod_countdown_page_server("countdown_page_1", data_levels, reactive(input$date), credentials)

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
