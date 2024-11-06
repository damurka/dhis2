box::use(
  reactable[reactableTheme],
  shiny[...],
  bslib[...],
  rlang[`!!!`],
  khisr[khis_base_url, khis_cred_clear],
  dplyr[`%>%`],
  shinycssloaders[hidePageSpinner, showPageSpinner]
)

box::use(
  app/logic/data_extraction[my_data_analytics, my_data_elements, my_data_levels, my_datasets],
  app/view/data_completeness_page,
  app/view/data_elements_filter,
  app/view/data_filters_page,
  app/view/data_levels_filter,
  app/view/service_data_page,
)

options(
  reactable.theme = reactableTheme(
    borderColor = '#e0ebd3',
    highlightColor = '#d4e7c8',
    cellPadding = "8px 12px",
    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
    searchInputStyle = list(width = "40%", borderColor = '#c0d6b8'),
    groupHeaderStyle = list(whiteSpace = 'normal', background = '#c0d6b8'),
    headerStyle = list(whiteSpace = 'normal', background = '#c0d6b8'),
    cellStyle = list(whiteSpace = 'nowrap')
  )
)

options(spinner.type = 5, spinner.color = "#7bc148")

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_navbar(
    title = "CD2030 Data Extractor",
    theme = bs_theme(version = 5),
    sidebar = sidebar(
      title = 'Data Controls',
      data_levels_filter$ui(ns('data_levels')),
      data_elements_filter$ui(ns('data_elements')),
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
      data_filters_page$ui(ns('data_filters'))
    ),
    nav_panel(
      title = 'Service Data',
      service_data_page$ui(ns('service_data'))
    ),
    nav_panel(
      title = 'Reporting Completeness',
      data_completeness_page$ui(ns('data_completeness'))
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

#' @export
server <- function(id, credentials) {

  moduleServer(id = id, module = function(input, output, session) {

    data_elements <- reactive({
      req(credentials$auth)

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      showPageSpinner()
      elements <- my_data_elements(session$userData$iso3, credentials$auth)
      hidePageSpinner()
      return(elements)
    })

    data_levels <- reactive({
      req(credentials$auth)

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      my_data_levels(session$userData$iso3, credentials$auth)
    })

    selected_elements <- data_elements_filter$server('data_elements', data_elements, credentials)
    selected_level <- data_levels_filter$server('data_levels', data_levels, credentials)

    filters <- data_filters_page$server('data_filters', data_elements, selected_elements, data_levels, selected_level, credentials)

    selected_elements_bound <- reactive({
      # req(credentials$auth)

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      selected_elements()

    })  %>% bindEvent(input$btn, input$logout)

    selected_level_bound <- reactive({
      # req(credentials$auth)

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      selected_level()
    }) %>% bindEvent(input$btn, input$logout)

    data_analytics <- reactive({
      req(selected_level_bound(), selected_elements_bound())

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      tryCatch(
        my_data_analytics(
          element_ids = selected_elements_bound(),
          level = selected_level_bound(),
          start_date = input$date[1],
          end_date = input$date[2],
          auth = credentials$auth
        ),
        error = function(e) e
      )
    }) %>% bindEvent(input$btn, input$logout)

    datasets <- reactive({
      req(selected_level_bound(), selected_elements_bound())

      if (is.null(credentials$auth)) {
        return(NULL)
      }

      tryCatch(
        my_datasets(
          data_elements = data_elements(),
          element_ids = selected_elements_bound(),
          level = selected_level_bound(),
          start_date = input$date[1],
          end_date = input$date[2],
          auth = credentials$auth
        ),
        error = function(e) e
      )

    }) %>% bindEvent(input$btn, input$logout)

    observeEvent(input$btn, {
      req(credentials$auth)
      if (!is.null(selected_elements_bound())) {
        service_data_page$server('service_data', data_analytics, data_levels, selected_level_bound, filters)
        data_completeness_page$server('data_completeness', datasets, data_levels, selected_level_bound, filters)
      } else {
        showNotification("No data elements are selected", type = "error", duration=5)
      }
    })

    observeEvent(input$logout, {
      khis_cred_clear(credentials$auth)
      credentials$auth <- NULL
    })

    output$profile_menu <- renderUI({
      username <- NULL
      if (!is.null(credentials$auth)) {
        username <- credentials$auth$get_profile()$get_display_name()
      }

      HTML(paste(
        "<span style='font-size:14px;'>", username, "</span><br>",
        "<span style='font-size:12px;'>", session$userData$country, "</span>"
      ))
    })
  }
  )
}
