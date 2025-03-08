#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {

  supported_countries <- get_supported_countries()

  page_fillable(
    theme = bs_theme(version = 5),
    golem_add_external_resources(),
    conditionalPanel(
      condition = "output.loggedIn === false",
      style = "display: none;",
      class = 'min-h-full',
      mod_login_page_ui("login_page_1", supported_countries)
    ),
    conditionalPanel(
      condition = "output.loggedIn === true",
      style = "display: none;",
      class = 'bslib-page-sidebar html-fill-container',
      mod_data_page_ui("data_page_1")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dhis2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
