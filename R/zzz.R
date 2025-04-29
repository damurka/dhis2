.onLoad <- function(libname, pkgname) {

  get_data_elements_ <<- memoise(get_data_elements_, omit_args = 'auth')
  get_organisations <<- memoise(get_organisations, omit_args = 'auth')

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

  invisible()
}
