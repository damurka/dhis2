#' countdown_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_countdown_page_ui <- function(id) {
  ns <- NS(id)
  # card(
  #   full_screen = TRUE,
  #   card_header('Countdown Indicator Mapping'),
  #   layout_columns(
  #     col_widths = c(4, 4),
  #     selectizeInput(ns("indicator"), "Indicator", choices = NULL),
  #     # data_elements_select(ns('data_elements')),
  #     selectizeInput(ns("categories"), "Select Categories for Group", choices = NULL, multiple = TRUE)
  #   ),

  # )
  card(
    full_screen = TRUE,
    card_header('Countdown Indicator Mapping'),
    layout_columns(
      col_widths = 12,
      fill = FALSE,
      layout_columns(
        col_widths = c(4, 2),
        fillable = FALSE,
        fill = FALSE,
        actionButton(ns("open_modal"), "Map Indicator"),
        actionButton(ns("delete"), "Delete")
      ),
      reactableOutput(ns('table'))
    )
  )
}

#' countdown_page Server Functions
#'
#' @noRd
mod_countdown_page_server <- function(id, credentials){
  moduleServer(
    id = id,
    module = function(input, output, session){
      ns <- session$ns

      mapped_data <- reactiveVal(
        tibble(
          hfd_id = character(),
          hfd_title = character(),
          element_id = character(),
          element = character(),
          category_id = character(),
          category = character()
        )
      )

      observeEvent(input$open_modal, {

        showModal( modalDialog(
          title = 'Define Indicator Map',
          layout_columns(
            col_widths = 12,
            selectizeInput(ns("indicator"), "Indicator", choices = NULL),
            data_elements_select(ns('data_elements')),
            selectizeInput(ns("categories"), "Select Categories for Group", choices = NULL, multiple = TRUE)
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_map"), "Add Mapping")
          ),
          easyClose = TRUE
        ))

        data_elements <- data_elements_select_server('data_elements', credentials)

        observe({

          choices <- hfd$hfd_id
          # names(choices) <- hfd$hfd_title_english

          updateSelectizeInput(session, 'indicator', choices = choices)
        })

        observe({
          req(data_elements()$items, data_elements()$selected)

          dt <- data_elements()$items %>%
            filter(element_id %in% data_elements()$selected) %>%
            distinct(category_id, category)

          choices <- dt$category_id
          names(choices) <- dt$category

          updateSelectizeInput(session, 'categories', choices = choices)
        })

        observeEvent(input$add_map, {

          dt_selected <- data_elements()$items %>%
            filter(element_id %in% input$data_elements) %>%
            mutate(hfd_id = input$group_name)

          if (is.null(input$categories) || length(input$categories) == 0) {
            new_group <- dt_selected %>%
              distinct()
          } else {
            new_group <- dt_selected %>%
              filter(category_id %in% input$categories) %>%
              mutate(
                hfd_id = input$indicator,
                hfd_title = NA,
              )
          }

          print(new_group)

          updated_groups <- rbind(mapped_data(), new_group) #%>% distinct()
          mapped_data(updated_groups)

          removeModal()
        })
      })

      output$table <- renderReactable({
        reactable(mapped_data(),
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  compact = TRUE,
                  selection = 'multiple',
                  onClick = 'select',
                  groupBy = 'hfd_title',
                  columns = list(
                    hfd_id = colDef(name = 'HFD ID', show = FALSE),
                    hfd_title = colDef(name = 'HFD Title', aggregate = "unique"),
                    element_id = colDef(name = 'Element ID', show = FALSE),
                    element = colDef(name = 'Element'),
                    category_id = colDef(name = 'Category ID', show = FALSE),
                    category = colDef(name = 'Category')
                  )
        )
      })

    }
  )
}
