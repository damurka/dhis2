#' custom_service_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_custom_service_page_ui <- function(id) {
  ns <- NS(id)
  page_fillable(
    layout_columns(
      col_widths = 12,
      fill = FALSE,
      layout_columns(
        col_widths = c(6, 6),
        fillable = FALSE,
        fill = FALSE,
        actionButton(ns("open_modal"), "Manage Custom Groups"),
        actionButton(ns("delete"), "Delete")
      ),
      reactableOutput(ns('custom_groups_table'))
    )
  )
}

#' custom_service_page Server Functions
#'
#' @noRd
mod_custom_service_page_server <- function(id, data_elements){
  stopifnot(is.reactive(data_elements))

  moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      custom_groups <- reactiveVal(
        tibble(
          group_name = character(),
          element_id = character(),
          element = character(),
          category_id = character(),
          category = character()
        )
      )

      elements <- reactive({
        req(data_elements()$selected, data_elements()$items)

        selected_data_elements <- data_elements()$selected
        data_elements()$items %>%
          filter(element_id %in% selected_data_elements) %>%
          distinct(element_id, element, category_id, category)
      })

      observeEvent(input$open_modal, {
        showModal(modalDialog(
          title = 'Define Custom Group',
          layout_columns(
            col_widths = 12,
            textInput(ns("group_name"), "Enter Custom Group Name", placeholder = "e.g., Age 0-14"),
            selectizeInput(ns("data_elements"), "Select Data Element", choices = NULL, multiple = TRUE),
            selectizeInput(ns("categories"), "Select Categories for Group", choices = NULL, multiple = TRUE)
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_group"), "Add Group")
          ),
          easyClose = TRUE
        ))

        observe({
          req(elements())

          dt_els <- elements() %>%
            distinct(element_id, element) %>%
            arrange(element)

          el_choices <- dt_els$element_id
          names(el_choices) <- dt_els$element

          updateSelectizeInput(session, 'data_elements', choices = el_choices)
        })

        observe({
          req(elements(), input$data_elements)

          dt_cats <- elements() %>%
            filter(element_id == input$data_elements) %>%
            distinct(category_id, category) %>%
            arrange(category)

          cat_choices <- dt_cats$category_id
          names(cat_choices) <- dt_cats$category

          updateSelectizeInput(session, 'categories', choices = cat_choices)
        })

        observeEvent(input$add_group, {
          req(elements(), input$group_name, input$data_elements)

          dt_selected <- elements() %>%
            filter(element_id %in% input$data_elements) %>%
            mutate(group_name = input$group_name)

          if (is.null(input$categories) || length(input$categories) == 0) {
            new_group <- dt_selected %>%
              distinct()
          } else {
            new_group <- dt_selected %>%
              filter(category_id %in% input$categories) %>%
              mutate(group_name = input$group_name)
          }

          updated_groups <- rbind(custom_groups(), new_group) %>% distinct()
          custom_groups(updated_groups)

          removeModal()
        })
      })

      observeEvent(input$delete, {
        rows <- getReactableState("custom_groups_table", "selected")

        if (not_null(rows)) {
          dt <- custom_groups()[-rows, ]
          custom_groups(dt)
        }
      })

      output$custom_groups_table <- renderReactable({
        reactable(custom_groups(),
                  striped = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  compact = TRUE,
                  selection = 'multiple',
                  onClick = 'select',
                  groupBy = 'group_name',
                  columns = list(
                    group_name = colDef(name = 'Custom Group Name'),
                    element = colDef(name = 'Data Element', aggregate = "unique"),
                    element_id = colDef(name = 'Data Element ID', show = FALSE),
                    category_id = colDef(name = 'Category ID', show = FALSE),
                    category = colDef(name = 'Category')
                  )
        )
      })

      return(custom_groups)
    }
  )
}
