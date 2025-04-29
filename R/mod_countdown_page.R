#' countdown_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_countdown_page_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header('Countdown Indicator Mapping'),
    layout_columns(
      col_widths = 12,
      fill = FALSE,
      layout_columns(
        # col_widths = c(4, 2),
        fillable = FALSE,
        fill = FALSE,
        actionButton(ns("open_modal"), "Map Data Elements"),
        actionButton(ns("open_dataset_modal"), "Map Datasets"),
        uiOutput(ns("download_ui")),
        downloadButton(ns("retrieve"), "Retrieve Data"),
        actionButton(ns("delete"), "Delete")
      ),
      reactableOutput(ns('table'))
    )
  )
}

#' countdown_page Server Functions
#'
#' @noRd
mod_countdown_page_server <- function(id, data_levels, selected_date, credentials){
  stopifnot(is.reactive(data_levels))
  stopifnot(is.reactive(selected_date))

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

      observeEvent(input$open_dataset_modal, {

        showModal( modalDialog(
          title = 'Define Daset Map',
          layout_columns(
            col_widths = 12,
            selectizeInput(ns("indicator"), "Dataset", choices = NULL),
            data_elements_select(ns('data_elements'))
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_map"), "Add Mapping")
          ),
          easyClose = TRUE
        ))

        data_elements <- data_elements_select_server('data_elements', credentials, use_dataset = TRUE)

        observe({

          mapped_ids <- mapped_data() %>%
            distinct(hfd_id) %>%
            pull(hfd_id)

          filtered_hfd <- hfd %>%
            filter(!hfd_id %in% mapped_ids, hfd_sheet == 'Reporting_completeness') %>%
            arrange(hfd_sheet, hfd_title_english)

          choices <- filtered_hfd$hfd_id
          names(choices) <- filtered_hfd$hfd_title_english

          updateSelectizeInput(session, 'indicator', choices = choices)
        })

        observeEvent(input$add_map, {

          dt_selected <- data_elements()$items %>%
            distinct(dataset_id , dataset) %>%
            filter(dataset_id %in% data_elements()$selected) %>%
            mutate(
              hfd_id = input$indicator,
              hfd_title = hfd %>% filter(hfd_id == input$indicator) %>% pull(hfd_title_english)
            ) %>%
            rename(
              element_id = dataset_id,
              element = dataset
            )

          updated_groups <- bind_rows(mapped_data(), dt_selected) %>% distinct()
          mapped_data(updated_groups)

          removeModal()
        }, ignoreInit = TRUE)
      })

      observeEvent(input$open_modal, {

        showModal( modalDialog(
          title = 'Define Data Element Map',
          layout_columns(
            col_widths = 12,
            selectizeInput(ns("indicator"), "Data Elements", choices = NULL),
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

          mapped_ids <- mapped_data() %>%
            distinct(hfd_id) %>%
            pull(hfd_id)

          filtered_hfd <- hfd %>%
            filter(!hfd_id %in% mapped_ids, hfd_sheet != 'Reporting completeness') %>%
            arrange(hfd_sheet, hfd_title_english)

          choices <- filtered_hfd$hfd_id
          names(choices) <- filtered_hfd$hfd_title_english

          updateSelectizeInput(session, 'indicator', choices = choices)
        })

        observe({
          req(data_elements()$items, data_elements()$selected)

          dt <- data_elements()$items %>%
            filter(element_id %in% data_elements()$selected) %>%
            distinct(category_id, category) %>%
            arrange(category)

          choices <- dt$category_id
          names(choices) <- dt$category

          updateSelectizeInput(session, 'categories', choices = choices)
        })

        observeEvent(input$add_map, {

          dt_selected <- data_elements()$items %>%
            select(element_id , element, category_id, category) %>%
            filter(element_id %in% data_elements()$selected) %>%
            mutate(
              hfd_id = input$indicator,
              hfd_title = hfd %>% filter(hfd_id == input$indicator) %>% pull(hfd_title_english)
            )

          if (is.null(input$categories) || length(input$categories) == 0) {
            new_group <- dt_selected %>%
              distinct()
          } else {
            new_group <- dt_selected %>%
              filter(category_id %in% input$categories)
          }

          updated_groups <- rbind(mapped_data(), new_group) %>% distinct()
          mapped_data(updated_groups)

          removeModal()
        }, ignoreInit = TRUE)
      })

      observeEvent(input$delete, {
        rows <- getReactableState("table", "selected")

        if (not_null(rows)) {
          dt <- mapped_data()[-rows, ]
          mapped_data(dt)
        }
      })

      observe({
        req(mapped_data())  # Ensure data is available
        show_button <- nrow(mapped_data()) > 0  # Check if table has data

        output$download_ui <- renderUI({
          if (show_button) {
            actionButton(
              ns("button"),
              label = "Download as CSV",
              icon = icon("download"),
              onclick = sprintf(
                "Reactable.downloadDataCSV('%s', 'countdown-map-%s.csv')",
                ns("table"), format(Sys.time(), "%Y%m%d%H%M%S")
              )
            )
          } else {
            NULL  # Hide button if no data
          }
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
                    element = colDef(name = 'Data Element',  aggregate = "unique"),
                    category_id = colDef(name = 'Category ID', show = FALSE),
                    category = colDef(name = 'Category')
                  )
        )
      })

      output$retrieve <- downloadHandler(
        filename = function() {
          paste("data-", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep="")
        },
        content = function(file) {
          admin_instrunction <- 'This sheet serves to map Health districts (C) to administrative units (D) and other health system data. Please fill in the information for each column starting from Column C. Health division units name (districts) in Column C should be unique, while it is possible that a set of districts (C) correspond to the same administrative unit (D).
    CAUTION: The order and names of health division units (districts) entered in this sheet SHOULD BE the same across all the sheets. And ensure that health division units (districts) in column C match with the Coverage data into the sheets "Service_data", "Reporting_completeness" & "Population_data". Do not enter any data beyond the total number of health subnational units (districts) in your country. "Yellow cells" are "Drop-down list" options. Please use "PASTE SPECIAL" and paste "ONLY VALUES" when you copy/paste data. Do not create new columns, remove or displace existing columns.'
          service_instruction <- 'Enter the reported number of individuals that received the specific service for each month and by subnational unit (district).
    CAUTION: The name and order of districts SHOULD BE the same across all the sheets. Do not create a new column or displace an existing column. Do not enter any data beyond the total number of health subnational units in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'
          population_instruction <- 'In this sheet, you will compile official population data used to produce denominators by the routine health information system in your country. These data are integrated into DHIS2 or other routine health data collection systems, and are usually derived from projections by the country\'s institute of statistics and/or demographic. Please enter data by district and year from year 2019 to 2023.
    WARNING: The order of the health districts MUST BE the same in all other sheets of this tool. Do not create new columns or move an existing column. Do not enter unnecessary data below the number of districts that exist in your country. Please use "PASTE SPECIAL" and paste "ONLY VALUES" in the appropriate cells when you copy/paste data.'

          wb <- createWorkbook()

          org_cols <- get_organisation_cols(data_levels()$items, data_levels()$selected)

          service_mapped_dt <- hfd %>%
            select(-ends_with('french'), -contains('desc')) %>%
            rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
            filter(str_detect(hfd_sheet, '^Service_data')) %>%
            left_join(mapped_data(), join_by(hfd_id, hfd_title))

          service_els <- service_mapped_dt %>%
            drop_na(element_id) %>%
            distinct(element_id) %>%
            pull(element_id)

          if (length(service_els) > 0) {
            service_dt <- get_data_analytics_(element_ids = service_els,
                                              level = data_levels()$selected,
                                              start_date = selected_date()[1],
                                              end_date = selected_date()[2],
                                              country_iso2 = session$userData$iso2,
                                              auth = credentials$auth) %>%
              left_join(service_mapped_dt, join_by(element, category)) %>%
              my_summary(
                data_levels = data_levels()$items,
                org_level =data_levels()$selected,
                .by = c('year', 'month', 'hfd_id', 'hfd_title', 'hfd_sheet'),
                value = sum(value)
              ) %>%
              drop_na(hfd_title) %>%
              append_missing_columns(service_mapped_dt, org_cols)

            create_sheets(
              wb = wb, .data = service_dt, header_rows = c(org_cols, 'year', 'month'),
              instruction = service_instruction, instruction_row_height = 140
            )
          }

          pop_mapped_dt <- hfd %>%
            select(-ends_with('french'), -contains('desc')) %>%
            rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
            filter(str_detect(hfd_sheet, '^Population_data')) %>%
            left_join(mapped_data(), join_by(hfd_id, hfd_title))

          pop_els <- pop_mapped_dt %>%
            drop_na(element_id) %>%
            distinct(element_id) %>%
            pull(element_id)

          if (length(pop_els) > 0) {

            pop_dt <- get_data_analytics_(element_ids = pop_els,
                                          level = data_levels()$selected,
                                          start_date = selected_date()[1],
                                          end_date = selected_date()[2],
                                          country_iso2 = session$userData$iso2,
                                          is_population = TRUE,
                                          auth = credentials$auth) %>%
              left_join(pop_mapped_dt, join_by(element, category)) %>%
              my_summary(
                data_levels = data_levels()$items,
                org_level =data_levels()$selected,
                .by = c('year', 'month', 'hfd_id', 'hfd_title', 'hfd_sheet'),
                value = sum(value)
              ) %>%
              drop_na(hfd_title) %>%
              append_missing_columns(pop_mapped_dt, org_cols)

            create_sheets(
              wb = wb, .data = pop_dt, header_rows = c(org_cols, 'year'),
              freeze_col = 3, instruction = population_instruction, instruction_row_height = 120
            )
          }

          # dt <- get_completeness_data_(mapped_data(),
          #                        level =data_levels()$selected,
          #                        start_date = selected_date()[1],
          #                        end_date = selected_date()[2],
          #                        auth = credentials$auth)
          # print(glimpse(dt))

          completeness_mapped_dt <- hfd %>%
            select(-ends_with('french'), -contains('desc')) %>%
            rename_with(~ gsub('_english', '', .x), 'hfd_title_english') %>%
            filter(str_detect(hfd_sheet, '^Reporting_completeness')) %>%
            left_join(mapped_data(), join_by(hfd_id, hfd_title)) %>%
            select(-category_id, -category) %>%
            drop_na(element_id)

          completeness_els <- completeness_mapped_dt %>%
            drop_na(element_id) %>%
            distinct(element_id) %>%
            pull(element_id)

          if (length(completeness_els) > 0) {

            completeness_dt <- get_data_sets_by_level(dataset_ids = completeness_els,
                                                      level =as.integer(data_levels()$selected),
                                                      start_date = selected_date()[1],
                                                      end_date = selected_date()[2],
                                                      auth = credentials$auth) %>%
              pivot_longer(cols = contains('report'), names_to = 'element', values_to = 'value') %>%
              left_join(completeness_mapped_dt, join_by(dataset == element)) %>%
              mutate(
                hfd_id = case_match(dataset,
                                    'REPORTING_RATE' ~ paste0(hfd_id, '_reporting_rate'),
                                    'ACTUAL_REPORTS' ~ paste0(hfd_id, '_reporting_received'),
                                    'EXPECTED_REPORTS' ~ paste0(hfd_id, '_reporting_expected')),
                hfd_subtitle = case_match(dataset,
                                          'REPORTING_RATE' ~ 'Reporting completeness rate (%)',
                                          'ACTUAL_REPORTS' ~ 'Received number (#)',
                                          'EXPECTED_REPORTS' ~ 'Expected number (#)')
              )

            # create_sheets(
            #   wb = wb, .data = completeness_dt, header_rows = c(org_cols, 'year', 'month')
            # )
          }

          saveWorkbook(wb, file, overwrite = TRUE)
        }
      )

    }
  )
}
