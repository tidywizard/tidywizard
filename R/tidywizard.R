#' The main tidywizard app
#' @import shiny
#' @import bs4Dash
#' @import rintrojs
#' @import here
#' @import dplyr
#' @import tidyr
#' @import rclipboard
#' @import tippy
#' @import fresh
#'
#' @export
#'
#' @returns Defines the tidywizard shiny application

tidywizard <- function(){
  intro <- tidywizard:::intro_text

  df <- ggplot2::mpg

  choices_fct <- c(
    # "select - Selecione colunas",
    "select - Select columns",
    # "filter - Filtre linhas",
    "filter - Filter rows",
    # "arrange - Ordene as colunas",
    "arrange - Order columns",
    # "count - Conte os valores",
    "count - Count the values",
    # "rename - Renomeie colunas",
    "rename - Rename columns",
    # "mutate - Crie/altere colunas",
    "mutate - Create/change columns",
    # "summarise - Resuma seus dados",
    "summarise - Summarise data",
    # "separate - Separe uma coluna",
    "separate - Separate a column",
    # "unite - Junte colunas",
    "unite - Unite columns"
  )

  numeric_summs <- c("mean", "median", "standard deviation",
                     "variance", "minimum", "maximum")

  mutate_choices <- c(
    # "Operações numéricas",
    "Numerical operations",
    # "Operações com funções",
    "Operations with functions"
    # "Conversão de classes",
    # "Somatórios",
    # "'Se, senão' (If, else)"
  )

  ui <- dashboardPage(
    freshTheme = TRUE,
    help = NULL,
    dark = NULL,
    ## Header
    bs4Dash::bs4DashNavbar(
      tags$style("
      .fa-solid, .fas {
      color:#fff;
      }"),
      title = bs4Dash::bs4DashBrand(htmltools::HTML("<strong>tidywizard</strong>"), opacity = 1, color = "primary",
                                    image = knitr::image_uri(fs::path_package("logo_tidywizard.png", package = "tidywizard"))),
      shiny::actionButton(inputId = "BTN_RESET",
                            label = "Reset Button",
                            icon = shiny::icon("backward-fast")),
      shiny::actionButton(inputId = "BTN_BACK",
                            label = "Backup a Transformation",
                            icon = shiny::icon("rotate-left"))
    ),
    ## Sidebar
    bs4Dash::dashboardSidebar(
      htmltools::includeCSS(fs::path_package("obs.css", package = "tidywizard")),
      width = "200",
      collapsed = T, id = "sidebar_id",
      bs4Dash::sidebarMenu(id = "TABS",
                  # menuItem(tabName = "TAB_HOME",
                  #   "Home", icon = shiny::icon("house")
                  # ),
                  bs4Dash::menuItem(tabName = "TAB_DATA",
                                    "Data Manipulation", icon = shiny::icon("filter")
                  )#,
                  # menuItem(tabName = "TAB_SHEET",
                  #   "Cheat Sheet", icon = shiny::icon("note-sticky")
                  # )
      )
    ),

    ## Body
    bs4Dash::bs4DashBody(
      htmltools::includeCSS(fs::path_package("obs.css", package = "tidywizard")),
      ## INTRO TOUR ----
      rintrojs::introjsUI(),

      fresh::use_theme(fresh::create_theme(
        fresh::bs4dash_color(
          blue = "#09529a"
        )
      )),

      ## ITEMS ----
      bs4Dash::tabItems(
        bs4Dash::tabItem(tabName = "TAB_DATA",
                shiny::fluidRow(
                  shiny::column(
                    width = 6,
                    shiny::fluidRow(
                      rintrojs::introBox(data.step = 1, data.intro = intro$text[1],
                                         data.position = "right",
                                         bs4Dash::box(
                                           id = "div_box_escolha_funcao",
                                           icon = shiny::icon("gears"),
                                           height = "110",
                                           width = NULL,
                                           title = "Choose a function",
                                           shiny::selectInput("SELEC_FCT", label = NULL,
                                                       width = "400px",
                                                       choices = choices_fct),
                                           footer = shiny::actionButton("BTN_SELEC_FCT", label = "Choose a function",
                                                                 icon = shiny::icon("check"))
                                         )
                      )
                    ),
                    shiny::fluidRow(
                      rintrojs::introBox(data.step = 2, data.intro = intro$text[2],
                               data.position = "right",
                               bs4Dash::tabBox(id = "BOX_FUN_INPUTS", width = NULL, icon = shiny::icon("wand-magic-sparkles"),
                                               title = "Apply the function", type = "pills",
                                               footer = shiny::splitLayout(id = "div_footer_split_botoes",
                                                                    shiny::fluidRow(
                                                                      shiny::fluidRow(shiny::actionButton(inputId = "BTN_ADD_TAB", icon = shiny::icon("plus"),
                                                                                            label = "Add variable")
                                                                      ),
                                                                      shiny::fluidRow(shiny::actionButton(inputId = "BTN_RMV_TAB", icon = shiny::icon("minus"),
                                                                                            label = "Remove variable")
                                                                      )
                                                                    ),
                                                                    rintrojs::introBox(data.step = 3, data.intro = intro$text[3],
                                                                                       data.position = "right",
                                                                                       shiny::actionButton(inputId = "BTN_EXEC_FCT", label = "Execute the function",
                                                                                                    icon = shiny::icon("filter"))
                                                                    )
                                               ),
                                               shiny::tabPanel(title = "Variable 1",
                                                        shiny::uiOutput("FUNCTION_INPUTS")
                                               )
                               )
                      )
                    )
                  ),
                  shiny::column(
                    width = 6,
                    rintrojs::introBox(data.step = 4, data.intro = intro$text[4],
                                       data.position = "bottom",
                                       reactable::reactableOutput("DATA") |>
                                         shinycssloaders::withSpinner()
                    )
                  )
                  # )
                ),
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    rclipboard::rclipboardSetup(),
                    tippy::tippy_this("BTN_CLIPBOARD",
                               tooltip = "Copied!",
                               trigger = "click"),
                    bs4Dash::box(
                      id = "BOX_R_CODE",
                      collapsible = F,
                      height = "300",
                      width = NULL,
                      title =
                        shiny::splitLayout(id = "div_header_code_box",
                                    htmltools::div(shiny::icon("code"), "R Code", id = "div_icon_title_code_box"),
                                    shiny::uiOutput("BTN_CLIPBOARD")
                      ),

                      rintrojs::introBox(data.step = 5, data.intro = intro$text[5],
                                         data.position = "top",
                                         # verbatimTextOutput("DEBUG")

                                         shiny::htmlOutput("CODE")
                      )
                    )
                  )
                )
        )
      )
    )
  )

  server <- function(input, output, session){
    ## INTRO ----
    shiny::observeEvent("", {
      shiny::showModal(shiny::modalDialog(
        # includeHTML("../intro_text.html"),
        htmltools::HTML(paste0(htmltools::div(style = 'position:relative;',
                                   htmltools::img(src = knitr::image_uri(fs::path_package("logo_tidywizard.png", package = "tidywizard")),
                                       style = 'display: block; margin-left: auto; margin-right: auto;',
                                       width = "30%")),
                    HTML('<h1 style="text-align: center;">Welcome to &nbsp;<code><strong>tidywizard</strong></code></h1>
                   <h4 style="text-align: justify;">An application with the goal of <span>instructing the user</span> how to manipulate data with R in an <span>intuitive way</span> using everyday functions of a data scientist.<br><br>You use tidywizard in a simple way: <span>choose and fill in the entries</span> according to your interest to transform the data, and <span>the wizard takes care of the job</span> of translating that into R.<br>Afterward, you can check the <span>wizard\'s formula</span> to execute your treatment.</h4>
                   <hr />
                   ')
        )),
        easyClose = TRUE,
        footer =
          shiny::splitLayout(
            shiny::actionButton(inputId = "BTN_SKIP_INTRO", label = "SKIP INTRODUCTION", icon = shiny::icon("forward")),
            shiny::actionButton(inputId = "INTRO", label = "START INTRODUCTION", icon = shiny::icon("info-circle"))
          )

      ))
    })

    shiny::observe(
      shiny::removeModal()
    ) |> shiny::bindEvent(input$BTN_SKIP_INTRO)


    shiny::observeEvent(input$INTRO, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$INTRO, {
      rintrojs::introjs(
        session,
        options = list("nextLabel" = "Next",
                       "prevLabel" = "Back",
                       "doneLabel" = "Finish")
      )
    })

    ## SERVER ----
    shiny::observe({
      reactive_values$data <- reactive_values$data_reset
      reactive_values$code <- "df"
      # reactive_values$code_to_html <- "df"

    }) |> shiny::bindEvent(input$BTN_RESET)

    shiny::observe({
      if(reactive_values$code != "df"){
        reactive_values$code <- stringr::str_remove(reactive_values$code, pattern = "\\s\\|>\\n.+$")
        reactive_values$data <- rlang::eval_tidy(rlang::parse_expr(reactive_values$code))
      }

    }) |> shiny::bindEvent(input$BTN_BACK)

    selected_function <- shiny::eventReactive(input$BTN_SELEC_FCT, {
      shiny::isolate({
        gsub("\\s*-.*", "", input$SELEC_FCT)
      })
    })

    reactive_values <- shiny::reactiveValues(data_reset = df,
                                      data = df,
                                      code = "df",
                                      # code_to_html = "df",
                                      cont_tabs = 1,
                                      mutate_type = NULL)


    output$FUNCTION_INPUTS <- shiny::renderUI({
      req(input$BTN_SELEC_FCT)

      if (selected_function() == "select") {
        # Renderizar os inputs específicos para a função select()
        # Exemplo: input para selecionar as colunas
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_COLS",
                      label = "Choose the column(s)",
                      choices = colnames(reactive_values$data),
                      multiple = TRUE),
          shiny::checkboxInput(inputId = "SELEC_REMOVE_COLS",
                        label = "Remove columns")
        )

      } else if (selected_function() == "filter") {
        # Renderizar os inputs específicos para a função filter()
        # Exemplo: inputs para selecionar a coluna, a condição e o valor
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_COL_FILTER",
                      label = "Choose the column to filter",
                      choices = colnames(reactive_values$data),
                      selected = NULL),
          shiny::renderUI({
            class_col <- class(reactive_values$data[[input$SELEC_COL_FILTER]])

            if(class_col == "numeric"){
              shiny::selectInput(inputId = "SELEC_CONDT",
                          label = "Choose the condition",
                          choices = c("equal to",
                                      "different from",
                                      "greater than",
                                      "lesser than",
                                      "greater or equal to",
                                      "lesser or equal to"))
            }
            else {
              shiny::selectInput(inputId = "SELEC_CONDT",
                          label = "Choose the condition",
                          choices = c(
                            "equal to",
                            "different from"
                          )
              )
            }
          }),
          renderUI({
            selected_column <- input$SELEC_COL_FILTER
            column_class <- class(reactive_values$data[[selected_column]])

            switch(column_class,
                   "numeric" = shiny::numericInput(inputId = "FILTER_VALUE",
                                            label = "Value to filter by",
                                            value = 0),
                   "integer" = shiny::numericInput(inputId = "FILTER_VALUE",
                                            label = "Value to filter by",
                                            value = 0),
                   "character" = shiny::selectInput(inputId = "FILTER_VALUE",
                                             label = "Value to filter by",
                                             choices = unique(reactive_values$data[[selected_column]]),
                                             multiple = TRUE),
                   "factor" = shiny::selectInput(inputId = "FILTER_VALUE",
                                          label = "Value to filter by",
                                          choices = levels(reactive_values$data[[selected_column]])),
                   # Caso o tipo da coluna não seja numeric, character ou factor
                   # p("Aviso: Tipo de coluna não suportado para filtro.")
            )
          })
        )
      } else if (selected_function() == "summarise"){
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_GROUP_BY",
                      label = "Choose a column to group by",
                      multiple = TRUE, choices = colnames(reactive_values$data)),

          shiny::selectInput(inputId = "SELEC_COL_SUMM_1",
                      label = "Choose the column to summarise",
                      choices = colnames(reactive_values$data)),
          shiny::renderUI({
            selected_column_summ1 <- input$SELEC_COL_SUMM_1
            summ1_class <- class(reactive_values$data[[selected_column_summ1]])

            switch(summ1_class,
                   "numeric" = shiny::selectInput(inputId = "FCT_SUMM_1",
                                           label = "Choose how to summarise",
                                           choices = numeric_summs),
                   "integer" = shiny::selectInput(inputId = "FCT_SUMM_1",
                                           label = "Choose how to summarise",
                                           choices = numeric_summs),
                   "character" = shiny::selectInput(inputId = "FCT_SUMM_1",
                                             label = "Choose how to summarise",
                                             choices = "frequency"),
                   "factor" = shiny::selectInput(inputId = "FCT_SUMM_1",
                                          label = "Choose how to summarise",
                                          choices = "frequency"))
          })
        )
      } else if (selected_function() == "count"){
        # tooltip(title = "Caso você queira contar por grupos, selecione a coluna de agrupamento, e então a coluna que deve ser contada", placement = "right",
        shiny::selectInput(inputId = "SELEC_COLS_COUNT",
                    label = "Choose the column(s) to count by",
                    choices = colnames(reactive_values$data),
                    multiple = TRUE)
        # )
      } else if (selected_function() == "arrange"){
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_COLS_ARRANGE",
                      label = "Choose the column(s) to order by",
                      choices = colnames(reactive_values$data),
                      multiple = TRUE),
          shiny::checkboxInput(inputId = "CHECKBOX_ARRANGE",
                        label = "Descending order",
                        value = F)
        )
      } else if (selected_function() == "rename"){
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_COL_RENAME_1",
                      label = "Choose the column to rename",
                      choices = colnames(reactive_values$data)),
          shiny::textInput(inputId = "TEXT_RENAME_COL_1",
                    label = "Rename the column",
                    placeholder = "Write here")
        )
      } else if (selected_function() == "separate"){
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_COL_SEPARATE",
                      label = "Choose the column to separate",
                      choices = colnames(reactive_values$data)),
          shiny::textInput(inputId = "TEXT_SEPARATE_SEP",
                    label = "Define a separator",
                    placeholder = "As in: ., and, +, -"),
          shiny::textInput(inputId = "TEXT_SEPARATE_INTO",
                    label = "Name the new columns",
                    placeholder = 'Structure as in "Column_1, Column_2", or "New columns" if you don\'t know the total number of columns')
        )
      } else if (selected_function() == "unite"){
        shiny::tagList(
          shiny::textInput(inputId = "TEXT_UNITE_NEW",
                    label = "Define the new column\'s name"),
          shiny::selectInput(inputId = "SELEC_COLS_UNITE",
                      label = "Choose which columns to unite",
                      choices = colnames(reactive_values$data), multiple = T),
          shiny::textInput(inputId = "TEXT_UNITE_SEP",
                    label = "Define a separator for the values",
                    placeholder = "As in: and, +, -"),
          shiny::checkboxInput(inputId = "CHECK_UNITE_REMOVE",
                        label = "Remove base columns?",
                        value = T)
        )
      } else if (selected_function() == "mutate"){
        shiny::tagList(
          shiny::selectInput(inputId = "SELEC_GROUP_BY",
                      label = "Choose columns to group by",
                      multiple = TRUE, choices = colnames(reactive_values$data)),
          shiny::textInput(inputId = "TEXT_MUTATE",
                    label = "Name the new column, or an existing column"),
          shiny::selectInput(inputId = "SELEC_MUTATE",
                      label = "Choose how to create or change the column",
                      choices = mutate_choices),
          shiny::renderUI({
            reactive_values$mutate_type <- input$SELEC_MUTATE

            switch(reactive_values$mutate_type,
                   "Numerical operations" = shiny::textInput(inputId = "TEXT_MUTATE_OPERACOES",
                                                      label = "Define an equation"),
                   "Operations with functions" = shiny::tagList(
                     shiny::selectInput(inputId = "SELEC_MUTATE_FUN_COL",
                                 label = "Choose the column to apply a function",
                                 choices = colnames(reactive_values$data)),
                     shiny::selectInput(inputId = "SELEC_MUTATE_FUN",
                                 label = "Choose the function",
                                 choices = c("log", "exp", "mean",
                                             "sum", "min", "max", "cumsum"))
                   )
            )
          })
        )
      }
    })

    ## INCLUIR NOVAS TABS ----
    add_tab_summ <- function(id){
      shiny::tagList(
        shiny::selectInput(inputId = paste0("SELEC_COL_SUMM_", id),
                    label = paste("Choose a column to summarise"),
                    choices = colnames(reactive_values$data),
                    selected = NULL),
        shiny::renderUI({
          selected_column_summ <- rlang::eval_tidy(rlang::parse_expr(paste0("input$SELEC_COL_SUMM_", id)))
          summ_class <- class(reactive_values$data[[selected_column_summ]])

          switch(summ_class,
                 "numeric" = shiny::selectInput(inputId = paste0("FCT_SUMM_", id),
                                         label = paste("Choose how to summarise"),
                                         choices = numeric_summs),
                 "integer" = shiny::selectInput(inputId = paste0("FCT_SUMM_", id),
                                         label = paste("Choose how to summarise"),
                                         choices = numeric_summs),
                 "character" = shiny::selectInput(inputId = paste0("FCT_SUMM_", id),
                                           label = paste("Choose how to summarise"),
                                           choices = "frequency"),
                 "factor" = shiny::selectInput(inputId = paste0("FCT_SUMM_", id),
                                        label = paste("Choose how to summarise"),
                                        choices = "frequency"))
        })
      )
    }

    add_tab_rename <- function(id){
      shiny::tagList(
        shiny::selectInput(inputId = paste0("SELEC_COL_RENAME_", id),
                    label = "Choose a column to rename",
                    choices = colnames(reactive_values$data)),
        shiny::textInput(inputId = paste0("TEXT_RENAME_COL_", id),
                  label = "Rename the column",
                  placeholder = "Write here")
      )
    }

    observe({
      shiny::isolate(input$BTN_ADD_TAB)

      if(selected_function() == "summarise"){
        reactive_values$cont_tabs <- reactive_values$cont_tabs + 1

        shiny::insertTab(inputId = "BOX_FUN_INPUTS", target = paste0("Variable ", reactive_values$cont_tabs - 1),
                           position = "after", select = TRUE,
                           tabPanel(title = paste0("Variable ", reactive_values$cont_tabs),
                                    add_tab_summ(reactive_values$cont_tabs)))
      }

      if(selected_function() == "rename"){
        reactive_values$cont_tabs <- reactive_values$cont_tabs + 1

        shiny::insertTab(inputId = "BOX_FUN_INPUTS", target = paste0("Variable ", reactive_values$cont_tabs - 1),
                           position = "after", select = TRUE,
                           tabPanel(title = paste0("Variable ", reactive_values$cont_tabs), add_tab_rename(reactive_values$cont_tabs)))
      }
    }) |>
      shiny::bindEvent(input$BTN_ADD_TAB)

    shiny::observe({
      shiny::isolate(input$BTN_RMV_TAB)

      if(reactive_values$cont_tabs > 1){
        shiny::removeTab(inputId = "BOX_FUN_INPUTS", target = paste0("Variable ", reactive_values$cont_tabs))

        reactive_values$cont_tabs <- reactive_values$cont_tabs - 1
      }
    }) |> shiny::bindEvent(input$BTN_RMV_TAB)

    shiny::observe({
      while(reactive_values$cont_tabs > 1){
        shiny::removeTab(inputId = "BOX_FUN_INPUTS", target = paste0("Variable ", reactive_values$cont_tabs))

        reactive_values$cont_tabs <- reactive_values$cont_tabs - 1
      }
    }) |>
      shiny::bindEvent(input$BTN_EXEC_FCT)


    ## R CODE ----
    shiny::observe({
      if(selected_function() == "select"){
        cols <- input$SELEC_COLS
        remove <- input$SELEC_REMOVE_COLS
        cols <- paste_cols_args(cols, remove)

        reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n select(", cols, ")")
      }

      else if(selected_function() == "filter"){
        condition <- logical_condition(input$SELEC_CONDT)
        class_col_filter <- class(reactive_values$data[[input$SELEC_COL_FILTER]])

        filtering <- paste_cols_filtering(class_col_filter,
                                          input$SELEC_COL_FILTER, condition, input$FILTER_VALUE)

        filter_code <- paste0(shiny::isolate(reactive_values$code), " |>\n filter(rlang::eval_tidy(rlang::parse_expr(filtering)))")

        reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n filter(", filtering, ")")
      }

      else if(selected_function() == "summarise"){
        summarise_code <- NULL

        for(i in 1:reactive_values$cont_tabs){
          summarise_col <- rlang::eval_tidy(rlang::parse_expr(paste0("input$SELEC_COL_SUMM_", i)))
          summarise_fct <- rlang::eval_tidy(rlang::parse_expr(paste0("input$FCT_SUMM_", i)))

          summarise_fct <- switch(summarise_fct,
                                  "mean" = "mean",
                                  "median" = "median",
                                  "standard deviation" = "sd",
                                  "variance" = "var",
                                  "minimum" = "min",
                                  "maximum" = "max")

          summarise_new_col <- paste(summarise_col, summarise_fct, sep = "_")

          summarise_act <- paste0(summarise_new_col, " = ", summarise_fct, "(", summarise_col, ")")

          summarise_code <- c(summarise_code, summarise_act)
        }

        if(any(input$SELEC_GROUP_BY %in% colnames(reactive_values$data))){
          group_cols <- input$SELEC_GROUP_BY
          group_cols <- paste_cols_args(group_cols)

          reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n group_by(", group_cols, ")")
        }

        summarise_code <- paste0(summarise_code, collapse = ",\n          ")

        summarise_code <- paste0("summarise(", summarise_code, ")")
        summarise_code <- paste0(shiny::isolate(reactive_values$code), " |>\n ", summarise_code)


        reactive_values$code <- summarise_code
      }
      else if (selected_function() == "count"){
        cols <- input$SELEC_COLS_COUNT
        cols <- paste_cols_args(cols)

        sprintf("%s", paste(sprintf("%s", cols), collapse = ", "))

        reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n count(", cols, ")")
      }

      else if (selected_function() == "arrange"){
        cols <- input$SELEC_COLS_ARRANGE

        if(isTRUE(input$CHECKBOX_ARRANGE)){
          cols <- sprintf("%s", paste(sprintf("desc(%s)", cols), collapse = ", "))
        }
        else {
          cols <- paste_cols_args(cols)
        }

        reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n arrange(", cols, ")")
      }

      else if (selected_function() == "rename"){
        rename_code <- NULL

        for(i in 1:reactive_values$cont_tabs){
          rename_old_name <- rlang::eval_tidy(rlang::parse_expr(paste0("input$SELEC_COL_RENAME_", i)))
          rename_new_name <- rlang::eval_tidy(rlang::parse_expr(paste0("input$TEXT_RENAME_COL_", i)))

          rename_code <- paste0(rename_new_name, " = ", rename_old_name)
        }

        rename_code <- paste0(rename_code, collapse = ",\n          ")

        rename_code <- paste0("rename(", rename_code, ")")
        rename_code <- paste0(shiny::isolate(reactive_values$code), " |>\n ", rename_code)

        reactive_values$code <- rename_code
      }
      else if (selected_function() == "separate"){
        col <- input$SELEC_COL_SEPARATE

        col_sep <- input$TEXT_SEPARATE_SEP
        col_sep <- escape_special_chars(col_sep)
        col_sep <- sprintf("'%s'", col_sep)

        # into_cols <- string
        into_cols <- input$TEXT_SEPARATE_INTO

        if(stringr::str_to_upper(into_cols) == "NOVAS COLUNAS"){
          tmp_sep <- stringr::str_replace(col_sep, pattern = "\\\\\\\\", replacement = "\\\\")
          tmp_sep <- stringr::str_replace_all(tmp_sep, pattern = "'", replacement = "")

          nmax <- max(stringr::str_count(purrr::pluck(reactive_values$data, col),
                                         pattern = tmp_sep)) + 1

          into_cols <- paste0("col_", seq_len(nmax))
          into_cols <- paste0("c(", paste0(sprintf("'%s'", into_cols), collapse = ", "), ")")

          separate_code <- paste0(shiny::isolate(reactive_values$code), " |>\n separate(col = ", col, ", into = ", into_cols, ", sep = ", col_sep, ")")

          reactive_values$code <- separate_code
        } else{
          into_cols <- stringr::str_replace_all(string = into_cols, pattern = '"', replacement = "'")
          into_cols <- stringr::str_replace_all(string = into_cols, pattern = "'", replacement = "")
          into_cols <- strsplit(into_cols, split = ",") |>
            unlist() |>
            stringr::str_trim()
          into_cols <- paste0("c(", paste0(sprintf("'%s'", into_cols), collapse = ", "), ")")

          separate_code <- paste0(shiny::isolate(reactive_values$code), " |>\n separate(col = ", col, ", into = ", into_cols, ", sep = ", col_sep, ")")

          reactive_values$code <- separate_code
        }
      }
      else if (selected_function() == "unite"){
        unite_new <- input$TEXT_UNITE_NEW

        unite_sep <- input$TEXT_UNITE_SEP
        unite_sep <- sprintf("'%s'", unite_sep)

        unite_cols <- input$SELEC_COLS_UNITE
        unite_cols <- paste0(unite_cols, collapse = ", ")

        unite_remove <- input$CHECK_UNITE_REMOVE

        unite_code <- paste0(shiny::isolate(reactive_values$code), " |>\n unite(col = ", unite_new, ", sep = ", unite_sep, ", remove = ", unite_remove, ", ", unite_cols, ")")

        reactive_values$code <- unite_code
      }
      else if (selected_function() == "mutate"){
        mutate_col <- input$TEXT_MUTATE
        mutate_code <- paste0("mutate(", mutate_col, " = ")

        mutate_code <- switch(reactive_values$mutate_type,
                              "Numerical operations" = paste0(mutate_code, input$TEXT_MUTATE_OPERACOES, ")"),
                              "Operations with functions" = paste0(mutate_code, input$SELEC_MUTATE_FUN, "(",
                                                                   input$SELEC_MUTATE_FUN_COL, "))")
        )

        if(any(input$SELEC_GROUP_BY %in% colnames(reactive_values$data))){
          group_cols <- input$SELEC_GROUP_BY
          group_cols <- sprintf("%s", paste(sprintf("%s", group_cols), collapse = ", "))

          reactive_values$code <- paste0(shiny::isolate(reactive_values$code), " |>\n group_by(", group_cols, ")")
        }

        mutate_code <- paste0(shiny::isolate(reactive_values$code), " |>\n ", mutate_code)

        reactive_values$code <- mutate_code
      }
    }) |>
      bindEvent(input$BTN_EXEC_FCT)

    observe({
      reactive_values$data <- rlang::eval_tidy(rlang::parse_expr(reactive_values$code))
    }) |>
      bindEvent(input$BTN_EXEC_FCT)

    output$DATA <- reactable::renderReactable({
      rlang::eval_tidy(rlang::parse_expr(reactive_values$code)) |>
        reactable::reactable(bordered = T, striped = T, outlined = T,
                             filterable = F, sortable = F)
    })

    output$CODE <- function(){
      reactive_values$code |>
        code_to_html() |>
        htmltools::HTML()
    }

    output$BTN_CLIPBOARD <- shiny::renderUI({
      rclipboard::rclipButton(
        "BTN_CLIP",
        "Copy to clipboard",
        clipText = reactive_values$code,
        icon = shiny::icon("clipboard")
      )
    })

    # output$DEBUG <- shiny::renderPrint({
    #   print(str(reactiveValuesToList(reactive_values)))
    # })
  }

  shinyApp(ui, server) |>
    suppressWarnings()
}
