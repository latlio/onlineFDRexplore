################################################################################
# UI modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 09:52:20 2020 ------------------------------
################################################################################

#### ALGORITHM INPUT UI ####
LONDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"),
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong(HTML(paste("Click for advanced options"))),
        prettyCheckbox(ns("checkbox"),
                       "Show me",
                       value = FALSE,
                       shape = "curve",
                       fill = TRUE,
                       animation = "pulse",
                       icon = icon("check"),
                       status = "info")
    ),
    shinyjs::hidden(
      div(
        id = ns("advopt"),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_dep"),
                        "Dependent:"),
            shinyWidgets::switchInput(ns("dep"), 
                                      NULL,
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px")
        ),
        shinyBS::bsTooltip(ns("label_dep"),
                           "Your p-values are dependent.",
                           placement = "right",
                           trigger = "hover"),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            strong("Original:"),
            shinyWidgets::switchInput(ns("original"), 
                                      NULL, 
                                      value = TRUE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px")),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_bound"),
                        "Bound:"),
            shinyWidgets::switchInput(ns("algbound"), 
                                      NULL, 
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px"),
            shinyBS::bsTooltip(ns("label_bound"),
                               "The default unbounded uses the number of p-values in your data. Setting bound to TRUE will allow you to set an upper bound for the number of hypotheses.",
                               placement = "right",
                               trigger = "hover")
        ),
        
        shinyjs::hidden(
          div(
            id = ns("boundtoggle"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                tags$strong("Number of hypotheses"),
                shiny::textInput(ns("boundnum"),
                                 NULL,
                                 value = 100,
                                 placeholder = "No bound",
                                 width = 80))
          )
        )
      ) #close div
    ), #close hidden
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
  # tagList(
  #   alphaUI(ns("alpha")),
  #   depUI(ns("dep")),
  #   randomUI(ns("random")),
  #   originalUI(ns("original")),
  #   br(),
  #   br(),
  #   div(style="display: inline-block;vertical-align:top; width: 100px;",
  #       shiny::actionButton(ns("go"),
  #                           "Calculate")),
  #   downloadUI("actualdownload")
  # )
}

LORDUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"),
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong("Version:"),
        shiny::selectInput(ns("version"), NULL, c("++", "3", "discard", "dep"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong(HTML(paste("Click for advanced options"))),
        prettyCheckbox(ns("checkbox"),
                       "Show me",
                       value = FALSE,
                       shape = "curve",
                       fill = TRUE,
                       animation = "pulse",
                       icon = icon("check"),
                       status = "info")
    ),
    shinyjs::hidden(
      div(
        id = ns("advopt"),
        shiny::textInput(ns("b0"), "Payout:", width = 80, value = 0.045, placeholder = ".045"),
        shinyBS::bsTooltip(ns("b0"),
                           "Payout for rejecting a hypothesis",
                           placement = "right",
                           trigger = "hover"),
        shiny::textInput(ns("tau.discard"), "Threshold:", width = 80, 
                         value = 0.5, placeholder = ".5"),
        shinyBS::bsTooltip(ns("tau.discard"),
                           "Optional threshold for hypotheses to be selected for testing",
                           placement = "right",
                           trigger = "hover"),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_bound"),
                        "Bound:"),
            shinyWidgets::switchInput(ns("algbound"), 
                                      NULL, 
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px"),
            shinyBS::bsTooltip(ns("label_bound"),
                               "The default unbounded uses the number of p-values in your data. Setting bound to TRUE will allow you to set an upper bound for the number of hypotheses.",
                               placement = "right",
                               trigger = "hover")
        ),
        
        shinyjs::hidden(
          div(
            id = ns("boundtoggle"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                tags$strong("Number of hypotheses"),
                shiny::textInput(ns("boundnum"),
                                 NULL,
                                 value = 100,
                                 placeholder = "No bound",
                                 width = 80))
          )
        )
      ) #close div
    ),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
}

SAFFRONUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong(HTML(paste("Click for advanced options"))),
        prettyCheckbox(ns("checkbox"),
                       "Show me",
                       value = FALSE,
                       shape = "curve",
                       fill = TRUE,
                       animation = "pulse",
                       icon = icon("check"),
                       status = "info")
    ),
    shinyjs::hidden(
      div(
        id = ns("advopt"),
        shiny::textInput(ns("lambda"), "Lambda", width = 80, value = 0.5, placeholder = ".5"),
        shinyBS::bsTooltip(ns("lambda"),
                           "Optional threshold for a candidate hypothesis",
                           placement = "right",
                           trigger = "hover"),
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_discard"),
                        "Discard"),
            shinyWidgets::switchInput(ns("discard"),
                                      NULL,
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px")),
        shinyBS::bsTooltip(ns("label_discard"),
                           "If TRUE then runs the ADDIS algorithm with 
                            adaptive discarding of conservative nulls",
                           placement = "right",
                           trigger = "hover"),
        shiny::textInput(ns("tau.discard"), "Threshold:", width = 80, 
                         value = 0.5, placeholder = ".5"),
        shinyBS::bsTooltip(ns("tau.discard"),
                           "Optional threshold for hypotheses to be selected for testing",
                           placement = "right",
                           trigger = "hover"),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_bound"),
                        "Bound:"),
            shinyWidgets::switchInput(ns("algbound"), 
                                      NULL, 
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px"),
            shinyBS::bsTooltip(ns("label_bound"),
                               "The default unbounded uses the number of p-values in your data. Setting bound to TRUE will allow you to set an upper bound for the number of hypotheses.",
                               placement = "right",
                               trigger = "hover")
        ),
        
        shinyjs::hidden(
          div(
            id = ns("boundtoggle"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                tags$strong("Number of hypotheses"),
                shiny::textInput(ns("boundnum"),
                                 NULL,
                                 value = 100,
                                 placeholder = "No bound",
                                 width = 80))
          )
        )
      ) #close div
    ),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
}

ADDISUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong(HTML(paste("Click for advanced options"))),
        prettyCheckbox(ns("checkbox"),
                       "Show me",
                       value = FALSE,
                       shape = "curve",
                       fill = TRUE,
                       animation = "pulse",
                       icon = icon("check"),
                       status = "info")
    ),
    shinyjs::hidden(
      div(
        id = ns("advopt"),
        shiny::textInput(ns("lambda"), "Lambda", width = 80, value = 0.5, placeholder = ".5"),
        shinyBS::bsTooltip(ns("lambda"),
                           "Optional threshold for a candidate hypothesis",
                           placement = "right",
                           trigger = "hover"),
        shiny::textInput(ns("tau"), "Threshold:", width = 80, 
                         value = 0.5, placeholder = ".5"),
        shinyBS::bsTooltip(ns("tau"),
                           "Optional threshold for hypotheses to be selected for testing",
                           placement = "right",
                           trigger = "hover"),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_bound"),
                        "Bound:"),
            shinyWidgets::switchInput(ns("algbound"), 
                                      NULL, 
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px"),
            shinyBS::bsTooltip(ns("label_bound"),
                               "The default unbounded uses the number of p-values in your data. Setting bound to TRUE will allow you to set an upper bound for the number of hypotheses.",
                               placement = "right",
                               trigger = "hover")
        ),
        
        shinyjs::hidden(
          div(
            id = ns("boundtoggle"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                tags$strong("Number of hypotheses"),
                shiny::textInput(ns("boundnum"),
                                 NULL,
                                 value = 100,
                                 placeholder = "No bound",
                                 width = 80))
          )
        )
      ) # close div
    ),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
}

alphainvestingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        tags$strong(id = ns("label_random"),
                    "Random:"),
        shinyWidgets::switchInput(ns("random"), 
                                  NULL, 
                                  value = TRUE,
                                  onLabel = "True",
                                  offLabel = "False", 
                                  width = "80px")),
    shinyBS::bsTooltip(ns("label_random"),
                       "The order of p-values in each batch of experiments is randomized.",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
                       placement = "right",
                       trigger = "hover"),
    div(style="display: inline-block;vertical-align:top; width: 200px;",
        strong(HTML(paste("Click for advanced options"))),
        prettyCheckbox(ns("checkbox"),
                       "Show me",
                       value = FALSE,
                       shape = "curve",
                       fill = TRUE,
                       animation = "pulse",
                       icon = icon("check"),
                       status = "info")
    ),
    shinyjs::hidden(
      div(
        id = ns("advopt"),
        
        div(style="display: inline-block;vertical-align:top; width: 200px;",
            tags$strong(id = ns("label_bound"),
                        "Bound:"),
            shinyWidgets::switchInput(ns("algbound"), 
                                      NULL, 
                                      value = FALSE,
                                      onLabel = "True",
                                      offLabel = "False",
                                      width = "80px"),
            shinyBS::bsTooltip(ns("label_bound"),
                               "The default unbounded uses the number of p-values in your data. Setting bound to TRUE will allow you to set an upper bound for the number of hypotheses.",
                               placement = "right",
                               trigger = "hover")
        ),
        
        shinyjs::hidden(
          div(
            id = ns("boundtoggle"),
            div(style="display: inline-block;vertical-align:top; width: 200px;",
                tags$strong("Number of hypotheses"),
                shiny::textInput(ns("boundnum"),
                                 NULL,
                                 value = 100,
                                 placeholder = "No bound",
                                 width = 80))
          )
        )
      ) # close div
    ),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
}

BatchUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
    useShinyjs(),
    div(style = "display: inline-block;vertical-align:top; width: 200px;",
        strong("Alpha:"),
        shiny::textInput(ns("alpha"), 
                         NULL,
                         width = 80, value = 0.05, placeholder = ".05")),
    shinyBS::bsTooltip(ns("alpha"), 
                       "Overall significance level of the FDR procedure",
                       placement = "right",
                       trigger = "hover"),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::actionBttn(
      inputId = ns("reset"),
      label = "Reset Inputs",
      style = "fill",
      color = "success"
    )
  )
}

#### OTHER UI ####
tableUI <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("table")) %>%
    shinycssloaders::withSpinner(type = 6,
                                 color = "#0066CC")
}

summaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    # shinyanimate::withAnim(),
    shinyjs::hidden(
      div(
        id = ns("downloadbutton"),
        uiOutput(ns("count"))
      ) #close div
    )
  )
}

compareUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    p("To use this feature, first click 'Reset Inputs'. Make sure you have already clicked 'Calculate'. This feature compares the results of two algorithms at their default settings. Changing any inputs will result in an unfair and nonsensical comparison. Note that for datasets with more than 50,000 p-values, expect a runtime between 30 seconds and 1 minute."),
    column(width = 12,
           align = "center",
           div(style = "display: inline-block;vertical-align:top;text-align:center",
               strong("Pick an algorithm for comparison"),
               shiny::selectInput(ns("alg"), NULL, c("LOND", "LORD", "LORD3", "LORDdiscard", "LORDdep", "SAFFRON", "ADDIS", "AlphaInvesting")))),
    shinyWidgets::actionBttn(
      inputId = ns("compare"),
      label = "Compare",
      style = "fill",
      color = "primary"
    ),
    br(),
    plotlyOutput(ns("comp")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),
    uiOutput(ns("compnum"))
  ) #close taglist
}

batchcompareUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    p("To use this feature, first click 'Reset Inputs'. Make sure you have already clicked 'Calculate'. This feature compares the results of two algorithms at their default alpha setting. Note that for datasets with more than 50,000 p-values, expect a runtime between 30 seconds and 1 minute."),
    column(width = 12,
           align = "center",
           div(style = "display: inline-block;vertical-align:top;text-align:center",
               strong("Pick an algorithm for comparison"),
               shiny::selectInput(ns("batchalg"), NULL, c("BatchPRDS", "BatchBH", "BatchStBH")))),
    shinyWidgets::actionBttn(
      inputId = ns("batchcompare"),
      label = "Compare",
      style = "fill",
      color = "primary"
    ),
    br(),
    plotlyOutput(ns("batchcomp")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    # br(),
    # plotlyOutput(ns("batchcomp2")) %>%
    #   shinycssloaders::withSpinner(type = 6,
    #                                color = "#0066CC")
  ) #close taglist
}

plotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("plot")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),
    uiOutput(ns("num"))
  )
}

#for saffron and addis
plot2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("plot2")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),
    uiOutput(ns("num2")),
    br(),
    uiOutput(ns("explain"))
  )
}

batchplotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("bplot1")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),     
    plotlyOutput(ns("bplot2")) %>%
      shinycssloaders::withSpinner(type = 6,
                                   color = "#0066CC"),
    br(),
    uiOutput(ns("bnum"))
  )
}

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

placeholderUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px")
  )
}

placeholder2UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder2"), 
      set_html_breaks(10),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px")
  )
}

placeholdermodUI <- function(id, n_breaks = 10) {
  ns <- NS(id)
  fluidRow(
    p(id = ns("placeholder"), 
      set_html_breaks(n_breaks),
      "Nothing calculated yet",
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px")
  )
}