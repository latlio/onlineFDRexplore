################################################################################
# SAFFRON Page
#
# Author: Lathan Liou
# Created: Tue Sep 22 14:09:34 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                box(
                  title = strong("SAFFRON"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  SAFFRONUI("inputSAFFRON")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputSAFFRON"),
                                  summaryUI("SAFFRONcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputSAFFRON"),
                                  plot2UI("SAFFRONplot")),
                  shiny::tabPanel("Compare",
                                  compareUI("SAFFRONcomp")),
                  shiny::tabPanel("Code", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/SAFFRON_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row