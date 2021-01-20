################################################################################
# LOND Page
#
# Author: Lathan Liou
# Created: Fri Sep 18 10:18:13 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("LOND"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  LONDUI("inputLOND")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputLOND"),
                                  summaryUI("LONDcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputLOND"),
                                  plotUI("LONDplot")),
                  shiny::tabPanel("Compare",
                                  compareUI("LONDcomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/LOND_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row