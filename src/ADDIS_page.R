################################################################################
# ADDIS (Synchronous) Page
#
# Author: Lathan Liou
# Created: Tue Sep 22 15:00:38 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("ADDIS (Synchronous)"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  ADDISUI("inputADDIS")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputADDIS"),
                                  summaryUI("ADDIScount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputADDIS"),
                                  plot2UI("ADDISplot")),
                  shiny::tabPanel("Compare",
                                  compareUI("ADDIScomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/ADDIS_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row