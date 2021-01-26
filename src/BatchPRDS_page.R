################################################################################
# Batch PRDS Page
#
# Author: Lathan Liou
# Created: Mon Jan 25 14:14:07 2021 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                box(
                  title = strong("Batch PRDS"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  BatchUI("inputBatchPRDS")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputBatchPRDS"),
                                  summaryUI("BatchPRDScount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputBatchPRDS"),
                                  batchplotUI("BatchPRDSplot")),
                  shiny::tabPanel("Compare",
                                  batchcompareUI("BatchPRDScomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/BatchPRDS_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row