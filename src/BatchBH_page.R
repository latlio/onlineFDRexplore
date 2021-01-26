################################################################################
# Batch BH Page
#
# Author: Lathan Liou
# Created: Tue Jan 26 13:08:41 2021 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                box(
                  title = strong("Batch BH"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  BatchUI("inputBatchBH")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputBatchBH"),
                                  summaryUI("BatchBHcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputBatchBH"),
                                  batchplotUI("BatchBHplot")),
                  shiny::tabPanel("Compare",
                                  batchcompareUI("BatchBHcomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/BatchBH_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row