################################################################################
# Batch StBH Page
#
# Author: Lathan Liou
# Created: Tue Jan 26 13:08:41 2021 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                box(
                  title = strong("Batch Storey-BH"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  BatchUI("inputBatchStBH")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary",
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputBatchStBH"),
                                  summaryUI("BatchStBHcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputBatchStBH"),
                                  batchplotUI("BatchStBHplot")),
                  # shiny::tabPanel("Compare",
                  #                 batchcompareUI("BatchPRDScomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/BatchStBH_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row