################################################################################
# LORD Page
#
# Author: Lathan Liou
# Created: Fri Sep 18 13:57:04 2020 ------------------------------
################################################################################

shiny::fluidRow(
  
  #### put input area here ####
  shiny::column(4,
                
                box(
                  title = strong("LORD"),
                  status = "primary",
                  solidHeader = TRUE,
                  tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
                  width = 12,
                  LORDUI("inputLORD")
                )
  ), ## close column 1
  
  #### put output here ####
  shiny::column(8,
                shiny::tabsetPanel(
                  shiny::tabPanel("Summary", 
                                  shinybusy::add_busy_bar(color = "#266EAB"),
                                  placeholderUI("inputLORD"),
                                  summaryUI("LORDcount")),
                  shiny::tabPanel("Plot",
                                  placeholder2UI("inputLORD"),
                                  plotUI("LORDplot")),
                  shiny::tabPanel("Compare",
                                  compareUI("LORDcomp")),
                  shiny::tabPanel("Help", withMathJax(),
                                  HTML(markdown::markdownToHTML(knit("src/LORD_code.Rmd", quiet = T))))
                ) ## close tabset panel
                
  ) ## close column
  
) ##close fluid row