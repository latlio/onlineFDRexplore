################################################################################
# Server logic of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:57:20 2020 ------------------------------
################################################################################
library(DT)
source("src/server-mods.R")
source("src/cicerone_guide.R")

#for alg recommendation feature
demodata <- read_csv("data/powerFDRdata.csv") %>%
  mutate(pi.vec = round(pi.vec, 2))

#for hover functionality
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

`%!in%` = Negate(`%in%`)

server <- function(input, output, session) {
  sever()
  Sys.sleep(0.5)
  waiter_hide()
  
  #### get started page ####
  
  #initialize walkthrough
  Sys.sleep(1.5)
  guide$init()$start()
  
  # power demo
  observe({
    toggle(id = "novice", condition = input$checkbox)
  })
  
  filter_data <- reactive({
    size = as.numeric(input$size)
    boundstat = ifelse(input$bound == "Known", 1, 0)
    depstat = ifelse(input$dep == "Independent", 0, 1)
    out <- demodata %>%
      filter(n == size,
             bound == boundstat,
             pi.vec == input$prop,
             dep == depstat) %>%
      select(-c(pi.vec, n, bound, dep)) %>%
      arrange(desc(power))
  })
  
  output$demores <- renderText({
    paste(filter_data() %>%
            head(1) %>%
            pull(procedure), "has the highest power.")
  })
  
  output$addiswarn <- renderText({
    if(input$size == 100 & input$prop == 0.4 & input$dep == "Independent"| 
       input$size == 1000 & input$prop < 0.5 & input$prop > 0.2 &input$dep == "Independent") {
      paste("Using ADDIS on a dataset > 100,000 may be too slow. Using onlineFDR::ADDIS() is recommended. ")
    }
  })
  
  # data format
  output$formatres <- renderUI({
    if(input$format == "Fully sequential") {
      div(
        DT::renderDT(tibble(id = c(1, "A", "1A"),
                        date = c("2014-12-01", "2014-12-01", "2014-12-3"),
                        pval = c(0.0674, 0.0532, 0.0127)),
                 options = list(info = F,
                                lengthChange = F,
                                ordering = F,
                                paging = F,
                                searching = F)),
        p("The id column can be a number, a string, or an alphanumeric sequence. The date column is optional, but if you choose to provide a date, please ensure that it is in YYYY-MM-DD, so that the app can correctly parse it. You may have to change the cell format to text if using something like Excel.")
      )
    } else {
      div(
       DT::renderDT(tibble(id = c(1, "A", "1A"),
                        pval = c(0.0674, 0.0532, 0.0127),
                        batch = c(1,1,2)),
                 options = list(info = F,
                                lengthChange = F,
                                ordering = F,
                                paging = F,
                                searching = F)),
        p("The id column can be a number, a string, or an alphanumeric sequence. Please ensure that the batches are numbered starting from 1.")
      )
    }
  })
  
  #Load in data
  in_data <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    shiny::validate(need(ext %in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv'), 
      "Please upload a csv file!"))
    
    data <- read_csv(input$file$datapath) %>%
      dplyr::mutate(across(any_of("date"), ~as.Date(.x, format = "%m/%d/%y")))
  })
  
  #Dynamically display alg jump button ONLY when file is uploaded
  output$showjump <- renderUI({
    if(is.null(in_data())) return()
    shiny::selectInput("tabjump", "Take me to the following algorithm page", c("Select","LOND", "LORD", "SAFFRON", "ADDIS"))
  })
  
  #jump to user-clicked algorithm
  observeEvent(input$tabjump, {
    if(input$tabjump == "LOND"){
      updateTabsetPanel(session, "navmaster", selected = "LOND")
    } else if (input$tabjump == "LORD") {
      updateTabsetPanel(session, "navmaster", selected = "LORD")
    } else if (input$tabjump == "SAFFRON") {
      updateTabsetPanel(session, "navmaster", selected = "SAFFRON")
    } else if (input$tabjump == "ADDIS") {
      updateTabsetPanel(session, "navmaster", selected = "ADDIS")
    }
    
  })
  
  #output warning if wrong file type
  observeEvent(input$file, {
    ext <- tools::file_ext(input$file$name)
    if (ext %!in% c(
      'text/csv',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'csv',
      'tsv')) {
      shiny::showNotification("Your file format is not supported. Please upload a CSV file!", type = "err", 
                              duration = NULL)
    }
  })
  
  #### LOND ####
  
  LOND_result <- callModule(LONDServer, id = "inputLOND", data = in_data)
  callModule(LONDcountServer, "LONDcount", LOND_result)
  callModule(LONDplotServer, "LONDplot", LOND_result)
  callModule(LONDcompServer, "LONDcomp", LOND_result, data = in_data)
  
  #### LORD ####
  
  LORD_result <- callModule(LORDServer, id = "inputLORD", data = in_data)
  callModule(LORDcountServer, "LORDcount", LORD_result)
  callModule(LORDplotServer, "LORDplot", LORD_result)
  callModule(LORDcompServer, "LORDcomp", LORD_result, data = in_data)
  
  # gray out inputs conditionally
  shinyjs::onclick("advanced2",
                   shinyjs::toggle(id = "advanced2", anim = TRUE))
  
  #### SAFFRON ####
  SAFFRON_result <- callModule(SAFFRONServer, id = "inputSAFFRON", data = in_data)
  callModule(SAFFRONcountServer, "SAFFRONcount", SAFFRON_result)
  callModule(SAFFRONplotServer, "SAFFRONplot", SAFFRON_result)
  callModule(SAFFRONcompServer, "SAFFRONcomp", SAFFRON_result, data = in_data)
  
  #### ADDIS Sync ####
  ADDIS_result <- callModule(ADDISServer, id = "inputADDIS", data = in_data)
  callModule(ADDIScountServer, "ADDIScount", ADDIS_result)
  callModule(ADDISplotServer, "ADDISplot", ADDIS_result)
  callModule(ADDIScompServer, "ADDIScomp", ADDIS_result, data = in_data)
  
  #### Alpha-Investing ####
  alphainvesting_result <- callModule(alphainvestingServer, id = "inputalphainvesting", data = in_data)
  callModule(alphainvestingcountServer, "alphainvestcount", alphainvesting_result)
  callModule(alphainvestingplotServer, "alphainvestplot", alphainvesting_result)
  callModule(alphainvestingcompServer, "alphainvestcomp", alphainvesting_result, data = in_data)

}
