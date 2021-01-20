# Shelved code

#### ADDIS Async ####
ADDISa_result <- callModule(ADDISServer, id = "inputADDISa", data = in_data)
callModule(ADDIScountServer, "ADDISacount", ADDIS_result)
callModule(ADDISplotServer, "ADDISaplot", ADDIS_result)
callModule(ADDIScompServer, "ADDISacomp", ADDIS_result, data = in_data)

#### LONDstar ####
LONDSTAR_result <- callModule(LONDSTARServer, id = "inputLONDSTAR", data = in_data)
callModule(LONDSTARcountServer, "LONDSTARcount", LONDSTAR_result)
callModule(LONDSTARplotServer, "LONDSTARplot", LONDSTAR_result)
callModule(LONDSTARcompServer, "LONDSTARcomp", LONDSTAR_result, data = in_data)

#### LORDstar ####
LORDSTAR_result <- callModule(LORDSTARServer, id = "inputLORDSTAR", data = in_data)
callModule(LORDSTARcountServer, "LORDSTARcount", LORDSTAR_result)
callModule(LORDSTARplotServer, "LORDSTARplot", LORDSTAR_result)
callModule(LORDSTARcompServer, "LORDSTARcomp", LORDSTAR_result, data = in_data)

#### SAFFRONstar ####
SAFFRONSTAR_result <- callModule(SAFFRONSTARServer, id = "inputSAFFRONSTAR", data = in_data)
callModule(SAFFRONSTARcountServer, "SAFFRONSTARcount", SAFFRONSTAR_result)
callModule(SAFFRONSTARplotServer, "SAFFRONSTARplot", SAFFRONSTAR_result)
callModule(SAFFRONSTARcompServer, "SAFFRONSTARcomp", SAFFRONSTAR_result, data = in_data)

# globaldata <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     stash <- reactiveValues()
#     stash$in_data <- reactive({
#       req(input$file)
#       
#       ext <- tools::file_ext(input$file$name)
#       validate(need(ext == "csv", "Please upload a csv file!"))
#       
#       data <- read_csv(input$file$datapath)
#     })
#     return(list(getdata = reactive(stash$in_data)))
#   })
# }

#### FUNCTION FACTORY ####
make_count_server_function <- function(algorithm_result, file_string) {
  force(file_string)
  count_server_function <- function(input, output, session) {
    ns <- session$ns
    #toggle download button
    observe({
      toggle(id = "downloadbutton")
    })
    
    output$count <- renderUI({  
      
      data <- algorithm_result[[1]]
      if(sum(data$R) == 1) {
        div(
          set_html_breaks(10),
          paste0("1 null hypothesis was rejected. See full results by downloading below"),
          set_html_breaks(2),
          shinyWidgets::downloadBttn(
            outputId = ns("download"),
            label = "Download results",
            style = "fill",
            color = "primary",
            size = "sm"
          ),
          style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
        )
      } else {
        div(
          set_html_breaks(10),
          paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
          set_html_breaks(2),
          shinyWidgets::downloadBttn(
            outputId = ns("download"),
            label = "Download results",
            style = "fill",
            color = "primary",
            size = "sm"
          ),
          style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
        )
      }
    })
    
    output$download <- downloadHandler(
      filename = function() {
        paste(file_string, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write_csv(algorithm_result[[1]], file)
      }
    )
  }
  return(count_server_function)
}

LONDtableServer <- function(input, output, session, LONDresult) {
  output$table <- renderReactable({
    data <- LONDresult$LONDres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LOND significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                         var total = 0
                         colInfo.data.forEach(function(row) {
                          total += row[colInfo.column.id]
                          })
                          return total
                                              }")
                )
              )
    ) #close reactable
  }) #close render reactable
}

#download handler
# global <- reactiveValues(response = FALSE)
# 
# observeEvent(input$init, {
#   shinyalert::shinyalert("Confirmation",
#                          "Do you want to download the data?",
#                          type = "success",
#                          callbackR = function(x) {
#                            global$response <- x
#                          },
#                          showCancelButton = TRUE
#   )
# })
# 
# observeEvent(global$response, {
#   if(global$response){
#     shinyjs::runjs(paste0("myModuleJS('", ns(""), "');"))
#     global$response <- FALSE
#   }
# })

LORDtableServer <- function(input, output, session, LORDresult) {
  output$table <- renderReactable({
    data <- LORDresult$LORDres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORD significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONtableServer <- function(input, output, session, SAFFRONresult) {
  output$table <- renderReactable({
    data <- SAFFRONresult$SAFFRONres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                id = colDef(name = "Experiment",
                            footer = "Total Rejected"),
                date = colDef(name = "Date"),
                pval = colDef(name = "P value",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRON significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

ADDIStableServer <- function(input, output, session, ADDISresult) {
  output$table <- renderReactable({
    data <- ADDISresult$ADDISres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "ADDIS significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

ADDISaServer <- function(input, output, session, data) {
  ns <- session$ns
  
  ADDISres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    tau = as.numeric(input$tau)
    req(input$tau)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau, {
      req(input$tau)
      if(as.numeric(input$tau) > 1 | as.numeric(input$tau) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = TRUE)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    
    if(!is.null(data())){
      tryCatch({
        ADDISres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("ADDISasyncparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(ADDISres = ADDISres))
}

alphainvestingtableServer <- function(input, output, session, alphainvestingresult) {
  output$table <- renderReactable({
    data <- alphainvestingresult$alphainvestingres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "Alpha_investing significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LONDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  LONDSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- LONDstar(d = data(),
                       alpha = alpha,
                       version = version)
    shiny::removeModal()
    
    output
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LONDSTARres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LONDSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LONDSTARres = LONDSTARres))
}

LONDSTARtableServer <- function(input, output, session, LONDSTARresult) {
  output$table <- renderReactable({
    data <- LONDSTARresult$LONDSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LONDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LONDSTARcountServer <- function(input, output, session, LONDSTARresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- LONDSTARresult$LONDSTARres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LONDSTAR-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("LONDSTAR-", Sys.Date(), ".csv", sep = "")
      write_csv(LONDSTARresult$LONDSTARres(), filename)
      R_session <- paste("LONDSTAR-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}

LONDSTARplotServer <- function(input, output, session, LONDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDSTARresult$LONDSTARres() %>%
      mutate(index = row_number(),
             LONDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LONDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
  
  output$num <- renderUI({
    current_alg_data <- LONDSTARresult$LONDSTARres()
    
    div(
      p(
        paste0("LONDstar rejected ", sum(current_alg_data$R), " null hypotheses.")
      ),
      p(
        paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses.")
      ),
      p(
        paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses.")
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
    ) #close div
  })
}

LONDSTARcompServer <- function(input, output, session, LONDSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LONDSTARresult$LONDSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LONDSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LONDSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}

LORDSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  LORDSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- LORDstar(d = data(),
                       alpha = alpha,
                       version = version,
                       w0 = w0)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LORDSTARres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LORDSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LORDSTARres = LORDSTARres))
}

LORDSTARtableServer <- function(input, output, session, LORDSTARresult) {
  output$table <- renderReactable({
    data <- LORDSTARresult$LORDSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "LORDstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

LORDSTARcountServer <- function(input, output, session, LORDSTARresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- LORDSTARresult$LORDSTARres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORDSTAR-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("LORDSTAR-", Sys.Date(), ".csv", sep = "")
      write_csv(LORDSTARresult$LORDSTARres(), filename)
      R_session <- paste("LORDSTAR-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}

LORDSTARplotServer <- function(input, output, session, LORDSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDSTARresult$LORDSTARres() %>%
      mutate(index = row_number(),
             LORDstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORDstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
  
  output$num <- renderUI({
    current_alg_data <- LORDSTARresult$LORDSTARres()
    
    div(
      p(
        paste0("LORDstar rejected ", sum(current_alg_data$R), " null hypotheses.")
      ),
      p(
        paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses.")
      ),
      p(
        paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses.")
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
    ) #close div
  })
}

LORDSTARcompServer <- function(input, output, session, LORDSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LORDSTARresult$LORDSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LORDSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORDSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}

SAFFRONSTARServer <- function(input, output, session, data) {
  ns <- session$ns
  
  SAFFRONSTARres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    discard = ifelse(input$discard == "True", T, F)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "alpha",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("alpha")
      }
    }
    )
    
    observeEvent(input$w0, {
      req(input$w0)
      if(as.numeric(input$w0) < 0 | as.numeric(input$w0) > as.numeric(input$alpha) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "w0",
          text = "Value must be non-negative and not greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("w0")
      }
    }
    )
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("Running algorithm..."))
    }
    
    output <- SAFFRONstar(d = data(),
                          alpha = alpha,
                          version = version,
                          w0 = w0,
                          lambda = lambda,
                          discard = discard,
                          tau.discard = tau.discard)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  #record user params
  user_params <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go",
             param != "download2_bttn")
  })
  
  # remove placeholder text
  observeEvent(input$go, {
    
    if(input$go == 0){
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    } else if(input$go > 0 && !is.null(data())) {
      shinyjs::hide(id = "placeholder")
      shinyjs::hide(id = "placeholder2")
    } 
    else {
      shinyjs::show(id = "placeholder")
      shinyjs::show(id = "placeholder2")
    }
    
  })
  
  # output no data loaded error message
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        SAFFRONSTARres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #provide download functionality
  # global <- reactiveValues(response = FALSE)
  # 
  # observeEvent(input$init, {
  #   shinyalert::shinyalert("Confirmation",
  #                          "Do you want to download the data?",
  #                          type = "success",
  #                          callbackR = function(x) {
  #                            global$response <- x
  #                          },
  #                          showCancelButton = TRUE
  #   )
  # })
  # 
  # observeEvent(global$response, {
  #   if(global$response){
  #     shinyjs::runjs("document.getElementById('download').click();")
  #     global$response <- FALSE
  #   }
  # })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("SAFFRONSTARparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(SAFFRONSTARres = SAFFRONSTARres))
}

SAFFRONSTARtableServer <- function(input, output, session, SAFFRONSTARresult) {
  output$table <- renderReactable({
    data <- SAFFRONSTARresult$SAFFRONSTARres()
    reactable(data,
              highlight = TRUE,
              filterable = TRUE,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(10, 50, 100),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                     headerClass = "sort-header"),
              columns = list(
                pval = colDef(name = "P value",
                              footer = "Total Rejected",
                              align = "center",
                              filterable = FALSE,
                              style = function(value) {
                                ifelse(data$R[which(data$pval == value)] == 1, color <- "#008000", color <- "#e00000")
                                list(color = color)
                              }),
                alphai = colDef(header = with_tooltip("Alpha",
                                                      "SAFFRONstar significance threshold"),
                                filterable = FALSE),
                R = colDef(header = with_tooltip("Rejection",
                                                 "1 = Rejected hypothesis"),
                           align = "center",
                           footer = JS("function(colInfo) {
                           var total = 0
                           colInfo.data.forEach(function(row) {
                            total += row[colInfo.column.id]
                            })
                            return total
                                                }")
                )
              )
    ) #close reactable
  })
}

SAFFRONSTARcountServer <- function(input, output, session, SAFFRONSTARresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- SAFFRONSTARresult$SAFFRONSTARres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        paste0("1 null hypothesis was rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below"),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results",
          style = "fill",
          color = "primary",
          size = "sm"
        ),
        style = "text-align: center;
        vertical-align: middle;
        font-family: Poppins, sans-serif;
        font-size: 18px;
        .shiny-download-link{
        width: 250px;
        }
        "
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("SAFFRONSTAR-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("SAFFRONSTAR-", Sys.Date(), ".csv", sep = "")
      write_csv(SAFFRONSTARresult$SAFFRONSTARres(), filename)
      R_session <- paste("SAFFRONSTAR-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}

SAFFRONSTARplotServer <- function(input, output, session, SAFFRONSTARresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- SAFFRONSTARresult$SAFFRONSTARres() %>%
      mutate(index = row_number(),
             SAFFRONstar = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRONstar, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Index", titlefont = font)
    why <- list(title = "Log adjusted test level", titlefont = font)
    plot_ly(new_data, x = ~index, y = ~alpha, color = ~adjustment) %>%
      add_lines() %>%
      layout(xaxis = ex, yaxis = why)
  })
  
  output$num <- renderUI({
    current_alg_data <- SAFFRONSTARresult$SAFFRONSTARres()
    
    div(
      p(
        paste0("SAFFRONstar rejected ", sum(current_alg_data$R), " null hypotheses.")
      ),
      p(
        paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses.")
      ),
      p(
        paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses.")
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 18px"
    ) #close div
  })
}

SAFFRONSTARcompServer <- function(input, output, session, SAFFRONSTARresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data, async = TRUE))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- SAFFRONSTARresult$SAFFRONSTARres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             SAFFRONSTAR = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRONSTAR, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
  })
  
  output$comp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Index", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~index, y = ~alpha, color = ~adjustment) %>%
        add_lines() %>%
        layout(xaxis = ex, yaxis = why)
    }
  })
}

LONDSTARUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyFeedback(),
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
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
                       placement = "right",
                       trigger = "hover"),
    shiny::textInput(ns("seed"), 
                     "Seed:",
                     width = 80, value = 1),
    shinyBS::bsTooltip(ns("seed"),
                       "Remember your number as this will let you access the same results in the future.",
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
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

LORDSTARUI <- function(id) {
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
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
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
        shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
        shinyBS::bsTooltip(ns("w0"),
                           "Initial wealth of the procedure",
                           placement = "right",
                           trigger = "hover")
      )
    ),
    shinyWidgets::actionBttn(
      inputId = ns("go"),
      label = "Calculate", 
      style = "fill",
      color = "success"
    ),
    br(),
    br(),
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}

SAFFRONSTARUI <- function(id) {
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
        shiny::selectInput(ns("version"), NULL, c("async", "dep", "batch"), width = 80)),
    shinyBS::bsTooltip(ns("version"),
                       "See Help page for more information",
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
        shiny::textInput(ns("w0"), "Wealth:", width = 80, value = 0.005, placeholder = ".005"),
        shinyBS::bsTooltip(ns("w0"),
                           "Initial wealth of the procedure",
                           placement = "right",
                           trigger = "hover"),
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
                           trigger = "hover")
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
    shinyWidgets::downloadBttn(
      outputId = ns("download2"),
      label = "Download inputs",
      style = "fill",
      color = "primary",
      size = "sm"
    )
  )
}