################################################################################
# Server Modules
#
# Author: Lathan Liou
# Created: Thu Oct  1 11:50:56 2020 ------------------------------
################################################################################

set_html_breaks <- function(n) {
  HTML(strrep(br(), n))
}

#### ALG SERVERS ####
LONDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run LOND algorithm
  LONDres <- eventReactive(input$go, {
    # validate(need(is.null(data()), "Please upload a dataset"))
    
    #check parameters
    alpha = as.numeric(input$alpha)
    dep = ifelse(input$dep == "True", T, F)
    random = ifelse(input$random == "True", T, F)
    original = ifelse(input$original == "True", T, F)
    seed = as.numeric(input$seed)
    
    set.seed(seed)
    
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
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())) {
      # shiny::showModal(modalDialog("Running algorithm..."))
    }
    
    out <- LOND(d = data(),
                alpha = alpha,
                random = random,
                original = original)
    shiny::removeModal()
    
    return(out)
  }) #close eventReactive
  
  #toggle advanced options
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
  # observeEvent(input$go, {
  #   ifelse(!is.null(datacheck), signal <- 1, signal <- 0)
  #   print(signal)
  #   if(signal == 0) {
  #     shiny::showNotification("Please upload a dataset first!", type = "err")
  #   }
  # })
  
  # Output error messages
  observeEvent(input$go, {
    
    if(!is.null(data())){
      tryCatch({
        LONDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("LONDparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LONDres = LONDres))
}
LORDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$version, {
    if(input$version == "++"){
      shinyjs::disable(id = "w0")
      shinyjs::disable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "3" || input$version == 3) {
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "discard"){
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "tau.discard")
      shinyjs::disable(id = "b0")
    }
    else if(input$version == "dep"){
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else{
      shinyjs::enable(id = "w0")
      shinyjs::enable(id = "b0")
      shinyjs::enable(id = "tau.discard")
    }
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
  
  # Run LORD algorithm
  LORDres <- eventReactive(input$go, {
    
    # if(is.null(data())){
    #   shiny::showNotification("Please upload a dataset", type = "err")
    # }
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    w0 = as.numeric(input$w0)
    req(input$w0)
    b0 = as.numeric(input$b0)
    req(input$b0)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    random = ifelse(input$random == "True", T, F)
    
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
    
    observeEvent(input$b0, {
      req(input$b0)
      if(as.numeric(input$b0) <= 0 | as.numeric(input$b0) > 
         as.numeric(input$alpha) - as.numeric(input$w0) |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "b0",
          text = "Value must be positive and 
          the sum of w0 and b0 must not be greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("b0")
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
      # shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- LORD(d = data(),
                   alpha = alpha,
                   version = version,
                   w0 = w0,
                   b0 = b0,
                   tau.discard = tau.discard,
                   random = random)
    shiny::removeModal()
    
    output
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
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
  
  # # output no data loaded error message
  # observeEvent(input$go, {
  #   if(is.null(data)) {
  #     shiny::showNotification("Please upload a dataset first!", type = "err")
  #   }
  # })
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        LORDres()
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
      paste("LONDparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(LORDres = LORDres))
}
SAFFRONServer <- function(input, output, session, data) {
  ns <- session$ns
  
  SAFFRONres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    random = ifelse(input$random == "True", T, F)
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
      # shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- SAFFRON(d = data(),
                      alpha = alpha,
                      w0 = w0,
                      lambda = lambda,
                      random = random,
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
  
  # Output error messages
  observeEvent(input$go, {
    if(!is.null(data())){
      tryCatch({
        SAFFRONres()
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
      paste("SAFFRONparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(SAFFRONres = SAFFRONres,
              alpha = reactive(input$alpha)))
}
ADDISServer <- function(input, output, session, data) {
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
      # shiny::showModal(modalDialog("Running algorithm..."))
    }
    output <- ADDIS(d = data(),
                    alpha = alpha,
                    w0 = w0,
                    lambda = lambda,
                    tau = tau,
                    async = FALSE)
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
  
  # Output error messages
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
      paste("ADDISparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(ADDISres = ADDISres,
              alpha = reactive(input$alpha)))
}
alphainvestingServer <- function(input, output, session, data) {
  ns <- session$ns
  
  alphainvestingres <- eventReactive(input$go, {
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    w0 = as.numeric(input$w0)
    req(input$w0)
    random = ifelse(input$random == "True", T, F)
    
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
      # shiny::showModal(modalDialog("Running algorithm..."))
    }
    
    output <- Alpha_investing(d = data(),
                              alpha = alpha,
                              w0 = w0,
                              random = random)
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
        alphainvestingres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #download params
  output$download2 <- downloadHandler(
    filename = function() {
      paste("alphainvestingparams-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_params(), file)
    }
  )
  
  return(list(alphainvestingres = alphainvestingres))
}

#### COUNT SERVERS ####
LONDcountServer <- function(input, output, session, LONDresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
    # shinyanimate::startAnim(session, "downloadbutton", "fadeInDown")
  })
  
  output$count <- renderUI({
    
    data <- LONDresult$LONDres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LOND-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("LOND-", Sys.Date(), ".csv", sep = "")
      write_csv(LONDresult$LONDres(), filename)
      R_session <- paste("LOND-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}
LORDcountServer <- function(input, output, session, LORDresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- LORDresult$LORDres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("LORD-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("LORD-", Sys.Date(), ".csv", sep = "")
      write_csv(LORDresult$LORDres(), filename)
      R_session <- paste("LORD-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}
SAFFRONcountServer <- function(input, output, session, SAFFRONresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- SAFFRONresult$SAFFRONres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("SAFFRON-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("SAFFRON-", Sys.Date(), ".csv", sep = "")
      write_csv(SAFFRONresult$SAFFRONres(), filename)
      R_session <- paste("SAFFRON-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}
ADDIScountServer <- function(input, output, session, ADDISresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- ADDISresult$ADDISres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("ADDIS-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("ADDIS-", Sys.Date(), ".csv", sep = "")
      write_csv(ADDISresult$ADDISres(), filename)
      R_session <- paste("ADDIS-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}
alphainvestingcountServer <- function(input, output, session, alphainvestingresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({  
    
    data <- alphainvestingresult$alphainvestingres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    } else {
      div(
        set_html_breaks(10),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
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
    font-family: Lato, sans-serif;
    font-size: 18px"
      )
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("alphainvesting-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("alphainvesting-", Sys.Date(), ".csv", sep = "")
      write_csv(alphainvestingresult$alphainvestingres(), filename)
      R_session <- paste("alphainvesting-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, R_session)
      zip(file, files)
    }
  )
}

#### PLOT SERVERS ####
LONDplotServer <- function(input, output, session, LONDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LONDresult$LONDres() %>%
      mutate(index = row_number(),
             LOND = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LOND, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    # highchart() %>%
    #   hc_xAxis(title = list(text = "Index")) %>%
    #   hc_yAxis(title = list(text = "Log adjusted test level ")) %>%
    #   hc_add_series(new_data,
    #                 "line",
    #                 hcaes(x = index,
    #                       y = alpha,
    #                       group = adjustment)) %>%
    #   hc_colors(c("#4682B4", "#FF6347", "#2E8B57"))
    
    # ggplot(new_data, aes(x = index, y = alpha, col = adjustment)) +
    #   geom_line() + 
    #   theme_bw() + 
    #   labs(x = "Index",
    #        y = "Log adjusted test level",
    #        col = "Adjustment")
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
    current_alg_data <- LONDresult$LONDres()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("LOND rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
          })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}
LORDplotServer <- function(input, output, session, LORDresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- LORDresult$LORDres() %>%
      mutate(index = row_number(),
             LORD = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORD, Bonferroni, Unadjusted),
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
    current_alg_data <- LORDresult$LORDres()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("LORD rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}
SAFFRONplotServer <- function(input, output, session, SAFFRONresult) {
  ns <- session$ns
  
  output$plot2 <- renderPlotly({
    #modify data
    new_data <- SAFFRONresult$SAFFRONres() %>%
      mutate(index = row_number(),
             SAFFRON = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRON, Bonferroni, Unadjusted),
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
  
  output$num2 <- renderUI({
    current_alg_data <- SAFFRONresult$SAFFRONres()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("SAFFRON rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
  
  observe({
    if(max(SAFFRONresult$SAFFRONres()$alphai) > SAFFRONresult$alpha()) {
      output$explain <- renderUI({
        div(
          p(
            "Note that in settings where SAFFRON is rejecting many p-values, the testing levels can go above alpha. For a more technical explanation, click More Info."
          ),
          shinyWidgets::actionBttn(ns("showexp"),
                                   label = "More Info",
                                   style = "fill",
                                   color = "primary"),
          style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 12px"
        )
      }) #close renderUI
    } else {
      div()
    }
  }) #close observe
  
  observeEvent(input$showexp, {
    showModal(modalDialog(
      title = "Technical Explanation",
      img(src = "tech-exp.png")
    ))
  })
}
ADDISplotServer <- function(input, output, session, ADDISresult) {
  output$plot2 <- renderPlotly({
    #modify data
    new_data <- ADDISresult$ADDISres() %>%
      mutate(index = row_number(),
             ADDIS = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS, Bonferroni, Unadjusted),
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
  
  output$num2 <- renderUI({
    current_alg_data <- ADDISresult$ADDISres()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("ADDIS rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
  
  observe({
    if(max(ADDISresult$ADDISres()$alphai) > ADDISresult$alpha()) {
      output$explain <- renderUI({
        div(
          p(
            "Note that in settings where ADDIS is rejecting many p-values, the testing levels can go above alpha. For a more technical explanation, click More Info."
          ),
          shinyWidgets::actionBttn(ns("showexp"),
                                   label = "More Info",
                                   style = "fill",
                                   color = "primary"),
          style = "text-align: center;
    vertical-align: middle;
    font-family: Poppins, sans-serif;
    font-size: 12px"
        )
      }) #close renderUI
    } else {
      div()
    }
  }) #close observe
  
  observeEvent(input$showexp, {
    showModal(modalDialog(
      title = "Technical Explanation",
      img(src = "tech-exp.png")
    ))
  })
}
alphainvestingplotServer <- function(input, output, session, alphainvestingresult) {
  output$plot <- renderPlotly({
    #modify data
    new_data <- alphainvestingresult$alphainvestingres() %>%
      mutate(index = row_number(),
             Alpha_investing = log(alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(Alpha_investing, Bonferroni, Unadjusted),
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
    current_alg_data <- alphainvestingresult$alphainvestingres()
    
    div(
      p(
        renderTextillate({
          textillate(paste0("Alpha Investing rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
    ) #close div
  })
}

#### COMPARE SERVERS ####
LONDcompServer <- function(input, output, session, LONDresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LONDresult$LONDres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx()
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(index = row_number(),
             LOND = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphai),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LOND, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- LONDresult$LONDres()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("LOND rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
LORDcompServer <- function(input, output, session, LORDresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- LORDresult$LORDres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             LORD = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(LORD, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- LORDresult$LORDres()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("LORD rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
SAFFRONcompServer <- function(input, output, session, SAFFRONresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- SAFFRONresult$SAFFRONres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             SAFFRON = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(SAFFRON, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- SAFFRONresult$SAFFRONres()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("SAFFRON rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
ADDIScompServer <- function(input, output, session, ADDISresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- ADDISresult$ADDISres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             ADDIS = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(ADDIS, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- ADDISresult$ADDISres()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("ADDIS rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}
alphainvestingcompServer <- function(input, output, session, alphainvestingresult, data) {
  select_alg <- function(alg, data) {
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    current_alg_data <- alphainvestingresult$alphainvestingres()
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$alg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>%
      rename(alphainew = alphai)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphainew) %>%
      mutate(index = row_number(),
             AlphaInvesting = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(AlphaInvesting, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  
  #to make compnum reactive
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data())
  })
  
  selected_alg_to_display <- eventReactive(input$compare, {
    out <- input$alg
  })
  
  output$compnum <- renderUI({
    if(!is.null(select_alg_data())) {
      select_alg_data <- select_alg_data()
      current_alg_data <- alphainvestingresult$alphainvestingres()
      
      div(
        p(
          renderTextillate({
            textillate(paste0("Alpha Investing rejected ", sum(current_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0(selected_alg_to_display(), " rejected ", sum(select_alg_data$R), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= 0.05/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        p(
          renderTextillate({
            textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= 0.05), " null hypotheses."), auto.start = TRUE) %>%
              textillateIn(effect = "fadeInDown",
                           sync = T)
          })
        ),
        style = "text-align: center;
    vertical-align: middle;
    font-family: Lato, sans-serif;
    font-size: 18px"
      ) #close div
    }
  })
}