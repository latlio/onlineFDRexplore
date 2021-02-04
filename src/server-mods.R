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
  
  #initialize bound
  observeEvent(input$algbound, {
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  # Run LOND algorithm
  LONDres <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    seed = as.numeric(input$seed)
    req(input$seed)
    
    set.seed(seed)
    
    #provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(input$alpha > 1 | input$alpha <= 0 |
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
    
    observeEvent(input$boundnum, {
      req(input$boundnum)
      if(as.numeric(input$boundnum) <= 0 | str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a positive number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())) {
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(!input$algbound) {
      out <- LOND(d = data(),
                  alpha = alpha,
                  random = input$random,
                  dep = input$dep,
                  original = input$original)
    } else if (input$algbound & as.numeric(input$boundnum) < nrow(data())) {
      shiny::showNotification(paste0("Please input a bound greater than or equal to the number of p-values in your data. The default dataset has 15 p-values."), type = "err", duration = NULL)
      
      out <- NULL
    } else {
      betai <- setBound("LOND", alpha = alpha, N = as.numeric(input$boundnum))
      out <- LOND(d = data(),
                  alpha = alpha,
                  betai = betai,
                  random = input$random,
                  dep = input$dep,
                  original = input$original)
    }
    
    shiny::removeModal()
    
    out
  }) %>% bindCache(data() %>% slice_head(),
                   input$alpha,
                   input$dep,
                   input$random,
                   input$original,
                   input$seed,
                   input$algbound,
                   input$boundnum) %>%
    bindEvent(input$go)
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "dep", value = FALSE)
    updateSwitchInput(session, "original", value = TRUE)
    updateSwitchInput(session, "algbound", value = FALSE)
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  #toggle advanced options
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$algbound)
  })
  
  #record user params
  LONDparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
  
  list(LONDres = LONDres,
       LONDparams = LONDparams,
       alpha = reactive(as.numeric(input$alpha)))
}
LORDServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$version, {
    if(input$version == "++"){
      shinyjs::disable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "3" || input$version == 3) {
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else if(input$version == "discard"){
      shinyjs::enable(id = "tau.discard")
      shinyjs::disable(id = "b0")
    }
    else if(input$version == "dep"){
      shinyjs::enable(id = "b0")
      shinyjs::disable(id = "tau.discard")
    }
    else{
      shinyjs::enable(id = "b0")
      shinyjs::enable(id = "tau.discard")
    }
  })
  
  observeEvent(input$algbound, {
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  #record user params
  LORDparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
  })
  
  # Run LORD algorithm
  LORDres <- reactive({
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    version = as.character(input$version)
    b0 = as.numeric(input$b0)
    req(input$b0)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    seed = as.numeric(input$seed)
    req(input$seed)
    
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
    
    observeEvent(input$b0, {
      req(input$b0)
      if(as.numeric(input$b0) <= 0 | as.numeric(input$b0) > 
         as.numeric(input$alpha) - 5e-10 |
         str_detect(input$b0, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "b0",
          text = "Value must be positive and 
          the sum of w0 and b0 must not be greater than alpha",
          icon = NULL
        )
      } else {
        hideFeedback("b0")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 |
         str_detect(input$tau.discard, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$boundnum, {
      if(as.numeric(input$boundnum) <= 0 | str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a positive number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(!input$algbound) {
      out <- LORD(d = data(),
                  alpha = alpha,
                  version = version,
                  b0 = b0,
                  tau.discard = tau.discard,
                  random = input$random)
    } else if (input$algbound & as.numeric(input$boundnum) < nrow(data())) {
      shiny::showNotification(paste0("Please input a bound greater than or equal to the number of p-values in your data. The default dataset has 15 p-values."), type = "err", duration = NULL)
      
      out <- NULL
    } else if(version != "dep") {
      gammai <- setBound("LORD", N = as.numeric(input$boundnum))
      out <- LORD(d = data(),
                  alpha = alpha,
                  gammai = gammai,
                  version = version,
                  b0 = b0,
                  tau.discard = tau.discard,
                  random = input$random)
    } else {
      gammai <- setBound("LORDdep", alpha = alpha, N = as.numeric(input$boundnum))
      out <- LORD(d = data(),
                  alpha = alpha,
                  gammai = gammai,
                  version = version,
                  b0 = b0,
                  tau.discard = tau.discard,
                  random = input$random)
    }
    shiny::removeModal()
    
    out
  }) %>%
    bindCache(data() %>% slice_head(),
              input$alpha, 
              input$version, 
              input$b0, 
              input$tau.discard,
              input$random,
              input$algbound,
              input$boundnum,
              input$seed) %>%
    bindEvent(input$go)
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateSelectInput(session, "version", selected = "++")
    updateTextInput(session, "b0", value = 0.045)
    updateSwitchInput(session, "random", value = TRUE)
    updateTextInput(session, "tau.discard", value = 0.5)
    updateSwitchInput(session, "algbound", value = FALSE)
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$algbound)
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
        LORDres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  list(LORDres = LORDres,
       LORDparams = LORDparams,
       alpha = reactive(as.numeric(input$alpha)))
}
SAFFRONServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$algbound, {
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  SAFFRONres <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    tau.discard = as.numeric(input$tau.discard)
    req(input$tau.discard)
    seed = as.numeric(input$seed)
    req(input$seed)
    
    set.seed(seed)
    
    # provide user feedback
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+") | is.null(input$alpha)) {
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
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$lambda, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$tau.discard, {
      req(input$tau.discard)
      if(as.numeric(input$tau.discard) > 1 | as.numeric(input$tau.discard) <= 0 | 
         str_detect(input$tau.discard, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau.discard",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau.discard")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$boundnum, {
      if(as.numeric(input$boundnum) <= 0 | str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a positive number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    if(!input$algbound) {
      out <- SAFFRON(d = data(),
                     alpha = alpha,
                     lambda = lambda,
                     random = input$random,
                     discard = input$discard,
                     tau.discard = tau.discard)
    } else if (input$algbound & as.numeric(input$boundnum) < nrow(data())) {
      shiny::showNotification(paste0("Please input a bound greater than or equal to the number of p-values in your data. The default dataset has 15 p-values."), type = "err", duration = NULL)
      
      out <- NULL
    } else {
      gammai <- setBound("SAFFRON", N = as.numeric(input$boundnum))
      out <- SAFFRON(d = data(),
                     alpha = alpha,
                     gammai = gammai,
                     lambda = lambda,
                     random = input$random,
                     discard = input$discard,
                     tau.discard = tau.discard)
    }
    
    shiny::removeModal()
    
    out
  }) %>%
    bindCache(data() %>% slice_head(),
              input$alpha,
              input$lambda,
              input$random,
              input$discard,
              input$tau.discard,
              input$algbound,
              input$boundnum,
              input$seed) %>%
    bindEvent(input$go)
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateTextInput(session, "lambda", value = 0.5)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "discard", value = TRUE)
    updateTextInput(session, "tau.discard", value = 0.5)
    updateSwitchInput(session, "algbound", value = FALSE)
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$algbound)
  })
  
  #record user params
  SAFFRONparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
  
  list(SAFFRONres = SAFFRONres,
       SAFFRONparams = SAFFRONparams,
       alpha = reactive(as.numeric(input$alpha)))
}
ADDISServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$algbound, {
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  ADDISres <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    lambda = as.numeric(input$lambda)
    req(input$lambda)
    tau = as.numeric(input$tau)
    req(input$tau)
    seed = as.numeric(input$seed)
    req(input$seed)
    
    set.seed(seed)
    
    observeEvent(input$alpha, {
      req(input$alpha)
      if(as.numeric(input$alpha) > 1 | as.numeric(input$alpha) <= 0 |
         str_detect(input$alpha, "[a-zA-Z\\,\\-]+") | is.null(input$alpha)) {
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
    
    observeEvent(input$lambda, {
      req(input$lambda)
      if(as.numeric(input$lambda) > 1 | as.numeric(input$lambda) <= 0 |
         str_detect(input$lambda, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "lambda",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("lambda")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$tau, {
      req(input$tau)
      if(as.numeric(input$tau) > 1 | as.numeric(input$tau) <= 0 |
         str_detect(input$tau, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "tau",
          text = "Value not between 0 and 1",
          icon = NULL
        )
      } else {
        hideFeedback("tau")
      }
    }, ignoreNULL = FALSE
    )
    
    observeEvent(input$boundnum, {
      if(as.numeric(input$boundnum) <= 0 | str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a positive number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    if(!input$algbound) {
      out <- ADDIS(d = data(),
                   alpha = alpha,
                   lambda = lambda,
                   tau = tau,
                   random = input$random)
    } else if (input$algbound & as.numeric(input$boundnum) < nrow(data())) {
      shiny::showNotification(paste0("Please input a bound greater than or equal to the number of p-values in your data. The default dataset has 15 p-values."), type = "err", duration = NULL)
      
      out <- NULL
    } else {
      gammai <- setBound("ADDIS", N = as.numeric(input$boundnum))
      out <- ADDIS(d = data(),
                   alpha = alpha,
                   gammai = gammai,
                   lambda = lambda,
                   tau = tau,
                   random = input$random)
    }
    
    shiny::removeModal()
    
    out
  }) %>%
    bindCache(data() %>% slice_head(),
              input$alpha,
              input$lambda,
              input$tau,
              input$random,
              input$algbound,
              input$boundnum,
              input$seed) %>%
    bindEvent(input$go)
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateTextInput(session, "lambda", value = 0.5)
    updateTextInput(session, "tau", value = 0.5)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "algbound", value = FALSE)
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$algbound)
  })
  
  #record user params
  ADDISparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
  
  list(ADDISres = ADDISres,
       ADDISparams = ADDISparams,
       alpha = reactive(as.numeric(input$alpha)))
}

alphainvestingServer <- function(input, output, session, data) {
  ns <- session$ns
  
  observeEvent(input$algbound, {
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  alphainvestingres <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    req(input$alpha)
    seed = as.numeric(input$seed)
    req(input$seed)
    
    set.seed(seed)
    
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
    
    observeEvent(input$boundnum, {
      if(as.numeric(input$boundnum) <= 0 | str_detect(input$boundnum, "[a-zA-Z\\,\\-]+")) {
        showFeedbackDanger(
          inputId = "boundnum",
          text = "Value not a positive number",
          icon = NULL
        )
      } else {
        hideFeedback("boundnum")
      }
    }, ignoreNULL = FALSE
    )
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    
    if(!input$algbound) {
      out <- Alpha_investing(d = data(),
                             alpha = alpha,
                             random = input$random)
    } else if (input$algbound & as.numeric(input$boundnum) < nrow(data())) {
      shiny::showNotification(paste0("Please input a bound greater than or equal to the number of p-values in your data. The default dataset has 15 p-values."), type = "err", duration = NULL)
      
      out <- NULL
    } else {
      gammai <- setBound("Alpha_investing", N = as.numeric(input$boundnum))
      out <- Alpha_investing(d = data(),
                             alpha = alpha,
                             gammai = gammai,
                             random = input$random)
    }
    
    shiny::removeModal()
    
    out
  }) %>%
    bindCache(data() %>% slice_head(),
              input$alpha,
              input$random,
              input$algbound,
              input$boundnum,
              input$seed) %>%
    bindEvent(input$go)
  
  #record user params
  alphainvestingparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
    updateSwitchInput(session, "random", value = TRUE)
    updateSwitchInput(session, "algbound", value = FALSE)
    updateTextInput(session, "boundnum", value = nrow(data()))
  })
  
  observe({
    toggle(id = "advopt", condition = input$checkbox)
  })
  
  observe({
    toggle(id = "boundtoggle", condition = input$algbound)
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
  
  list(alphainvestingres = alphainvestingres,
       alphainvestingparams = alphainvestingparams,
       alpha = reactive(as.numeric(input$alpha)))
}
BatchPRDSServer <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run BatchPRDS algorithm
  BatchPRDSres <- reactive({
    
    #check parameters
    alpha = as.numeric(input$alpha)
    
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
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    #cache collision?
    out <- BatchPRDS(d = data(),
                     alpha = alpha)
    shiny::removeModal()
    
    out
  }) %>% bindCache(data() %>% slice(50), 
                   input$alpha) %>%
    bindEvent(input$go)
  
  #record user params
  BatchPRDSparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
        BatchPRDSres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
  })
  
  list(BatchPRDSres = BatchPRDSres,
       BatchPRDSparams = BatchPRDSparams,
       alpha = reactive(as.numeric(input$alpha)))
}
BatchBHServer <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run BatchBH algorithm
  BatchBHres <- reactive({
    #check parameters
    alpha = as.numeric(input$alpha)
    
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
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    #cache collision?
    out <- BatchBH(d = data(),
                   alpha = alpha)
    shiny::removeModal()
    
    out
  }) %>% bindCache(data() %>% slice(50),
                   input$alpha) %>%
    bindEvent(input$go)
  
  #record user params
  BatchBHparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
        BatchBHres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
  })
  
  list(BatchBHres = BatchBHres,
       BatchBHparams = BatchBHparams,
       alpha = reactive(as.numeric(input$alpha)))
}
BatchStBHServer <- function(input, output, session, data) {
  ns <- session$ns
  
  # Run BatchStBH algorithm
  BatchStBHres <- reactive({
    #check parameters
    alpha = as.numeric(input$alpha)
    
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
    
    if(!is.null(data())){
      shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 5 and 30 seconds..."))
    }
    
    #cache collision?
    out <- BatchStBH(d = data(),
                     alpha = alpha)
    shiny::removeModal()
    
    out
  }) %>% bindCache(data() %>% slice(50),
                   input$alpha) %>%
    bindEvent(input$go)
  
  #record user params
  BatchStBHparams <- reactive({
    params <- reactiveValuesToList(input)
    data.frame(
      param = names(params),
      value = unlist(params, use.names = FALSE)
    ) %>%
      filter(param != "go")
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
        BatchStBHres()
      },
      error = function(err){
        shiny::showNotification(paste0(err), type = "err", duration = NULL)
      })
    }
  })
  
  #reset inputs
  observeEvent(input$reset, {
    updateTextInput(session, "alpha", value = 0.05)
  })
  
  list(BatchStBHres = BatchStBHres,
       BatchStBHparams = BatchStBHparams,
       alpha = reactive(as.numeric(input$alpha)))
}

#### COUNT SERVERS ####
LONDcountServer <- function(input, output, session, LONDresult) {
  ns <- session$ns
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
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
          label = "Download results & inputs",
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
          label = "Download results & inputs",
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
      filename2 <- paste("LONDparams-", Sys.Date(), ".csv", sep = "")
      write_csv(LONDresult$LONDparams(), filename2)
      R_session <- paste("LOND-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
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
          label = "Download results & inputs",
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
          label = "Download results & inputs",
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
      filename2 <- paste("LORDparams-", Sys.Date(), ".csv", sep = "")
      write_csv(LORDresult$LORDparams(), filename2)
      R_session <- paste("LORD-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
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
          label = "Download results & inputs",
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
          label = "Download results & inputs",
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
      filename2 <- paste("SAFFRONparams-", Sys.Date(), ".csv", sep = "")
      write_csv(SAFFRONresult$SAFFRONparams(), filename2)
      R_session <- paste("SAFFRON-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
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
          label = "Download results & inputs",
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
          label = "Download results & inputs",
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
      filename2 <- paste("ADDISparams-", Sys.Date(), ".csv", sep = "")
      write_csv(ADDISresult$ADDISparams(), filename2)
      R_session <- paste("ADDIS-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
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
          label = "Download results & inputs",
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
          label = "Download results & inputs",
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
      filename2 <- paste("alphainvestingparams-", Sys.Date(), ".csv", sep = "")
      write_csv(alphainvestingresult$alphainvestingparams(), filename2)
      R_session <- paste("alphainvesting-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}
BatchPRDScountServer <- function(input, output, session, BatchPRDSresult) {
  ns <- session$ns
  
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- BatchPRDSresult$BatchPRDSres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
      paste("BatchPRDS-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("BatchPRDS-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchPRDSresult$BatchPRDSres(), filename)
      filename2 <- paste("BatchPRDSparams-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchPRDSresult$BatchPRDSparams(), filename2)
      R_session <- paste("BatchPRDS-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}
BatchBHcountServer <- function(input, output, session, BatchBHresult) {
  ns <- session$ns
  
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- BatchBHresult$BatchBHres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
      paste("BatchBH-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("BatchBH-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchBHresult$BatchBHres(), filename)
      filename2 <- paste("BatchBHparams-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchBHresult$BatchBHparams(), filename2)
      R_session <- paste("BatchBH-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
      zip(file, files)
    }
  )
}
BatchStBHcountServer <- function(input, output, session, BatchStBHresult) {
  ns <- session$ns
  
  #toggle download button
  observe({
    toggle(id = "downloadbutton")
  })
  
  output$count <- renderUI({
    
    data <- BatchStBHresult$BatchStBHres()
    if(sum(data$R) == 1) {
      div(
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0("1 null hypothesis was rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
        set_html_breaks(3),
        renderTextillate({
          textillate(paste0(sum(data$R), " null hypotheses were rejected. See full results by downloading below."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        }),
        set_html_breaks(2),
        shinyWidgets::downloadBttn(
          outputId = ns("download"),
          label = "Download results & inputs",
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
      paste("BatchStBH-", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      
      filename <- paste("BatchStBH-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchStBHresult$BatchStBHres(), filename)
      filename2 <- paste("BatchStBHparams-", Sys.Date(), ".csv", sep = "")
      write_csv(BatchStBHresult$BatchStBHparams(), filename2)
      R_session <- paste("BatchStBH-", Sys.Date(), "sessioninfo.txt", sep = "")
      writeLines(capture.output(sessionInfo()), R_session)
      files <- c(filename, filename2, R_session)
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
             Bonferroni = log(LONDresult$alpha()/index),
             Unadjusted = rep(log(LONDresult$alpha()), nrow(.))) %>%
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
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(LONDresult$LONDres() %>% slice_tail())
  
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
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= LONDresult$alpha()/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= LONDresult$alpha()), " null hypotheses."), auto.start = TRUE) %>%
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
             Bonferroni = log(LORDresult$alpha()/index),
             Unadjusted = rep(log(LORDresult$alpha()), nrow(.))) %>%
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
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(LORDresult$LORDres() %>% slice_tail())
  
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
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= LORDresult$alpha()/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= LORDresult$alpha()), " null hypotheses."), auto.start = TRUE) %>%
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
             Bonferroni = log(SAFFRONresult$alpha()/index),
             Unadjusted = rep(log(SAFFRONresult$alpha()), nrow(.))) %>%
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
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(SAFFRONresult$SAFFRONres() %>% slice_tail())
  
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
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= SAFFRONresult$alpha()/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= SAFFRONresult$alpha()), " null hypotheses."), auto.start = TRUE) %>%
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
    alphacheck <- SAFFRONresult$alpha()
    if(!is.na(alphacheck)){
      if(max(SAFFRONresult$SAFFRONres()$alphai) > alphacheck) {
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
             Bonferroni = log(ADDISresult$alpha()/index),
             Unadjusted = rep(log(ADDISresult$alpha()), nrow(.))) %>%
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
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(ADDISresult$ADDISres() %>% slice_tail())
  
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
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= ADDISresult$alpha()/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= ADDISresult$alpha()), " null hypotheses."), auto.start = TRUE) %>%
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
    alphacheck <- ADDISresult$alpha()
    if(!is.na(alphacheck)){
      if(max(ADDISresult$ADDISres()$alphai) > alphacheck) {
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
             Bonferroni = log(alphainvestingresult$alpha()/index),
             Unadjusted = rep(log(alphainvestingresult$alpha()), nrow(.))) %>%
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
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(alphainvestingresult$alphainvestingres() %>% slice_tail())
  
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
          textillate(paste0("Bonferroni rejected ", sum(current_alg_data$pval <= alphainvestingresult$alpha()/length(current_alg_data$pval)), " null hypotheses."), auto.start = TRUE) %>%
            textillateIn(effect = "fadeInDown",
                         sync = T)
        })
      ),
      p(
        renderTextillate({
          textillate(paste0("No adjustment rejected ", sum(current_alg_data$pval <= alphainvestingresult$alpha()), " null hypotheses."), auto.start = TRUE) %>%
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
BatchPRDSplotServer <- function(input, output, session, BatchPRDSresult) {
  output$bplot1 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font, dtick = 1)
    why <- list(title = "Log adjusted test level", titlefont = font)
    
    new_data <- BatchPRDSresult$BatchPRDSres() %>%
      distinct(batch, .keep_all = TRUE)
    
    plot_ly(new_data, x = ~batch, y = ~log(alphai)) %>%
      add_lines(mode = "lines+markers") %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchPRDSresult$BatchPRDSres() %>% slice_tail())
  
  output$bplot2 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font)
    why <- list(title = "Number of Rejections", titlefont = font)
    
    #modify data
    new_data <- BatchPRDSresult$BatchPRDSres() %>%
      group_by(batch) %>%
      mutate(RBonf = pval <= (BatchPRDSresult$alpha()/nrow(.)),
             RUnadj = pval <= BatchPRDSresult$alpha()) %>%
      summarize(BatchPRDS = sum(R),
                Bonferroni = sum(RBonf),
                Unadjusted = sum(RUnadj)) %>%
      ungroup() %>%
      pivot_longer(cols = c(BatchPRDS, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "R")
    
    plot_ly(new_data, x = ~batch, y = ~R, type = "bar", color = ~adjustment) %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchPRDSresult$BatchPRDSres() %>% slice_head())
}
BatchBHplotServer <- function(input, output, session, BatchBHresult) {
  output$bplot1 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font, dtick = 1)
    why <- list(title = "Log adjusted test level", titlefont = font)
    
    new_data <- BatchBHresult$BatchBHres() %>%
      distinct(batch, .keep_all = TRUE)
    
    plot_ly(new_data, x = ~batch, y = ~log(alphai)) %>%
      add_lines(mode = "lines+markers") %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchBHresult$BatchBHres() %>% slice_tail())
  
  output$bplot2 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font)
    why <- list(title = "Number of Rejections", titlefont = font)
    
    #modify data
    new_data <- BatchBHresult$BatchBHres() %>%
      group_by(batch) %>%
      mutate(RBonf = pval <= (BatchBHresult$alpha()/nrow(.)),
             RUnadj = pval <= BatchBHresult$alpha()) %>%
      summarize(BatchBH = sum(R),
                Bonferroni = sum(RBonf),
                Unadjusted = sum(RUnadj)) %>%
      ungroup() %>%
      pivot_longer(cols = c(BatchBH, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "R")
    
    plot_ly(new_data, x = ~batch, y = ~R, type = "bar", color = ~adjustment) %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchBHresult$BatchBHres() %>% slice_head())
}
BatchStBHplotServer <- function(input, output, session, BatchStBHresult) {
  output$bplot1 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font, dtick = 1)
    why <- list(title = "Log adjusted test level", titlefont = font)
    
    new_data <- BatchStBHresult$BatchStBHres() %>%
      distinct(batch, .keep_all = TRUE)
    
    plot_ly(new_data, x = ~batch, y = ~log(alphai)) %>%
      add_lines(mode = "lines+markers") %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchStBHresult$BatchStBHres() %>% slice_tail())
  
  output$bplot2 <- renderPlotly({
    font <- list(
      family = "Lato"
    )
    ex <- list(title = "Batch", titlefont = font)
    why <- list(title = "Number of Rejections", titlefont = font)
    
    #modify data
    new_data <- BatchStBHresult$BatchStBHres() %>%
      group_by(batch) %>%
      mutate(RBonf = pval <= (BatchStBHresult$alpha()/nrow(.)),
             RUnadj = pval <= BatchStBHresult$alpha()) %>%
      summarize(BatchStBH = sum(R),
                Bonferroni = sum(RBonf),
                Unadjusted = sum(RUnadj)) %>%
      ungroup() %>%
      pivot_longer(cols = c(BatchStBH, Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "R")
    
    plot_ly(new_data, x = ~batch, y = ~R, type = "bar", color = ~adjustment) %>%
      layout(xaxis = ex, yaxis = why)
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(BatchStBHresult$BatchStBHres() %>% slice_head())
}

#### COMPARE SERVERS ####
LONDcompServer <- function(input, output, session, LONDresult, data) {
  select_alg <- function(alg, data, alpha) {
    set.seed(47)
    switch(alg,
           LOND = LOND(data, alpha),
           LORD = LORD(data, alpha),
           LORD3 = LORD(data, alpha, version = 3),
           LORDdiscard = LORD(data, alpha, version = "discard"),
           LORDdep = LORD(data, alpha, version = "dep"),
           SAFFRON = SAFFRON(data, alpha),
           ADDIS = ADDIS(data, alpha),
           AlphaInvesting = Alpha_investing(data, alpha))
  }
  
  select_alg_data <- eventReactive(input$compare, {
    out <- select_alg(alg = input$alg, data = data(),
                      alpha = as.numeric(LONDresult$alpha()))
  })
  
  data_to_plot <- eventReactive(input$compare, {
    
    current_alg_data <- LONDresult$LONDres()
    
    select_alg_data <- select_alg_data()
    
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
  # bindCache(LONDresult$LONDres() %>% slice_tail(),
  #           select_alg_data() %>% slice_tail()) %>%
  # bindEvent(input$compare)
  
  output$comp <- renderPlotly({
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime between 30 seconds and 1 minute..."))
    
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
    # shiny::removeModal()
  }) %>%
    bindCache(data_to_plot() %>% slice_tail())
  
  #to make compnum reactive
  
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
    set.seed(47)
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data),
           AlphaInvesting = Alpha_investing(data))
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
             CurrentLORD = log(alphai),
             !!rlang::quo_name(input$alg) := log(select_alg_data$alphainew),
             Bonferroni = log(0.05/index),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(CurrentLORD, !!rlang::quo_name(input$alg), Bonferroni, Unadjusted),
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
  })  %>%
    bindCache(data_to_plot() %>% slice_tail())
  
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
    set.seed(47)
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data),
           AlphaInvesting = Alpha_investing(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
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
    
    # shiny::removeModal()
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
  }) %>%
    bindCache(data_to_plot() %>% slice_tail())
  
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
    set.seed(47)
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data),
           AlphaInvesting = Alpha_investing(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
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
    
    # shiny::removeModal()
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
  }) %>%
    bindCache(data_to_plot() %>% slice_tail())
  
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
    set.seed(47)
    switch(alg,
           LOND = LOND(data),
           LORD = LORD(data),
           LORD3 = LORD(data, version = 3),
           LORDdiscard = LORD(data, version = "discard"),
           LORDdep = LORD(data, version = "dep"),
           SAFFRON = SAFFRON(data),
           ADDIS = ADDIS(data),
           AlphaInvesting = Alpha_investing(data))
  }
  
  data_to_plot <- eventReactive(input$compare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
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
    
    # shiny::removeModal()
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
  }) %>%
    bindCache(data_to_plot() %>% slice_tail())
  
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
BatchPRDScompServer <- function(input, output, session, BatchPRDSresult, data) {
  select_alg <- function(alg, data) {
    set.seed(47)
    switch(alg,
           BatchPRDS = BatchPRDS(data),
           BatchBH = BatchBH(data),
           BatchStBH = BatchStBH(data))
  }
  
  data_to_plot <- eventReactive(input$batchcompare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$batchalg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>% 
      distinct(batch, .keep_all = TRUE)
    
    current_alg_data <- BatchPRDSresult$BatchPRDSres() %>%
      distinct(batch, .keep_all = TRUE)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(BatchPRDS = log(alphai),
             !!rlang::quo_name(input$batchalg) := log(select_alg_data$alphai),
             Bonferroni = rep(log(0.05/nrow(.)), nrow(.)),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(BatchPRDS, !!rlang::quo_name(input$batchalg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    # shiny::removeModal()
  })
  
  output$batchcomp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Batch", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~batch, y = ~alpha, color = ~adjustment) %>%
        add_lines(mode = "lines+markers") %>%
        layout(xaxis = ex, yaxis = why)
    }
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(data_to_plot() %>% slice_tail())
}
BatchBHcompServer <- function(input, output, session, BatchBHresult, data) {
  select_alg <- function(alg, data) {
    set.seed(47)
    switch(alg,
           BatchPRDS = BatchPRDS(data),
           BatchBH = BatchBH(data),
           BatchStBH = BatchStBH(data))
  }
  
  data_to_plot <- eventReactive(input$batchcompare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$batchalg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>% 
      distinct(batch, .keep_all = TRUE)
    
    current_alg_data <- BatchBHresult$BatchBHres() %>%
      distinct(batch, .keep_all = TRUE)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(BatchBH = log(alphai),
             !!rlang::quo_name(input$batchalg) := log(select_alg_data$alphai),
             Bonferroni = rep(log(0.05/nrow(.)), nrow(.)),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(BatchBH, !!rlang::quo_name(input$batchalg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    # shiny::removeModal()
  })
  
  output$batchcomp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Batch", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~batch, y = ~alpha, color = ~adjustment) %>%
        add_lines(mode = "lines+markers") %>%
        layout(xaxis = ex, yaxis = why)
    }
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(data_to_plot() %>% slice_tail())
}
BatchStBHcompServer <- function(input, output, session, BatchStBHresult, data) {
  select_alg <- function(alg, data) {
    set.seed(47)
    switch(alg,
           BatchPRDS = BatchPRDS(data),
           BatchBH = BatchBH(data),
           BatchStBH = BatchStBH(data))
  }
  
  data_to_plot <- eventReactive(input$batchcompare, {
    
    # shiny::showModal(modalDialog("For datasets with more than 50,000 p-values, expect a runtime of up to a minute..."))
    
    select_alg_rx <- reactive({
      out <- select_alg(alg = input$batchalg, data = data())
    })
    
    select_alg_data <- select_alg_rx() %>% 
      distinct(batch, .keep_all = TRUE)
    
    current_alg_data <- BatchStBHresult$BatchStBHres() %>%
      distinct(batch, .keep_all = TRUE)
    
    data_to_plot <- cbind(current_alg_data, select_alg_data$alphai) %>%
      mutate(BatchStBH = log(alphai),
             !!rlang::quo_name(input$batchalg) := log(select_alg_data$alphai),
             Bonferroni = rep(log(0.05/nrow(.)), nrow(.)),
             Unadjusted = rep(log(0.05), nrow(.))) %>%
      pivot_longer(cols = c(BatchStBH, !!rlang::quo_name(input$batchalg), Bonferroni, Unadjusted),
                   names_to = "adjustment",
                   values_to = "alpha")
    
    # shiny::removeModal()
  })
  
  output$batchcomp <- renderPlotly({
    if(!is.null(data_to_plot())) {
      data_to_plot <- data_to_plot()
      
      font <- list(
        family = "Lato"
      )
      ex <- list(title = "Batch", titlefont = font)
      why <- list(title = "Log adjusted test level", titlefont = font)
      plot_ly(data_to_plot, x = ~batch, y = ~alpha, color = ~adjustment) %>%
        add_lines(mode = "lines+markers") %>%
        layout(xaxis = ex, yaxis = why)
    }
  }) %>%
    #only need small chunk of data to use as cache key
    bindCache(data_to_plot() %>% slice_tail())
}
