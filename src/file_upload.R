################################################################################
# File upload page
#
# Author: Lathan Liou
# Created: Mon Sep 21 15:02:57 2020 ------------------------------
################################################################################
fluidPage(
  fluidRow(
    #### USER GUIDE ####
    h1("User Guide", id = "guidetitle"),
    bsplus::bs_accordion(id = "guide") %>%
      bs_set_opts(panel_type = "primary", use_heading_link = T) %>%
      bs_append(title = "Introduction", content = p("This application is designed to allow users to interactively run procedures that control the False Discovery Rate (FDR) for online hypothesis testing. Source code and additional information for this application are available via", a(href = "https://github.com/dsrobertson/onlineFDR", target = "_blank", rel = "noopener noreferrer", "our GitHub"))) %>%
      bs_append(title = "False Discovery Rate vs. Family-wise Error Rate", content = p("FDR is the expected proportion of false rejections out of all rejections. FWER is the probability of making any Type I errors at all. Controlling the FWER is generally more conservative than controlling the FDR. Note that in the case when all null hypotheses are true, the FDR and FWER are the same. For the FWER Explore app, click", a(href = "https://mrc-bsu.shinyapps.io/onlineFWERExplore", target = "_blank", rel = "noopener noreferrer", "here."))) %>%
      bs_append(title = "Which algorithm do I use?", content = p(
        "In general, there are two 'flavors' your data could come in: fully sequential and batch. This app provides algorithms that control the FDR for both flavors. For guidance on understanding which 'flavor', check out our ", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html#which-function-do-i-use-", target = "_blank", rel = "noopener noreferrer", "flowchart."), 
        br(),
        br(),
        "This app also provides a helper tool that indicates which algorithm is more powerful across different parameters based on simulated data and a helper tool that indicates how your data should be formatted.", 
        br(),
        br(),
        "For more information about the algorithms themselves, check out the ", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html", target = "_blank", rel = "noopener noreferrer", "Get Started"), "page in our vignette."))  %>%
      bs_append(title = "Help & feedback", content = p("For additional help or to submit feedback or bug reports, please contact:", 
       br(),
       "David Robertson",
       br(),
       "MRC Biostatistics Unit",
       br(),
       a(href = "mailto:david.robertson@mrc-bsu.cam.ac.uk", "Email"),
       br(),
       br(),
       "Lathan Liou",
       br(),
       "Merck & Co.",
       br(),
       a(href = "mailto:lathanliu21@gmail.com", "Email")))
  ), #close fluidrow
  fluidRow(
    #### CHECKBOX FIRST TIME USER ####
    prettyCheckbox("checkbox",
                   strong("Click me if you're a first time user"), 
                   value = FALSE,
                   shape = "curve",
                   fill = TRUE,
                   animation = "pulse",
                   icon = icon("check"),
                   status = "info")
  ),
  shinyjs::hidden(
    div(
      id = "novice",
      fluidRow(
        h1("Which fully sequential algorithm do I use?"),
        p("The following tool is designed to help inform which algorithm is most appropriate to use and is based on simulated data in ideal conditions. Please note that in your real-world data, you never know the number of non-null hypotheses, and thus, this tool is meant to be exploratory rather than definitive. Given your specified parameters, this tool will report which algorithm has the highest power given."),
        br(),
        p("For more information, please use the", a(href = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html#which-function-do-i-use-", target = "_blank", rel = "noopener noreferrer", "flowchart"), "to help determine which algorithm to use."),
        br(),
        br(),
        column(3,
               strong("Number of p-values"),
               br(),
               shinyWidgets::prettyRadioButtons("size", NULL, c(50,
                                                                100,
                                                                1000),
                                                icon = icon("check"),
                                                bigger = TRUE,
                                                status = "info",
                                                animation = "jelly")
        ),
        column(3,
               align = "center",
               strong("Proportion of expected non-null hypotheses"),
               shiny::sliderInput("prop", NULL, min = 0.1, max = 1, value = 0.5, step = 0.1),
               shinyBS::bsTooltip("prop",
                                  "Proportion of true non-null hypotheses",
                                  placement = "right",
                                  trigger = "hover")
        ),
        column(3,
               strong("Number of expected p-values"),
               shinyWidgets::prettyRadioButtons("bound", NULL, c("Known", 
                                                                 "Infinite"),
                                                icon = icon("check"),
                                                bigger = TRUE,
                                                status = "info",
                                                animation = "jelly")
        ),
        column(3,
               strong("Data dependency"),
               br(),
               shinyWidgets::prettyRadioButtons("dep", NULL, c("Independent", 
                                                               "Dependent"),
                                                icon = icon("check"),
                                                bigger = TRUE,
                                                status = "info",
                                                animation = "jelly"))
      ),
      fluidRow(
        column(width = 12,
               align = "center",
               boxPlus(
                 title = "Your recommended algorithm", 
                 closable = FALSE, 
                 width = NULL,
                 status = "primary", 
                 solidHeader = FALSE, 
                 background = "aqua",
                 collapsible = TRUE,
                 enable_dropdown = TRUE,
                 dropdown_icon = "wrench",
                 dropdown_menu = dropdownItemList(
                   dropdownItem(url = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html", name = "More information"),
                   dropdownItem(url = "https://dsrobertson.github.io/onlineFDR/articles/onlineFDR.html#which-function-do-i-use-", name = "User Flowchart"),
                 ),
                 p(textOutput("demores"))
               )
        )
      ),
      tags$head(tags$style("#demores{font-size: 18px;
                         font-family: Arial;
                         text-align: center;")),
      br(),
      textOutput("addiswarn"),
      tags$head(tags$style("#addiswarn{font-size: 14px;
                         font-family: Arial;
                         text-align: center;
                         color: red")),
      fluidRow(
        h1("How should I format my data?"),
        p("Now that you've familiarized yourself with the different algorithms, depending on the 'flavor', there are two main data formats to consider to make full use of this app. Select a 'flavor' to see how you should format your data."),
        column(3,
               strong("Choose a data format"),
               br(),
               shinyWidgets::prettyRadioButtons("format", NULL, c("Fully sequential",
                                                                  "Batch"),
                                                icon = icon("check"),
                                                bigger = TRUE,
                                                status = "info",
                                                animation = "jelly")
        ),
        column(9,
               uiOutput("formatres"))
      ) #close fluidrow
    ) #close div
  ), #close hidden
  br(),
  fluidRow(
    #### UPLOAD DATA ####
    h1("Upload your dataset", id = "upload"),
    p("Ensure that your CSV file contains at the minimum, a column of p-values with the name 'pval'. If you're including dates, ensure that they are in the format YYYY-MM-DD. "),
    column(
      width = 8,
      fileInput("file", NULL,
                multiple = FALSE,
                accept = c('text/csv', 
                           'text/comma-separated-values',
                           'text/plain',
                           '.csv'))
    ),
    column(
      width = 4,
      uiOutput("showjump")
    )
  )
) #close fluidpage