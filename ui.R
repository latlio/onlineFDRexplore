################################################################################
# UI of the Shiny app
#
# Author: Lathan Liou
# Created: Fri Sep 18 09:50:19 2020 ------------------------------
################################################################################
library(DT)

source("src/ui-mods.R")

ui <- shiny::fluidPage(
  tagList(
    includeCSS("www/css/styles.css"),
    shinyjs::useShinyjs(),
    shinyWidgets::useShinydashboard(),
    shinyWidgets::useShinydashboardPlus(),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    sever::use_sever(),
    waiter::waiter_show_on_load(html = tagList(waiter::spin_fading_circles(),
                                               "Initializing onlineFDRexplore",)),
    cicerone::use_cicerone(),
    tags$head(
      tags$script(src = "src/JSModule.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/lato.css"),
      tags$style(
                 HTML(
                 ".bttn { vertical-align: middle; height: 30px; width: 100%; font-size: 12px; font-family: Lato, sans-serif;}",
                 ".panel-group {font-family: Lato, sans-serif; font-size: 14px;} ",
                 ".h1 {font-family: Lato;}",
                 ".p {font-family: Lato;}")
    )),
    ####make the navbar pages####
    shiny::navbarPage(HTML(paste0("onlineFDR", tags$sub("explore"))),
                      windowTitle = "onlineFDRExplore",
                      id = "navmaster",
                      shiny::tabPanel("Get Started",
                                      source("src/file_upload.R")$value),
                      shiny::navbarMenu("Fully sequential algorithms",
                                        shiny::tabPanel("LOND",
                                                        source("src/LOND_page.R")$value),
                                        shiny::tabPanel("LORD",
                                                        source("src/LORD_page.R")$value), #close tabPanel
                                        shiny::tabPanel("SAFFRON",
                                                        source("src/SAFFRON_page.R")$value), #close tabPanel
                                        shiny::tabPanel("ADDIS",
                                                        source("src/ADDIS_page.R")$value), #close tabPanel
                                        shiny::tabPanel("Alpha-investing",
                                                        source("src/Alpha_investing_page.R")$value), #close
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }")
                      ),# close navbarMenu
                      shiny::tabPanel("About",
                                      source("src/about_page.R")$value)
    ) ##close navbarpage
  ) ## close taglist
) ## close fluid page