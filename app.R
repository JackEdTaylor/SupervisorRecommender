library(shiny)
library(shinydashboard)
library(tidyverse)

sidebarwidth <- 400
dat <- read_csv("supervisors.csv") %>%
  mutate(
    topics = str_replace_all(topics, "(,) +", ","),
    methods = str_replace_all(methods, "(,) +", ",")
  ) %>%
  separate(name, c("name_first", "name_last"), " ", remove = FALSE) %>%
  arrange(name_last)
topics_vec <- sort(unique(unlist(str_split(dat$topics, ","))))
methods_vec <- sort(unique(unlist(str_split(dat$methods, ","))))

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "Supervisor Recommender", titleWidth = sidebarwidth,
                  tags$li(a(href = "https://github.com/JackEdTaylor/SupervisorRecommender",
                            HTML(paste(icon("github"), "&nbsp;Code")),
                            title = "GitHub Repository"),
                          class="dropdown")),
  
  dashboardSidebar(
    width = sidebarwidth,
    sidebarMenu(
      fluidRow(
        column(12, HTML(
          '<p>Welcome! This app is designed to help you identify a suitable supervisor for the <a href="https://www.gla.ac.uk/postgraduate/taught/psychologicalscienceresearchmethodsof/">Research Methods of Psychological Science MSc</a> course at the University of Glasgow.<br><br>Just tick any topics or methods you are interested in, and the app will suggest supervisors in order of how relevant their research is.<br><br>Click on a supervisor to find out more about them.</p>'
        ))
      ),
      fluidRow(
        tags$div(id = "lh-checkboxes", column(6, checkboxGroupInput("topics", "Topics", topics_vec))),
        column(6, checkboxGroupInput("methods", "Methods", methods_vec))
      )
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    lapply(1:nrow(dat), function(i) {
      box_id <- sprintf("box_%i", i)
      uiOutput(box_id)
    })
    )
  
    )

server <- function(input, output) {
  
  # process preferences
  dat_rel <- reactive({
    out <- dat
    
    out$methods_n <- sapply(dat$methods, function(sup_meth) {
      matches <- sapply(str_split(sup_meth, ","), function(meth) {meth %in% input$methods})
      length(matches[matches])
    }) %>%
      unname()
    
    out$topics_n <- sapply(dat$topics, function(sup_meth) {
      matches <- sapply(str_split(sup_meth, ","), function(meth) {meth %in% input$topics})
      length(matches[matches])
    }) %>%
      unname()
    
    out$total_n <- out$topics_n + out$methods_n
    
    out %>%
      mutate(total_n = topics_n + methods_n) %>%
      arrange(desc(total_n))
  })
  
  lapply(1:nrow(dat), function(i) {
    box_id <- sprintf("box_%i", i)
    output[[box_id]] <- renderUI({
      dat_rel_df <- dat_rel()
      sup_dat <- dat_rel_df[i,]
      
      box_title <- if (length(c(input$methods, input$topics)) > 0) {
        sprintf("%s (%i matches)", sup_dat$name, sup_dat$total_n)
      } else {
        sup_dat$name
      }
      
      topics_matches_str <- if (length(input$topics) > 0) {
        topics_bolded <- unlist(str_split(sup_dat$topics, ","))
        topics_bolded[topics_bolded %in% input$topics] <- paste("<b>",topics_bolded[topics_bolded %in% input$topics],"</b>", sep="")
        HTML(sprintf("Topics <b>(%i matches)</b>:<br>%s", sup_dat$topics_n, paste(topics_bolded, collapse=", ")))
      } else {
        HTML(sprintf("Topics:<br>%s", str_replace_all(sup_dat$topics, ",", ", ")))
      }
      
      methods_matches_str <- if (length(input$methods) > 0) {
        methods_bolded <- unlist(str_split(sup_dat$methods, ","))
        methods_bolded[methods_bolded %in% input$methods] <- paste("<b>",methods_bolded[methods_bolded %in% input$methods],"</b>", sep="")
        HTML(sprintf("Methods <b>(%i matches)</b>:<br>%s", sup_dat$methods_n, paste(methods_bolded, collapse=", ")))
      } else {
        HTML(sprintf("Methods:<br>%s", str_replace_all(sup_dat$methods, ",", ", ")))
      }
      
      fluidRow(
        box(
          title = a(href = sup_dat$page, box_title),
          width = 12, status = "info", solidHeader = TRUE,
          fluidRow(
            
            HTML(
              paste(
                c(
                  sprintf('<p><a href="%s"><img src="%s"></a>%s', sup_dat$page, sup_dat$image, sup_dat$message),
                  topics_matches_str,
                  methods_matches_str,
                  "</p>"),
                collapse = "<br><br>"
              )
            )
          )
          
        )
      )
      
    })
    
  })
  
}

shinyApp(ui = ui, server = server)