## app.R ##
setwd(getwd())
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(tidyr)
#library(bslib)
#library(thematic)
source("make_model.r")
## create a base theme for bslib
#theme <- bs_theme(bootswatch = "minty")

# Let thematic know to use the font from bs_lib
#thematic_shiny(font = "auto")

ui <- shinyUI(
  navbarPage(
  "Support Chatbot",
  tabPanel(
    "Ask a question",
    fluidPage(tags$head(includeHTML(("google-analytics.html"))),
              setBackgroundColor(color="ghostwhite"),
#     theme = theme,
      div(style = "display:inline-block",
        selectInput(
          "category_field",
          "Category",
          choices = unique(sort(data$Area)),
          selected = "amazon",
          width = "300px"
        ),
        h5(HTML(paste0("<b>","Teams Channel","</b>"))),
# add channel, confluence & teams links, based on category inside the div 
        verbatimTextOutput("teams_channel",placeholder = TRUE)
      ),
      uiOutput("chatbox_q"),
      div(
        style = "display:inline-block",
        textAreaInput(
          "question_field",
          "Your question",
          width = "800px",
          height = "50px"
        )
      ),
      br(),
      div(style = "display:inline-block",
          actionButton("send", "Find your answer",
                       icon("paper-plane"),
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                       )),
      br(),
      verbatimTextOutput("chatbox",
                         placeholder = TRUE)
    ),
# value checboxes for users to provide quality feedback
      div(style = "display:inline-block",
          actionButton(
            "helpful",
            "That was awesome!",
            icon("thumbs-up"),
            style = "color: #fff; background-color: #2e8b57; border-color: #355e3b"
          ),
        actionButton(
          "not_helpful",
          "Update this question!",
          icon("thumbs-down"),
          style = "color: #fff; background-color: #f50000; border-color: #940000"
        )),

      br(),
      h5("Top 25 Trending Questions"),
      h6("*From last 30 days"),
      dataTableOutput("trending")
  ),
  tabPanel(
    "Update the question database",
    div(
      selectInput(
        "update_category_field",
        "Category to update",
        choices = unique(sort(data$Area)),
        selected = "Make a selection",
        width = "300px"
      )),
    br(),
    uiOutput("chatbox_t"),
    div(
      style = "display:inline-block",
      textAreaInput(
        "db_question_field",
        "Add a question to database",
        width = "800px",
        height = "50px"
      )
    ),
    br(),
    div(
      style = "display:inline-block",
      textAreaInput(
        "Q_answer_field",
        "What is the answer to this question?",
        width = "800px",
        height = "50px"
      )
    ),
    br(),
    div(style = "display:inline-block",
        actionButton("question_update", "Update database",
                     icon("paper-plane"),
                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    br(),
    verbatimTextOutput("confirm_chatbox",
                       placeholder = TRUE)
  ),
  tabPanel(
    "View logs",
    dateRangeInput(
      "time_selection",
      "Time Period",
      start = "2021-06-01",
      end = "2021-06-01",
      min = NULL,
      max = NULL,
      format = "yyyy-mm-dd",
      startview = "month",
      weekstart = 1,
      language = "en",
      separator = " to ",
      width = NULL,
      autoclose = TRUE
    ),
    br(),
    # Show a plot of the generated distribution
    plotOutput("myusage"),
    plotOutput("myplot"),
    plotOutput("line_chart"),
    plotOutput("question_count"),
    dataTableOutput("question_table"),
    div(
      downloadButton("download_log_Data", "Download Logs",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      downloadButton("download_question_Data", "Download Questions",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      downloadButton("download_feedback", "Download Feedback",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton(
        "train_models",
        "Retrain Question Models",
        icon("robot"),
        style = "color: #fff; background-color: #f50000; border-color: #940000"
      )),
      
      helpText(
        div(
          style = "display:inline-block",
        "When you click the retrain models button above, the application could stall, while the server is processing")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  observeEvent(input$send, {   
    output$chatbox <- renderText({
      if (input$category_field == "amazon") {
        out <- amazonpred(toString(input$question_field))
      }
      if (input$category_field == "catch") {
        out <- catchpred(toString(input$question_field))
      }
      if (input$category_field == "customers") {
        out <- customerspred(toString(input$question_field))
      }
      if (input$category_field == "data") {
        out <- datapred(toString(input$question_field))
      }
      if (input$category_field == "design") {
        out <- designpred(toString(input$question_field))
      }
      if (input$category_field == "dns") {
        out <- dnspred(toString(input$question_field))
      }
      if (input$category_field == "ebay") {
        out <- ebaypred(toString(input$question_field))
      }
      if (input$category_field == "emails") {
        out <- emailspred(toString(input$question_field))
      }
      if (input$category_field == "facebook") {
        out <- facebookpred(toString(input$question_field))
      }
      if (input$category_field == "instagram") {
        out <- instagrampred(toString(input$question_field))
      }
      if (input$category_field == "google") {
        out <- googlepred(toString(input$question_field))
      }
      if (input$category_field == "integrations") {
        out <- integrationspred(toString(input$question_field))
      }
      if (input$category_field == "inventory") {
        out <- inventorypred(toString(input$question_field))
      }
      if (input$category_field == "kogan") {
        out <- koganpred(toString(input$question_field))
      }
      if (input$category_field == "myob") {
        out <- myobpred(toString(input$question_field))
      }
      if (input$category_field == "mydeal") {
        out <- mydealpred(toString(input$question_field))
      }
      if (input$category_field == "neto_analytics") {
        out <- neto_analyticspred(toString(input$question_field))
      }
      if (input$category_field == "payments") {
        out <- paymentspred(toString(input$question_field))
      }
      if (input$category_field == "permissions") {
        out <- permissionspred(toString(input$question_field))
      }
      if (input$category_field == "pick_n_pack") {
        out <- pick_n_packpred(toString(input$question_field))
      }
      if (input$category_field == "pos") {
        out <- pospred(toString(input$question_field))
      }
      if (input$category_field == "shipping") {
        out <- shippingpred(toString(input$question_field))
      }
      if (input$category_field == "trademe") {
        out <- trademepred(toString(input$question_field))
      }
      if (input$category_field == "webstore") {
        out <- webstorepred(toString(input$question_field))
      }
      if (input$category_field == "xero") {
        out <- xeropred(toString(input$question_field))
      }
      logs <- logs %>%
        add_row(
          area = as.character(input$category_field),
          question = as.character(input$question_field),
          answer = as.character(out),
          timestamp = Sys.time()
        )
      saveRDS(logs, file = "logs.RDS")
      logs <- readRDS("logs.RDS")
      paste(out)
    })
  })
  
  observeEvent(input$helpful, { 
    feedback <- feedback %>%
    add_row(
      area = as.character(input$category_field),
      question = as.character(input$question_field),
      #answer = as.character(output$chatbox),
      helpful = as.character("TRUE"),
      timestamp = ymd_hms(Sys.time()))
    saveRDS(feedback, file = "feedback.RDS")
  })
  
  observeEvent(input$not_helpful, { 
    feedback <- feedback %>%
      add_row(
        area = as.character(input$category_field),
        question = as.character(input$question_field),
        #answer = as.character(output$chatbox),
        helpful = as.character("FALSE"),
        timestamp = ymd_hms(Sys.time()))
    saveRDS(feedback, file = "feedback.RDS")
  })
  
  observeEvent(input$category_field, {
    output$teams_channel <- renderText({
      if (input$category_field == "amazon") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "catch") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "customers") {
        out <- toString("General")
      }
      if (input$category_field == "data") {
        out <- toString("Import/Export")
      }
      if (input$category_field == "design") {
        out <- toString("Partners Team: Developer Support")
      }
      if (input$category_field == "dns") {
        out <- toString("MAD")
      }
      if (input$category_field == "ebay") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "emails") {
        out <- toString("MAD")
      }
      if (input$category_field == "facebook") {
        out <- toString("Import/Export")
      }
      if (input$category_field == "instagram") {
        out <- toString("General")
      }
      if (input$category_field == "google") {
        out <- toString("General")
      }
      if (input$category_field == "integrations") {
        out <- toString("PII")
      }
      if (input$category_field == "inventory") {
        out <- toString("PII")
      }
      if (input$category_field == "kogan") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "myob") {
        out <- toString("PII")
      }
      if (input$category_field == "mydeal") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "neto_analytics") {
        out <- toString("MAD")
      }
      if (input$category_field == "payments") {
        out <- toString("PII")
      }
      if (input$category_field == "permissions") {
        out <- toString("General")
      }
      if (input$category_field == "pick_n_pack") {
        out <- toString("Shipping")
      }
      if (input$category_field == "pos") {
        out <- toString("Sales Channels")
      }
      if (input$category_field == "shipping") {
        out <- toString("Shipping")
      }
      if (input$category_field == "trademe") {
        out <- toString("Import/Export")
      }
      if (input$category_field == "webstore") {
        out <- toString("Webstore")
      }
      if (input$category_field == "xero") {
        out <- toString("PII")
      }
      out
    })
  })

  observeEvent(input$category_field, {
    output$trending <- 
      renderDataTable(logs %>% 
                        filter(area==input$category_field &
                                      question != "NULL" & question != "" &
                                      timestamp >= Sys.Date()-30 & timestamp <= Sys.Date()) %>%
                                      select(question, answer) %>% 
                                      count(question, answer, sort = TRUE) %>%
                                      top_n(25) %>% 
                                      select(question, answer)
                                      )
                                      
                    
  })
  
  observeEvent(input$question_update, {
    output$confirm_chatbox <- renderText({
      "Thank you for contributing!"
    })
  })
  
  observeEvent(input$question_update, {
      data <- data %>%
        add_row(
          Area = as.character(input$update_category_field),
          Question = as.character(input$db_question_field),
          Answers = paste("Predicted question: ",Question,"\n",  "Predicted answer: ",as.character(input$Q_answer_field),sep="\n"),
          Timestamp = ymd_hms(Sys.time()))
          saveRDS(data, file = "data.RDS")
    })
    
  observeEvent(input$question_update, {
      logs <- logs %>%
        add_row(
          area = as.character(input$update_category_field),
          area_update = as.character(input$update_category_field),
          question_update = as.character(input$db_question_field),
          answer_update = as.character(input$Q_answer_field),
          timestamp = ymd_hms(Sys.time())
        )
      saveRDS(logs, file = "logs.RDS")
      logs = readRDS("logs.RDS")
    })
  
  observeEvent(input$time_selection, {
    output$myplot <- renderPlot(
      logs %>%
        filter(
          timestamp >= input$time_selection[1] &
            timestamp <= input$time_selection[2]
        ) %>%
        group_by(area, question_update) %>% count(question_update) %>% ggplot(aes(area, n, fill = area)) +
        geom_col() +
        theme_minimal() +
        ggtitle("Questions Updated") +
        labs(x = "Topic", y = "Volume") +
        theme_light() +
        theme(legend.position = "none") +
        coord_flip()
    )
  })
  
  observeEvent(input$time_selection, {
    output$myusage <- renderPlot(
      logs %>%
        filter(
          timestamp >= input$time_selection[1] &
            timestamp <= input$time_selection[2] &
            question != is.na(question)
        ) %>%
        group_by(area, question) %>% count(question) %>% ggplot(aes(area, n, fill = area)) +
        geom_col() +
        theme_minimal() +
        ggtitle("Queries Asked") +
        labs(x = "Topic", y = "Volume") +
        theme_light() +
        theme(legend.position = "none") +
        coord_flip()
    )
  })
  
  observeEvent(input$time_selection, {
    output$line_chart <- renderPlot(
      logs %>%
        filter(
          timestamp >= input$time_selection[1] &
            timestamp <= input$time_selection[2]
        ) %>%
        mutate(Date_Time = floor_date(timestamp, unit = "days")) %>%
        group_by(Date_Time) %>% count(question) %>% ggplot(aes(Date_Time, n)) +
        geom_col() +
        theme_minimal() +
        ggtitle("Questions Asked") +
        labs(x = "Date", y = "Volume") +
        theme_light() +
        theme(legend.position = "none")
    )
  })
  
  observeEvent(input$time_selection, {
    output$question_count <- renderPlot(
      data %>%
        group_by(Area,Answers) %>% count(Answers) %>% ggplot(aes(Area, n, fill=Area)) +
        geom_col() +
        theme_minimal() +
        ggtitle("Questions in Database") +
        labs(x = "Area", y = "Question Count") +
        theme_light() +
        theme(legend.position = "none")+
        coord_flip()
    )
  })
  
  observeEvent(input$time_selection, {
    output$question_table <-
      renderDataTable(logs %>% 
                        filter(question != "NULL" & question != "") %>% 
                          select(question, answer, timestamp) %>% drop_na())
  })
  
  output$download_log_Data <- downloadHandler(
    filename = function() {
      paste("logsdata-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(logs, file)
    }
  )
  
  output$download_question_Data <- downloadHandler(
    filename = function() {
      paste("questions-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  output$download_feedback <- downloadHandler(
    filename = function() {
      paste("feedback-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(feedback, file)
    }
  )
  
  observeEvent(input$train_models, {
    train <- source("make_model.r")
    train
  })
})
  
shinyApp(ui, server)