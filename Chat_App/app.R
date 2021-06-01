## app.R ##
setwd(getwd())
library(shiny)
library(shinydashboard)
library(ggplot2)
#library(bslib)
#library(thematic)
source("make_model.r")

## create a base theme for bslib
#theme <- bs_theme(bootswatch = "minty")

# Let thematic know to use the font from bs_lib
#thematic_shiny(font = "auto")

ui <- shinyUI(navbarPage(
  "Support Chatbot",
  tabPanel(
    "Ask a question",
    fluidPage(
 #    theme = theme,
      div(
        selectInput(
          "category_field",
          "Category",
          choices = unique(sort(data$Area)),
          selected = "Make a selection",
          width = "300px"
        )
      ),
      uiOutput("chatbox_q"),
      div(
        style = "display:inline-block",
        textAreaInput(
          "question_field",
          "Your question",
          width = "800px",
          height = "100px"
        )
      ),
      br(),
      div(style = "display:inline-block",
          actionButton("send", "Find your answer")),
      br(),
      verbatimTextOutput("chatbox",
                         placeholder = TRUE)
    )
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
      ),
    ),
    br(),
    uiOutput("chatbox_t"),
    div(
      style = "display:inline-block",
      textAreaInput(
        "db_question_field",
        "Add a question to database",
        width = "800px",
        height = "100px"
      )
    ),
    br(),
    div(
      style = "display:inline-block",
      textAreaInput(
        "Q_answer_field",
        "What is the answer to this question?",
        width = "800px",
        height = "100px"
      )
    ),
    br(),
    div(style = "display:inline-block",
        actionButton("question_update", "Update database")),
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
    dataTableOutput("question_table"),
    div(
    downloadButton("download_log_Data", "Download Logs"),
    downloadButton("download_question_Data", "Download Questions"),
    actionButton("train_models", "Retrain Question Models",
                 icon("paper-plane"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              
    helpText("When you click the retrain models button above, the application could stall, while the server is processing")
    )
  )
  )
)


server <- shinyServer(function(input, output, session) {
  observeEvent(input$send, {
    output$chatbox <- renderText({
      if (input$category_field == "amazon") {
        out <- amazonpred(toString(input$question_field))
      }
      if (input$category_field == "catch") {
        out <- catchpred(toString(input$question_field))
      }
      if (input$category_field == "data") {
        out <- datapred(toString(input$question_field))
      }
      if (input$category_field == "design") {
        out <- designpred(toString(input$question_field))
      }
      if (input$category_field == "ebay") {
        out <- ebaypred(toString(input$question_field))
      }
      if (input$category_field == "google") {
        out <- xeropred(toString(input$question_field))
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
      if (input$category_field == "payments") {
        out <- paymentspred(toString(input$question_field))
      }
      if (input$category_field == "shipping") {
        out <- shippingpred(toString(input$question_field))
      }
      if (input$category_field == "webstore") {
        out <- xeropred(toString(input$question_field))
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
      logs = readRDS("logs.RDS")
      out
    })
  })
  observeEvent(input$question_update, {
    output$confirm_chatbox <- renderText({
      "Thank you for contributing!"
    })
    observeEvent(input$question_update, {
      data <- data %>%
        add_row(
          Area = as.character(input$update_category_field),
          Question = as.character(input$db_question_field),
          Answers = as.character(input$Q_answer_field),
          Timestamp = ymd_hms(Sys.time())
        )
      saveRDS(data, file = "data.RDS")
    })
    observeEvent(input$question_update, {
      logs <- logs %>%
        add_row(
          area = as.character(input$update_category_field),
          question_update = as.character(input$db_question_field),
          answer_update = as.character(input$Q_answer_field),
          timestamp = ymd_hms(Sys.time())
        )
      saveRDS(logs, file = "logs.RDS")
      logs = readRDS("logs.RDS")
    })

  })
  observeEvent(input$time_selection, {
  output$myplot <- renderPlot(
    logs %>% 
      filter(timestamp >=input$time_selection[1] & timestamp <= input$time_selection[2]) %>% 
      group_by(area,question_update) %>% count(question_update) %>% ggplot(aes(area, n, fill = area)) +
      geom_col()+
      theme_minimal()+
      ggtitle("Questions Updated")+
      labs(x="Topic",y="Volume")+
      theme_light()+
      theme(legend.position = "none")+
      coord_flip()
  )
  })
  observeEvent(input$time_selection, {
    output$myusage <- renderPlot(
      logs %>% 
        filter(timestamp >=input$time_selection[1] & timestamp <= input$time_selection[2] & question !=is.na(question)) %>% 
        group_by(area,question) %>% count(question) %>% ggplot(aes(area, n, fill = area)) +
        geom_col()+
        theme_minimal()+
        ggtitle("Queries Asked")+
        labs(x="Topic",y="Volume")+
        theme_light()+
        theme(legend.position = "none")+
        coord_flip()
    )
  })
  observeEvent(input$time_selection, {
    output$line_chart <- renderPlot(
      logs %>% 
        filter(timestamp >=input$time_selection[1] & timestamp <= input$time_selection[2]) %>% 
        mutate(Date_Time = floor_date(timestamp,unit = "days")) %>% 
        group_by(Date_Time) %>% count(question) %>% ggplot(aes(Date_Time,n)) +
        geom_line()+
        theme_minimal()+
        ggtitle("Questions Asked")+
        labs(x="Date",y="Volume")+
        theme_light()+
        theme(legend.position = "none")
    )
  })
  observeEvent(input$time_selection, {
    output$question_table <- renderDataTable(logs %>% select(question,answer,timestamp) %>% drop_na())
  })
  
  output$download_log_Data <- downloadHandler(
    filename = function() {
      paste("logsdata-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(logs, file)
    }
  )
  
  output$download_question_Data <- downloadHandler(
    filename = function() {
      paste("questions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  observeEvent(input$train_models, {
   train <- source("make_model.r")
   train
  })
})
shinyApp(ui, server)