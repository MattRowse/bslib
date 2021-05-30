## app.R ##
setwd(getwd())
library(shiny)
library(shinydashboard)
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
      #     theme = theme,
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
      )
    ),
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
    div(
      style = "display:inline-block",
      textAreaInput(
        "Q_answer_field",
        "What is the answer to this question?",
        width = "800px",
        height = "100px"
      )
    ),
    div(style = "display:inline-block",
        actionButton("question_update", "Update database")),
    verbatimTextOutput("confirm_chatbox",
                       placeholder = TRUE)
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
      if (input$category_field == "data") {
        out <- datapred(toString(input$question_field))
      }
      if (input$category_field == "design") {
        out <- designpred(toString(input$question_field))
      }
      if (input$category_field == "ebay") {
        out <- ebaypred(toString(input$question_field))
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
      if (input$category_field == "payments") {
        out <- paymentspred(toString(input$question_field))
      }
      if (input$category_field == "xero") {
        out <- xeropred(toString(input$question_field))
      }
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
          Timestamp = as.character(Sys.time())
        )
      saveRDS(data, file = "data.RDS")
    })
#      observeEvent(input$question_update, {
#        logs <- logs %>%
#           add_row(user=session$user, area=as.character(input$update_category_field),question_update=as.character(input$db_question_field), answer_update=as.character(input$Q_answer_field),timestamp=Sys.time())
#       saveRDS(logs, file = "logs.RDS")})
  })
})
shinyApp(ui, server)