## app.R ##
library(shiny)
library(shinydashboard)
library(bslib)
library(thematic)

source("make_model.r")

## create a base theme for bslib
theme <- bs_theme(
    bg = "#0b3d91", fg = "white", primary = "#FCC780",
    base_font = font_google("Montserrat"),
    code_font = font_google("Montserrat")
)

# Let thematic know to use the font from bs_lib
thematic_shiny(font = "auto")

ui <- shinyUI(fluidPage(
    theme = theme,    
  titlePanel("Neto Support AI Chatbot App"),
  div(selectInput("category_field", "Category", 
                  choices = unique(sort(data$Area)),
                  selected = "Make a selection", 
                  width = "300px")),
  uiOutput("chatbox_q"),
  div(style = "display:inline-block",
  textInput("question_field", "Your question", width = "800px")),
  div(style = "display:inline-block",
  actionButton("send", "Send")),
#  uiOutput("chatbox_a"),
#  div(style = "display:inline-block"),
  fluidRow(
      column(1,
          textOutput("chatbox", 
                     container = div
                     ))
  )
))

server <- shinyServer( function(input, output, session) {
    observeEvent(input$send, {
        output$chatbox <- renderText({
            if(input$category_field =="ebay"){
            out <- ebaypred(toString(input$question_field))
        }
            if(input$category_field =="inventory"){
            out <- inventorypred(toString(input$question_field))
            }    
            out
    })
    })
})
shinyApp(ui, server)