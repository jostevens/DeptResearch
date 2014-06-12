library(shiny)

shinyUI(fluidPage(
  # Title
  titlePanel("Department Enrollment"),
  
  #Sidebar
  fluidRow(
    
    column(3,
           checkboxGroupInput("checkGroup", 
                              label = h3("Departments"), 
                              choices = list("Sociology" = "SOC", 
                                             "Geography" = "GEOG", 
                                             "Psychology" = "PSY",
                                             "Anthropolgy" = "ANTH",
                                             "Family and Consumer" = "FCS",
                                             "Political Science" = "POLS",
                                             "Economics" = "ECON"),
                              selected = "SOC"),
           h3("Actions"),
           submitButton( "Refresh Data"),
           h6("(this may take some time)"),
           br(),
           br(), 
           actionButton("action", label ="Make Graphic")
           )
 
  ),
  
    #Main Panel
    mainPanel(
      textOutput("ET01"),
      plotOutput("ET02")
      )
    )
  
  )