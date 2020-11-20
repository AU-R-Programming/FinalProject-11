install.packages("shiny")
install.packages("shinythemes")
library(shiny)
library(shinythemes)

# Define UI

ui <- FluidPage(theme = shinytheme("flatly"),
                navbarPage(
                     "FinalProject Group 11",
                     tabPanel("NavigationBar1",
                              sidebarPanel(
                                   tag$h3("Input:"),
                                   textInput("txt1", "First Name:", ""),
                                   textInput("txt2", "Last Name:", ""),
                              
                              ), # this is the Side Bar Panel - sidebarPanel
                              mainPanel(
                                        h1("Header 1"),
                                   
                                        h4("Output"),
                                        verbatimTextOutput("txtout"),
                              ) # and this is the Main Panel - mainPanel
                    

                )))












# Create a Shiny App Object

