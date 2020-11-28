
library(shiny)
library(shinythemes)



# Define UI

ui <- fluidPage(theme = shinytheme("flatly"),
                withMathJax(), # to be able to use LaTeX expressions within the text
                
                navbarPage(
                     "Final Project - Group 11",
                     
                     tabPanel("The Assignment",
                              sidebarPanel(style="text-align: center;",
                                           tags$h2("Project Information"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h3("Objective"),
                                           tags$h5("To develop an R package implementing linear regression"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h3("Group"),
                                           tags$h5("Ana Gabriela Itokazu"),
                                           tags$h5("Eyoel Berhane"),
                                           tags$h5("John Musah"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h3("Sources"),
                                           a(href="https://github.com/AU-R-Programming/FinalProject-11", "Package"),
                                           tags$br(),
                                           a(href="https://github.com/AU-R-Programming/FinalProject-11/tree/main/shiny", "Shiny App"),
                                           tags$br(),
                                           a(href="https://github.com/AU-R-Programming/FinalProject-11", "RMarkdown"),
                                           tags$br(),
                                           a(href="https://github.com/AU-R-Programming/FinalProject-11", "GitHub Repository"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h3("Class"),
                                           tags$h5("STAT 6210"),
                                           tags$h5("R Programming for Data Science"),
                                           tags$h5("Prof. Dr. Roberto Molinari"),
                                           tags$h5("Auburn University - Fall 2020"),
                                           
                              ), # sidebarPanel
                              
                              mainPanel(style="text-align: justify;",
                                        
                                        h1("The Assignment"),
                                        em("This package was built as part of the requirements for the 'R Programming for Data Science' course, by Prof. Dr. Roberto Molinari. The assignment was lined up as follows:"),
                                        br(),
                                        br(),
                                        p("The final project will be evaluated on 100 points and the goal is to develop an R package implementing linear regression as highlighted in",
                                          a(href="https://smac-group.github.io/ds/section-functions.html#section-example-continued-least-squares-function", "Section 6.4 of the book"),
                                          "."),
                                        p("The package must contain the basic functions to perform linear regression (", em("e.g."), "estimate the coefficient vector \\(\\beta\\)) and obtain different statistics from the procedure. Using the notation from the book and without using any of the linear regression functions already available in R (", em("i.e."), "all outputs must be produced using formulas provided in the book and in this document), the basic outputs from the procedure must be the following:"),
                                        tags$ul(
                                             tags$li("Confidence intervals: the user must be able to choose the significance level \\(\\alpha\\) to obtain for the \\(1−\\alpha\\) confidence intervals for \\(\\beta\\) and whether to use the asymptotic or bootstrap approach for this."),
                                             tags$li("Plots (with ", em("e.g."), "ggplot2) including:",
                                                     tags$ol(
                                                          tags$li("Residuals vs fitted-value."),
                                                          tags$li("qq-plot of residuals."),
                                                          tags$li("Histogram (or density) of residuals."),
                                                     ),
                                             ),
                                             tags$li("Mean Square Prediction Error (MSPE) computed in matrix form."),
                                             tags$li("F-test: compute the statistic in matrix form and output the corresponding p-value."),
                                             tags$li("Help documentation for all functions (for example using the", em("roxygen2"), "package)"),
                                        ),
                                        br(),
                                        hr(),
                                        p("The package will be made available for download on a GitHub repository in the",
                                        a(href="https://github.com/AU-R-Programming", "AU-R-Programming organization"),
                                        "and the submission will be an html file on Canvas. The html file wil be a so-called vignette which indicates the name of the GitHub repository (and package) where you explain and give examples of how to use the package functions for all the desired outputs using one of the datasets on the Canvas course page."),
                                        hr(),
                                        br(),
                                        
                              ) # mainPanel
                              
                     ), # tabPanel, The Assignment
                     
                     tabPanel("The Package",
                              "Page under construction...."
                             # sidebarPanel(
                             #      tags$h3("Input:"),
                             #      textInput("txt1", "First Name:", ""),
                             #      textInput("txt2", "Last Name:", ""),
                             # 
                             # ), # sidebarPanel
                             # mainPanel(
                             #      h1("Header 1"),
                             # 
                             #      h4("Output"),
                             #      verbatimTextOutput("txtout"),
                             # ) # mainPanel

                     ), # tabPanel, The Package
 
                    tabPanel("The Theory Behind It",
                             "Page under construction...."
                              
                     ), # tabPanel, The Theory
 
                    tabPanel("How to use it",
                             "Page under construction...."
                              
                    ) # tabPanel, Examples
 
#                    tabPanel("Try It Yourself!",
#                             mainPanel(style="text-align: justify;",
#                                       p("You want to try it yourself to see if we really did something? Sure! Just follow the link below to our page:"),
#                                       a(href="www.rstudio.com", "Click here!"),
                                       
                                       
#                             ) # tabPanel, Try It Yourself
                            
                ) # navbarPage

        ) #fluidPage 


# Define server function

server <- function(input, output) {
        
} # server


# Create a Shiny Object

shinyApp(ui = ui, server = server)