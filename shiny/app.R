
library(shiny)
library(shinythemes)



# Define UI

ui <- fluidPage(theme = shinytheme("flatly"),
                withMathJax(), # to be able to use LaTeX expressions within the text
                
                navbarPage(
                     "FinalProject Group 11",
                     tabPanel("The Assignment",
                              sidebarPanel(style="text-align: justify;",
                                           tags$h3("Project Information"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h4("Objective"),
                                           tags$h5("To develop an R package implementing linear regression"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h4("Class"),
                                           tags$h5("STAT 6210 - R Programming for Data Science"),
                                           tags$h5("Prof. Dr. Roberto Molinari"),
                                           tags$h5("Auburn University - Fall 2020"),
                                           tags$p(),
                                           tags$br(),
                                           tags$h4("Group"),
                                           tags$h5("Ana Gabriela Itokazu"),
                                           tags$h5("Eyoel Berhane"),
                                           tags$h5("John Musah"),
                                           
                              ), # sidebarPanel
                              
                              mainPanel(style="text-align: justify;",
                                        
                                        h1("The Assignment"),
                                        em("This package was built as part of the requirements for the 'R Programming for Data Science' course, by Prof. Dr. Roberto Molinari. The assignment was lined up as follows:"),
                                        br(),
                                        br(),
                                        p("The final project will be evaluated on 100 points and the goal is to develop an R package implementing linear regression as highlighted in Section 6.4 of the book."),
                                        p("The package must contain the basic functions to perform linear regression (e.g. estimate the coefficient vector \\(\\beta\\)) and obtain different statistics from the procedure. Using the notation from the book and without using any of the linear regression functions already available in R (i.e. all outputs must be produced using formulas provided in the book and in this document), the basic outputs from the procedure must be the following:"),
                                        tags$ul(
                                             tags$li("Confidence intervals: the user must be able to choose the significance level \\(\\alpha\\) to obtain for the \\(1−\\alpha\\) confidence intervals for \\(\\beta\\) and whether to use the asymptotic or bootstrap approach for this."),
                                             tags$li("Plots (with e.g. ggplot2) including:",
                                                     tags$ol(
                                                          tags$li("Residuals vs fitted-value (fitted values are \\(\\hat{y}= X \\cdot \\hat{\\beta}\\))."),
                                                          tags$li("qq-plot of residuals."),
                                                          tags$li("Histogram (or density) of residuals."),
                                                     ),
                                             ),
                                             tags$li("Mean Square Prediction Error (MSPE) computed in matrix form: $$MSPE:=1n∑i=1n(y−y)^2$$, where \\(n\\) is the number of observations in the data (i.e. number of rows)."),
                                             tags$li("F-test: compute the statistic in matrix form and output the corresponding p-value. With y¯ representing the sample mean of y, let SSM:=∑i=1n(y^i−y¯), SSE:=∑i=1n(yi−y^i), and DFM=p−1 and DFE=n−p. Then we can define MSM=SSM/DFM and MSE=SSE/DFE and obtain the F-statistic as follows: F^∗=MSMMSE.Using the appropriate distribution in R, compute P(F>F∗) which corresponds to the p-value."),
                                             tags$li("Help documentation for all functions (for example using the roxygen2 package)"),
                                        ),
                                        p("The package will be made available for download on a GitHub repository in the AU-R-Programming organization and the submission will be an html file on Canvas. The html file wil be a so-called vignette which indicates the name of the GitHub repository (and package) where you explain and give examples of how to use the package functions for all the desired outputs using one of the datasets on the Canvas course page."),
                                        hr(),
                                        em("The package will be made available for download on a GitHub repository in the AU-R-Programming organization and the submission will be an html file on Canvas The html file wil be a so-called vignette which indicates the name of the GitHub repository (and package) where you explain and give examples of how to use the package functions for all the desired outputs using one of the datasets on the Canvas course page."),
                                        hr(),
                                        br(),
                                        
                              ) # mainPanel
                              
                     ), # tabPanel, The Assignment
                     
                     tabPanel("The Package",
                              "Page under construction...."
 #                             sidebarPanel(
 #                                  tags$h3("Input:"),
 #                                  textInput("txt1", "First Name:", ""),
 #                                  textInput("txt2", "Last Name:", ""),
 #                                  
 #                             ), # sidebarPanel
 #                             mainPanel(
 #                                  h1("Header 1"),
 #                                  
 #                                  h4("Output"),
 #                                  verbatimTextOutput("txtout"),
 #                             ) # mainPanel
 #                             
                     ), # tabPanel, The Package
 
                    tabPanel("The Theory Behind It",
                             "Page under construction...."
                              
                     ), # tabPanel, The Theory
 
                    tabPanel("Some Examples",
                             "Page under construction...."
                              
                    ), # tabPanel, Examples
 
                    tabPanel("Try It Yourself!",
                             "Page under construction...."
                            
                    ) # tabPanel, Try It Yourself
                     
                ) # navbarPage
) #fluidPage 


# Define server function

server <- function(input, output) {
     
     output$txtout <- renderText({
          paste(input$txt1, input$txt2, sep = " " )
     })
} # server


# Create a Shiny Object

shinyApp(ui = ui, server = server)