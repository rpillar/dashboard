
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

shinyUI(navbarPage(   
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #34495e;
      }

    "))
  ),             
  
  'dashBOARD', inverse=TRUE, 
  
  tabPanel('CAFE', 
    
    fluidRow(
      column(2),
      column(3,
         h1('Cafe Sales', style='font-family:Lobster;font-size:54px;padding-bottom:5px;margin-bottom:30px;font-weight:500;')
      ),
      column(7)
    ),
  
    fluidRow(
      
      column(2),
      
      column(2, 
        class = 'well', 
        style = 'background-color:#F0FFFF;min-height:350px;border:1px solid gray;',
        h2(
          span(
            'Weekly Takings', 
            style = 'border-bottom:1px solid gray;color:#34495e;'
          ),
          style='font-weight:400;'), 
        br(),
        h4('View :-', style='font-weight:400;color: #34495e;'),
        checkboxInput('trend_2014', label = 'Current Year Trend', value = FALSE), 
        checkboxInput('forecast_2014', label = 'Forecast (per week)', value = FALSE), 
        h4('Compare to last year',style='font-weight:400;color: #34495e;'),
        checkboxInput('compare_2013', label = 'Sales - 2013', value = TRUE),
        checkboxInput('trend_2013', label = 'Trend - 2013', value = FALSE)  
      ),  
    
      column(6,
        plotOutput('plot1')
      ),
    
      column(2)
    ),
    
    fluidRow(
      
      column(2),
      column(8,
       h2('Customer Numbers', style='font-family:Lobster;font-size:38px;padding-bottom:5px;margin-bottom:30px;font-weight:500;color:#34495e;')   
      ),
      column(2)
    ),
    
    fluidRow(
      
      column(2),
      column(2),
      column(6,
       plotOutput('plot2')   
      ),
      column(2)
    )
    
  )
  
))
