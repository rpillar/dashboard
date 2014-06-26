
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage('dashBOARD', inverse=TRUE,
  
  tabPanel('Project', 
           
    fluidRow(
      column(2),
      column(6,
        h1(
          span(
            'Project Income / Expenditure - 2014', 
            style = 'border-bottom: 1px solid black;'
          ),
          style = 'padding-bottom:5px;margin-bottom:30px;font-weight:400;')
      ),
      column(5)
    ),
    
    fluidRow(
      
      column(2),
      
      column(2, 
         class = 'well', 
         style = 'background-color:#F0FFFF;min-height:350px;',
         h2(
           span(
             'Project Details', 
             style = 'border-bottom:1px solid gray;'
           ),
          style='font-weight:400;'
         ),
         br(),
         h4('Last 12 months view', 
           style='font-weight:400;'
         ),
         p('(Income / Expenditure / Profit)')
      ),  
      
      column(6,
             plotOutput('plot1')
      ),
      
      column(2)
    ),
    
    fluidRow(
      column(2),
      column(3,
         h1(
           span(
             'Cafe Sales - 2014', 
             style = 'border-bottom: 1px solid black;'
         ),
         style = 'padding-bottom:5px;margin-bottom:30px;font-weight:400;')
      ),
      column(7)
    ),
  
    fluidRow(
      
      column(2),
      
      column(2, 
        class = 'well', 
        style = 'background-color:#F0FFFF;min-height:350px;',
        h2(
          span(
            'Weekly Takings', 
            style = 'border-bottom:1px solid gray;'
          ),
          style='font-weight:400;'), 
        br(),
        h4('View :-', style='font-weight:400;'),
        checkboxInput('trend_2014', label = 'Current Year Trend', value = FALSE), 
        checkboxInput('forecast_2014', label = 'Forecast (per week)', value = FALSE), 
        h4('Compare to last year',style='font-weight:400;'),
        checkboxInput('compare', label = 'Sales - 2013', value = TRUE),
        checkboxInput('trend_2013', label = 'Trend - 2013', value = FALSE)  
      ),  
    
      column(6,
        plotOutput('plot2')
      ),
    
      column(2)
    )
  ))
)
