
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# libraries
library(plyr)
library(shiny)

# project income - get data / pre-process
proj_income <- read.csv('data/project_inc_exp_2014.csv', header=TRUE,sep=',')
graphx_labels <- c('June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May')
first_month <- 3
last_month = first_month + 11
plot_data <- proj_income[c(first_month:last_month),]
yaxis_lim <- min(plot_data$Profit)

# cafe income - get data / pre-process
curr_yr <- 14
cafe_income <- read.csv('data/income_2013-14.csv', header=TRUE,sep=';')

# subset - yr_14 / yr_13
yr_14 <- subset(cafe_income, Year == 14) 
yr_13 <- subset(cafe_income, Year == 13) 
inc <- yr_14

weeks_14  <- aggregate(Total~Week,data=yr_14,FUN=sum, na.rm=TRUE, na.action=NULL)
months_14 <- aggregate(Total~Month,data=yr_14,FUN=sum, na.rm=TRUE, na.action=NULL)
weeks_13  <- aggregate(Total~Week,data=yr_13,FUN=sum)
months_13 <- aggregate(Total~Month,data=yr_13,FUN=sum)

#forecast data - assumes takings of 43,330 for the year - 833.27 per week.
forecast <- c(1:52)
forecast[1:52] <- 833.27

# create 'monthly (weekly/daily) averages and max/min' dataframe
df <- data.frame(total=integer(0),weekly_avg=integer(0),daily_avg=integer(0),max=integer(0),min=integer(0))
x <- c()
for( i in 1:12) {
  stuff <- subset(inc, Month == i) 
  y <- ddply(stuff,.(Month,Week,Day),summarize,Total=sum(Total))
  z <- ddply(stuff,.(Month,Week),summarize,Total=sum(Total))
  a <- sum(y$Total)
  b <- mean(y$Total)
  b <- round(b,digits=2)
  c <- max(y$Total,na.rm=FALSE)
  d <- min(y$Total,na.rm=FALSE)
  e <- mean(z$Total)
  e <- round(e,digits=2)
  x <- data.frame(total=a,weekly_avg=e,daily_avg=b,max=c,min=d)
  df <- rbind(df,x)
} 

# create monthly average vector - assumes '4/4/5' x 4
wk_avg <- c(1:52)
wk_avg[1:4]   <- df$weekly_avg[1]
wk_avg[5:8]   <- df$weekly_avg[2]
wk_avg[9:13]  <- df$weekly_avg[3]
wk_avg[14:17] <- df$weekly_avg[4]
wk_avg[18:21] <- df$weekly_avg[5]
wk_avg[22:26] <- df$weekly_avg[6]
wk_avg[27:30] <- df$weekly_avg[7]
wk_avg[31:34] <- df$weekly_avg[8]
wk_avg[35:39] <- df$weekly_avg[9]
wk_avg[40:43] <- df$weekly_avg[10]
wk_avg[44:47] <- df$weekly_avg[11]
wk_avg[48:52] <- df$weekly_avg[12]

# shiny stuff ...
shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({ 
    data <- plot_data
    lineType <- 'b' 
    xLabel <- 'Month'
    #yRange <- c(0,12000)
    yRange <- c(-2000,12000)
    data$Income[data$Income==0] <- NA
    data$Expenses[data$Expenses==0] <- NA
    plot(data$Income,type=lineType,axes=FALSE,ylim=yRange,col='blue',ylab='Amount',xlab=xLabel,na.action=NULL)
    axis(1,at=c(1:12),labels=graphx_labels)
    axis(2,las=1)
    lines(data$Expenses,type=lineType,xlab='',ylab='',col='black')
    legend("topright"
      ,inset=.09
      ,c(paste("Income"),paste("Expenditure"),paste('Profit'))
      ,fill=c('blue','black','gray')
      ,horiz=FALSE)
    par(new=TRUE)
    barplot(data$Profit,axes=FALSE,ylim=yRange,ylab='',xlab='',col='gray')
    abline(h=0,lty=2,col='red')
  })
  
  output$plot2 <- renderPlot({ 
    data <- weeks_14
    lineType <- 'b' 
    xLabel <- 'Week Number'
    yRange <- c(0,1500)
    data$Total[data$Total==0] <- NA
    plot(data$Total,type=lineType,axes=FALSE,ylim=yRange,col='navy',ylab='Takings',xlab=xLabel,na.action=NULL)
    axis(1,)
    axis(2,las=1)
    legend("topright"
      , inset=.05
      , c(paste("Takings - 2014"),paste("Takings - 2013"),paste('Forecast - 2014'))
      ,fill=c('blue','black','green')
      ,horiz=FALSE)
   
    # need to set limits of data 
    if (input$trend_2014 == TRUE ) {
      lines(lowess(data$Total[1:9]),col='darkgreen',type='b')
    }  
    if (input$trend_2013 == TRUE ) {
      lines(lowess(weeks_13$Total),col='darkgreen',type='b')
    }  
    
    # forecast line
    if (input$forecast_2014 == TRUE) {
      lines(forecast,type='l',col='green')
    }
    # compare to last year
    if ( input$compare ) {
      lines(weeks_13$Total,col='black',type='b')
    }
    
  })
  
})