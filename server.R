
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# libraries
library(plyr)
library(shiny)
library(ggplot2)

# project income - get data / pre-process
proj_income <- read.csv('data/project_inc_exp_2014.csv', header=TRUE,sep=',')
graphx_labels <- c('June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May')
first_month <- 3
last_month = first_month + 11
plot_data <- proj_income[c(first_month:last_month),]
yaxis_lim <- min(plot_data$Profit)
#### just change to use barplot ####
plot_data <- matrix(NA,2,12)
plot_data[1,1:12] <- proj_income$Income[1:12]
plot_data[2,1:12] <- proj_income$Expenses[1:12]

# cafe income - get data / pre-process
curr_yr <- 14
cafe_income <- read.csv('data/income_2013-14.csv', header=TRUE,sep=';')

# subset - yr_14 / yr_13
yr_14 <- subset(cafe_income, Year == 14) 
yr_13 <- subset(cafe_income, Year == 13) 
inc <- yr_14

weeks_14  <- aggregate(Total~Week,data=yr_14,FUN=sum, na.rm=FALSE, na.action=NULL)
months_14 <- aggregate(Total~Month,data=yr_14,FUN=sum, na.rm=FALSE, na.action=NULL)
weeks_13  <- aggregate(Total~Week,data=yr_13,FUN=sum)
months_13 <- aggregate(Total~Month,data=yr_13,FUN=sum)

# forecast data 
# cafe sales - assumes takings of 43,330 for the year - 833.27 per week.
curr_month <- 3
cfs_forecast <- c(1:52)
cfs_forecast[1:52] <- 833.27
df.cfs_forecast <- data.frame(Week=integer(0),Total=integer(0))
for (i in 1:52) {
  df.cfs_forecast <- rbind(df.cfs_forecast,data.frame(Week=i,Total=cfs_forecast[i]))
}

# project income / expenses - current data will always be at the 'end'
pinc_forecast  <- c(1:12)
pexp_forecast  <- c(1:12)
pprof_forecast <- c(1:12)
start <- 12 - curr_month
pinc_forecast[1:start] <- NA
start <- (12 - curr_month) + 1 
pinc_forecast[start:12] <- 6911.83 
start <- 12 - curr_month
pexp_forecast[1:start] <- NA
start <- (12 - curr_month) + 1 
pexp_forecast[start:12] <- 6776.33 

# create 'monthly (weekly/daily) averages and max/min' dataframe
df.summary <- data.frame(total=integer(0),weekly_avg=integer(0),daily_avg=integer(0),max=integer(0),min=integer(0))
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
  df.summary <- rbind(df.summary,x)
} 

# create monthly average vector - assumes '4/4/5' x 4
wk_avg <- c(1:52)
wk_avg[1:4]   <- df.summary$weekly_avg[1]
wk_avg[5:8]   <- df.summary$weekly_avg[2]
wk_avg[9:13]  <- df.summary$weekly_avg[3]
wk_avg[14:17] <- df.summary$weekly_avg[4]
wk_avg[18:21] <- df.summary$weekly_avg[5]
wk_avg[22:26] <- df.summary$weekly_avg[6]
wk_avg[27:30] <- df.summary$weekly_avg[7]
wk_avg[31:34] <- df.summary$weekly_avg[8]
wk_avg[35:39] <- df.summary$weekly_avg[9]
wk_avg[40:43] <- df.summary$weekly_avg[10]
wk_avg[44:47] <- df.summary$weekly_avg[11]
wk_avg[48:52] <- df.summary$weekly_avg[12]

# shiny stuff ...
shinyServer(function(input, output) {
  
  output$plot3 <- renderPlot({ 
    
    # base plot
    data <- weeks_14
    lineType <- 'b' 
    xLabel <- 'Week Number'
    yRange <- c(0,1500)
    g <- ggplot(data,aes(x=Week,y=Total))
    g <- g + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,1500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    
    # current year trend
    if ( input$trend_2014 == TRUE ) {
      g <- g + geom_smooth(data=weeks_14, aes(x=Week,y=Total),method='lm',na.rm=TRUE)
    }
    # current year forecast
    if ( input$forecast_2014 == TRUE ) {
      g <- g + geom_line(data=df.cfs_forecast, aes(x=Week,y=Total),color='green',size=2,alpha=0.4)
    }
    
    # compare to last year
    if ( input$compare_2013 == TRUE ) {
      g <- g + geom_line(data = weeks_13,aes(x=Week,y=Total),alpha = 0.3) + geom_point(data=weeks_13,aes(x=Week,y=Total),size=4,alpha=0.2)
    }
    
    # last years trend
    if ( input$trend_2013 == TRUE ) {
      g <- g + geom_smooth(data=weeks_13, aes(x=Week,y=Total),method='lm')
    }
    
    # display plot
    print(g)
    
  })
  
})