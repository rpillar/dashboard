
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# libraries
library(plyr)
library(shiny)
library(ggplot2)

## set 'control' data ##
#-----------------------
graphx_labels <- c('Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar')

## cafe expenses - get data / pre-process ##
#-------------------------------------------
cafe_expenses <- read.csv('data/cafe_expenses.csv', header=TRUE,sep=',')

# subset - yr_14 / yr_13
exp_yr_14 <- subset(cafe_expenses, Year == 14) 
exp_yr_13 <- subset(cafe_expenses, Year == 13) 

# create 'Monthly' figures
exp_yr_13$Month <- factor(exp_yr_13$Month, levels=c('Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar'))
exp_yr_14$Month <- factor(exp_yr_14$Month, levels=c('Apr','May','June','July','Aug','Sept','Oct','Nov','Dec','Jan','Feb','Mar'))
exp_13_mths <- data.frame(Month=character(0),Total=integer(0))
exp_14_mths <- data.frame(Month=character(0),Total=integer(0))
for (i in 1:12) {
  exp_13_mths <- rbind(exp_13_mths,data.frame(Month = exp_yr_13[i,2],Total=rowSums( exp_yr_13[i,3:21] ) ) )
}
for (i in 1:12) {
  exp_14_mths <- rbind(exp_14_mths,data.frame(Month = exp_yr_14[i,2],Total=rowSums( exp_yr_14[i,3:21] ) ) )
}

# calculate expenses details
exp_14_total <- sum(exp_14_mths$Total,na.rm=TRUE)

payr_exp_tot_14  <- sum(exp_yr_14$Payroll,na.rm=TRUE)
payr_exp_tot_14  <- round(payr_exp_tot_14,digits=2)
payr_exp_avg_14  <- mean(exp_yr_14$Payroll,na.rm=TRUE)
payr_exp_avg_14  <- round(payr_exp_avg_14,digits=2)
payr_exp_perc_14 <- (payr_exp_tot_14 / exp_14_total) * 100
payr_exp_perc_14<- round(payr_exp_perc_14,digits=2)

food_exp_tot_14  <- sum(exp_yr_14$Food,na.rm=TRUE)
food_exp_tot_14  <- round(food_exp_tot_14,digits=2)
food_exp_avg_14  <- mean(exp_yr_14$Food,na.rm=TRUE)
food_exp_avg_14  <- round(food_exp_avg_14,digits=2)
food_exp_perc_14 <- (food_exp_tot_14 / exp_14_total) * 100
food_exp_perc_14 <- round(food_exp_perc_14,digits=2)

util_exp_tot_14 <- sum(exp_yr_14$Utilities,na.rm=TRUE)
util_exp_tot_14 <- round(util_exp_tot_14,digits=2)
util_exp_avg_14 <- mean(exp_yr_14$Utilities,na.rm=TRUE)
util_exp_avg_14 <- round(util_exp_avg_14,digits=2)
util_exp_perc_14 <- (util_exp_tot_14 / exp_14_total) * 100
util_exp_perc_14 <- round(util_exp_perc_14,digits=2)

# cafe income - get data / pre-process
#-------------------------------------
# need some way of working these out ...
curr_yr <- 14
curr_wk <- 21

cafe_income <- read.csv('data/income_2013-14.csv', header=TRUE,sep=',')

# subset - yr_14 / yr_13
yr_14 <- subset(cafe_income, Year == 14) 
yr_13 <- subset(cafe_income, Year == 13) 
yr_13$Day <- factor(yr_13$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
yr_14$Day <- factor(yr_14$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))

# create 'Week' / 'Monthly' figures
weeks_14     <- aggregate(Total~Week,data=yr_14,FUN=sum, na.rm=FALSE, na.action=NULL)
weeks_14_cn  <- aggregate(CustNumbers~Week,data=yr_14,FUN=sum, na.rm=FALSE, na.action=NULL)
months_14    <- aggregate(Total~Month,data=yr_14,FUN=sum, na.rm=FALSE, na.action=NULL)
weeks_13     <- aggregate(Total~Week,data=yr_13,FUN=sum)
weeks_13_cn  <- aggregate(CustNumbers~Week,data=yr_13,FUN=sum, na.rm=FALSE, na.action=NULL)
months_13    <- aggregate(Total~Month,data=yr_13,FUN=sum)

# forecast data 
# -- not used currently --

# income - create 'monthly (weekly/daily) averages and max/min' dataframe (currently only for '14')
#df.inc_14.summary <- data.frame(total=integer(0),weekly_avg=integer(0),daily_avg=integer(0),max=integer(0),min=integer(0))
#x <- c()
#for( i in 1:12) {
#  stuff <- subset(yr_14, Month == i) 
#  y <- ddply(stuff,.(Month,Week,Day),summarize,Total=sum(Total))
#  z <- ddply(stuff,.(Month,Week),summarize,Total=sum(Total))
#  a <- sum(y$Total)
#  b <- mean(y$Total)
#  b <- round(b,digits=2)
#  c <- max(y$Total,na.rm=FALSE)
#  d <- min(y$Total,na.rm=FALSE)
#  e <- mean(z$Total)
#  e <- round(e,digits=2)
#  x <- data.frame(total=a,weekly_avg=e,daily_avg=b,max=c,min=d)
#  df.inc_14.summary <- rbind(df.inc_14.summary,x)
#} 

# income - create monthly average vector - assumes '4/4/5' x 4
#wk_avg <- c(1:52)
#wk_avg[1:4]   <- df.inc_14.summary$weekly_avg[1]
#wk_avg[5:8]   <- df.inc_14.summary$weekly_avg[2]
#wk_avg[9:13]  <- df.inc_14.summary$weekly_avg[3]
#wk_avg[14:17] <- df.inc_14.summary$weekly_avg[4]
#wk_avg[18:21] <- df.inc_14.summary$weekly_avg[5]
#wk_avg[22:26] <- df.inc_14.summary$weekly_avg[6]
#wk_avg[27:30] <- df.inc_14.summary$weekly_avg[7]
#wk_avg[31:34] <- df.inc_14.summary$weekly_avg[8]
#wk_avg[35:39] <- df.inc_14.summary$weekly_avg[9]
#wk_avg[40:43] <- df.inc_14.summary$weekly_avg[10]
#wk_avg[44:47] <- df.inc_14.summary$weekly_avg[11]
#wk_avg[48:52] <- df.inc_14.summary$weekly_avg[12]

# calculate takings details
cafe_inc_avg_13 <- mean(weeks_13$Total,na.rm=TRUE)
cafe_inc_avg_13 <- round(cafe_inc_avg_13,digits=2)
cafe_inc_avg_14 <- mean(weeks_14$Total,na.rm=TRUE)
cafe_inc_avg_14 <- round(cafe_inc_avg_14,digits=2)
cafe_inc_max_13 <- max(weeks_13$Total,na.rm=TRUE)
cafe_inc_max_14 <- max(weeks_14$Total,na.rm=TRUE)
cafe_min <- subset(weeks_13,Total > 0)
cafe_inc_min_13 <- min(cafe_min$Total,na.rm=FALSE)
cafe_inc_min_13 <- round(cafe_inc_min_13,digits=2)
cafe_min <- subset(weeks_14,Total > 0)
cafe_inc_min_14 <- min(cafe_min$Total,na.rm=FALSE)
cafe_inc_min_14 <- round(cafe_inc_min_14,digits=2)

# create latest week df (and weeks -1, -2 and -3 )
df.wk_inc <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk & Year == curr_yr),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc$Day <- factor(df.wk_inc$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_1 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-1 & Year == curr_yr),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_1$Day <- factor(df.wk_inc_1$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_2 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-2 & Year == curr_yr),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_2$Day <- factor(df.wk_inc_2$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_3 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-3 & Year == curr_yr),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_3$Day <- factor(df.wk_inc_3$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))

df.wk_inc_13 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk & Year == curr_yr-1),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_13$Day <- factor(df.wk_inc_13$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_13_1 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-1 & Year == curr_yr-1),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_13_1$Day <- factor(df.wk_inc_13_1$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_13_2 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-2 & Year == curr_yr-1),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_13_2$Day <- factor(df.wk_inc_13_2$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))
df.wk_inc_13_3 <- aggregate(Total~Day,data=subset(cafe_income,Week == curr_wk-3 & Year == curr_yr-1),FUN=sum,na.rm=TRUE,na.action=NULL)
df.wk_inc_13_3$Day <- factor(df.wk_inc_13_3$Day, levels=c('Tues','Wed','Thurs','Fri','Sat'))

# create takings variance data for 2013 / 2014
yr_13_Var <- yr_13
yr_13_Var$Month <- factor(yr_13_Var$Month, labels = graphx_labels)
yr_14_Var <- yr_14
yr_14_Var$Month <- factor(yr_14_Var$Month, labels = graphx_labels)

# shiny stuff ...
shinyServer(function(input, output) {
  
  ## cafe takings ##
  
  output$plot1 <- renderPlot({ 
    
    # base plot
    data <- weeks_14
    lineType <- 'b' 
    xLabel <- 'Week Number'
    yRange <- c(0,1500)
    g <- ggplot(data,aes(x=Week,y=Total))
    g <- g + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,1500) + labs(title='Cafe Takings - 2013 / 2014 (by Week)',x='Week Number',y='Amount') + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    
    # current year trend
    #if ( input$trend_2014 == TRUE ) {
    #  g <- g + geom_smooth(data=weeks_14, aes(x=Week,y=Total),method='lm',na.rm=TRUE)
    #}
    # current year forecast
    #if ( input$forecast_2014 == TRUE ) {
    #  g <- g + geom_line(data=df.cfs_forecast, aes(x=Week,y=Total),color='green',size=2,alpha=0.4)
    #}
    
    # compare to last year
    #if ( input$compare_2013 == TRUE ) {
      g <- g + geom_line(data = weeks_13,aes(x=Week,y=Total),alpha = 0.3) + geom_point(data=weeks_13,aes(x=Week,y=Total),size=4,alpha=0.2)
    #}
    
    # last years trend
    #if ( input$trend_2013 == TRUE ) {
    #  g <- g + geom_smooth(data=weeks_13, aes(x=Week,y=Total),method='lm')
    #}
    
    # display plot
    print(g)
    
  })
  
  # cafe takings - table values
  output$avg_2013 <- renderText({ cafe_inc_avg_13 })
  output$avg_2014 <- renderText({ cafe_inc_avg_14 })
  output$max_2013 <- renderText({ cafe_inc_max_13 })
  output$max_2014 <- renderText({ cafe_inc_max_14 })
  output$min_2013 <- renderText({ cafe_inc_min_13 })
  output$min_2014 <- renderText({ cafe_inc_min_14 })
  
  ## takings variance ##
  output$plot2a <- renderPlot({
    k <- ggplot(yr_13_Var,aes(x=Month,y=Takings))
    k <- k + geom_boxplot(aes(fill=Month)) + ylim(0,600) + labs(title='Cafe Takings - 2013',x='Month',y='Daily Takings') + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    
    # display plot
    print(k)
  })
  
  output$plot2b <- renderPlot({
    l <- ggplot(yr_14_Var,aes(x=Month,y=Takings))
    l <- l + geom_boxplot(aes(fill=Month)) + ylim(0,600) + labs(title='Cafe Takings - 2014',x='Month',y='Daily Takings') + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    
    # display plot
    print(l)
  })
  
  output$plot2c <- renderPlot({
    # base plot
    data <- df.wk_inc
    lineType <- 'b' 
    xLabel <- 'Day'
    yRange <- c(0,300)
    n <- ggplot(data,aes(x=Day,y=Total,group=1))
    n <- n + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'gray'))
    n <- n + geom_line(data = df.wk_inc_13,aes(x=Day,y=Total),alpha = 0.3) + geom_point(data=df.wk_inc_13,aes(x=Day,y=Total),size=4,alpha=0.2)
    n <- n + labs(title=paste('Daily Takings - Week ',curr_wk),x='Day',y='Amount')
    
    # display plot
    print(n)
  })
  
  output$plot2d <- renderPlot({
    # base plot
    data <- df.wk_inc_1
    lineType <- 'b' 
    xLabel <- 'Day'
    yRange <- c(0,300)
    o <- ggplot(data,aes(x=Day,y=Total,group=1))
    o <- o + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'gray'))
    o <- o + geom_line(data = df.wk_inc_13_1,aes(x=Day,y=Total),alpha = 0.3) + geom_point(data=df.wk_inc_13_1,aes(x=Day,y=Total),size=4,alpha=0.2)
    o <- o + labs(title=paste('Daily Takings - Week ',curr_wk - 1),x='Day',y='Amount')
    
    # display plot
    print(o)
  })
  
  output$plot2e <- renderPlot({
    # base plot
    data <- df.wk_inc_2
    lineType <- 'b' 
    xLabel <- 'Day'
    yRange <- c(0,300)
    p <- ggplot(data,aes(x=Day,y=Total,group=1))
    p <- p + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'gray'))
    p <- p + geom_line(data = df.wk_inc_13_2,aes(x=Day,y=Total),alpha = 0.3) + geom_point(data=df.wk_inc_13_2,aes(x=Day,y=Total),size=4,alpha=0.2)
    p <- p + labs(title=paste('Daily Takings - Week ',curr_wk - 2),x='Day',y='Amount')
    
    # display plot
    print(p)
  })
  
  output$plot2f <- renderPlot({
    # base plot
    data <- df.wk_inc_3
    lineType <- 'b' 
    xLabel <- 'Day'
    yRange <- c(0,300)
    q <- ggplot(data,aes(x=Day,y=Total,group=1))
    q <- q + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'gray'))
    q <- q + geom_line(data = df.wk_inc_13_3,aes(x=Day,y=Total),alpha = 0.3) + geom_point(data=df.wk_inc_13_3,aes(x=Day,y=Total),size=4,alpha=0.2)
    q <- q + labs(title=paste('Daily Takings - Week ',curr_wk - 3),x='Day',y='Amount')
    
    # display plot
    print(q)
  })
  
  ## expenses ##
  
  output$plot3 <- renderPlot({
    # base plot
    data <- exp_14_mths
    lineType <- 'b' 
    xLabel <- 'Month'
    yRange <- c(0,8000)
    m <- ggplot(data,aes(x=Month,y=Total,group=1))
    m <- m + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,9000) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    m <- m + geom_line(data=exp_13_mths,aes(x=Month,y=Total),alpha = 0.3) + geom_point(data=exp_13_mths,aes(x=Month,y=Total),size=4,alpha=0.2)
    m <- m + labs(title='Cafe Expenses - 2013 / 2014',x='Month',y='Amount')
    
    # display plot
    print(m)
  })
  
  output$payr_tot  <- renderText({ payr_exp_tot_14 })
  output$payr_avg  <- renderText({ payr_exp_avg_14 })
  output$payr_perc <- renderText({ paste(payr_exp_perc_14,'%') })
  
  output$payr_graph <- renderPlot({
    # base plot
    data <- exp_yr_14
    n <- ggplot(data,aes(x=Month,y=Payroll,fill=Month))
    #n <- n + geom_bar(stat='identity',na.rm=TRUE,fill="#9999FF") + ylim(0,2000) + theme_bw() + theme(panel.border = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")
    n <- n + geom_bar(stat='identity',na.rm=TRUE,fill="#9999FF") + theme_bw() + theme(panel.border = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")
    
    # display plot
    print(n)
    
  })
  
  output$food_tot <- renderText({ food_exp_tot_14 })
  output$food_avg <- renderText({ food_exp_avg_14 })
  output$food_perc <- renderText({ paste(food_exp_perc_14,'%') })
  
  output$food_graph <- renderPlot({
    # base plot
    data <- exp_yr_14
    o <- ggplot(data,aes(x=Month,y=Food,fill=Month))
    o <- o + geom_bar(stat='identity',na.rm=TRUE,fill="#9999FF") + ylim(0,2000) + theme_bw() + theme(panel.border = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")
    
    # display plot
    print(o)
    
  })
  
  output$util_tot <- renderText({ util_exp_tot_14 })
  output$util_avg <- renderText({ util_exp_avg_14 })
  output$util_perc <- renderText({ paste(util_exp_perc_14,'%') })
  
  output$util_graph <- renderPlot({
    # base plot
    data <- exp_yr_14
    p <- ggplot(data,aes(x=Month,y=Utilities,fill=Month))
    p <- p + geom_bar(stat='identity',na.rm=TRUE,fill="#9999FF") + ylim(0,1000) + theme_bw() + theme(panel.border = element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none")
    
    # display plot
    print(p)
    
  })
  
  ## customer details ##

  output$plot4a <- renderPlot({
    # base plot
    data <- yr_13
    yRange <- c(0,600)
    j <- ggplot(data,aes(x=Day,y=CustNumbers))
    #j <- j + geom_bar(aes(fill=Day),stat='identity',na.rm=TRUE) + scale_fill_brewer(palette='Set2') + labs(title='Customers (by Day) - 2014',x='Day',y='Customers') + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))    
    j <- j + geom_bar(aes(fill=Day),stat='identity',na.rm=TRUE) + scale_fill_brewer(palette='BuGn') + labs(title='Customers (by Day) - 2014',x='Day',y='Customers') + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))    
    
    # display plot
    print(j)
    
  })
  
  output$plot4b <- renderPlot({
    # base plot
    data <- weeks_14_cn
    lineType <- 'b' 
    xLabel <- 'Week Number'
    yRange <- c(0,600)
    h <- ggplot(data,aes(x=Week,y=CustNumbers))
    h <- h + geom_line(na.rm=TRUE,color='blue') + geom_point(size=4,color='blue',alpha=0.3,na.rm=TRUE) + ylim(0,500) + theme_bw() + theme(panel.border = element_blank()) + theme(axis.line = element_line(color = 'black'))
    h <- h + geom_line(data=weeks_13_cn,aes(x=Week,y=CustNumbers),alpha = 0.3) + geom_point(data=weeks_13_cn,aes(x=Week,y=CustNumbers),size=4,alpha=0.2)
    h <- h + labs(title='Customers Numbers (by Week) - 2013 / 2014',x='Week Number',y='Customer Numbers')
    
    # display plot
    print(h)
  })
  
})