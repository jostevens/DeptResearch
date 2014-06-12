library(shiny)
library(ggplot2)
library(dplyr)

d01 <- read.delim("../Query1.txt", header=TRUE, sep="\t") # This data was queried from the access database in this directory

options(digit=2)

s1 <- c("spring", "fall")
s2 <- c(1, 2)
s3 <- data.frame(s1, s2)

d01 <- merge(d01, s3, by.x="Semester", by.y="s1")
d01$SYear <- d01$Year + d01$s2*0.1

remove(s1)
remove(s2)
remove(s3)

#d10 <- d01[which(d01$SYear == 2014.1),]
#d01 <- d01[which(d01$SYear != 2014.1),]

d02 <- d01 %.%
  group_by(Year) %.%
  summarise(total = sum(CurrentEnroll))



shinyServer(
  function(input, output){
    
    output$ET01  <-  renderText({
    paste("The following graphics show the trends of the ", input$checkGroup, " department.")
  })

  output$ET02 <- renderPlot({
    s <- ggplot(d02, aes(x=Year, y=total))
    s + geom_line()
    plot(d02$Year)
  })
})