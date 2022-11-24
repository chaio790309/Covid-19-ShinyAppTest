library(shiny)
library(dplyr)
library(ggplot2)


r<-read.csv(file="https://od.cdc.gov.tw/eic/covid19/covid19_tw_specimen.csv")
r<-r[,c(1,5)]
names(r)[1]<-list("Date")
#日期格式為文字(x軸順序會亂掉)，轉為日期
r$Date<-as.Date(r$Date)
#三個新欄位給均線用
r$weekavg<-array(0,nrow(r))
r$monthavg<-array(0,nrow(r))
r$quarteravg<-array(0,nrow(r))
#計算各平均值
for(i in 7:nrow(r)){
  r$weekavg[i]<-mean(r$Total[(i-7+1):i])
}
for(i in 30:nrow(r)){
  r$monthavg[i]<-mean(r$Total[(i-30+1):i])
}
for(i in 90:nrow(r)){
  r$quarteravg[i]<-mean(r$Total[(i-90+1):i])
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange","Select date range","2022-05-01",Sys.Date()-1,min="2020-01-15",max=Sys.Date()-1),
      radioButtons("cycle","Select Moving Average days",list("weekavg","monthavg","quarteravg"),"weekavg")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {

  datasubset <- reactive(
    r %>% filter(Date >= input$daterange[1] & Date <= input$daterange[2])
    )
  output$plot<-renderPlot(
    ggplot(datasubset())+
      geom_line(aes(Date,Total,group=1,color="Total"))+
      geom_line(aes_string("Date",input$cycle,group=1),color="purple")+
      scale_colour_manual(name="每日與均線",values = c("Total"="blue",Average="purple"))
    
  )
}

shinyApp(ui, server)


