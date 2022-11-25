library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)


data<-read.csv(file="https://covid-19.nchc.org.tw/api/csv?CK=covid-19@nchc.org.tw&querydata=4051&limited=TWN",header=TRUE,sep=",",fileEncoding = "big5")
data<-data[,c(5:7)]
names(data)<-list("Date","Total","Num")
#日期格式為文字(x軸順序會亂掉)，轉為日期
data$Date<-as.Date(data$Date)
#三個新欄位給均線用
data$weekavg<-array(0,nrow(data))
data$monthavg<-array(0,nrow(data))
data$quarteravg<-array(0,nrow(data))
#計算各平均值
for(i in 7:nrow(data)){
  data$weekavg[i]<-mean(data$Num[(i-7+1):i])
}
for(i in 30:nrow(data)){
  data$monthavg[i]<-mean(data$Num[(i-30+1):i])
}
for(i in 90:nrow(data)){
  data$quarteravg[i]<-mean(data$Num[(i-90+1):i])
}

ui <- fluidPage(
  titlePanel("Practive Shimy"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange","Select date range","2022-09-01",Sys.Date()-1,min="2022-01-01",max=Sys.Date()-1),
      radioButtons("cycle","Select Moving Average days",list("weekavg","monthavg","quarteravg"),"weekavg")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Days",
          plotOutput("plot",brush = "plot_brush"),
          dataTableOutput("table"),
          verbatimTextOutput("test")
          ),
        tabPanel(
          "Total",
          
        )
      )

    )
  )
)

server <- function(input, output, session) {
  

  datasubset <- reactive(
    data %>% filter(Date >= input$daterange[1] & Date <= input$daterange[2])
    )
  output$plot<-renderPlot(
    ggplot(datasubset())+
      geom_bar(aes(Date,Num), stat = "identity",fill="#00BBFF")+
      geom_line(aes_string("Date",input$cycle,group=1),color="purple")
      #scale_colour_manual(name="每日與均線",values = c(Average="purple"))
  )

  output$test<-renderText(
    if(max(input$daterange[1],input$plot_brush[[1]])==input$daterange[1]){
      input$daterange[1]
    }else{
      input$plot_brush[[1]]
    }
  )

  output$table<-renderDataTable(
    brushedPoints(data,input$plot_brush)
    #data %>% filter(Date >= input$plot_brush$xmin & Date <= input$plot_brush$xmax)

  )

  
}

shinyApp(ui, server)


