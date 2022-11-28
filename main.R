library(shiny)
library(dplyr)
library(ggplot2)

data<-read.csv(file="https://covid-19.nchc.org.tw/api/csv?CK=covid-19@nchc.org.tw&querydata=4051&limited=TWN",header=TRUE,sep=",",fileEncoding = "big5")
data<-data[,c(5:7)]
names(data)<-list("Date","Total","Num")
#日期格式為文字(x軸順序會亂掉)，轉為日期
data$Date<-as.Date(data$Date)
#二個新欄位給均線用
data$week_avg<-array(NA,nrow(data))
data$month_avg<-array(NA,nrow(data))
data$month<-format(data$Date,format="%m")
#計算各平均線
for(i in 7:nrow(data)){
  data$week_avg[i]<-mean(data$Num[(i-7+1):i])
}
for(i in 30:nrow(data)){
  data$month_avg[i]<-mean(data$Num[(i-30+1):i])
}

ui <- fluidPage(
  #樣式
  #theme = bslib::bs_theme(),
  titlePanel("Practive Shiny"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date_range",
                  "選取資料範圍:",
                  min = as.Date("2022-01-01","%Y-%m-%d"),
                  max = as.Date(data$Date[nrow(data)],"%Y-%m-%d"),
                  value=c(as.Date("2022-09-01"),data$Date[nrow(data)]),
                  timeFormat="%Y-%m-%d"),
      #選擇均線
      radioButtons("cycle","選擇均線種類",list("7日"="week_avg","30日"="month_avg"),"week_avg")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Days",
          tags$h3(tags$b("　　2022每日新增確診數與均線圖")),
          plotOutput("day_plot",brush = "plot_brush"),
          tags$b("表格會顯示框選範圍日期的數據",style="color:blue"),
          dataTableOutput("day_table"),

          ),
        tabPanel(
          "Total",
          tags$h3(tags$b("　　2022新增與累計確診數組合圖")),
          plotOutput("total_plot",click = "plot_click"),
          tags$b("表格會顯示滑鼠點擊位置周圍天數的數據",style="color:blue"),
          dataTableOutput("total_table"),
        )
      )

    )
  )
)

server <- function(input, output, session) {
  
  #資料篩選範圍
  data_subset <- reactive(
    data %>% filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    )
  #每日與均線圖
  output$day_plot<-renderPlot(
    ggplot(data_subset())+
      geom_bar(aes(Date,Num,fill=month), stat = "identity")+
      geom_line(aes_string("Date",input$cycle),color="blue")+
      scale_x_date(date_labels = "%b", 
                   date_breaks = "1 month")+
      labs(x="日期",y="確診數")+
      theme(panel.background = element_rect(fill="#faebd7")) 
  )
  #合計圖
  output$total_plot<-renderPlot(
    ggplot(data_subset())+
      geom_bar(aes(Date,Num,fill=month), stat = "identity")+
      geom_density(aes(Date,Total/150), stat = "identity",color="blue",fill="blue",alpha=0.1)+
      scale_x_date(date_labels = "%b", 
                   date_breaks = "1 month", )+
      scale_y_continuous(name="每日新增確診數",
                         sec.axis = sec_axis(~.*150/10000, name="累計確診數(單位:萬)"))+
      labs(x="日期")+
      theme(panel.background = element_rect(fill="#faebd7"))
  )
  #表格用最早日期
  min_date<-reactive(
    if(is.null(input$plot_brush[[1]])){
      input$date_range[1]
    }else{
      round(max(input$date_range[1],input$plot_brush[[1]]),0)
    }
  )
  #表格用最後日期
  max_date<-reactive(
    if(is.null(input$plot_brush[[2]])){
      input$date_range[2]
    }else{
      round(min(input$date_range[2],input$plot_brush[[2]]),0)
    }
  )
  #顯示範圍內的資料
  output$day_table<-renderDataTable(
    data2<-data %>% filter(Date >= min_date() & Date <= max_date()) %>% select("日期"=Date,"新增確診數"=Num,"7日平均數"=week_avg,"30日平均數"=month_avg),
    options=list(pageLength=10,searching = FALSE)
  )
  #表格用判定X軸
  click_date<-reactive(
    if(is.null(input$plot_click[[1]])){
      input$date_range[2]
    }else{
      round(input$plot_click[[1]],0)
    }
  )
  #顯示指向日期資料 paging(分頁) searching(搜尋功能)
  output$total_table<-renderDataTable(
    data %>% filter(Date >= click_date()-3 & Date <= click_date()+3 & Date >= min_date()) %>% select("日期"=Date,"新增確診數"=Num,"確診累積數"=Total),
    options=list(pageLength = 1,paging=FALSE,searching = FALSE)
  )
}

shinyApp(ui, server)


