library(shiny)
library(shinydashboard)
library(plotly)
library(ROCR)

data <- read.csv("C:\\Users\\ThanhQuy\\Desktop\\ESP\\cars.csv", TRUE ,",")
names(data)
str(data)
dim(data)
#kiểm tra giá trị null
anyNA(data)
datacs <- read.csv("C:\\Users\\ThanhQuy\\Desktop\\ESP\\cars.csv", TRUE ,",")
#Tạo ra một biến Equiment trong đó nếu xe đó sẽ hiện thị hệ thống cầu dẫn của xe đó 
data$Equiment[data$AWD == 0] <- "NO"
data$Equiment[data$AWD == 1] <- "AWD"
data$Equiment[data$RWD == 1] <- "RWD"

datacs$Equiment[datacs$AWD == 0] <- 0
datacs$Equiment[datacs$AWD == 1] <- 1
datacs$Equiment[datacs$RWD == 1] <- 2

datacs$PriceLevel[datacs$Retail > mean(datacs$Retail)] <- as.numeric(1)
datacs$PriceLevel[datacs$Retail < mean(datacs$Retail)] <- as.numeric(0)
library(caTools)
set.seed(100)
split =sample.split(datacs$PriceLevel, SplitRatio = 0.65)

mauXayDung = subset(datacs, split == TRUE )
mauKiemDinh = subset(datacs, split == FALSE)

str(datacs)
#xay dung mo hinh
mohinh1 = glm(PriceLevel ~  Equiment+Engine + Cylinders + Horsepower + CityMPG + HighwayMPG + Weight + Wheelbase + Length + Width   ,data = mauXayDung,family = binomial)
summary(mohinh1)

mohinh2 = glm(PriceLevel ~  Equiment+Engine + Cylinders + Horsepower  + HighwayMPG + Weight +  Width   ,data = mauXayDung,family = binomial)
summary(mohinh2)

mohinh = glm(PriceLevel ~  Equiment+Engine + Cylinders + Horsepower  + Weight +  Width   ,data = mauXayDung,family = binomial)
summary(mohinh)

#chuan doan tren bo mau xay dung
duBaoXayDung = predict(mohinh,type = "response",newdata = mauXayDung)
summary(duBaoXayDung)
duBaoKiemDinh = predict(mohinh , type = "response",newdata = mauKiemDinh)
table(mauKiemDinh$PriceLevel , duBaoKiemDinh>0.5)

#Do chinh xac
docx <-(62+54)/nrow(mauKiemDinh)
###############################
ROCRpred = prediction(duBaoKiemDinh,mauKiemDinh$PriceLevel)
ROCRperf = performance(ROCRpred,"tpr","fpr")
chiso <- as.numeric(performance(ROCRpred,"auc")@y.values)

##############################

mohinh3 = lm(Retail ~Equiment+Engine + Cylinders + Horsepower + CityMPG + HighwayMPG + Weight + Wheelbase + Length + Width   ,data = datacs)
summary(mohinh3)
mohinh4 = lm(Retail ~Equiment+Engine + Cylinders + Horsepower  + HighwayMPG + Weight + Wheelbase     ,data = datacs)
summary(mohinh4)
pairs(~ Retail + Equiment+Engine + Cylinders + Horsepower  + HighwayMPG +   Weight + Wheelbase    ,data = datacs)
dubao_xaydung_tt = predict(mohinh4,mauXayDung)
RSS_XayDung = sum((dubao_xaydung_tt - mauXayDung$Retail)^2)
TSS_XayDung = sum((mean(mauXayDung$Retail)-mauXayDung$Retail)^2)
R2_xayDUng = 1-(RSS_XayDung/TSS_XayDung)
dubao_KiemDinh_tt = predict(mohinh4,mauKiemDinh)
RSS_KiemDinh = sum((dubao_KiemDinh_tt - mauKiemDinh$Retail)^2)
TSS_KiemDinh = sum((mean(mauXayDung$Retail)-mauKiemDinh$Retail)^2)
R2_KiemDinh = 1-(RSS_KiemDinh/TSS_KiemDinh)
library("dplyr")

data <- data %>%relocate(Equiment, .after = "NAME")
data <- subset(data, select = -c(AWD,RWD) )
data$Retail <- data$Retail*22.765
data$Dealer <- data$Dealer*22.765

minName <- data$NAME[data$Retail==min(data$Retail)]
maxName <- data$NAME[data$Retail==max(data$Retail)]
minnl <- data$NAME[data$CityMPG==max(data$CityMPG)]

dataprofit <- data
dataprofit$Profit <- (dataprofit$Retail - dataprofit$Dealer) 
dataprofit2 <- dataprofit
dataprofit2 <- subset(dataprofit2, select = -c(Equiment,Engine,Cylinders,Horsepower,CityMPG,HighwayMPG	,Weight,	Wheelbase	,Length	,Width	,Class))


shinyServer(function(input, output, session){
  
  output$data <- renderDataTable(data)
  output$min_ <- renderInfoBox({
    infoBox(title = minName, 
            value = min(data$Retail),
            subtitle = "Đây Là Chiếc Xe Rẻ Nhất",
            fill = TRUE) 
    
  })
  output$max_ <- renderInfoBox({
    infoBox(title = maxName, 
            value = max(data$Retail),
            subtitle = "Đây là chiếc xe đắt nhất", fill = T, color = "yellow") 
    
  })
  output$minnl <- renderInfoBox({
    infoBox(title = minnl, 
            value = max(data$CityMPG),
            subtitle = "Xe Tiết Kiệm Nhiên Liệu Nhất", icon("fire",lib = "glyphicon")) 
    
  })
  output$plot1 <- renderPlotly({
    if (input$selectPF == "P") {chon = data$Retail}
    if (input$selectPF == "F"){chon= data$CityMPG}
    plot_ly(data=data, 
            x=data$Class,
            y=chon,
            type = "scatter",
            marker = list(size = 8,
                          color = 'rgba(255, 182, 193, .9)',
                          line = list(color = 'rgba(152, 0, 0, .8)',
                                      width = 2)))
    
  })
  
  
  ## Plotly Histogram
  output$plot2 <- renderPlotly({
   plot_ly(data,  labels = data$Equiment, type = 'pie')
    
    
  })
  output$cach <- renderText("   ---------------------------------------------------------------------------------------------------------------------------
                            --------------------------------------------------------------")
  output$cach2 <- renderText("   ---------------------------------------------------------------------------------------------------------------------------
                            --------------------------------------------------------------")
  output$ln <- renderInfoBox({
    name <-input$selectcar
    SearchInfo <- subset(data, NAME == name)
    profit <- (SearchInfo$Retail - SearchInfo$Dealer )*input$sl
    infoBox(title = input$select01, 
            value = profit,
            subtitle = "Lợi nhuận thu được ", icon("dollar-sign",lib = "font-awesome"))
  })
  output$plotln <- renderPlotly({
    class <-input$selectclass
    dataprofit <- subset(dataprofit,Class == class)
    
    plot_ly(dataprofit, x = dataprofit$NAME, y = dataprofit$Profit, type = 'scatter', mode = 'lines')
  })
  output$dataprofit <- renderDataTable(dataprofit2)
  output$logistic<-renderPlot(plot(ROCRperf,colorize =TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)))
  output$dochxac <- renderInfoBox({
    infoBox(title = "Độ Chính Xác Của Mô Hình",
            value = ceiling(docx*100),
            subtitle ="%")
  })
  
  output$chisoauv <- renderInfoBox({
    infoBox(title = "Chỉ số AUC",
            value = ceiling(chiso*100),
            subtitle ="%")
  })
  output$tuyentinh<-renderPlot(pairs(~ Retail + Equiment+Engine + Cylinders + Horsepower  + HighwayMPG    ,data = datacs))
  output$R2Xaydung <- renderInfoBox({
    infoBox(title = "R2 của Mô Hình Xây Dựng",
            value = ceiling(R2_xayDUng*100),
            subtitle ="%")
  })
  
  output$R2Kiemdinh <- renderInfoBox({
    infoBox(title = "R2 của Mô Hình Kiểm Định",
            value = ceiling(R2_KiemDinh*100),
            subtitle ="%")
  })
  output$about <-renderText("Cac Bien Trong Database ")
  output$about1 <-renderText("Name : Ten Cua chiec xe")
  output$about2 <-renderText("AWD: (All Wheel Drive)he dan dong 4 banh tu đong ")
  output$about3 <-renderText(" RWD: ( Rear-Wheel Drive) he cau dan he thong")
  output$about4 <-renderText("Retail: Gia ban le")
  output$about5 <-renderText("Dealer: Gia ban si")
  output$about6 <-renderText("Engine: Dong cơ")
  output$about7 <-renderText("Cylinders: Xi lanh")
  output$about8 <-renderText("Horsepower: Ma Luc")
  output$about9 <-renderText("Nhien lieu tieu hao di chuyen trong Thanh pho")
  output$about10 <-renderText("HighwayMPG:Nhien lieu tieu hao di chuyen tren cao toc ")
  output$about11<-renderText("Weight: Khoi luong")
  output$about12<-renderText("Wheelbase: Kich co banh xe")
  output$about13 <-renderText("Length: Chieu dai")
  output$about14 <-renderText("Width: Chieu rong")
  output$about15 <-renderText("Class: Dong xe")
  
                                            
}
)

