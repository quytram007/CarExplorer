
library(shiny)
library(shinydashboard)
library(plotly)
data <- read.csv("C:\\Users\\ThanhQuy\\Desktop\\ESP\\cars.csv", TRUE ,",")
title <- tags$a(href='http://127.0.0.1:6903/',
                icon("car"),
                'Car Explorer', target="_blank")


shinyUI(
  dashboardPage(
    dashboardHeader(title = title, titleWidth = 600,
                    tags$li(class="dropdown",tags$a(href="https://www.facebook.com/Torrard98", icon("user"), "Teammate", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://www.facebook.com/profile.php?id=100014806776023" ,icon("linkedin"), "My Profile", target="_blank")),
                    tags$li(class="dropdown",tags$a(href="https://data.world/jackchang/carsdata/workspace/file?filename=cars.csv", icon("database"), "Database", target="_blank"))
                    
                    
    ),
    
    
    dashboardSidebar(
      # add menu items to the sidebar
      # menu items are like tabs when clicked open up a page in tab item
      sidebarMenu(
        menuItem(text = "About", tabName = "about", icon=icon("clipboard")),
        menuItem("Data", tabName = "data", icon=icon("database")),
        menuItem("InfoBox",tabName = "info",icon = icon("info")),
        menuItem("Plot", tabName = "box", icon=icon("line-chart")),
        menuItem("Profit",tabName = "loinhuan",icon = icon("hand-holding-usd",lib = "font-awesome")),
        menuItem("Logistic Regression",tabName = "mohinh",icon = icon("laptop-code",lib = "font-awesome")),
        menuItem("Linear regression",tabName = "mohinh2",icon = icon("keyboard")),
        menuItem("Link to code files",  href = "https://www.google.com", icon=icon("code"))
        
        
      )
    ),
    
    
    
    dashboardBody(tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
        tabItems(
                tabItem(tabName = "about",textOutput("about"),textOutput("about1"),
                        textOutput("about2"),textOutput("about3"),
                        textOutput("about4"),textOutput("about5"),
                        textOutput("about6"),textOutput("about7"),
                        textOutput("about8"),textOutput("about9"),
                        textOutput("about10"),textOutput("about11"),
                        textOutput("about12"),textOutput("about13"),
                        textOutput("about14"),textOutput("about15")),
                tabItem( tabName = "data",dataTableOutput("data")),
            
                tabItem(tabName = "info",
                         fluidRow(infoBoxOutput("min_", width = 3),infoBoxOutput("max_", width = 3), infoBoxOutput("minnl", width = 3))
                  
                 ),
                tabItem(tabName = "box",
                      
                        
                                  selectInput(inputId = "selectPF",
                                      label = "Lựa Chọn",
                                      choices = list( Price ="P",
                                                      Fuel="F"),
                                      width = 830),
                        submitButton(text = "Biểu đồ" ,icon = icon("line-chart")),
                        fluidRow(
                                  box(title = "Biểu đồ giữa các dòng xe", plotlyOutput("plot1", height = 250),status = "success", solidHeader = T, collapsible = T),
                                  box(title = "Biểu đồ Equiment", plotlyOutput("plot2", height = 250),status = "danger", solidHeader = T, collapsible = T)
                                 
                        )
                ),
                tabItem(tabName = "loinhuan",
                       
                        selectInput(inputId = "selectcar",
                                              label = "Xe muốn kinh doanh:",
                                              choices = list(Name = data$NAME),
                                              width = 800),
                        numericInput(inputId = "sl",
                                             label = "Số lượng xe: ",
                                             value = 0,
                                              min = 5,
                                              max = 100,
                                             step = 5,
                                              width = 800),
                        submitButton(text = "Tính toán" ,icon = icon("calculator")),
                        textOutput(outputId="cach"),
                        fluidRow(
                            infoBoxOutput("ln", width = 6)
                        ),
                        
                          selectInput(inputId = "selectclass",
                                      label = "Dòng xe",
                                      choices = list(Class=dataprofit$Class)
                                      ),
                          submitButton(text = "Biểu Đồ" ,icon = icon("line-chart")),
                          textOutput(outputId="cach2"),
                          fluidRow(box(title = "Biểu đồ lợi nhuận", plotlyOutput("plotln", height = 250),status = "success", solidHeader = T, collapsible = T),
                                   box(title = "Bảng Lợi Nhuận", dataTableOutput("dataprofit"), width = 4,solidHeader = T, status="success"))
                          
                ),
                tabItem(tabName = "mohinh",
                        fluidRow(
                        box(title = "Đường ROC Hồi Quy Logistic", plotOutput("logistic", height = 550),status = "success", solidHeader = T, collapsible = T),
                        infoBoxOutput("dochxac",width = 5),
                        infoBoxOutput("chisoauv",width = 5))),
                tabItem(tabName = "mohinh2",
                        fluidRow(
                          box(title = "Biều Đồ Phân Tán Giữa Các Biến Quan Trọng", plotOutput("tuyentinh", height = 900),width = 8,status = "success", solidHeader = T, collapsible = T),
                          infoBoxOutput("R2Xaydung",width = 4),
                          infoBoxOutput("R2Kiemdinh",width = 4)))
                )
    )
  )
)







