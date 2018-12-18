library(vcd)
source("packages.RData")
shinyUI
(
  
  dashboardPage
  ( skin = "black",
    dashboardHeader(title = "Shiny Project - Principle of Data Science",titleWidth = 500),
    
    #*************************************************************
    dashboardSidebar
    (
      sidebarMenu
      (id= "sidebarmenu",
        menuItem(text = "About",tabName = "about",icon = icon("clipboard")),
        menuItem(text = "Data",
                 icon = icon("database"),
                 menuSubItem(text = "Dataset",tabName = "DS",icon = icon("database")),
                 menuSubItem(text = "Data Structure",tabName = "datastr",icon = icon("sitemap")),
                 menuSubItem(text = "Data Summary",tabName = "summary",icon = icon("filter"))),
        menuItem(text = "UK residents visiting overseas",
                 icon = icon("plane"),
                 menuSubItem(text = "Plots", tabName = "UKtoOverseasPlots",icon = icon("line-chart"))),
        menuItem(text = "Overseas residents visiting UK",
                 icon = icon("bus"),
                 menuSubItem(text = "Plots", tabName = "OverseastoUK", icon = icon("line-chart"))),
        menuItem(text = "Simple recommendation system",tabName = "recommendation", icon = icon("desktop")),
        menuItem(text = "Link to code files",href = "https://www.google.com",icon = icon("code"))
        
        
        
      )
      
    ),
    #*************************************************************
    dashboardBody
    (
      tabItems
      (
        tabItem(tabName = "about",
                
                fluidRow(box(title =h1(textOutput("desc"),align="center"),solidHeader = TRUE,width = 12,background = "black" ),
                         br(),
                         infoBoxOutput(outputId = "cat_",width = 100),
                         infoBoxOutput(outputId = "con_", width = 100))
                ),
        #***************************
        tabItem(tabName= "DS",
                fluidRow(box(tableOutput("mydata"),width = 12,height = 980,background = "black"))
                ),
        #***************************
        tabItem(tabName = "datastr",
                fluidRow(box(verbatimTextOutput("structure"),width = 12,background = "black"))
                ),
        #***************************
        tabItem(tabName = "summary",
                fluidRow(box(verbatimTextOutput("summary"),width = 12,background = "black"))
                ),
        #***************************
        tabItem(tabName = "UKtoOverseasPlots",
                fluidRow(box(uiOutput("UK_select_plot_type"),width = 3,height = 135,background = "black"),
                         box(uiOutput("UK_selected_plot_type"),width = 3,height = 135,background = "black" ),
                         box(uiOutput("UK_selected_input_var_box"),width = 6,height = 135,background = "black")),
                fluidRow(box(uiOutput("UK_PLOTS"),background="black",width = 12))
                

                ),
        #***************************
        tabItem(tabName ="OverseastoUK" ,
                fluidRow(box(uiOutput("OS_select_plot_type"),width = 3,height = 135,background = "black"),
                         box(uiOutput("OS_selected_plot_type"),width = 3,height = 135,background = "black" ),
                         box(uiOutput("OS_selected_input_var_box"),width = 6,height = 135,background = "black")),
                fluidRow(box(uiOutput("OS_PLOTS"),background="black",width = 12))
                ),
        #***************************
        tabItem(tabName = "recommendation",
                fluidRow(uiOutput("recom_sys_input")),
                fluidRow(uiOutput("recom_sys_output"))
                )
      )
      
    )
    
  )
  
  
)
