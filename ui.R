###############################################################
#Título: Parámetros Hidrogeológicos (Proyecto de Tesis)       #
#Autor: Báez Reyes Héctor Eduardo                             #  
#Director de Tesis: Dr. António Hernández Espriú              #
#Universidad Nacional Autónoma de México                      #
#Facultad de Ingeniería                                       #
###############################################################

##Paqueterías
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(shinyjs)
library(shinyalert)
library(plotly)


###Carga los datos .csv de la carpeta de trabajo
database<-read.csv("hydraulic_parameters.csv",header=TRUE,sep=",",fileEncoding = "UTF-8")


# Define el UI para la aplicación indicando las páginas de navegación (tabpanel) indicando los paneles laterales (sidebarPanel) y el contenido (mainPanel)
shinyUI <- tagList( 
 
  navbarPage(
    theme = shinytheme("flatly"),
    title=div(
      img(src="unamlogo.jpg", style="position:relative; margin-top: -10px;", height = 50),
      "Hydrogeological Parameters",style="position:relative; margin-top: -7px;"),
    windowTitle="Hydrogeological Parameters",
    id="tabs",
    
    
    tabPanel("Data",
        
             titlePanel("Hydrogelogical Parameters Dataset"),
             fluidRow(column(DT::dataTableOutput("data"),
                             width=12))
             
             ),
    tabPanel("Boxplot",
             
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 shinyalert::useShinyalert(),
                 
                 radioButtons("propertie1",           
                              "Select Hydraulic Propertie",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")),
                              choiceValues=c("Tporosity","Eporosity","K","S","Sy","Compressibility"),
                              selected="K" ),
                 
                 checkboxInput("logy",
                               "Log Scale",
                               value=TRUE
                 ),
                 
                 checkboxGroupInput("type2",
                                    "Select Type",
                                    choices=unique(database$Type),
                                    selected="Igneous"
                 ),
                 
                 checkboxInput("litologyb",
                               "Search by litology",
                               value=FALSE
                 ),
                 
                 selectInput("litology1",
                             "Select Litology",
                             choices=unique(database$Litology),
                             multiple=TRUE,
                             selected="Basalt"
                )
               ),
               
               mainPanel(
                 plotlyOutput("boxplot"),
                 br(),
                 br(),
                 div(tableOutput("view1"),align = "center")
               )
             
             )
             
             ),#########Final tab panel 2
    tabPanel("Histogram",        
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 shinyalert::useShinyalert(),
                 
                 
                 radioButtons("propertie",             #se puede usar checkboxGroupInput Y radioButtons
                              "Select Hydraulic Propertie",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")),
                              choiceValues=c("Tporosity","Eporosity","K","S","Sy","Compressibility"),
                              selected="K"
                 ),
                 
                 checkboxInput("logx",
                               "Log Scale",
                               value=TRUE
                 ),
                 
                 checkboxGroupInput("type1",
                                    "Select Type",
                                    choices=unique(database$Type),
                                    selected="Igneous"
                                    ),
                 
                 checkboxInput("litologya",
                               "Search by litology",
                               value=FALSE
                 ),
                 
                 selectInput("litology",
                             "Select Litology",
                             choices=unique(database$Litology),
                             multiple=TRUE,
                             selected="Basalt"
                             
                 ),
                 
                 sliderInput("bins", "Bins", min = 10, max = 40, value = 20,
                             step = 5)
                 
               ),
               mainPanel(
                 plotlyOutput("histogram"),
                 br(),
                 br(),
                 div(tableOutput("view"),align = "center")
               )
             )),
    tabPanel("Correlation",
             
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 
                 radioButtons("propertie4",             
                              "Select Hydraulic Propertie",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")),
                              choiceValues=c("Tporosity","Eporosity","K","S","Sy","Compressibility"),
                              selected="K"
                 ),
                 
                 radioButtons("propertie5",            
                              "Select Hydraulic Propertie",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")),
                              choiceValues=c("Tporosity","Eporosity","K","S","Sy","Compressibility"),
                              selected="S"
                 ),
                 
                 checkboxInput("logy1",
                               "Log Scale y",
                               value=TRUE
                 ),
                 
                 checkboxInput("logx1",
                               "Log Scale x",
                               value=TRUE
                 ),
                 
               ),
               mainPanel(
                 
                 plotlyOutput("correlation")
               )
               
             )
      
             ),
    
    tabPanel("Download",
             mainPanel(
              
               downloadButton("download","Download Database (.csv)"),
               br(),
               br(),
               downloadButton("download1","Download Sources (.pdf)")
               
             )
             ),
    
    tabPanel("About",
             div(
             span(img(src="unamlogo.jpg", style="position:relative; margin-left: 55px;", height = 150)),
             span(img(src="filogo.jpg", style="position:relative;margin-right: 80px;", height = 150,align="right"))
             ),
             
             br(),
             br(),
            
             h2("About", style="margin-left: 50px"),
             
             br(),
             
             h4('"Hydrogeological Parameters Dataset: Compilation and Statistical Analysis using R", 
             by Héctor Báez (Principal Developer) and Antonio Hernández-Espriú (Adviser).', 
                style="margin-left: 50px;margin-right:50px;text-align: justify;"),
             
             br(),
             br(),
             
             h3('Please cite us as:',style="margin-left: 50px;margin-right: 50px;"),
             
             br(),
             
             h4('Báez, Héctor (2020). Hydrogeological Parameters Dataset: Compilation and Statistical Analysis using R 
             [In Spanish: Base de datos de parámetros hidrogeológicos: compilación y análisis estadístico usando R], BS, BE Thesis, Faculty of Engineering, 
               Universidad Nacional Autónoma de México (UNAM), Mexico. ',style="margin-left: 50px;margin-right: 50px;text-align: justify;")
             
             
             
             )
  
    
    
  )#####Corchete final de "navbar page"
) #Corchete final "UI" 









