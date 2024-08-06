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


# Define el UI para la aplicación indicando las páginas de navegación (tabpanel), paneles laterales (sidebarPanel) y el contenido (mainPanel)
shinyUI <- tagList( 
  
  tags$head(
    tags$style(HTML("
      
      p {
        text-align: justify
      }

    ")) # justifica el texto del apartado "About"
  ),
  
  
  navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    title=div(
      img(src="unamlogo.jpg", style="position:relative; margin-top: -10px;", height = 50),
      "Aquifer Parameters",style="position:relative; margin-top: -7px;"),
    windowTitle="Aquifer Parameters",
    id="tabs",
    
    
    tabPanel("Data",
        
             titlePanel("AquiParameter: Aquifer Parameter Dataset"),
             fluidRow(column(DT::dataTableOutput("data"),
                             width=12))
             
             ),
    tabPanel("Boxplot",
             
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 shinyalert::useShinyalert(),
                 
                 radioButtons("propertie1",           
                              "Select Parameter",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",HTML("\u03B1","(m<sup>2</sup>/N)")),
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
                 
                 checkboxInput("lithologyb",
                               "Search by lithology",
                               value=FALSE
                 ),
                 
                 selectInput("lithology1",
                             "Select Lithology",
                             choices=unique(database$Lithology),
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
                              "Select Parameter",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",HTML("\u03B1","(m<sup>2</sup>/N)")),
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
                 
                 checkboxInput("lithologya",
                               "Search by lithology",
                               value=FALSE
                 ),
                 
                 selectInput("lithology",
                             "Select Lithology",
                             choices=unique(database$Lithology),
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
                              "Select Parameter",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",HTML("\u03B1","(m<sup>2</sup>/N)")),
                              choiceValues=c("Tporosity","Eporosity","K","S","Sy","Compressibility"),
                              selected="K"
                 ),
                 
                 radioButtons("propertie5",            
                              "Select Parameter",
                              choiceNames=list(HTML(" \u03A6<sub>T</sub>"," (%)"),HTML("\u03A6<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",HTML("\u03B1","(m<sup>2</sup>/N)")),
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
             
               fluidRow(
                 h4(strong("AquiParameter:")," Aquifer Parameter Dataset. Compilation and Statistical Analysis using R.", style="justify-content: center; text-align: center;"),
                 h4("Héctor Báez, Antonio Hernández-Espriú", style="justify-content: center; text-align: center;"),
                 h4("Hydrogeology Group, Faculty of Engineering, UNAM, 2024. ", style="justify-content: center; text-align: center;")
                 
               ),
             
             br(),
             br(),
               
               fluidRow(
                 
                 column( 12, align = "center", 
                         downloadButton("download","Download Database (.csv)"),
                         br(),
                         br(),
                         downloadButton("download1","Download References (.pdf)")
                 )
               ),
             
             br(),
             br()
      
             ),
    
    tabPanel("About",
             
             fluidRow(
               column(4,align = "center",img(src="unamlogo.jpg", height = 150)),
               column(4, align="center", img(src="hydrogroup.png",height = 150)),
               column(4,align = "center",img(src="filogo.jpg", height = 150))
             
             ),
             
             br(),
             
             fluidRow(style="margin: auto 10% ;",
              column(12,
               h4(strong("AquiParameter:")," Aquifer Parameter Dataset. Compilation and Statistical Analysis using R.", style="justify-content: center; text-align: center;"),
               h4("Héctor Báez, Antonio Hernández-Espriú", style="justify-content: center; text-align: center;"),
               h4("Hydrogeology Group, Faculty of Engineering, UNAM, 2024. ", style="justify-content: center; text-align: center;"),
               br(),
               h4(strong("Description")),
               p("An accurate and reliable estimation of aquifer hydraulic parameters is essential to assess groundwater pumping conditions, predict solute migration rates, 
                 develop remediation strategies, or properly calibrate and validate flow and transport numerical models. One common practice among hydrogeologists is to assign hydraulic 
                 parameters as a function of a previously known aquifer lithology. This procedure is often useful as a preliminary evaluation, to further explore a detailed hydraulic characterization,
                 by means of in-situ aquifer testing or lab methods."),
               br(),
               p("Virtually all Groundwater Hydrology textbooks contain very little hydraulic values that generally served as rough guidelines. Here, we introduce",
               strong('"AquiParameter"'), "an extensive hydrogeological dataset that provides ~6,000 records of total porosity, effective porosity, hydraulic conductivity, storage coefficient, specific yield, and rock compressibility, 
                for more than 40 different igneous, sedimentary, metamorphic and unconsolidated lithologies. "),
               br(),
               p("The", strong("AquiParameter"),"dataset was compiled manually based on a substantial literature review (~900 sources), such as Peer-Review papers, technical reports, textbooks, 
                 and academic thesis/dissertations, considering records from several parts of the world. Moreover, some interactive statistical tools were added to allow the user to display, 
                 visualize and compare boxplots, histograms, and correlation plots between rock groups and specific lithologies. Finally, in the “Download” Section, the user can freely download the entire dataset in a CSV-based MS Excel 
                 compatible format, and to download all our cited references in the PDF file. "),
               br(),
               p("Our dataset was developed using R (",span("R Core Team, 2017", style = "color:blue"),"), a robust and open-source language and cohesive environment for statistical computing. We used", strong("Rstudio"),"(",
                 span("Allaire, 2012", style = "color:blue"),"), an Integrated Development Environment for the R programming language."),
               br(),
               p("Overall,", strong("AquiParameter"), "was particularly developed for students, academicians, and teachers. Of course, geoscientists, researchers and groundwater practitioners are more than welcome to use our product. 
                 Please feel free to explore, navigate and download AquiParameter. We hope that you can find this interactive dataset useful and fun; and please, don´t forget to cite us! =)"),
               br(),
               p(em("Héctor and Antonio.")),
               br(),
               br(),
               h4(strong("Dataset Acronyms ")),
               p("- \u03A6",HTML("<sub>T</sub>") ," = Total Porosity (%)"),
               p("- \u03A6",HTML("<sub>e</sub>"),"= Effective Porosity (%)"),
               p("- K = Hydraulic Conductivity (Meter/Day; m/d)"),
               p("- S = Storage Coefficient (Dimensionless)"),
               p("- Sy = Specific Yield (Dimensionless)"),
               p("- \u03B1 = Rock compressibility (Square Meter/Newton;",HTML("(m<sup>2</sup>/N)"),")"),
               p("- n = Number of observations"),
               p("- q", HTML("<sub>1</sub>") ,"= First quartile (the median of the lower half of the dataset)"),
               p("- q", HTML("<sub>3</sub>"),"= Third quartile (the median of the upper half of the dataset)"),
               p("- \u03C3", HTML("<sub>logx</sub>"),"= Standard deviation of the logarithm of the variable "),
               br(),
               br(),
               h4(strong("Author’s Affiliation")),
               p("Earth Sciences Division, Faculty of Engineering, Universidad Nacional Autonoma de Mexico (UNAM)"),
               p("Correspondence to",span("ahespriu@unam.mx", style = "color:blue; text-decoration: underline blue;")),
               br(),
               br(),
               h4(strong("Citation")),
               p("Please cite us as:"),
               p("Báez, H, Hernández-Espriú, A (2024). AquiParameter: Aquifer Parameter Dataset. Compilation and Statistical Analysis using R."),
               p("Báez, H (2021). Hydrogeological Parameters Dataset: Compilation and Statistical Analysis using R [In Spanish: Base de datos de parámetros hidrogeológicos: compilación y análisis estadístico usando R], BS, BE Thesis, Faculty of Engineering, Universidad Nacional Autónoma de México (UNAM), Mexico."),
               br(),
               br(),
               h4(strong("Acknowledgements")),
               p("We are grateful to ",
                 tags$a(href="https://www.ingenieria.unam.mx/", "Faculty of Engineering (UNAM)",style="color:blue; text-decoration: underline blue;"),
                 " for granting us a room in their Web Server  to host AquiParameter. We truly thank to Alejandro Velázquez Mena, Head of the Electrical and Computing Engineering Division, for kindly supporting this project."),
               br(),
               br(),
               h4(strong("References")),
               p("Allaire, J., 2012. RStudio: integrated development environment for R. Boston, MA, 770, p.394."),
               p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL ", tags$a(href="https://www.R-project.org/", "https://www.R-project.org/",style="color:blue; text-decoration: underline blue;"), "."),
               br(),
               br()
              )
               
               
             
             ),
             br(),
             br(),
             
             fluidRow(style="margin: auto 10% ;",
               span(textOutput("visits"), style = "color:blue;"),
               span(textOutput("downloadcount"), style = "color:blue;")
             ),
             br()
             
    ),
    navbarMenu(title = "Code", icon = icon("github"),
               tabPanel(title = a("Get Code", href="https://github.com/Darcy1856/hydrogeologicalparameters_db", target="_blank"))   ## navbarMenu hyperlink test (adds "phantom" option)
    )
  
    
    
  )#####Corchete final de "navbar page"
) #Corchete final "UI" 









