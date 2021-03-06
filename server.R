###############################################################
#Título: Parámetros Hidrogeológicos (Proyecto de Tesis)       #
#Autor:Báez Reyes Héctor Eduardo                              #  
#Director de Tesis: Dr. António Hernández Espriú              #
#Universidad Nacional Autónoma de México                      #
#Facultad de Ingeniería                                       #
###############################################################

###librerías para realizar los procesos dentro del servidor
library(shiny)
library(shinyjs)
library(DT)
library(shinythemes)
library(dplyr)
library(stats) 
library(EnvStats)
library(ggplot2)
library(rlang)
library(plotly)

###Carga los datos .csv de la carpeta de trabajo
database<-read.csv("hydraulic_parameters.csv",header=TRUE,sep=",",fileEncoding = "UTF-8")


##Crea las etiquetas de los histogramas
lab<-names(database)[3:8]
label<-data.frame(properties=lab,names=c(paste(" \u03A6","T"," (%)"),paste("\u03A6","e"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (m<sup>2</sup>/N)")))

##crea las etiquetas de los boxplot
lab1<-names(database)[3:8]
label1<-data.frame(properties=lab1,names=c(paste(" \u03A6","T"," (%)"),paste("\u03A6","e"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (m<sup>2</sup>/N)")))

##crea las etiquetas de la tabla principal
lab3<-c("Type","Lithology",paste(" \u03A6","<sub>T</sub>"," (%)"),paste("\u03A6","<sub>e</sub>"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1","(m<sup>2</sup>/N)"),"Note","Location","Reference_ID")


## Define las operaciones del servidor
shinyServer(function(input,output){
  ## Se definen los nuevos subgrupos a partir de las selecciones en el UI, para los histogramas
  properties<-reactive({
    database[database$Lithology%in%input$lithology,c("Type","Lithology",input$propertie)]
  })
  
  properties2<-reactive({
    database[database$Type%in%input$type1,c("Type","Lithology",input$propertie)]
  })

  ##Se definen los nuevos subgrupos a partir de las selecciones en el UI, para los diagramas de caja 
  properties1<-na.omit(reactive({
    database[database$Lithology%in%input$lithology1,c("Type","Lithology",input$propertie1, "Note")]
  }))
  
  properties3<-reactive({
    database[database$Type%in%input$type2,c("Type","Lithology",input$propertie1, "Note")]
  })
  
  ##Se defienen los nuevos subgrupos a partir de las selecciones en el UI, para las correlaciones
  properties5<-reactive({
    database[,c("Type","Lithology",input$propertie4,input$propertie5,"Note")]
  })
  
  
 
  

#####Crea las tablas resumen para los histogramas
  means<-reactive({properties()%>%
    group_by(Lithology)%>%
      summarize(n=length(na.omit(!!rlang::sym(input$propertie))),
                geometric_mean=formatC(geoMean(!!rlang::sym(input$propertie),na.rm=TRUE),format="e",digits=2),
                "&#x3C3;<sub>logx</sub>"=sd(log(!!rlang::sym(input$propertie)),na.rm=TRUE))
    })
  
  means2<-reactive({properties2()%>%
      group_by(Type)%>%
      summarize(n=length(na.omit(!!rlang::sym(input$propertie))),
                geometric_mean=formatC(geoMean(!!rlang::sym(input$propertie),na.rm=TRUE),format="e",digits=2),
                "&#x3C3;<sub>logx</sub>"=sd(log(!!rlang::sym(input$propertie)),na.rm=TRUE))
  })
  
  ##argumento para genera el mensaje de error en histogramas
  l<-reactive({input$lithology[as.numeric(length(input$lithology))]})
  num<-reactive({means()[means()$Lithology%in%l(),2]})
  ##argumento que genera el mensaje de error de los boxplot
  l1<-reactive({input$lithology1[as.numeric(length(input$lithology1))]})
  num1<-reactive({means1()[means1()$Lithology%in%l1(),2]})
  
  ##Crea las tablas resumen de los diagramas de caja
  means1 <- reactive({properties1() %>% 
      group_by(Lithology)%>%
      summarize(n=length(na.omit(!!rlang::sym(input$propertie1))),
                min.=formatC(min(!!rlang::sym(input$propertie1),na.rm=TRUE),format="e",digits=2),
                q1=formatC(quantile(!!rlang::sym(input$propertie1),0.25,na.rm=TRUE),format="e",digits=2),
                median=formatC(quantile(!!rlang::sym(input$propertie1),0.5,na.rm=TRUE),format="e",digits=2),
                q3=formatC(quantile(!!rlang::sym(input$propertie1),0.75,na.rm=TRUE),format="e",digits=2),
                max.=formatC(max(!!rlang::sym(input$propertie1),na.rm=TRUE),format="e",digits=2))
  })
  
  means3 <- reactive({properties3() %>% 
      group_by(Type)%>%
      summarize(n=length(na.omit(!!rlang::sym(input$propertie1))),
                min.=formatC(min(!!rlang::sym(input$propertie1),na.rm=TRUE),format="e",digits=2),
                q1=formatC(quantile(!!rlang::sym(input$propertie1),0.25,na.rm=TRUE),format="e",digits=2),
                median=formatC(quantile(!!rlang::sym(input$propertie1),0.5,na.rm=TRUE),format="e",digits=2),
                q3=formatC(quantile(!!rlang::sym(input$propertie1),0.75,na.rm=TRUE),format="e",digits=2),
                max.=formatC(max(!!rlang::sym(input$propertie1),na.rm=TRUE),format="e",digits=2))
  })
 
  #####genera la tabla principal 
  output$data<-DT::renderDataTable(
    DT::datatable({
      names(database)<-lab3
      database
    },
    filter="top",
    selection="multiple",
    style="bootstrap",
    extensions = 'Responsive',
    escape = FALSE) # escape permíte leer carácteres especiales
  )
  
  ## Genera el botón de descarga del archivo .csv
  output$download<-downloadHandler(
    filename="database.csv",
    content=function(file){
      write.csv(database,file)
    }
  )
  
  ## Genera el botón de descarga del archivo .pdf
  output$download1<-downloadHandler(
    filename="References.pdf",
    content=function(file){
      file.copy("References.pdf",file)
    }
  )
  

 ## Genera los elementos de salida para la página "histogram" 
 output$histogram<-renderPlotly({
   ##Se desactivan entradas de acuerdo a la selección del usuario
   observeEvent(input$lithologya, { 
     if(input$lithologya == F){
       shinyjs::disable("lithology")
       shinyjs::enable("type1")
       
     } else {
       shinyjs::enable("lithology")
       shinyjs::disable("type1")
     }}) 
   
   
   ##Muestra mensaje de error si el número de datos es menor a 15
  if(input$lithologya==TRUE){
   if(as.numeric(num())<15){
     shinyalert(title = "There is no enough data!", type = "warning",cancelButtonText = "Cancel")
   }}
   
 ###Plots de los histogramas en función de las selecciones del usuario
   if(input$lithologya==T){
     if(input$logx==T){
       plot1<-ggplot(properties(),aes(x=properties()[,3],fill=Lithology))+theme_bw()+
         scale_x_log10()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+scale_fill_brewer(palette="Dark2")+
         labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")+
         facet_wrap(~Lithology, ncol =2)
       plot1p<-ggplotly(plot1)%>%layout(showlegend = FALSE)%>% config(displayModeBar = F)%>%
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
       # Función que cambia las etiquetas (log(x)) que por default arroja plotly
       for(i in 1:length(plot1p[["x"]][["data"]])){
         plot1p[["x"]][["data"]][[i]][["text"]]<-10^(plot1p[["x"]][["data"]][[i]][["x"]])
         
       }
       
       plot1p%>%style(text=paste("x:",plot1p[["x"]][["data"]][[1]][["text"]]))
       
      }else{
        plot2<-ggplot(properties(),aes(x=properties()[,3],fill=Lithology))+theme_bw()+
          geom_histogram(bins=input$bins,na.rm=TRUE)+scale_fill_brewer(palette="Dark2")+
          labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")+facet_wrap(~Lithology, ncol =2)
        plot2p<-ggplotly(plot2)%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        
        plot2p%>%style(text=paste("x:",plot2p[["x"]][["data"]][[1]][["x"]]))
        }
   }else{
     if(input$logx==T){
       plot3<-ggplot(properties2(),aes(x=properties2()[,3],fill=Type))+theme_bw()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+ scale_fill_brewer(palette="Dark2")+facet_wrap(~Type, ncol =2)+
         scale_x_log10()+labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")
       plot3p<-ggplotly(plot3,tooltip = c("y", "Type"))%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
       
       # Función que cambia las etiquetas (log(x)) que por default arroja plotly
       for(i in 1:length(plot3p[["x"]][["data"]])){
         plot3p[["x"]][["data"]][[i]][["text"]]<-10^(plot3p[["x"]][["data"]][[i]][["x"]])
         
       }
       
       plot3p%>%style(text=paste("x:",plot3p[["x"]][["data"]][[1]][["text"]]))
       
     }else{
       
       plot4<-ggplot(properties2(),aes(x=properties2()[,3],fill=Type))+theme_bw()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+facet_wrap(~Type, ncol =2)+scale_fill_brewer(palette="Dark2")+
         labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")
       plot4p<-ggplotly(plot4)%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
       
       plot4p%>%style(text=paste("x:",plot4p[["x"]][["data"]][[1]][["x"]]))
     }
     
     
   }   
 })##Llaves finales del output$histogram<-renderPlot({
 
 ### Crea los productos de salida de las tablas resumen
 observeEvent(input$lithologya,{
   if(input$lithologya==T){
     output$view <- renderTable({                     ###Tabla de medias
       means()[means()$Lithology%in%input$lithology,]
       },bordered=TRUE,align="c",sanitize.text.function = identity,extensions = 'Responsive')           ###Llaves finales de tabla de medias
     }else{
       output$view <- renderTable({                     ###Tabla de medias
         means2()[means2()$Type%in%input$type1,]
       },bordered=TRUE,align="c",sanitize.text.function = identity,extensions = 'Responsive')  
       }
 })  

 ## Genera los elementos de salida para la página "boxplot" 
  output$boxplot<-renderPlotly({
    ## Desactiva secciones del UI de acuerdo a las selecciones del usuario
    observeEvent(input$lithologyb, { 
      if(input$lithologyb == F){
        shinyjs::disable("lithology1")
        shinyjs::enable("type2")
        
      } else {
        shinyjs::enable("lithology1")
        shinyjs::disable("type2")
      }})
    
    ##Mensaje de error si el número de datos es menor a 15
    if(input$lithologyb==TRUE){
      if(as.numeric(num1())<15){
        shinyalert(title = "There is no enough data!", type = "warning",cancelButtonText = "Cancel")
      }}
    
    ##Genera los plots de los diagramas de caja en función de las seleciones del usuario
    if(input$lithologyb==T){
      
      if(input$logy==T){
        
        plot_ly(properties1(), y = ~properties1()[,3], x=~as.character(properties1()[,2]), type = "box",
                color=~as.character(properties1()[,2]),colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Lithology: ',Lithology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties1()[,3]))%>%
          layout(yaxis=list(type="log"))%>%
          add_boxplot(properties1(), y = ~properties1()[,3], x=~as.character(properties1()[,2]), type = "box",color=~as.character(properties1()[,2]),
                      boxpoints = "outliers", text=~text,
                      showlegend=FALSE)%>%
          layout(yaxis = list(title=as.character(label[label1$proper%in%input$propertie1,"names"]),
                              tickvals = c("1e-10","1e-9","1e-8","1e-7","1e-6","1e-5","1e-4","1e-3","1e-2","1e-1","1e+0","1e-1","1e+2","1e+1","1e+3","1e+4"),
                              ticktext=c("1e-10","1e-09","1e-08","1e-07","1e-06","1e-05","1e-04","1e-03","1e-02","1e-01","1e+00","1e-01","1e+02","1e+01","1e+03","1e+04"),
                              type = "log",
                              showline = TRUE,
                              fixedrange=TRUE),
                 xaxis= list(title=FALSE,
                               showline=TRUE,
                             fixedrange=TRUE))%>%
          config(displayModeBar = F)
        
        
      }else{
        
        plot_ly(properties1(), y = ~properties1()[,3], x=~as.character(properties1()[,2]), type = "box",
                color=~as.character(properties1()[,2]),colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Lithology: ',Lithology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties1()[,3]))%>%
          add_boxplot(properties1(), y = ~properties1()[,3], x=~as.character(properties1()[,2]), type = "box",color=~as.character(properties1()[,2]),
                      boxpoints = "outliers",text=~text,
                      showlegend=FALSE)%>%
          layout(yaxis = list(title=as.character(label[label1$proper%in%input$propertie1,"names"]),
                              showline = TRUE,fixedrange=TRUE),
                 xaxis= list(title=FALSE,fixedrange=TRUE))%>%
          config(displayModeBar = F)
      }
      
    }else{
      
      if(input$logy==T){
        
        plot_ly(properties3(), y = ~properties3()[,3], x=~as.character(properties3()[,1]), type = "box",
                color=~as.character(properties3()[,1]),colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Lithology: ',Lithology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties3()[,3]))%>%
          layout(yaxis=list(type="log"))%>%
          add_boxplot(properties3(), y = ~properties3()[,3], x=~as.character(properties3()[,1]), type = "box",color=~as.character(properties3()[,1]),
                      boxpoints = "outliers",text=~text,
                      showlegend=FALSE)%>%
          layout(yaxis = list(title=as.character(label[label1$proper%in%input$propertie1,"names"]),
                              tickvals = c("1e-10","1e-9","1e-8","1e-7","1e-6","1e-5","1e-4","1e-3","1e-2","1e-1","1e+0","1e-1","1e+2","1e+1","1e+3","1e+4"),
                              ticktext=c("1e-10","1e-09","1e-08","1e-07","1e-06","1e-05","1e-04","1e-03","1e-02","1e-01","1e+00","1e-01","1e+02","1e+01","1e+03","1e+04"),
                              type = "log",
                              showline = TRUE,fixedrange=TRUE),
                 xaxis= list(title=FALSE,
                             showline=TRUE,fixedrange=TRUE))%>%
          config(displayModeBar = F)
        
      }else{
        
        plot_ly(properties3(), y = ~properties3()[,3], x=~as.character(properties3()[,1]), type = "box",
                color=~as.character(properties3()[,1]),colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Lithology: ',Lithology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties3()[,3]))%>%
          add_boxplot(properties3(), y = ~properties3()[,3], x=~as.character(properties3()[,1]), type = "box",color=~as.character(properties3()[,1]),
                      boxpoints = "outliers",text=~text,
                      showlegend=FALSE)%>%
          layout(yaxis = list(title=as.character(label[label1$proper%in%input$propertie1,"names"]),
                              showline = TRUE,fixedrange=TRUE),
                 xaxis= list(title=FALSE,
                             showline=TRUE,fixedrange=TRUE))%>%
          config(displayModeBar = F)
      }
    }
    
    
 })
   
 
 ## Genera los productos de salida de las tablas resumen
 observeEvent(input$lithologyb,{
   if(input$lithologyb==TRUE){
     output$view1 <- renderTable({                     
       means1()
     },bordered=TRUE,align="c",sanitize.text.function = identity,width = 2)
   }else{
     output$view1 <- renderTable({                     
       means3() 
     },bordered=TRUE,align="c",sanitize.text.function = identity,width = 2)
   }
 })
  

 ##Genera los plots de las correlaciones en función de las selecciones del usuario
 output$correlation<-renderPlotly({
   
   if(input$logy1==T && input$logx1==T){
     plot6<-ggplot(properties5(),aes(x=properties5()[,3],y=properties5()[,4],color=Type))+
       geom_point(aes(text= paste('</br> Type: ', Type,
                               '</br> Lithology: ',Lithology,
                               '</br> Note: ',Note,
                               '</br> x: ', properties5()[,3],
                               '</br> y: ',properties5()[,4])))+
       theme_bw()+scale_x_log10()+scale_y_log10()+labs(x=as.character(label[label$proper%in%input$propertie4,"names"]), 
                                                       y=as.character(label[label$proper%in%input$propertie5,"names"]))
     
     ggplotly(plot6,tooltip = "text")%>%config(displayModeBar = F) %>% 
       layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
  
   } else if(input$logy1==T && input$logx1==F){
     
     plot6<-ggplot(properties5(),aes(x=properties5()[,3],y=properties5()[,4],color=Type))+
       geom_point(aes(text= paste('</br> Type: ', Type,
                                  '</br> Lithology: ',Lithology,
                                  '</br> Note: ',Note,
                                  '</br> x: ', properties5()[,3],
                                  '</br> y: ',properties5()[,4])))+
       theme_bw()+scale_y_log10()+labs(x=as.character(label[label$proper%in%input$propertie4,"names"]), 
                                       y=as.character(label[label$proper%in%input$propertie5,"names"]))
     
     ggplotly(plot6,tooltip = "text")%>%config(displayModeBar = F) %>% 
       layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
     
   } else if(input$logy1==F && input$logx1==T){
     plot6<-ggplot(properties5(),aes(x=properties5()[,3],y=properties5()[,4],color=Type))+
       geom_point(aes(text= paste('</br> Type: ', Type,
                                  '</br> Lithology: ',Lithology,
                                  '</br> Note: ',Note,
                                  '</br> x: ', properties5()[,3],
                                  '</br> y: ',properties5()[,4])))+
       theme_bw()+scale_x_log10()+labs(x=as.character(label[label$proper%in%input$propertie4,"names"]), 
                                       y=as.character(label[label$proper%in%input$propertie5,"names"]))
     
     ggplotly(plot6,tooltip = "text")%>%config(displayModeBar = F) %>% 
       layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
     
   } else{
     plot6<-ggplot(properties5(),aes(x=properties5()[,3],y=properties5()[,4],color=Type))+
       geom_point(aes(text= paste('</br> Type: ', Type,
                                  '</br> Lithology: ',Lithology,
                                  '</br> Note: ',Note,
                                  '</br> x: ', properties5()[,3],
                                  '</br> y: ',properties5()[,4])))+
       theme_bw()+labs(x=as.character(label[label$proper%in%input$propertie4,"names"]), 
                                       y=as.character(label[label$proper%in%input$propertie5,"names"]))
     
     ggplotly(plot6,tooltip = c("text"))%>%config(displayModeBar = F) %>% 
       layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
     
   }
  
 })
 
 # Genera la variable de texto de salida del contador de clicks de descarga de la base de datos
 
 
 
 output$downloadcount <- 
   renderText({
     if (!file.exists("counterd.Rdata")) {
       counterd <- input$rnd
       save(counterd, file="counterd.Rdata")
     } else {
       load(file="counterd.Rdata")
       counterd  <- counterd+input$rnd
       save(counterd, file="counterd.Rdata")     
     }
     paste('Database Downloads: ', counterd)
     
   })
 
 # Genera el contador de clicks de la base de datos (código javascript)
   observe({
     if(is.null(input$rnd)){
       runjs("
            var click = 0;
            Shiny.onInputChange('rnd', click)
            var download = document.getElementById('download')
            download.onclick = function() {click += 1; Shiny.onInputChange('rnd', click)};
            ")      
     }
   })
 
 
 # Crea el contador de visitas de la página
 output$visits <- 
   renderText({
     if (!file.exists("counter.Rdata")) {
       counter <- 0
       save(counter, file="counter.Rdata") 
     } else {
        load(file="counter.Rdata")
       counter  <- counter + 1
       save(counter, file="counter.Rdata")     
       paste("Visits: ", counter)
     }
   })
 
 
 
})##Llaves finales de los procesos del servidor servidor