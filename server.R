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
label<-data.frame(properties=lab,names=c(paste(" \u03A6","T"," (%)"),paste("\u03A6","e"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")))

##crea las etiquetas de los boxplot
lab1<-names(database)[3:8]
label1<-data.frame(properties=lab1,names=c(paste(" \u03A6","T"," (%)"),paste("\u03A6","e"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)")))

##crea las etiquetas de la tabla principal
lab3<-c("Type","Litology",paste(" \u03A6","T"," (%)"),paste("\u03A6","e"," (%)"),"K (m/d)","S (-)","Sy (-)",paste("\u03B1"," (Pa-1)"),"Note","Location","Source")


## Define las operaciones del servidor
shinyServer(function(input,output){
  ## Se definen los nuevos subgrupos a partir de las selecciones en el UI, para los histogramas
  properties<-reactive({
    database[database$Litology%in%input$litology,c("Type","Litology",input$propertie)]
  })
  
  properties2<-reactive({
    database[database$Type%in%input$type1,c("Type","Litology",input$propertie)]
  })

  ##Se definen los nuevos subgrupos a partir de las selecciones en el UI, para los diagramas de caja 
  properties1<-na.omit(reactive({
    database[database$Litology%in%input$litology1,c("Type","Litology",input$propertie1, "Note")]
  }))
  
  properties3<-reactive({
    database[database$Type%in%input$type2,c("Type","Litology",input$propertie1, "Note")]
  })
  
  ##Se defienen los nuevos subgrupos a partir de las selecciones en el UI, para las correlaciones
  properties5<-reactive({
    database[,c("Type","Litology",input$propertie4,input$propertie5,"Note")]
  })
  
  
 
  

#####Crea las tablas resumen para los histogramas
  means<-reactive({properties()%>%
    group_by(Litology)%>%
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
  
  ##argumento para generar el mensaje de error en histogramas
  l<-reactive({input$litology[as.numeric(length(input$litology))]})
  num<-reactive({means()[means()$Litology%in%l(),2]})
  ##argumento que genera el mensaje de error de los boxplot
  l1<-reactive({input$litology1[as.numeric(length(input$litology1))]})
  num1<-reactive({means1()[means1()$Litology%in%l1(),2]})
  
  ##Crea las tablas resumen de los diagramas de caja
  means1 <- reactive({properties1() %>% 
      group_by(Litology)%>%
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
    extensions = 'Responsive')
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
    filename="sources.pdf",
    content=function(file){
      file.copy("sources.pdf",file)
    }
  )
  

 ## Genera los elementos de salida para la página "histogram" 
 output$histogram<-renderPlotly({
   ##Se desactivan entradas de acuerdo a la selección del usuario
   observeEvent(input$litologya, { 
     if(input$litologya == F){
       shinyjs::disable("litology")
       shinyjs::enable("type1")
       
     } else {
       shinyjs::enable("litology")
       shinyjs::disable("type1")
     }}) 
   
   
   ##Muestra mensaje de error si el número de datos es menor a 15
  if(input$litologya==TRUE){
   if(as.numeric(num())<15){
     shinyalert(title = "There is no enough data!", type = "warning",cancelButtonText = "Cancel")
   }}
   
 ###Plots de los histogramas en función de las selecciones del usuario
   if(input$litologya==T){
     if(input$logx==T){
       plot1<-ggplot(properties(),aes(x=properties()[,3],fill=Litology))+theme_bw()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+scale_fill_brewer(palette="Dark2")+
         labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")+
         facet_wrap(~Litology, ncol =2)+scale_x_log10()
       ggplotly(plot1,tooltip=c("y","Type"))%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
       
      }else{
        plot2<-ggplot(properties(),aes(x=properties()[,3],fill=Litology))+theme_bw()+
          geom_histogram(bins=input$bins,na.rm=TRUE)+scale_fill_brewer(palette="Dark2")+
          labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")+facet_wrap(~Litology, ncol =2)
        ggplotly(plot2,tooltip=c("y","Type"))%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
          layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
        }
   }else{
     if(input$logx==T){
       plot3<-ggplot(properties2(),aes(x=properties2()[,3],fill=Type))+theme_bw()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+ scale_fill_brewer(palette="Dark2")+facet_wrap(~Type, ncol =2)+
         scale_x_log10()+labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")
       ggplotly(plot3,tooltip = c("y", "Type"))%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
       
     }else{
       
       plot4<-ggplot(properties2(),aes(x=properties2()[,3],fill=Type))+theme_bw()+
         geom_histogram(bins=input$bins,na.rm=TRUE)+facet_wrap(~Type, ncol =2)+scale_fill_brewer(palette="Dark2")+
         labs(x=as.character(label[label$proper%in%input$propertie,"names"]), y="Frequency")
       ggplotly(plot4,tooltip=c("y","Type"))%>%layout(showlegend = FALSE)%>% config(displayModeBar = F) %>% 
         layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
     }
     
     
   }   
 })##Llaves finales del output$histogram<-renderPlot({
 
 ### Crea los productos de salida de las tablas resumen
 observeEvent(input$litologya,{
   if(input$litologya==T){
     output$view <- renderTable({                     ###Tabla de medias
       means()[means()$Litology%in%input$litology,]
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
    observeEvent(input$litologyb, { 
      if(input$litologyb == F){
        shinyjs::disable("litology1")
        shinyjs::enable("type2")
        
      } else {
        shinyjs::enable("litology1")
        shinyjs::disable("type2")
      }})
    
    ##Mensaje de error si el número de datos es menor a 15
    if(input$litologyb==TRUE){
      if(as.numeric(num1())<15){
        shinyalert(title = "There is no enough data!", type = "warning",cancelButtonText = "Cancel")
      }}
    
    ##Genera los plots de los diagramas de caja en función de las seleciones del usuario
    if(input$litologyb==T){
      
      if(input$logy==T){
        
        plot_ly(properties1(), y = ~properties1()[,3], x=~properties1()$Litology, type = "box",
                color=~Litology,colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Litology: ',Litology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties1()[,3]))%>%
          layout(yaxis=list(type="log"))%>%
          add_boxplot(properties1(), y = ~properties1()[,3], x=~properties1()$Litology, type = "box",color=~Litology,
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
        
        plot_ly(properties1(), y = ~properties1()[,3], x=~properties1()$Litology, type = "box",
                color=~Litology,colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Litology: ',Litology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties1()[,3]))%>%
          add_boxplot(properties1(), y = ~properties1()[,3], x=~properties1()$Litology, type = "box",color=~Litology,
                      boxpoints = "outliers",text=~text,
                      showlegend=FALSE)%>%
          layout(yaxis = list(title=as.character(label[label1$proper%in%input$propertie1,"names"]),
                              showline = TRUE,fixedrange=TRUE),
                 xaxis= list(title=FALSE,fixedrange=TRUE))%>%
          config(displayModeBar = F)
      }
      
    }else{
      
      if(input$logy==T){
        
        plot_ly(properties3(), y = ~properties3()[,3], x=~properties3()$Type, type = "box",
                color=~Type,colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Litology: ',Litology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties3()[,3]))%>%
          layout(yaxis=list(type="log"))%>%
          add_boxplot(properties3(), y = ~properties3()[,3], x=~properties3()$Type, type = "box",color=~Type,
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
        
        plot_ly(properties3(), y = ~properties3()[,3], x=~properties3()$Type, type = "box",
                color=~Type,colors = "Dark2",boxpoints = "all",
                hoverinfo = "text",
                text = ~paste('</br> Type: ', Type,
                              '</br> Litology: ',Litology,
                              '</br> Note: ',Note,
                              '</br> value: ', properties3()[,3]))%>%
          add_boxplot(properties3(), y = ~properties3()[,3], x=~properties3()$Type, type = "box",color=~Type,
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
 observeEvent(input$litologyb,{
   if(input$litologyb==TRUE){
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
                               '</br> Litology: ',Litology,
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
                                  '</br> Litology: ',Litology,
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
                                  '</br> Litology: ',Litology,
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
                                  '</br> Litology: ',Litology,
                                  '</br> Note: ',Note,
                                  '</br> x: ', properties5()[,3],
                                  '</br> y: ',properties5()[,4])))+
       theme_bw()+labs(x=as.character(label[label$proper%in%input$propertie4,"names"]), 
                                       y=as.character(label[label$proper%in%input$propertie5,"names"]))
     
     ggplotly(plot6,tooltip = c("text"))%>%config(displayModeBar = F) %>% 
       layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
     
   }
  
 })
 
})##Llaves finales de los procesos del servidor servidor