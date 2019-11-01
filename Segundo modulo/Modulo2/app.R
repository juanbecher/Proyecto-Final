library(shiny)
library(tidyverse)
library(RODBC)
library(dplyr)
library(data.table)
library(lubridate)
#library(openxlsx)
library(stringr)
library(readxl)
library(quantmod)
library(ggplot2)
library(scales)
library(highcharter)
library(shinydashboard)
library(shinyjs)
library(PruebaLibreria)
library(formattable)


load("C:/Users/20390538333/Desktop/Proyecto/RDA/Modulo 2/DF_Retro.rda")
load("C:/Users/20390538333/Desktop/Proyecto/RDA/Dato_basico.rda")
load("C:/Users/20390538333/Desktop/Proyecto/RDA/Dato_baseImp.rda")
load("C:/Users/20390538333/Desktop/Proyecto/RDA/Sectores.rda")
header <- dashboardHeader(
  title = span(tags$img(src="Isotipo.png", width = '50', height = '45'))
)

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("HOME", tabName = "Home", icon = icon("home")
  ),
  menuItem("Proyecciones", tabName = "Proyecciones", icon = icon("chart-line"),
           menuSubItem("Variacion base imp", tabName = "baseImp"),
           menuSubItem("Retroactividad", tabName = "retro"),
           menuSubItem("Basico", tabName = "basico")
  )
  
)
)

body <- dashboardBody(
  shinyjs::useShinyjs(),
  #tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "logo.css"),
  tabItems(
    tabItem(tabName = "Home",
            fluidRow(
              h1(strong("Portal de transparencia"), style = "font-family: 'Times New Roman', cursive; text-align: center; color: #202D33;")
              
            )),
    tabItem(tabName = "Proyecciones",
          fluidRow(
            h1(strong("Portal de transparencia"), style = "font-family: 'Times New Roman', cursive; text-align: center; color: #202D33;")
            
          )),
    tabItem(tabName = "baseImp",
            fluidRow(
              column(4,
                     selectInput(
                       inputId = "Sector",
                       label = h5(strong("Sector"), style = "font-family: Arial;"),
                       choices = c("Municipalidad", "Bancarios","EPEC"),
                       selected = "Municipalidad"
                     )
              ),
              column(3,offset = 2,
                     numericInput(
                       inputId = "Porcentaje",
                       label = h5(strong("Porcentaje base Imp"), style = "font-family: Arial;"),
                       value = 70
                     )
              )
            ),
            br(),
            wellPanel(
              highchartOutput("baseImp", height = "600px"))),
    tabItem(tabName = "retro",
            
            fluidRow(
              br(),
              valueBoxOutput("KPI_Retro"),
              valueBoxOutput("KPI_Retro2"),
              #tags$hr()
              valueBoxOutput("KPI_Retro3")
            ),
            wellPanel(
            fluidRow(
              column(4,
                     dateInput(format = "dd-mm-yyyy",
                       inputId = "Fecha_retro",
                       label = h5(strong("Ingrese fecha de retro"), style = "font-family: Arial;")
                     )
              ),
              column(4,offset = 1,
                     selectInput(
                       inputId = "Sector_retro",
                       label = h5(strong("Sector"), style = "font-family: Arial;"),
                       width = '100%',
                       choices = c("Todos",Sectores$Sector),
                       selected = "Todos"
                     ))
            ),
            br(),
            wellPanel(
              DT::dataTableOutput("tablaRetro")
              ))
            ),
    tabItem(tabName = "basico",
            fluidRow(
              column(6,
                     selectInput(
                       inputId = "Sector_basico",
                       label = h5(strong("Sector"), style = "font-family: Arial;"),
                       width = '100%',
                       choices = Sectores$Sector
                     )
              ),
              column(3,offset = 1,
                     numericInput(
                       inputId = "Porcentaje_sector",
                       label = h5(strong("Porcentaje aumento"), style = "font-family: Arial;"),
                       value = 10
                     )
              )
            ),
            br(),
            wellPanel(
              highchartOutput("grafico_basico", height = "600px")))
    
  )

)
#c("080100-080200(Muni)", "060100-060300(Bancarios)","070100-070200(EPEC)"),


ui <- dashboardPage(skin = "black",header, sidebar, body)

server <- function(input, output, session) {
  output$baseImp <- renderHighchart({
    
    # validate(
    #   need(input$IAnioDesde <= input$IAnioHasta & input$IAnioDesde <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    # )
    
    Dato <- getProyeccionBaseImp(input$Sector, (input$Porcentaje/100) )
    #Dato_baseImp <- getProyeccionBaseImp("Municipalidad", 0.7)
    
    Dato1 <- as.data.frame( c("Sueldo Promedio","Gasto Promedio"))
    colnames(Dato1)[1] <- "Tipo"
    Dato1$Valor <- c(mean(Dato$Sueldo),mean(Dato$Gasto))
    
    Dato2 <- as.data.frame( c("Sueldo Promedio","Gasto Promedio"))
    colnames(Dato2)[1] <- "Tipo"
    Dato2$Valor <- c(mean(Dato$SueldoE2.1),mean(Dato$GastoE2.1))
    
    highchart() %>%
      #hc_chart(type= input$type) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato1, name = "Vigente", type = "column", hcaes(x = Tipo, y = Valor), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato2, name = "Proyeccion", type = "column", hcaes(x = Tipo, y = Valor), color = c("#84C77E") ) %>%
      # hc_add_series(data = Dato, name = "Jubilacón", type = input$type, hcaes(x = mesAnio, y = J), color = c("#DFCA63") ) %>%
      # hc_add_series(data = Dato, name = "Pensión", type = input$type, hcaes(x = mesAnio, y = P), color = c("#EEA13F") ) %>%
      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Base imponible </span> ", useHTML = TRUE) %>%
      
      hc_xAxis(title = list(text = "Tipo")) %>%
      hc_yAxis(title = list(text = "Cantidad"))
    
    
    
  })
  

  
  output$tablaRetro <- DT::renderDataTable({
    fecha1 <- input$Fecha_retro
    fecha <- paste(substr(fecha1,9,10),"/",substr(fecha1,6,7),"/",substr(fecha1,1,4), sep = "")
    
    tabla <- Dato_Retro
    #Dato_Retro <- getRetro("01/09/2019")
    if (input$Sector_retro != "Todos") {
      sector <- substr(input$Sector_retro,1,6)
      tabla <- tabla[tabla$PAGCALSECT == sector, ]
    }
    validate(
      need(nrow(tabla) != 0, ("EL SECTOR SELECCIONADO NO TUVO RETROACTIVIDAD"))
    )
    
    tabla <- tabla[ , -c(2,4:12)]
    colnames(tabla)[8] <- "Descripcion"
    tabla$Descripcion <- ifelse(tabla$Descripcion == 1, "Incorrecto","Correcto")
    DT::datatable(tabla)})
  
  output$KPI_Retro <- renderValueBox({
    tabla <- Dato_Retro
    if (input$Sector_retro != "Todos") {
      sector <- substr(input$Sector_retro,1,6)
      tabla <- tabla[tabla$PAGCALSECT ==sector, ]
    }
    
    prom <- mean(tabla$Retro)
    if (nrow(tabla) == 0) {
      prom <- 0
    }
    valueBox(
      value =  round(prom,2),
      subtitle = paste("Promedio retroactividad"),
      icon = icon("address-book"),
      color = "olive"
    )
  })
  
  output$KPI_Retro2 <- renderValueBox({
    tabla <- Dato_Retro
    if (input$Sector_retro != "Todos") {
      sector <- substr(input$Sector_retro,1,6)
      tabla <- tabla[tabla$PAGCALSECT == sector, ]
    }
    
    suma <- sum(tabla$Retro)
    if (nrow(tabla) == 0) {
      suma <- 0
    }
    valueBox(
      value =  round(suma,2),
      subtitle = paste("Total retroactividad"),
      icon = icon("address-book"),
      color = "light-blue"
    )
  })
  
  output$KPI_Retro3 <- renderValueBox({
    tabla <- Dato_Retro
    if (input$Sector_retro != "Todos") {
      sector <- substr(input$Sector_retro,1,6)
      tabla <- tabla[tabla$PAGCALSECT == sector, ]
    }
    
    cantidad <- as.numeric(length(unique(tabla$PAGCALSOLN)))
    if (nrow(tabla) == 0) {
      cantidad <- 0
    }
    valueBox(
      value =  cantidad,
      subtitle = paste("Cantidad solicitudes pagadas"),
      icon = icon("address-book"),
      color = "aqua"
    )
  })
  
  output$grafico_basico <- renderHighchart({
    
    # validate(
    #   need(input$IAnioDesde <= input$IAnioHasta & input$IAnioDesde <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    # )
    #print(input$Sector_basico)
    sector <- input$Sector_basico
    sector <- substr(sector,1,6)
    #sector <- as.numeric(sector)
    Dato <- getBasico(sector, input$Porcentaje_sector/100)
    #Dato_basico <- getBasico("020100",10)
    #Dato <- getProyeccionBaseImp("Municipalidad", 0.7)
    #prue <- dcast(Prueba,Cuil2 + PERAPE + PERNOM + SECTOR ~ DESCRIPCIONAUSENCIA.1, var = "cant")
    Dato1 <- as.data.frame( c("Promedio","Gasto"))
    colnames(Dato1)[1] <- "Tipo"
    Dato1$Valor <- c(Dato[1, ]$PromedioBasico,Dato[1, ]$GastoActual / 1000)
    Dato1$Tipo <- as.character(Dato1$Tipo)
    
    Dato2 <- as.data.frame( c("Promedio","Gasto"))
    colnames(Dato2)[1] <- "Tipo"
    Dato2$Valor <- c(Dato[1, ]$PromedioAumento,Dato[1, ]$GastoAumento / 1000)
    Dato2$Tipo <- as.character(Dato2$Tipo)
    
    highchart() %>%
      #hc_chart(type= input$type) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato1, name = "Vigente", type = "column", hcaes(x = Tipo, y = Valor), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato2, name = "Proyeccion", type = "column", hcaes(x = Tipo, y = Valor), color = c("#84C77E") ) %>%
      # hc_add_series(data = Dato, name = "Jubilacón", type = input$type, hcaes(x = mesAnio, y = J), color = c("#DFCA63") ) %>%
      # hc_add_series(data = Dato, name = "Pensión", type = input$type, hcaes(x = mesAnio, y = P), color = c("#EEA13F") ) %>%
      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Proyeccion de basico </span> ", useHTML = TRUE) %>%
      
      hc_xAxis(title = list(text = "Tipo")) %>%
      hc_yAxis(title = list(text = "Cantidad"))
    
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

