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
#library(PruebaLibreria)
library(formattable)
library(shiny)


load("C:/BECHER/RDA/Beneficios.rda")
cantBeneficios <- as.numeric(nrow(Beneficios))
cantBeneficios <- prettyNum(cantBeneficios, scientific = FALSE, big.mark= ",")

load("C:/BECHER/RDA/DemoraJO.rda")



#DemoraJI <- getDemoraBeneficioJI()
load("C:/BECHER/RDA/DemoraJI.rda")


# CARGA LA PROYECCION DE ALTAS ANUAL
#Proyeccion <- getProyeccionAltas()
load("C:/BECHER/RDA/ProyeccionAltas.rda")
Proyeccion <- PersonaProx
# CARGA
#Altas <- getAltas()
#load("C:/BECHER/RDA/Altas.rda")
load("C:/BECHER/RDA/Altas2.rda")

# Vencidos
#Vencidos <- getVencidos()
load("C:/BECHER/RDA/Vencidos.rda")
Vencidos$PRESTDSC <- trimws(Vencidos$PRESTDSC, "both")

header <- dashboardHeader(
  
  #titleWidth='80%',
  #tags$link(rel = "stylesheet", type = "text/css", href = "logo.css"),
  
  title = span(tags$img(src="Isotipo.png", width = '50', height = '45'))
  
  #tags$img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSspEFiANdEmogpOL9rFgrpNWQ1PrVlhoZtSKTbNyTR7xn3ZJD3", width = '100%', maxWidth = '450px',  height = '125px', align = 'center'),
  
  # column(12,
  #        tags$img(src="http://pactoglobal.org.ar/wp-content/uploads/2015/05/Isologo-CAJA-DE-JUBILACIONES.jpg", width = '100%', height = '125', align = 'center'))
  
)

###############  SIDEBAR ##############
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("HOME", tabName = "Home", icon = icon("home")),
    menuItem("BENEFICIOS", tabName = "Beneficios", icon = icon("calculator"),
             menuSubItem("Nuevos beneficios", tabName = "NuevosBeneficios"),
             menuSubItem("Tiempo de beneficios", tabName = "DemoraBeneficios"),
             menuSubItem("Vencimiento de vig", tabName = "VencimientoVig")),
    menuItem(text = "RRHH", tabName = "RRHH", icon = icon("users")),
    menuItem("FINANZAS", tabName = "Finanzas", icon = icon("balance-scale")),
    menuItem("SISTEMAS", tabName = "Sistemas", icon = icon("laptop")),
    menuItem("LEGALES", tabName = "Legales", icon = icon("book")),
    menuItem("SOCIAL MEDIA", tabName = "SocialMedia", icon = icon("thumbs-up"))
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
              
            ),
            
            br(),
            hr(),
            wellPanel(style = "background-color: white;",
                      
                      
                      
                      
                      #h3("\n Cantidad de beneficios"),
                      h2("Total prestaciones \n ", cantBeneficios, style = "font-family:'Times New Roman'; text-align: center"),
                      
                      br(),
                      fluidRow(
                        column(6, highchartOutput("tortaCantidadBeneficiosPOJ", height = "350px")),
                        column(6, highchartOutput("tortaCantidadBeneficiosSEXO", height = "350px"))
                      )
                      
                      
                      
                      
                      
                      
                      
            )
            
            
    ),
    ##### BENEF_BODY #####
    tabItem(tabName = "NuevosBeneficios",
            fluidRow(
              br(),
              valueBoxOutput("contNuevosBenef"),
              valueBoxOutput("ProyeccionAltas"),
              #tags$hr()
              valueBoxOutput("ultimoMesBeneficio")
            ),
            wellPanel(
              
              tabsetPanel(
                tabPanel("Gráfico",
                         h3("", style = "color:black;text-align: center;font-family: Arial, Times, serif;"),
                         
                         
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesde",
                                    label = h5(strong("Año inicio"), style = "font-family: Arial;"),
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2018
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHasta",
                                    label = h5(strong("Año fin"), style = "font-family: Arial;"),
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           )
                         ),
                         
                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "type",
                               label = h5(strong("Tipo de gráfico"), style = "font-family: Arial;"),
                               choices = c("line", "column", "bar", "spline")
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descarga", h5("Descargar"))
                           )
                         ),
                         wellPanel(style = "background-color : #ecf0f5;",
                                   highchartOutput("hcontainer", height = "600px")),
                         br(),
                         br()
                         #highchartOutput("torta", height = "500px")
                         
                         
                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("table")),
                tabPanel("Futuras Altas",
                         br(),
                         downloadButton("descarga4", h5("Descargar")),
                         br(),
                         br(),
                         DT::dataTableOutput("tablaProyeccionAltas"))
              )
              
            )
            
    ),
    tabItem(tabName = "DemoraBeneficios",
            
            # fluidRow(
            #   br(),
            #   valueBoxOutput("contNuevosBenef"),
            #   valueBoxOutput("contNuevosBenef2"),
            #   valueBoxOutput("contNuevosBenef3")
            # ),
            fluidRow(
              column(6,
                     actionButton(inputId = "JO", icon = icon("user-circle", class="user-clock"), width = '100%',style = "font-size: 48px; color: Dodgerblue;" ,h2("JO"))
              ),
              column(6,
                     actionButton(inputId = "JI", icon = icon("user-circle"), width = '100%',style = "font-size: 48px; color: green;" , h2("JI"))
              )
              
            ),
            shinyjs::hidden(
              div( id = "menuDemora",
                   fluidRow(
                     br(),
                     valueBoxOutput("DemoraKPI"),
                     valueBoxOutput("DemoraKPI2"),
                     valueBoxOutput("DemoraKPI3")
                   ),
                   wellPanel(
                     tabsetPanel(
                       tabPanel("Gráfico",
                                
                                
                                #h3("\n Demora de beneficios por area"),
                                br(),
                                fluidRow(
                                  column(3,
                                         selectInput(
                                           inputId = "IAnioDesdeDemora",
                                           label = "Año inicio",
                                           choices = c(2016, 2017, 2018, 2019, 2020),
                                           selected = 2019
                                         )
                                  ),
                                  column(3,
                                         selectInput(
                                           inputId = "IAnioHastaDemora",
                                           label = "Año fin",
                                           choices = c(2016, 2017, 2018, 2019, 2020),
                                           selected = 2019
                                         )
                                  )),
                                fluidRow(column(
                                  width = 3,
                                  selectInput(
                                    "type2",
                                    label = "Tipo de gráfico",
                                    choices = c("line", "column", "bar", "spline")
                                  )
                                ),
                                column(3,offset = 6,
                                       downloadButton("descarga2", h5("Descargar"))
                                )),
                                #plotOutput("nuevosBenef")
                                wellPanel(style = "background-color : #ecf0f5;",
                                  highchartOutput("graficoDemoraJO", height = "600px"))
                                
                       ),
                       tabPanel("Tabla",
                                DT::dataTableOutput("tablaDemoraJO"))
                     )
                     
                   ))),
            shinyjs::hidden(
              div( id = "menuDemoraJI",
                   fluidRow(
                     br(),
                     valueBoxOutput("DemoraJI_KPI"),
                     valueBoxOutput("DemoraJI_KPI2"),
                     valueBoxOutput("DemoraJI_KPI3")
                   ),
                   wellPanel(
                     tabsetPanel(
                       tabPanel("Gráfico",
                                
                                br(),
                                #h3("\n Demora de beneficios por area"),
                                
                                fluidRow(
                                  column(3,
                                         selectInput(
                                           inputId = "IAnioDesdeDemoraJI",
                                           label = "Año inicio",
                                           choices = c(2016, 2017, 2018, 2019, 2020),
                                           selected = 2019
                                         )
                                  ),
                                  column(3,
                                         selectInput(
                                           inputId = "IAnioHastaDemoraJI",
                                           label = "Año fin",
                                           choices = c(2016, 2017, 2018, 2019, 2020),
                                           selected = 2019
                                         )
                                  )),
                                fluidRow(column(
                                  width = 3,
                                  selectInput(
                                    "type3",
                                    label = "Tipo de gráfico",
                                    choices = c("line", "column", "bar", "spline")
                                  )
                                ),
                                column(3,offset = 6,
                                       downloadButton("descarga3", h5("Descargar"))
                                )),
                                #plotOutput("nuevosBenef")
                                wellPanel(style = "background-color : #ecf0f5;",
                                  highchartOutput("graficoDemoraJI", height = "500px"))
                                
                       ),
                       tabPanel("Tabla",
                                DT::dataTableOutput("tablaDemoraJI"))
                     )
                     
                   )))
            
    ),
    tabItem(tabName = "CantidadBeneficios",
            
            # fluidRow(
            #   br(),
            #   valueBoxOutput("contNuevosBenef"),
            #   valueBoxOutput("contNuevosBenef2"),
            #   valueBoxOutput("contNuevosBenef3")
            # ),
            wellPanel(
              # tabsetPanel(
              #   tabPanel("Gráfico",
              #
              #
              #            #h3("\n Cantidad de beneficios"),
              #            h4("TOTAL PRESTACIONES \n ", cantBeneficios, style = "font-family: Arial; text-align: center"),
              #
              #            br(),
              #            fluidRow(
              #              column(6, highchartOutput("tortaCantidadBeneficiosPOJ", height = "350px")),
              #              column(6, highchartOutput("tortaCantidadBeneficiosSEXO", height = "350px"))
              #            )
              #
              #
              #
              #
              #   )
              # )
              
            )
            
    ),
    tabItem(tabName = "VencimientoVig",
            
            fluidRow(
              br(),
              valueBoxOutput("VenciKPI"),
              valueBoxOutput("VenciKPI2"),
              valueBoxOutput("VenciKPI3")
            ),
            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         br(),
                         br(),
                         
                         
                         h3("\n Beneficios con vencimiento de vigencia"),
                         br(),
                         
                         
                         fluidRow(
                           column(6,
                                  dateRangeInput("rangoValidacion", label = "Seleccione fecha de vigencia", start = (Sys.Date() - 28),end = Sys.Date(), width = '400px')
                           ),
                           column(3,
                                  selectInput(
                                    "tipoPrestacion",
                                    label = "Prestación",
                                    choices = sort(unique(Vencidos$PRESTDSC))
                                  )
                           )),
                         
                         br(),
                         DT::dataTableOutput("tablaVencidos"))
                
              )
              
            )
            
    )
))


ui <- dashboardPage(skin = "black",header, sidebar, body)
