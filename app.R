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

# CARGA CANTIDAD DE BENEFICIOS
#Beneficios <- getCantidadBeneficios()
load("C:/BECHER/RDA/Beneficios.rda")
cantBeneficios <- as.numeric(nrow(Beneficios))
cantBeneficios <- prettyNum(cantBeneficios, scientific = FALSE, big.mark= ",")
#cantBeneficios <- comma(cantBeneficios, format = "f", big.mark = ",")
# CARGA DEMORA DE BENEFICIOS POR MES
#DemoraJO <- getDemoraBeneficioJO()
load("C:/BECHER/RDA/DemoraJO.rda")



#DemoraJI <- getDemoraBeneficioJI()
load("C:/BECHER/RDA/DemoraJI.rda")


# CARGA LA PROYECCION DE ALTAS ANUAL
#Proyeccion <- getProyeccionAltas()
load("C:/BECHER/RDA/ProyeccionAltas.rda")
Proyeccion <- PersonaProx
# CARGA
#Altas <- getAltas()
load("C:/BECHER/RDA/Altas.rda")
load("C:/BECHER/RDA/Altas2.rda")

# Vencidos
#Vencidos <- getVencidos()
load("C:/BECHER/RDA/Vencidos.rda")

#RRRHH LICENCIAS

load("C:/BECHER/RDA/RRHH_Licencias.rda")
rh_lic <- rh_lic %>% arrange(SECTOR, DESCRIPCIONAUSENCIA.1)

load("C:/BECHER/RDA/RRHH_ACA.rda")
load("C:/BECHER/RDA/RRHH_Ausencias.rda")
rh_ausencias$periodo <- paste(year(rh_ausencias$FECHA),"-" ,month(rh_ausencias$FECHA), sep = "")
load("C:/BECHER/RDA/RRHH_ComisionesPorAgente.rda")
rh_com <- rh_com %>% arrange(SECTOR)
load("C:/BECHER/RDA/RRHH_detalleComisiones.rda")
load("C:/BECHER/RDA/RRHH_Productividad.rda")
rh_productividad$periodo <- paste(rh_productividad$AÑO, "-", rh_productividad$MES, sep= "")

load("C:/Users/20390538333/Desktop/Tesis/RDA/variacionGasto.rda")

load("C:/Users/20390538333/Desktop/Tesis/RDA/variacionIngreso.rda")

load("C:/Users/20390538333/Desktop/Tesis/RDA/gastoMensual.rda")

load("C:/Users/20390538333/Desktop/Tesis/RDA/ingresoMensual.rda")

#
# #GASTO MENSUAL_ IND 26 Y 28
# gastoMensual <- getGastoMensual()
#
# #VARIACION MENSUAL Y ANUAL DEL GASTO (52 = MENSUAL ;  53 = ANUAL)
# VariacionGasto <- getVariacionGasto()
#
# #INGRESO MENSUAL IND 44
# ingresoMensual <- getIngresoMensual()
#
# #VARIACION MENSUAL Y ANUAL DEL INGRESO (54 = MENSUAL ;  55 = ANUAL)
# variacionIngreso <- getVariacionIngreso()


###############  HEADER ###############
header <- dashboardHeader(

  #titleWidth='80%',
  #tags$link(rel = "stylesheet", type = "text/css", href = "logo.css"),

     title = span(tags$img(src="Isotipo.png", width = '50', height = '45'))

    #tags$img(src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSspEFiANdEmogpOL9rFgrpNWQ1PrVlhoZtSKTbNyTR7xn3ZJD3", width = '100%', maxWidth = '450px',  height = '125px', align = 'center'),

    # column(12,
    #        tags$img(src="http://pactoglobal.org.ar/wp-content/uploads/2015/05/Isologo-CAJA-DE-JUBILACIONES.jpg", width = '100%', height = '125', align = 'center'))

  )


  #dropdownMenuOutput("helpMenu")


###############  SIDEBAR ##############
sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("HOME", tabName = "Home", icon = icon("home")),
    menuItem("BENEFICIOS", tabName = "Beneficios", icon = icon("calculator"),
             menuSubItem("Nuevos beneficios", tabName = "NuevosBeneficios"),
             menuSubItem("Tiempo de beneficios", tabName = "DemoraBeneficios"),
             menuSubItem("Vencimiento de vig", tabName = "VencimientoVig")),
    menuItem("RRHH", tabName = "RRHH", icon = icon("users"),
             menuSubItem("Control de licencias", tabName = "ControlLicencias"),
             menuSubItem("Ausentismo", tabName = "Ausencias"),
             menuSubItem("ACA", tabName = "ACA"),
             menuSubItem("Comisiones Act Laboral", tabName = "ComisionesActLaboral"),
             menuSubItem("Productividad", tabName = "Productividad")),
    menuItem("FINANZAS", tabName = "Finanzas", icon = icon("balance-scale"),
             menuSubItem("Gastos", tabName = "GastoMensual"),
             menuSubItem("Ingresos", tabName = "Ingresos")
             ),
    menuItem("SISTEMAS", tabName = "Sistemas", icon = icon("laptop"),
             menuSubItem("Concurrencia", tabName = "ConcurrenciaBD"),
             menuSubItem("Cambios BD", tabName = "CambioBD")),
    menuItem("LEGALES", tabName = "Legales", icon = icon("book")),
    menuItem("SOCIAL MEDIA", tabName = "SocialMedia", icon = icon("thumbs-up"))
  )
)

#tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
###############  BODY ################
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
                         wellPanel(
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
                                wellPanel(
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

    ),


    ##### RRHH_BODY #####
    tabItem(tabName = "ControlLicencias",
            #h2("Widgets tab content"),
            fluidRow(
              br(),
                     
              valueBoxOutput("totalLicencia"),
              valueBoxOutput("totalLicencia2"),
              valueBoxOutput("totalLicencia3")
              # valueBoxOutput("VariacionMensual"),
              # valueBoxOutput("VariacionAnual")

            ),

            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesdeLicencia",
                                    label = "Año inicio",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHastaLicencia",
                                    label = "Año fin",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "tipoLicencia",
                                    label = "Tipo de licencia",
                                    choices = unique(rh_lic$DESCRIPCIONAUSENCIA.1),
                                    selected = "TODAS"
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeLicencia",
                               label = "Tipo de grafico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(
                             width = 3,
                             selectInput(
                               "sectorLicencia",
                               label = "Sector",
                               choices = c(unique(rh_lic$SECTOR), "TODOS"),
                               selected = "TODOS"
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaLicencia", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                           highchartOutput("graficoLicencia", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaLicencia"))
              )


            )
    )
    ,
    tabItem(tabName = "ACA",
            fluidRow(
              br()
              # valueBoxOutput("ultimoMesGasto"),
              # valueBoxOutput("VariacionMensual"),
              # valueBoxOutput("VariacionAnual")

            ),

            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesdeACA",
                                    label = "Año inicio",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHastaACA",
                                    label = "Año fin",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           )
                           # column(3,
                           #        selectInput(
                           #          inputId = "tipoLicenciaACA",
                           #          label = "Tipo de licencia",
                           #          choices = unique(rh_lic$DESCRIPCIONAUSENCIA.1),
                           #          selected = "TODAS"
                           #        )
                           # )
                           ),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeACA",
                               label = "Tipo de grafico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(
                             width = 3,
                             selectInput(
                               "sectorACA",
                               label = "Sector",
                               choices = c(unique(rh_lic$SECTOR), "TODOS"),
                               selected = "TODOS"
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaACA", h5("Descargar"))
                           )
                         ),
                         DT::dataTableOutput("tablaACA")


                ),
                tabPanel("Tabla",
                         h1("tabla"))
              )

            )
    )
    ,
    tabItem(tabName = "Productividad",
            #h2("Widgets tab content"),
            fluidRow(
              br(),
              valueBoxOutput("produ"),
              valueBoxOutput("produ2"),
              valueBoxOutput("produ3")

            ),

            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesdeProdu",
                                    label = "Año inicio",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHastaProdu",
                                    label = "Año fin",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeProdu",
                               label = "Tipo de grafico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(
                             width = 3,
                             selectInput(
                               "sectorProdu",
                               label = "Sector",
                               choices = c(unique(rh_productividad$SECTOR), "TODOS"),
                               selected = "SISTEMAS"
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaProdu", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                           highchartOutput("graficoProdu", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaProdu"))
              )),
              br(),
              br(),
            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "periodoProduExt",
                                    label = "Periodo",
                                    choices = unique(rh_productividad$periodo),
                                    selected = "2019-5"
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeProduExt",
                               label = "Tipo de grafico",
                               choices = c("bar","column", "line",  "spline")
                             )
                           ),
                           # column(
                           #   width = 3,
                           #   selectInput(
                           #     "sectorProduExt",
                           #     label = "Sector",
                           #     choices = c(unique(rh_productividad$SECTOR), "TODOS"),
                           #     selected = "SISTEMAS"
                           #   )
                           # ),
                           column(3,offset = 6,
                                  downloadButton("descargaProduExt", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                           highchartOutput("graficoProduExt", height = "800px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaProduExt"))
              )

            )
    )
    ,
    tabItem(tabName = "ComisionesActLaboral",
            #h2("Widgets tab content"),
            fluidRow(
              br()
              #valueBoxOutput("produ")
              # valueBoxOutput("VariacionMensual"),
              # valueBoxOutput("VariacionAnual")

            )
            ,
            br(),
            br(),
            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "periodoComision",
                                    label = "Periodo   (Año-Mes)",
                                    choices = sort(unique(rh_com$periodo)),
                                    selected = "2019-5"
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeComision",
                               label = "Tipo de grafico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(
                             width = 3,
                             selectInput(
                               "sectorComision",
                               label = "Sector",
                               choices = c(unique(rh_com$SECTOR), "TODOS"),
                               selected = "TODOS"
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaComision", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                           highchartOutput("graficoComision", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaComision"))
              )

            )
    )
    ,
    tabItem(tabName = "Ausencias",
            #h2("Widgets tab content"),
            fluidRow(
              br()
              #valueBoxOutput("produ")
              # valueBoxOutput("VariacionMensual"),
              # valueBoxOutput("VariacionAnual")

            )
            ,
            br(),
            br(),
            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "periodoAusencia",
                                    label = "Periodo   (Año-Mes)",
                                    choices = sort(unique(rh_ausencias$periodo)),
                                    selected = "2019-5"
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeAusencia",
                               label = "Tipo de grafico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(
                             width = 3,
                             selectInput(
                               "sectorAusencia",
                               label = "Sector",
                               choices = c(unique(rh_ausencias$SECTOR), "TODOS"),
                               selected = "TODOS"
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaAusencia", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                           highchartOutput("graficoAusencia", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaAusencia"))
              )

            )
    )
    ,
    ######## FINANZAS_BODY #####
    tabItem(tabName = "GastoMensual",
            fluidRow(
              br(),
              valueBoxOutput("ultimoMesGasto"),
              valueBoxOutput("VariacionMensual"),
              valueBoxOutput("VariacionAnual")

            ),

            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n GASTOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesdeGasto",
                                    label = "Año inicio",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHastaGasto",
                                    label = "Año fin",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeGasto",
                               label = "Tipo de gráfico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaGasto", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                         highchartOutput("graficoGastoMensual", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaGastoMensual"))
              )

            )
            ),
    tabItem(tabName = "Ingresos",
            fluidRow(
              br(),
              valueBoxOutput("ultimoMesIngreso"),

              valueBoxOutput("VariacionMensualIngresos"),
              valueBoxOutput("VariacionAnualIngresos")
              #infoBoxOutput("progreso"),
              #infoBoxOutput("progreso2")

            ),

            wellPanel(
              tabsetPanel(
                tabPanel("Gráfico",
                         #h3("\n INGRESOS MENSUALES"),
                         br(),
                         fluidRow(
                           column(3,
                                  selectInput(
                                    inputId = "IAnioDesdeIngreso",
                                    label = "Año inicio",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           ),
                           column(3,
                                  selectInput(
                                    inputId = "IAnioHastaIngreso",
                                    label = "Año fin",
                                    choices = c(2016, 2017, 2018, 2019, 2020),
                                    selected = 2019
                                  )
                           )),

                         fluidRow(
                           column(
                             width = 3,
                             selectInput(
                               "typeIngreso",
                               label = "Tipo de gráfico",
                               choices = c("column", "line", "bar", "spline")
                             )
                           ),
                           column(3,offset = 6,
                                  downloadButton("descargaIngreso", h5("Descargar"))
                           )
                         ),
                         wellPanel(
                         highchartOutput("graficoIngresoMensual", height = "500px"))


                ),
                tabPanel("Tabla",
                         DT::dataTableOutput("tablaIngresoMensual"))
              )

            )
    )
  )
)

####################################d

ui <- dashboardPage(skin = "black",header, sidebar, body)


server <- function(input, output, session) {
  # stockdata <- getSymbols(input$accion, src="google", from = input$fechadesde,
  #                         to = input$fechahasta, auto.assign = FALSE)


  ####### DESCARGAS ######
  output$descarga<- downloadHandler(
    filename = function() {
      paste('NuevosBeneficios_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = Altas, file, row.names = TRUE)
    },
    contentType = "text/csv")

  output$descarga2<- downloadHandler(
    filename = function() {
      paste('DemoraJO_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = DemoraJO, file, row.names = TRUE)
    },
    contentType = "text/csv")

  output$descarga3<- downloadHandler(
    filename = function() {
      paste('DemoraJI_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = DemoraJI, file, row.names = TRUE)
    },
    contentType = "text/csv")

  output$descarga4<- downloadHandler(
    filename = function() {
      paste('ProyeccionAltas_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = Proyeccion, file, row.names = TRUE)
    },
    contentType = "text/csv")

  output$descargaIngreso<- downloadHandler(
    filename = function() {
      paste('Ingresos_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = ingresoMensual, file, row.names = TRUE)
    },
    contentType = "text/csv")

  output$descargaGasto<- downloadHandler(
    filename = function() {
      paste('Gasto_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      write.xlsx(x = gastoMensual, file, row.names = TRUE)
    },
    contentType = "text/csv")

  ########################################A

  gen_plot <- function(){
    Altas$TPINDID <- as.numeric(Altas$TPINDID)
    Dato <- Altas %>%
      filter(year(Altas$Periodo) >= input$IAnioDesde,
             year(Altas$Periodo) <= input$IAnioHasta)
    Dato <- Dato %>% group_by(anio) %>% mutate(Prom = mean(TPINDDVAL))
    #
    # hchart(input$type, hcaes(x = mesAnio, y = TPINDDVAL),   color = c("#84C77E")) %>%
    # hc_title(text = "<span style=\"color:#68AD62\"> NUEVOS BENEFICIOS </span> ", useHTML = TRUE) %>%
    # hc_tooltip(pointFormat = paste('Cantidad: {point.y} <br/>')) %>%
    #
    # hc_xAxis(title = list(text = "Período")) %>%
    # hc_yAxis(title = list(text = "Cantidad"))
    hc  <- highchart() %>%
      hc_chart(type= input$type) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato, name = "Cant", type = input$type, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%
      hc_add_series(data = Dato, name = "Promedio", type = input$type, hcaes(x = mesAnio, y = Prom), color = c("red") ) %>%
      hc_xAxis(title = list(text = "Período")) %>%
      hc_yAxis(title = list(text = "Cantidad"))

    return(hc)
  }

  # output$descarga <- downloadHandler(filename ="1.png",
  #                                         content = function(file) {
  #                                           png(file, width=800, height=800)
  #                                           gen_plot()
  #                                           dev.off()
  #                                         },
  #                                         contentType = "image/png")


  ######## BENEF_SV #######

  output$hcontainer <- renderHighchart({

    validate(
      need(input$IAnioDesde <= input$IAnioHasta & input$IAnioDesde <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    #Altas$TPINDID <- as.numeric(Altas$TPINDID)

    Dato <- Altas_modif %>%
      filter(Altas_modif$Año >= input$IAnioDesde,
             Altas_modif$Año <= input$IAnioHasta)
    Dato <- Dato %>% group_by(Año) %>% mutate(Prom = mean(cont))
    #
    # hchart(input$type, hcaes(x = mesAnio, y = TPINDDVAL),   color = c("#84C77E")) %>%
    # hc_title(text = "<span style=\"color:#68AD62\"> NUEVOS BENEFICIOS </span> ", useHTML = TRUE) %>%
    # hc_tooltip(pointFormat = paste('Cantidad: {point.y} <br/>')) %>%
    #
    # hc_xAxis(title = list(text = "Período")) %>%
    # hc_yAxis(title = list(text = "Cantidad"))#EEA13F


    highchart() %>%
      hc_chart(type= input$type) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato, name = "Total", type = input$type, hcaes(x = mesAnio, y = cont), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato, name = "Promedio", type = input$type, hcaes(x = mesAnio, y = Prom), color = c("#84C77E") ) %>%
      hc_add_series(data = Dato, name = "Jubilacón", type = input$type, hcaes(x = mesAnio, y = J), color = c("#DFCA63") ) %>%
      hc_add_series(data = Dato, name = "Pensión", type = input$type, hcaes(x = mesAnio, y = P), color = c("#EEA13F") ) %>%
      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Nuevos Beneficios </span> ", useHTML = TRUE) %>%

      hc_xAxis(title = list(text = "Período")) %>%
      hc_yAxis(title = list(text = "Cantidad"))



  })
  
  output$VenciKPI <- renderValueBox({
    DF  <- Vencidos %>%
      filter(Vencidos$JBSOLFECVI >= input$rangoValidacion[1],
             Vencidos$JBSOLFECVI <= input$rangoValidacion[2]) 
    
    n <- DF[DF$PRESTDSC == input$tipoPrestacion, ]
    cant <- as.numeric(nrow(n))
    valueBox(
      value =  cant,
      subtitle = paste("Cantidad a vencer", sep = ""),
      icon = icon("address-book"),
      color = "olive"
    )
  })
  output$VenciKPI2 <- renderValueBox({
    
    n <- last(DemoraJI)
    
    valueBox(
      value =  n$DIAS,
      subtitle = paste("Días en ", n$mesAnio, sep = ""),
      icon = icon("address-book"),
      color = "teal"
    )
  })
  output$VenciKPI3 <- renderValueBox({
    
    n <- last(DemoraJI)
    
    valueBox(
      value =  n$DIAS,
      subtitle = paste("Días en ", n$mesAnio, sep = ""),
      icon = icon("address-book"),
      color = "blue"
    )
  })
  



  output$torta <- renderHighchart({
    validate(
      need(input$IAnioDesde <= input$IAnioHasta & input$IAnioDesde <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    Altas %>%
      hchart("pie", hcaes(x = anio, y = TPINDDVAL))
  })

######################f
  output$graficoDemoraJO <- renderHighchart({
    validate(
      need(input$IAnioDesdeDemora <= input$IAnioHastaDemora & input$IAnioDesdeDemora <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    Dato <- DemoraJO %>%
      filter(year(DemoraJO$fecha_fin) >= input$IAnioDesdeDemora,
             year(DemoraJO$fecha_fin) <= input$IAnioHastaDemora)

    highchart() %>%
      hc_chart(type= input$type2) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato, name = "Días", type = input$type2, hcaes(x = mesAnio, y = DIAS), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato, name = "Promedio", type = input$type2, hcaes(x = mesAnio, y = Prom), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Tiempo Jubilación Ordinaria </span> ", useHTML = TRUE) %>%
      #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "Periodo")) %>%
      hc_yAxis(title = list(text = "Días"))


  })
  ######################f
  output$graficoDemoraJI <- renderHighchart({
    validate(
      need(input$IAnioDesdeDemoraJI <= input$IAnioHastaDemoraJI & input$IAnioDesdeDemoraJI <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    Dato <- DemoraJI %>%
      filter(year(DemoraJI$fecha_fin) >= input$IAnioDesdeDemoraJI,
             year(DemoraJI$fecha_fin) <= input$IAnioHastaDemoraJI)
    highchart() %>%
      hc_chart(type= input$type3) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato, name = "Días", type = input$type2, hcaes(x = mesAnio, y = DIAS), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato, name = "Promedio", type = input$type2, hcaes(x = mesAnio, y = Prom), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Tiempo Jubilación Invalidez </span> ", useHTML = TRUE) %>%
      #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "Periodo")) %>%
      hc_yAxis(title = list(text = "Dias"))


  })
############## RRHH_SV #####
  output$graficoProdu <- renderHighchart({
    validate(
      need(input$IAnioDesdeProdu <= input$IAnioHastaProdu & input$IAnioDesdeProdu <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )

     #rh_productividad <- rh_productividad[rh_productividad$DESCRIPCIONAUSENCIA.1 == input$tipoProdu , ]


    rh_productividad <- rh_productividad[rh_productividad$SECTOR == input$sectorProdu , ]


     rh_productividad$AÑO <- as.numeric(rh_productividad$AÑO)

    rh_productividad <- rh_productividad[rh_productividad$AÑO >=input$IAnioDesdeProdu & rh_productividad$AÑO <=input$IAnioHastaProdu, ]

    #   filter(rh_productividad$AÑO  >= 2018)]
    #
    rh_productividad <- rh_productividad %>%
      group_by(MES, AÑO, mesAnio) %>%
      summarise(Prom = mean(PorcentajeCumpl))


    highchart() %>%
      hc_chart(type= input$typeProdu) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = rh_productividad, name = "Productividad", type = input$typeProdu, hcaes(x = mesAnio , y = Prom), color = c("#68BAF3") ) %>%
      # hc_add_series(data = , name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


       hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\">Productividad Por Mes</span> ", useHTML = TRUE) %>%
      # # #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
       hc_xAxis(title = list(text = "PERIODO")) %>%
       hc_yAxis(title = list(text = "CUMPLIMIENTO"))


  })
  output$graficoProduExt <- renderHighchart({
    validate(
      need(input$IAnioDesdeProdu <= input$IAnioHastaProdu & input$IAnioDesdeProdu <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )



    rh_productividad <- rh_productividad[rh_productividad$periodo == input$periodoProduExt , ]




    #   filter(rh_productividad$AÑO  >= 2018)]
    #
    rh_productividad <- rh_productividad %>%
      group_by(SECTOR) %>%
      summarise(Prom = round(mean(PorcentajeCumpl),2))


    highchart() %>%
      hc_chart(type= input$typeProduExt) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = rh_productividad, name = "Productividad", type = input$typeProduExt, hcaes(x = SECTOR , y = Prom), color = c("#68BAF3") ) %>%
      # hc_add_series(data = , name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Productividad Por Sector </span> ", useHTML = TRUE) %>%
      # # #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "PERIODO")) %>%
      hc_yAxis(title = list(text = "CUMPLIMIENTO"))


  })

  output$tablaLicencia <- DT::renderDataTable({
    rh_lic  <- rh_lic[!(rh_lic$Cuil2 == " "), ]
    DT::datatable(rh_lic)})

  ##tablaProdu

  output$tablaACA <- DT::renderDataTable({
    if(input$sectorACA != "TODOS"){
      rh_ACA <- rh_ACA[rh_ACA$SECTOR == input$sectorACA, ]
    }

    DT::datatable(rh_ACA)})
  ##
  output$tablaProdu <- DT::renderDataTable({
    rh_productividad <- rh_productividad[rh_productividad$SECTOR == input$sectorProdu , ]
    DT::datatable(rh_productividad)
      })

  output$graficoLicencia <- renderHighchart({
    validate(
      need(input$IAnioDesdeLicencia <= input$IAnioHastaLicencia & input$IAnioDesdeLicencia <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )

    rh_lic <- rh_lic[rh_lic$DESCRIPCIONAUSENCIA.1 == input$tipoLicencia , ]

    if( input$sectorLicencia != "TODOS"){
      rh_lic <- rh_lic[rh_lic$SECTOR == input$sectorLicencia , ]
    }

    rh_lic$AÑO <- as.numeric(rh_lic$AÑO)

    rh_lic <- rh_lic %>%
      filter(rh_lic$AÑO  >= input$IAnioDesdeLicencia,
             rh_lic$AÑO <= input$IAnioHastaLicencia)


    data <- rh_lic %>%
      group_by(DESCRIPCIONAUSENCIA.1,mesAnio) %>%
      summarise(total = sum(cant))


    highchart() %>%
      hc_chart(type= input$typeLicencia) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = data, name = "Licencias", type = input$typeLicencia, hcaes(x = mesAnio, y = total), color = c("#68BAF3") )  %>%
      # hc_add_series(data = , name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Licencias Mensuales </span> ", useHTML = TRUE) %>%
      # #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "PERIODO")) %>%
      hc_yAxis(title = list(text = "CANTIDAD"))


  })

  output$graficoAusencia <- renderHighchart({
    # validate(
    #   need(input$IAnioDesdeLicencia <= input$IAnioHastaLicencia & input$IAnioDesdeLicencia <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    # )

    if( input$sectorAusencia != "TODOS"){
      rh_ausencias <- rh_ausencias[rh_ausencias$SECTOR == input$sectorAusencia , ]
    }

    rh_ausencias <- rh_ausencias[rh_ausencias$periodo == input$periodoAusencia, ]

    dato <- rh_ausencias %>%
      group_by(SECTOR) %>%
      summarise(cont = n())


    highchart() %>%
      hc_chart(type= input$typeAusencia) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = dato, name = "Ausencias", type = input$typeAusencia, hcaes(x = SECTOR, y = cont), color = c("#68BAF3") )  %>%
      # hc_add_series(data = , name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Ausencias Por Sector </span> ", useHTML = TRUE) %>%
      # #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "SECTOR")) %>%
      hc_yAxis(title = list(text = "CANTIDAD"))


  })


  output$graficoComision <- renderHighchart({
    # validate(
    #   need(input$IAnioDesdeLicencia <= input$IAnioHastaLicencia & input$IAnioDesdeLicencia <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    # )

    #rh_lic <- rh_lic[rh_lic$DESCRIPCIONAUSENCIA.1 == input$tipoLicencia , ]

    if( input$sectorComision != "TODOS"){
      rh_com <- rh_com[rh_com$SECTOR == input$sectorComision , ]
    }
    rh_com <- rh_com[rh_com$periodo == input$periodoComision, ]

    dato <- rh_com %>%
      group_by(SECTOR) %>%
      summarise(Total = sum(cant))


    highchart() %>%
      hc_chart(type= input$typeComision) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = dato, name = "Licencias", type = input$typeComision, hcaes(x = SECTOR, y = Total), color = c("#68BAF3") )  %>%
      # hc_add_series(data = , name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Comisiones </span> ", useHTML = TRUE) %>%
      # #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "SECTOR")) %>%
      hc_yAxis(title = list(text = "CANTIDAD"))


  })
  ##### FINANZAS_SV #####
  output$graficoGastoMensual <- renderHighchart({
    validate(
      need(input$IAnioDesdeGasto <= input$IAnioHastaGasto & input$IAnioDesdeGasto <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )

    Dato26 <- gastoMensual[gastoMensual$TPINDID == 26, ]

    Dato26 <- Dato26 %>%
      filter(year(Dato26$TPINDDEMI) >= input$IAnioDesdeGasto,
             year(Dato26$TPINDDEMI) <= input$IAnioHastaGasto)

    Dato28 <- gastoMensual[gastoMensual$TPINDID == 28, ]

    Dato28 <- Dato28 %>%
      filter(year(Dato28$TPINDDEMI) >= input$IAnioDesdeGasto,
             year(Dato28$TPINDDEMI) <= input$IAnioHastaGasto)

    highchart() %>%
      hc_chart(type= input$typeGasto) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato26, name = "Gasto Recibos", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#68BAF3") ) %>%
      hc_add_series(data = Dato28, name = "Gasto Contable", type = input$typeGasto, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#84C77E") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Gasto Mensual </span> ", useHTML = TRUE) %>%
      #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "PERIODO")) %>%
      hc_yAxis(title = list(text = "MONTO            (EN MILLONES)"))


  })


  output$graficoIngresoMensual <- renderHighchart({
    validate(
      need(input$IAnioDesdeIngreso <= input$IAnioHastaIngreso & input$IAnioDesdeIngreso <= (year(Sys.Date())), ("FECHAS INGRESADAS NO VALIDAS"))
    )

    Dato <- ingresoMensual

    Dato <- Dato %>%
      filter(year(Dato$TPINDDEMI) >= input$IAnioDesdeGasto,
             year(Dato$TPINDDEMI) <= input$IAnioHastaGasto)

    highchart() %>%
      hc_chart(type= input$typeIngreso) %>%
      hc_xAxis(type="category") %>%
      hc_add_series(data = Dato, name = "INGRESOS", type = input$typeIngreso, hcaes(x = mesAnio, y = TPINDDVAL), color = c("#68BAF3") ) %>%


      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> Ingreso Mensual </span> ", useHTML = TRUE) %>%
      #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "PERIODO")) %>%
      hc_yAxis(title = list(text = "MONTO       (EN MILLONES)"))


  })

  ########### TABLAS ####


  output$table <- DT::renderDataTable(DT::datatable(Altas_modif))

#####################d
  output$tablaProyeccionAltas <- DT::renderDataTable(DT::datatable(Proyeccion))

######################d

  output$tablaDemoraJO <-
    DT::renderDataTable(DT::datatable(DemoraJO))
  ######################d

  output$tablaDemoraJI <-
    DT::renderDataTable(DT::datatable(DemoraJI))

  ######################################a

  output$tablaGastoMensual <-
    DT::renderDataTable(DT::datatable(gastoMensual))
  ######################################a

  output$tablaIngresoMensual <-
    DT::renderDataTable(DT::datatable(ingresoMensual))


######################d

  output$tablaVencidos <- DT::renderDataTable({
    validate(
      need(input$rangoValidacion[1] <= input$rangoValidacion[2] & input$rangoValidacion[1] <= Sys.Date(), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    Vencidos <- Vencidos[Vencidos$PRESTDSC == input$tipoPrestacion, ]

    Vencidos %>%
      filter(Vencidos$JBSOLFECVI >= input$rangoValidacion[1],
             Vencidos$JBSOLFECVI <= input$rangoValidacion[2]) %>%

    DT::datatable()
  })
#####################d

#####################D
  output$tortaCantidadBeneficiosPOJ <- renderHighchart({
    dato <- Beneficios %>%
      group_by(JBSOLPOJ) %>% summarise(cant = n())
    dato$JBSOLPOJ <- ifelse(dato$JBSOLPOJ == "J", "Jubilación", "Pensión")
    dato$cant <- comma(dato$cant, format = "f", big.mark = ",")

    highchart() %>%
    #   hc_add_series(data = dato, hcaes(JBSOLPOJ, cant), type = "pie", color = c("green","pink"),
    #                 tooltip = list(pointFormat = "<br><b>{point.percentage:.1f}%</b><br>{point.cant}")) %>%
    #   hc_tooltip(crosshairs = TRUE,  borderWidth = 5, sort = TRUE, shared = TRUE, table = FALSE) %>%
    #
    #   hc_title(text = "Porcentaje por P y J",
    #          margin = 20,
    #          style = list(color = "#144746", useHTML = TRUE))
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values(labels = dato$JBSOLPOJ, values = dato$cant, text = dato$cant, color = c("#84C77E", "#144746")) %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = FALSE,
                 pointFormat = paste('{point.y} <br/><b>{point.percentage:.1f}%</b>'))%>%
      hc_title(text = "Porcentaje por pensión y jubilación",
               margin = 20,
               style = list(color = "#144746", useHTML = TRUE))

  })
  ##################A
  output$tortaCantidadBeneficiosSEXO <- renderHighchart({
    dato <- Beneficios %>%
      group_by(PERSEXO) %>% summarise(cant = n())
    dato$PERSEXO <- ifelse(dato$PERSEXO == "M", "Masculino", "Femenino")
    dato$cant <- as.numeric(dato$cant)
    #dato$cant <- as.numeric(prettyNum(dato$cant, big.mark = ",", scientific = FALSE))
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values(labels = dato$PERSEXO, values = dato$cant, color = c("#73A7D9", "#F36868")) %>%
      hc_tooltip(crosshairs = TRUE, borderWidth = 5, sort = TRUE, shared = TRUE, table = FALSE, pointFormat = paste('{point.y} <br/><b>{point.percentage:.1f}%</b>'))%>%
      hc_title(text = "Porcentaje por sexo",
               margin = 20,
               style = list(color = "#144746", useHTML = TRUE))

  })




  ####################D

  # output$textProyeccion <- renderPrint({
  #
  #   n <- nrow(Proyeccion)
  #
  #   print(n)
  #
  #
  # })




  #### KPI ####
  output$contNuevosBenef <- renderValueBox({
    dato <- Altas_modif %>%
      filter(Altas_modif$Año >= input$IAnioDesde,
             Altas_modif$Año <= input$IAnioHasta)
    valueBox(
      value =  sum(dato$cont),
      subtitle = paste("Total beneficios ",input$IAnioDesde, "-",input$IAnioHasta, sep = ""),
      icon = icon("address-book"),
      color = "olive"
    )
  })

  output$ProyeccionAltas <- renderValueBox({

    n <- nrow(Proyeccion)

    valueBox(
      value =  n,
      subtitle = "Futuras altas anuales",
      icon = icon("chart-area"),
      color = "aqua"
    )
  })

  output$DemoraKPI <- renderValueBox({

    n <- last(DemoraJO)

    valueBox(
      value =  n$DIAS,
      subtitle = paste("Días en ", n$mesAnio, sep = ""),
      icon = icon("address-book"),
      color = "green"
    )
  })
  output$DemoraKPI2 <- renderValueBox({

    n <- DemoraJO %>%
      filter(year(fecha_fin) == 2019) %>%
      summarise(DE = sd(DIAS))

    valueBox(
      value =  round(n, 2),
      subtitle = "Desviacion estandar 2019",
      icon = icon("chart-area"),
      color = "aqua"
    )
  })
  output$DemoraKPI3 <- renderValueBox({

    n <- nrow(Proyeccion)

    valueBox(
      value =  n,
      subtitle = "Futuras altas anuales",
      icon = icon("chart-area"),
      color = "light-blue"
    )
  })


  output$DemoraJI_KPI <- renderValueBox({

    n <- last(DemoraJI)

    valueBox(
      value =  n$DIAS,
      subtitle = paste("Días en ", n$mesAnio, sep = ""),
      icon = icon("address-book"),
      color = "green"
    )
  })
  output$DemoraJI_KPI2 <- renderValueBox({

    n <- DemoraJI %>%
      filter(year(fecha_fin) == 2019) %>%
      summarise(DE = sd(DIAS))

    valueBox(
      value =  round(n, 2),
      subtitle = "Desviacion estandar 2019",
      icon = icon("chart-area"),
      color = "aqua"
    )
  })
  output$DemoraJI_KPI3 <- renderValueBox({

    n <- nrow(Proyeccion)

    valueBox(
      value =  n,
      subtitle = "Futuras altas anuales",
      icon = icon("chart-area"),
      color = "light-blue"
    )
  })

  output$ultimoMesBeneficio <- renderValueBox({
    dato <- Altas[Altas$TPINDDEMI == max(Altas$TPINDDEMI), ]
    texto <- paste("Altas en ", dato$mesAnio)
    valueBox(
      value =  dato$TPINDDVAL,
      subtitle = texto,
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  output$VariacionMensual <- renderValueBox({
    dato <- VariacionGasto[VariacionGasto$TPINDID == 52 & VariacionGasto$TPINDDEMI == max(VariacionGasto$TPINDDEMI), ]

    valueBox(
      paste0(dato$TPINDDVAL, "%"), "Variación mensual", icon = icon("address-book"),
      color = "green"
    )
  })

  output$VariacionAnual <- renderValueBox({
    dato <- VariacionGasto[VariacionGasto$TPINDID == 53 & VariacionGasto$TPINDDEMI == max(VariacionGasto$TPINDDEMI), ]
    # valueBox(
    #   value =  dato$TPINDDVAL,
    #   subtitle = "Variacion anual",
    #   icon = icon("chart-area"),
    #   color = "light-blue"
    # )
    valueBox(
      paste0(dato$TPINDDVAL, "%"), "Variación anual", icon = icon("chart-area"),
      color = "light-blue"
    )
  })

  output$VariacionMensualIngresos <- renderValueBox({
    dato <- variacionIngreso[variacionIngreso$TPINDID == 54 & variacionIngreso$TPINDDEMI == max(variacionIngreso$TPINDDEMI), ]
    valueBox(
      paste0(dato$TPINDDVAL, "%"), "Variación mensual", icon = icon("address-book"),
      color = "green"
    )
  })

  output$VariacionAnualIngresos <- renderValueBox({
    dato <- variacionIngreso[variacionIngreso$TPINDID == 55 & variacionIngreso$TPINDDEMI == max(variacionIngreso$TPINDDEMI), ]

    valueBox(
      paste0(dato$TPINDDVAL, "%"), "Variación anual", icon = icon("chart-area"),
      color = "light-blue"
    )
  })

  output$ultimoMesGasto <- renderValueBox({
    dato <- gastoMensual[gastoMensual$TPINDDEMI == max(gastoMensual$TPINDDEMI), ]
    texto <- paste("Gasto en ", dato$mesAnio)
    valueBox(
      value =  dato$TPINDDVAL,
      subtitle = texto,
      icon = icon("address-book"),
      color = "blue"
    )
  })

  output$ultimoMesIngreso <- renderValueBox({
    dato <- ingresoMensual[ingresoMensual$TPINDDEMI == max(ingresoMensual$TPINDDEMI), ]
    texto <- paste("Ingreso en ", dato$mesAnio)
    valueBox(
      value =  dato$TPINDDVAL,
      subtitle = texto,
      icon = icon("address-book"),
      color = "blue"
    )
  })

  output$totalLicencia <- renderValueBox({
    rh_lic <- rh_lic[rh_lic$DESCRIPCIONAUSENCIA.1 == input$tipoLicencia , ]
    if( input$sectorLicencia != "TODOS"){
      rh_lic <- rh_lic[rh_lic$SECTOR == input$sectorLicencia , ]
    }
    dato <- rh_lic
    texto <- paste("Total de licencias")
    valueBox(
      value =  sum(dato$cant),
      subtitle = texto,
      icon = icon("address-book"),
      color = "olive"
    )
  })
  
  output$totalLicencia2 <- renderValueBox({
    rh_lic <- rh_lic[rh_lic$DESCRIPCIONAUSENCIA.1 == input$tipoLicencia , ]
    if( input$sectorLicencia != "TODOS"){
      rh_lic <- rh_lic[rh_lic$SECTOR == input$sectorLicencia , ]
    }
    dato <- rh_lic
    texto <- paste("Total de licencias")
    valueBox(
      value =  sum(dato$cant),
      subtitle = texto,
      icon = icon("address-book"),
      color = "aqua"
    )
  })
  
  output$totalLicencia3 <- renderValueBox({
    rh_lic <- rh_lic[rh_lic$DESCRIPCIONAUSENCIA.1 == input$tipoLicencia , ]
    if( input$sectorLicencia != "TODOS"){
      rh_lic <- rh_lic[rh_lic$SECTOR == input$sectorLicencia , ]
    }
    dato <- rh_lic
    texto <- paste("Total de licencias")
    valueBox(
      value =  sum(dato$cant),
      subtitle = texto,
      icon = icon("address-book"),
      color = "blue"
    )
  })

  output$produ <- renderValueBox({
    rh_productividad <- rh_productividad[rh_productividad$periodo == input$periodoProduExt, ]
    dato <- rh_productividad %>%
      group_by(periodo) %>%
      summarise(Prom = mean(PorcentajeCumpl))

    valueBox(
      value =  round(dato$Prom,2),
      subtitle = paste("Productividad ",input$periodoProduExt, sep = ""),
      icon = icon("chart-area"),
      color = "olive"
    )
  })
  
  output$produ2 <- renderValueBox({
    rh_productividad <- rh_productividad[rh_productividad$periodo == input$periodoProduExt, ]
    dato <- rh_productividad %>%
      group_by(periodo) %>%
      summarise(Prom = mean(PorcentajeCumpl))
    
    valueBox(
      value =  round(dato$Prom,2),
      subtitle = paste("Productividad ",input$periodoProduExt, sep = ""),
      icon = icon("chart-area"),
      color = "aqua"
    )
  })
  
  output$produ3 <- renderValueBox({
    rh_productividad <- rh_productividad[rh_productividad$periodo == input$periodoProduExt, ]
    dato <- rh_productividad %>%
      group_by(periodo) %>%
      summarise(Prom = mean(PorcentajeCumpl))
    
    valueBox(
      value =  round(dato$Prom,2),
      subtitle = paste("Productividad ",input$periodoProduExt, sep = ""),
      icon = icon("chart-area"),
      color = "blue"
    )
  })




  observe({
    shinyjs::onclick("JO",{
      shinyjs::hide(id = "menuDemoraJI")
      shinyjs::show(id = "menuDemora")}

    )
    shinyjs::onclick("JI",{
      shinyjs::hide(id = "menuDemora")
      shinyjs::show(id = "menuDemoraJI")}

    )

  })


  # output$progreso <- renderInfoBox({
  #   infoBox(
  #     "Progress", paste0(25, "%"), icon = icon("chart-area"),
  #     color = "green"
  #   )
  # })
  #
  # output$progreso2 <- renderInfoBox({
  #   infoBox(
  #     "Progress", paste0(25, "%"), icon = icon("list"),
  #     color = "green", fill = TRUE
  #   )
  # })

}



shinyApp(ui, server)
