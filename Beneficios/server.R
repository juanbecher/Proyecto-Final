


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
load("C:/BECHER/RDA/Altas.rda")
load("C:/BECHER/RDA/Altas2.rda")

# Vencidos
#Vencidos <- getVencidos()
load("C:/BECHER/RDA/Vencidos.rda")
Vencidos$PRESTDSC <- trimws(Vencidos$PRESTDSC, "both")


shinyServer(function(input, output) {
   
  
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
      hc_title(text = "<span style=\"color:#005c64;font-family: Arial ; font-size: 25px\"> NUEVOS BENEFICIOS </span> ", useHTML = TRUE) %>%
      
      hc_xAxis(title = list(text = "Período")) %>%
      hc_yAxis(title = list(text = "Cantidad"))
    
    
    
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
    load("C:/BECHER/RDA/DemoraJO.rda")
    
    
    
    #DemoraJI <- getDemoraBeneficioJI()
    load("C:/BECHER/RDA/DemoraJI.rda")
    
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
      
      
      hc_title(text = "<span style=\"color:#68AD62;font-family: Arial\"> TIEMPO JUBILACIÓN ORDINARIA </span> ", useHTML = TRUE) %>%
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
      
      
      hc_title(text = "<span style=\"color:#68AD62;font-family: Arial\"> TIEMPO JUBILACIÓN INVALIDEZ </span> ", useHTML = TRUE) %>%
      #hc_tooltip(pointFormat = paste('Días: {point.y} <br/>')) %>%
      hc_xAxis(title = list(text = "Periodo")) %>%
      hc_yAxis(title = list(text = "Dias"))
    
    
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
  
  output$contNuevosBenef <- renderValueBox({
    dato <- Altas_modif %>%
      filter(Altas_modif$Año >= input$IAnioDesde,
             Altas_modif$Año <= input$IAnioHasta)
    valueBox(
      value =  sum(dato$cont),
      subtitle = paste("Total beneficios ",input$IAnioDesde, "-",input$IAnioHasta, sep = ""),
      icon = icon("address-book"),
      color = "green"
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
  
  
  output$table <- DT::renderDataTable(DT::datatable(Altas_modif))
  
  #####################d
  output$tablaProyeccionAltas <- DT::renderDataTable(DT::datatable(Proyeccion))
  
  ######################d
  
  output$tablaDemoraJO <-
    DT::renderDataTable(DT::datatable(DemoraJO))
  ######################d
  
  output$tablaDemoraJI <-
    DT::renderDataTable(DT::datatable(DemoraJI))
  
  
  output$tablaVencidos <- DT::renderDataTable({
    validate(
      need(input$rangoValidacion[1] <= input$rangoValidacion[2] & input$rangoValidacion[1] <= Sys.Date(), ("FECHAS INGRESADAS NO VALIDAS"))
    )
    DF <- Vencidos[Vencidos$PRESTDSC == input$tipoPrestacion, ]
    
    DF %>%
      filter(DF$JBSOLFECVI >= input$rangoValidacion[1],
             DF$JBSOLFECVI <= input$rangoValidacion[2]) %>%
      
      DT::datatable()
  })
  #####################d
  
  
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
      color = "green"
    )
  })
  output$VenciKPI2 <- renderValueBox({
    
    n <- last(DemoraJI)
    
    valueBox(
      value =  n$DIAS,
      subtitle = paste("Días en ", n$mesAnio, sep = ""),
      icon = icon("address-book"),
      color = "aqua"
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
      paste("Demora_JI", Sys.Date(), ".xlsx", sep='')
    },
    content = function(filename) {
      write.xlsx(x = DemoraJI, filename, row.names = TRUE)
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
  
})
