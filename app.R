pacman::p_load(shiny,shinydashboard,shinythemes,httr,tidyverse,jsonlite,janitor,leaflet,leaflet.extras,pacman)


# url <- 'https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/'
# 
# #cargo url
# GET(url)
# 
# #guardo info de la url
# df_gas <- url %>% 
#   fromJSON() %>% 
#   .$ListaEESSPrecio %>% 
#   tibble()  
# 
# #limpio ds 
# df_gas <- df_gas %>% 
#   clean_names() %>% 
#   type_convert(locale = locale(decimal_mark = ',')) %>% 
#   rename(longitud=longitud_wgs84)
# 
# 
# #creo una variable que contiene los precio y el idccaa del df
# precio_combustible <- df_gas[9:22] %>% 
#   mutate(df_gas$idccaa) %>% 
#   rename(idccaa=`df_gas$idccaa`) 
# 
# 
# #agrupo por idccaa y calculo la media de precios
# precio_combustible <- precio_combustible %>% 
#   group_by(idccaa) %>% 
#   summarise_all(mean,na.rm=TRUE)
# 
# 
# 
# 
# #creo una variable con los nuevos nombre
# nuevos_nombre <- c('media_precio_biodiesel','media_precio_bioetanol', 'media_precio_gas_natural_comprimido',
#                    'media_precio_gas_natural_licuado','media_precio_gases_licuados_del_petroleo', 'media_precio_gasoleo_a',
#                    'media_precio_gasoleo_b','media_precio_gasoleo_premium','media_precio_gasolina_95_e10', 'media_precio_gasolina_95_e5', 
#                    'media_precio_gasolina_95_e5_premium', 'media_precio_gasolina_98_e10', 'media_precio_gasolina_98_e5', 'media_precio_hidrogeno' )
# 
# 
# #cambio los nombre antiguos por los nuevos
# for(i in 2:16){
#   precio_combustible <- precio_combustible %>% 
#     rename_at(i, function(x) nuevos_nombre[i-1])
# }
# 
# 
# #uno el df original con el que trabajado los precios
# df_gas <- merge(x = df_gas, y = precio_combustible, by = c("idccaa", "idccaa")) %>% 
#   as_tibble()
# 
# 
# 
# 
# nuevos_nombre <- c('valoracion_precio_biodiesel','valoracion_precio_bioetanol', 'valoracion_precio_gas_natural_comprimido',
#                    'valoracion_precio_gas_natural_licuado','valoracion_precio_gases_licuados_del_petroleo', 'valoracion_precio_gasoleo_a',
#                    'valoracion_precio_gasoleo_b','valoracion_precio_gasoleo_premium','valoracion_precio_gasolina_95_e10', 'valoracion_precio_gasolina_95_e5', 
#                    'valoracion_precio_gasolina_95_e5_premium', 'valoracion_precio_gasolina_98_e10', 'valoracion_precio_gasolina_98_e5', 'valoracion_precio_hidrogeno' )
# 
# 
# 
# #true ofalse si es mas caro o no que la media
# df_gas<-df_gas %>% mutate(valoracion_precio_biodiesel= (df_gas$precio_biodiesel>df_gas$media_precio_biodiesel))
# df_gas<-df_gas %>% mutate(valoracion_precio_bioetanol= (df_gas$precio_bioetanol>df_gas$media_precio_bioetanol))
# df_gas<-df_gas %>% mutate(valoracion_precio_gas_natural_comprimido= (df_gas$precio_gas_natural_comprimido>=df_gas$media_precio_gas_natural_comprimido))
# df_gas<-df_gas %>% mutate(valoracion_precio_gas_natural_licuado= (df_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
# df_gas<-df_gas %>% mutate(valoracion_precio_gases_licuados_del_petroleo= (df_gas$precio_gases_licuados_del_petroleo>=df_gas$media_precio_gases_licuados_del_petroleo))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_a= (df_gas$precio_gasoleo_a>df_gas$media_precio_gasoleo_a))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_b= (df_gas$precio_gasoleo_b>df_gas$media_precio_gasoleo_b))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasoleo_premium= (df_gas$precio_gasoleo_premium>df_gas$media_precio_gasoleo_premium))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e10= (df_gas$precio_gasolina_95_e10>df_gas$media_precio_gasolina_95_e10))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e5= (df_gas$precio_gasolina_95_e5>df_gas$media_precio_gasolina_95_e5))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_95_e5_premium= (df_gas$precio_gasolina_95_e5_premium>=df_gas$media_precio_gasolina_95_e5_premium))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_98_e10= (df_gas$precio_gasolina_98_e10>df_gas$media_precio_gasolina_98_e10))
# df_gas<-df_gas %>% mutate(valoracion_precio_gasolina_98_e5= (df_gas$precio_gasolina_98_e5>df_gas$media_precio_gasolina_98_e5))
# df_gas<-df_gas %>% mutate(valoracion_precio_hidrogeno= (df_gas$precio_hidrogeno>df_gas$media_precio_hidrogeno))
# 
# 
# 
# 
# 
# for(i in 0:13){
#   df_gas[47+i] = ifelse(df_gas[47+i] == 'TRUE','no_low_cost','low_cost')
# }
# 
# 
# ccaa_nombre=c('ANDALUCIA','ARAGON','PRINCIPADO DE ASTURIAS', 'ILLES BALEARS', 'CANARIAS', 'CANTABRIA', 'CASTILLA Y LEON',
#               'CASTILLA-LA MANCHA', 'CATALU�A', 'COMUNITAT VALENCIANA', 'EXTREMADURA','GALICIA', 'COMUNIDAD DE MADRID', 
#               'REGION DE MURCIA', 'COMUNIDAD FORAL DE NAVARRA', 'PAIS VASCO', 'LA RIOJA', 'CEUTA', 'MELILLA')
# 
# 
# 
# df_gas <-df_gas%>%
#   mutate(ccaa=case_when(idccaa=='01'~ccaa_nombre[1], idccaa=='02'~ccaa_nombre[2], idccaa=='03'~ccaa_nombre[3], idccaa=='04'~ccaa_nombre[4], 
#                         idccaa=='05'~ccaa_nombre[5], idccaa=='06'~ccaa_nombre[6], idccaa=='07'~ccaa_nombre[7], idccaa=='08'~ccaa_nombre[8], 
#                         idccaa=='09'~ccaa_nombre[9], idccaa=='10'~ccaa_nombre[10], idccaa=='11'~ccaa_nombre[11], idccaa=='12'~ccaa_nombre[12], 
#                         idccaa=='13'~ccaa_nombre[13], idccaa=='14'~ccaa_nombre[14], idccaa=='15'~ccaa_nombre[15], idccaa=='16'~ccaa_nombre[16], 
#                         idccaa=='17'~ccaa_nombre[17], idccaa=='18'~ccaa_nombre[18], idccaa=='19'~ccaa_nombre[19]))  
# 



##CARGA PUNTO GUARDADO 1####
df_gas <- read_rds('df_gas_v2.xlsx')



#INPUTS INICIALIZACION -----------------------------------------------------------####
municipio_seleccionado <-  NULL
combustible_seleccionado <- NULL


 

# if(!is.null(municipio_seleccionado) || !is.null(combustible_seleccionado)){
#   if(!is.null(municipio_seleccionado)){
#     df_gas <- df_gas %>%
#       filter(municipio==municipio_seleccionado)
# 
#   }
#   if(!is.null(combustible_seleccionado)){
#     df_gas <- df_gas %>%
#       select(!starts_with('precio'),contains(combustible_seleccionado)) %>%
#       select(!starts_with('media'),contains(combustible_seleccionado)) %>%
#       select(!starts_with('valoracion'),contains(combustible_seleccionado)) %>%
#       glimpse()
#   }
# 
#   df_gas %>%
#     leaflet() %>%
#     addProviderTiles('Esri') %>%
#     addMarkers(~longitud, ~latitud)
# 
# }else{
#   df_gas %>%
#     leaflet() %>%
#     addProviderTiles('Esri') %>%
#     fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))}
# 



#SHINY APP------------------------------------------------------------------------####
ui <- dashboardPage(
  dashboardHeader(title = 'titulo 1'),
  dashboardSidebar(
    sidebarMenu(id='sidebarMenuID',
                menuItem('CLIENTE',
                         icon = icon('glyphicon glyphicon-user'),
                         tabName = 'tab_cliente'),
                menuItem('INVERSOR',
                         icon = icon('glyphicon glyphicon-euro'),
                         tabName = 'tab_inversor')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'tab_cliente',
              fluidPage(
                titlePanel("Cliente"),
                sidebarLayout(
                  sidebarPanel(
                    textInput('municipio', 'Introduce munipio'),
                    selectInput('combustible',
                                'Selecciona combustible',
                                choices = list("Gasoleo A" = 'precio_gasoleo_a',
                                               "Gasoleo B" = 'precio_gasoleo_b',
                                               "Gasoleo Premium" = 'precio_gasoleo_premium',
                                               'Gasolina 95 e10'='precio_gasolina_95_e10',
                                               'Gasolina 95 e5'='precio_gasolina_95_e5',
                                               'Gasolina 98 e5'='precio_gasolina_98_e5')),
                    sliderInput("precio",
                                "Seleccion precio",
                                min =min(df_gas[,20], na.rm = TRUE),
                                max =max(df_gas[,20], na.rm = TRUE),
                                value =max(df_gas[,20], na.rm = TRUE)
                                ),
                    
                    checkboxInput('valoracion_precio',
                                  'Baratas',
                                  value = FALSE)
                    
      
                  ),
                  
                  mainPanel(
                    leafletOutput("df_gas_map")
                  )
                )
              )
      ),
      tabItem(tabName = 'tab_inversor')
    )
  )
)


#SERVIDOR ----------------------------------------------------------------####

server <- function(input, output, ShinySession) {
  
  output$df_gas_map <- renderLeaflet({
    
    
    if(!is.null(municipio_seleccionado()) || !is.null(combustible_seleccionado())){
      if(!is.null(municipio_seleccionado())){
        df_gas <- df_gas %>%
          filter(municipio==municipio_seleccionado())
          
        
      }
      if(!is.null(combustible_seleccionado())){
        df_gas <- df_gas %>%
          select(!starts_with('precio'),contains(combustible_seleccionado())) %>%
          select(!starts_with('media'),contains(combustible_seleccionado())) %>%
          select(!starts_with('valoracion'),contains(combustible_seleccionado())) %>% 
          filter(df_gas[,20]<=precio_seleccionado()) %>%
          # filter(df_gas[,22]=='no_low_cost') %>% 
          glimpse()
      }
      
      df_gas %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        addMarkers(~longitud, ~latitud)
      
    }else{
      df_gas %>%
        leaflet() %>%
        addProviderTiles('Esri') %>%
        fitBounds(~min(longitud), ~min(latitud), ~max(longitud), ~max(latitud))}
    
      
    })
  
   

##MUNICIPIO ---------------------------------------------------------------####

    simpleCap <- function(x) {
      x <- tolower(x)  
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
            sep="", collapse=" ")
    }
    
    municipio_seleccionado <- reactive({
      sapply(input$municipio, simpleCap)
    })
    
    
    # municipio_seleccionado <- reactive({
    #   df_gas %>%
    #     filter(municipio == sapply(input$municipio, simpleCap))
    # })


    # observeEvent(municipio_seleccionado(), {
    #   leafletProxy('df_gas_map', data =municipio_seleccionado()) %>%
    #     clearMarkers() %>%
    #     addMarkers(~longitud, ~latitud)
    # })
    
    
##COMBUSTIBLE -------------------------------------------------------------####
    
    
    combustible_seleccionado <- reactive({
      input$combustible
    })
    
   precio_seleccionado <- reactive({
      input$precio
    })
    
    # combustible_seleccionado <- reactive({
    #   df_gas %>%
    #     select(!starts_with('precio'),contains(input$combustible)) %>%
    #     select(!starts_with('media'),contains(input$combustible)) %>%
    #     select(!starts_with('valoracion'),contains(input$combustible))
    # 
    # })
    
    # 
    # observeEvent(combustible_seleccionado(), {
    #   leafletProxy('df_gas_map', data =combustible_seleccionado()) %>%
    #     clearMarkers() %>%
    #     addMarkers(~longitud, ~latitud)
    # })
    
    

##PRECIO COMBUSTIBLE ------------------------------------------------------####

    observeEvent(input$combustible, {
      updateSliderInput(inputId = "precio",
                        min =min(df_gas[,20], na.rm = TRUE),
                        max =max(df_gas[,20], na.rm = TRUE),
                        value =max(df_gas[,20], na.rm = TRUE)
                        )
    })
   

##BOTON BARATAS -----------------------------------------------------------####
  
   
   
   valoracion_precio <- reactive({})
   
}



shinyApp(ui,server)
