library(shiny)
library(shinydashboard)
source('global.R')


ui <- dashboardPage(
    dashboardHeader(title = HTML("<div style>
                                  <img src = ' https://iili.io/J0gVXp.png' width='150' height = '70'  >
                                  </div>")),
    dashboardSidebar(
        sidebarMenu(id = "menu",
                    menuItem("INDICADORES", tabName = "indicadores",icon = icon("fas fa-tachometer-alt")),
                    menuItem("BUSCADOR", tabName = "buscador", icon = icon("search")),
                    menuItem("ACERCA DE",tabName = "acerca",icon = icon("question"), selected = TRUE)),
        hr(),
        uiOutput("u_i1")
        
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
                          /* Imagen de header */
                              .skin-blue .main-header .navbar {
                              height: 100px;
                              background-image:url("https://iili.io/J0cEDQ.png?format=1w");
                              background-size: 1700px 100px;
                              background-repeat: no-repeat;
                              
                               }

                         /* color del logo */
                               .skin-blue .main-header .logo {
                                background-color: #222d32;
                                height: 100px;
                                }

                        /* color del logo con cursor */
                                .skin-blue .main-header .logo:hover {
                                background-color:  #222d32;
                                }

                        /* color de botón menú  */
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #d5d6da;
                               }

                        /* color de sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #222d32;
                                top: 48px;
                                
                                }
                                
                        /* body */
                                .content-wrapper .right-side {
                                background-color: #ffffff;
                                }
                        /* letra de box */
                        
                                .box-header h3.box-title {
                                 font-weight: bold;
                                 text-align: center;
                                }
                                
                                
                                /* tamaño de gauge */
                                       .html-widget.gauge svg {
                                        height: 300px;
                                        width:  400px;
                                        }
                                  '))),
        
        tabItems(
            tabItem(tabName = "indicadores",
                    fluidRow(
                        valueBoxOutput("total_lic"),
                        valueBoxOutput("lic_activos"),
                        valueBoxOutput("lic_inactivos")    
                            ),
                    uiOutput("u_i3")
                    ),
            tabItem(tabName = "buscador",
                    h2("buscador")),
            tabItem(tabName = "acerca",
                    h2("acerca"))
            
        )
        
        
        
    )
)


server <- function(input, output,session) {

#Reactives de tablas y graficos
nacional <- reactive({
    tabla1 <- datos
    tabla1
})

grafico1 <- reactive({
    graph1 <- nacional() %>% group_by(Provincia) %>% summarise(Licenciatarios=n())
    graph1 <- as.data.frame(graph1)
    graph1
})

grafico2 <- reactive({
    graph2 <- nacional() %>% filter(`Estado Actual`=="Activa") %>% group_by(Provincia) %>% summarise(Licenciatarios=n())
    graph2 <- as.data.frame(graph2)
    graph2
})


tabla1 <- reactive({
    table1 <- datos %>% group_by(Provincia,`Tipo de licencia (1-7)`) %>% summarise(Licenciatarios=n()) %>% 
})

#Render de gráficos

output$plot1 <- renderPlotly({
    plot_ly(grafico1(), labels = ~Provincia, values = ~Licenciatarios, sort = F) %>%
        add_pie(hole = 0.3) %>%
        layout(title = "Porcentaje Licenciatarios Nacional",
               legend = list(orientation = 'h'), 
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})

output$plot2 <- renderPlotly({
    plot_ly(grafico2(), labels = ~Provincia, values = ~Licenciatarios, sort = F) %>%
        add_pie(hole = 0.3) %>%
        layout(title = "Porcentaje Licenciatarios Nacionales Activos",
               legend = list(orientation = 'h'), 
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})


    
    
#Desarrollo de Ui's
    
    output$u_i1<- renderUI({        #ui de menu indicadores o buscador
        if (input$menu=="indicadores") {
            fluidPage(
                style = "position: fixed; overflow: visible;",
                h5(icon("dashboard"),"Controles"),
                radioButtons("filtro","Filtros:",choices = c("Nacional"="Nacional","Provincias"="Provincias"),inline = TRUE,selected = character(0)),
                uiOutput("u_i2"),
                br()
            )
        } else{
            if (input$menu=="buscador") {
                fluidPage(
                    textInput("nombre","Nombres:"),
                    textInput("apellido","Apellidos:"),
                )
            }
        }
    })
    
    output$u_i2 <- renderUI({   #ui de filtro
        if (!is.null(input$filtro)) {
            if (input$filtro!="Nacional") {
                fluidPage(
                    selectizeInput("provincias","Provincias:",choices=unique(nacional()$Provincia),size=7,multiple=TRUE,width = 200),
                    selectizeInput("cantones","Cantones:",choices=NULL,size=7,multiple=TRUE,width = 200)
                )
                
            }
            
        }
        
    })
    
    
    output$u_i3 <- renderUI({   #ui de box de acuerdo a opcion de filtro
        if(!is.null(input$filtro)){
            if (input$filtro!="Nacional") {
                fluidRow(
                    box(
                        title = "Licencias Por Tipo Y Provincia",status = 'success',
                        plotlyOutput("plot3", height = 500),solidHeader = TRUE,collapsible = TRUE),
                    box(
                        title = "Licencias Inactivas Por Provincia",status = 'success',
                        plotlyOutput("plot4", height = 500),solidHeader = TRUE,collapsible = TRUE),
                    box(
                        title = "Superficie Por Provincia Y Tipo De Licencia",status = 'primary',width = 12,
                        plotlyOutput("plot5", height = 700),solidHeader = TRUE,collapsible = TRUE)
                    
                ) 
            }else{
                fluidRow(
                    box(
                        title = "Licenciatarios Por Provincia",status = 'success',
                        plotlyOutput("plot1", height = 700),solidHeader = TRUE,collapsible = TRUE,width = 4),
                    box(
                        title = "Licencias Activas",status = 'success',solidHeader = TRUE,collapsible = TRUE,width = 8,
                        fluidRow(
                            plotlyOutput("plot2",height = 700)
                        ))
                )    
            }
            
        }
    })
    
# Observe de cantones
    
    
    observeEvent(input$provincias,{
        updateSelectizeInput(session,'cantones',
                             choices=sort(unique(datos$Cantón[datos$Provincia %in% input$provincias])))
    })
    
#Render de Valuebox
    
    output$total_lic <- renderValueBox({
        if(!is.null(input$filtro)){
            if(input$filtro!="Nacional"){
                if (!is.null(input$cantones)) {
                    nacional() %>% filter(Provincia %in% input$provincias & Cantón %in% input$cantones) %>%  tally() %>% 
                        valueBox(subtitle = h4("Total De Licenciatarios"),icon = icon("universal-access"),color = "blue")    
                }else{
                    nacional() %>% filter(Provincia %in% input$provincias) %>%  tally() %>% 
                        valueBox(subtitle = h4("Total De Licenciatarios"),icon = icon("universal-access"),color = "blue")
                }
                
            }else{
                nacional() %>% tally() %>% 
                    valueBox(subtitle = h4("Total De Licenciatarios"),icon = icon("universal-access"),color = "blue") 
            }
        }else{
            valueBox(0,subtitle = h4("Total De Licenciatarios"),icon = icon("universal-access"),color = "blue") 
        }
            
        
    })
    
    
    output$lic_activos <- renderValueBox({
        if(!is.null(input$filtro)){
            if(input$filtro!="Nacional"){
                if (!is.null(input$cantones)) {
                    nacional() %>% filter(Provincia %in% input$provincias & Cantón %in% input$cantones & `Estado Actual`=="Activa") %>%  tally() %>% 
                        valueBox(subtitle = h4("Licenciatarios Activos"),icon = icon("fas fa-check-circle"),color = "light-blue")    
                }else{
                    nacional() %>% filter(Provincia %in% input$provincias & `Estado Actual`=="Activa") %>%  tally() %>% 
                        valueBox(subtitle = h4("Licenciatarios Activos"),icon = icon("fas fa-check-circle"),color = "light-blue") 
                }
                
            }else{
                nacional() %>% filter(`Estado Actual`=="Activa") %>% tally()  %>% 
                    valueBox(subtitle = h4("Licenciatarios Activos"),icon = icon("fas fa-check-circle"),color = "light-blue")
            }
        }else{
            valueBox(0,subtitle = h4("Licenciatarios Activos"),icon = icon("fas fa-check-circle"),color = "light-blue")
        }
    })
    
    output$lic_inactivos <- renderValueBox({
        if(!is.null(input$filtro)){
            if(input$filtro!="Nacional"){
                if (!is.null(input$cantones)) {
                    nacional() %>% filter(Provincia %in% input$provincias & Cantón %in% input$cantones & `Estado Actual`=="Inactiva") %>%  tally() %>% 
                        valueBox(subtitle = h4("Licenciatarios Inactivos"),icon = icon("far fa-times-circle"),color = "teal")    
                }else{
                    nacional() %>% filter(Provincia %in% input$provincias & `Estado Actual`=="Inactiva") %>%  tally() %>% 
                        valueBox(subtitle = h4("Licenciatarios Inactivos"),icon = icon("far fa-times-circle"),color = "teal")     
                }
                
            }else{
                nacional() %>% filter(`Estado Actual`=="Inactiva") %>% tally()  %>% 
                    valueBox(subtitle = h4("Licenciatarios Inactivos"),icon = icon("far fa-times-circle"),color = "teal")    
            }
        }else{
            valueBox(0,subtitle = h4("Licenciatarios Inactivos"),icon = icon("far fa-times-circle"),color = "teal")    
        }
        
    })
    
    
    
}

shinyApp(ui = ui, server = server)
