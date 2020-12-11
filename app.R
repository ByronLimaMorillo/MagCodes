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
                        valueBoxOutput("lic_activa"),
                        valueBoxOutput("lic_inactiva")    
                            )
                    
                    
                    ),
            tabItem(tabName = "buscador",
                    h2("buscador")),
            tabItem(tabName = "acerca",
                    h2("acerca"))
            
        )
        
        
        
    )
)


server <- function(input, output,session) {

    #Reactives de tablas 
nacional <- reactive({
    tabla1 <- datos
    tabla1
})

# provincias <- reactive({
#     prov <- datos %>% filter(Provincias %in% input$provincias)
# })
    
    
    
    
#Desarrollo de Ui's
    
    output$u_i1<- renderUI({
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
    
    output$u_i2 <- renderUI({
        if (!is.null(input$filtro)) {
            if (input$filtro!="Nacional") {
                fluidPage(
                    selectizeInput("provincias","Provincias:",choices=unique(nacional()$Provincia),size=7,multiple=TRUE,width = 200),
                    selectizeInput("cantones","Cantones:",choices=NULL,size=7,multiple=TRUE,width = 200)
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
        }else
            valueBox(0,subtitle = h4("Total De Licenciatarios"),icon = icon("universal-access"),color = "blue") 
        
    })
    
    
    
}

shinyApp(ui = ui, server = server)
