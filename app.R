library(shiny)
library(shinydashboard)
source('global.R')
#Codigo Java para colapsar todos los box

jscode <- "
shinyjs.expandBox = function(boxid) {
if (document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}};
shinyjs.collapseBox = function(boxid) {
if (!document.getElementById(boxid).parentElement.className.includes('collapsed-box')) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}}"



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
            
        ),
        #Mencionar las funciones Java para colapsar las box
        useShinyjs(),
        extendShinyjs(text = jscode,functions =c("toggleBox","expandBox","collapseBox"))
        
        
        
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
    graph1 <- as.data.frame(graph1[order(graph1$Licenciatarios),])
    gradient <- colorRampPalette(c("#1fa159","#1f90a1","#1f2aa1","#a11f30"))
    graph1$colors <- gradient(dim(graph1)[1])[as.numeric(cut(graph1$Licenciatarios,breaks = dim(graph1)[1]))]
    graph1
})

grafico2 <- reactive({
    graph2 <- nacional() %>% filter(`Estado Actual`=="Activa") %>% group_by(Provincia) %>% summarise(Licenciatarios=n())
    graph2 <- as.data.frame(graph2[order(graph2$Licenciatarios),])
    gradient <- colorRampPalette(c("#1fa159","#1f90a1","#1f2aa1","#a11f30"))
    graph2$colors <- gradient(dim(graph2)[1])[as.numeric(cut(graph2$Licenciatarios,breaks = dim(graph2)[1]))]
    graph2
    
})



tabla1 <- reactive({
    table1 <- nacional() %>% filter(`Estado Actual`=="Activa") %>% group_by(Provincia,`Tipo de licencia (1-7)`) %>% summarise(Licenciatarios=n())
    table1$label <- paste0("Licencia Tipo ",table1$`Tipo de licencia (1-7)`)
    table1 <- data.table(table1)
    
})

tabla456 <- reactive({
  table456 <- nacional() %>% filter(`Estado Actual`=="Inactiva") %>% group_by(Provincia,Estado) %>% summarise(Licenciatarios=n())  
  table456 <- as.data.frame(table456)
  table456
  })

grafico7 <- reactive({
    graph7 <- nacional() %>% filter(Provincia %in% input$provincias) %>% 
        group_by(Provincia,`Estado Actual`,`Tipo de licencia (1-7)`) %>% 
        summarise(Licenciatarios=n()) %>% 
        mutate(Tipo=paste0("Licencia Tipo ",`Tipo de licencia (1-7)`))
    graph7 <- as.data.frame(graph7)
    graph7
    
})

grafico8 <- reactive({
    graph8 <- nacional() %>% filter(Provincia %in% input$provincias) %>% 
        group_by(Provincia,`Estado Actual`,`Tipo de licencia (1-7)`) %>% 
        summarise(Hectáreas=sum(`Área Destinada`)) %>% 
        mutate(Tipo=paste0("Licencia Tipo ",`Tipo de licencia (1-7)`))
    graph8 <- as.data.frame(graph8)
    graph8
    
})


#Render de gráficos y tablas

output$plot1 <- renderPlotly({
   
    
    plot_ly(grafico1(), labels = ~Provincia, values = ~Licenciatarios, sort = TRUE,
            textinfo = 'label',marker = list(colors = ~colors),textfont = list(size = 9)) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Porcentaje Licenciatarios Nacional",
               legend = list(orientation = 'h'), 
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
       
})

output$plot2 <- renderPlotly({
    plot_ly(grafico2(), labels = ~Provincia, values = ~Licenciatarios, sort = TRUE,
        textinfo = 'label',marker = list(colors = ~colors),textfont = list(size = 9)) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Porcentaje Licenciatarios Nacionales Activos",
               legend = list(orientation = 'h'), 
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
})

output$plot3 <- renderPlot({
    
    ggplot(tabla1(), aes(label,Provincia)) + 
        geom_tile(aes(fill=Licenciatarios)) +
        geom_text(aes(label=Licenciatarios),colour="white")+
        ggtitle("Licencias Activas Por Tipo Y Provincia")+
        labs(x = "",y="") + 
        theme(plot.title = element_text(hjust = 0.5,size = 17,face = "bold"))+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle=90,hjust=1,vjust=1.0),
              axis.text.y = element_text(size = 10),
              panel.background = element_blank(),
              legend.position = "none")
    
   })


output$plot456 <- renderPlotly({
    
    a <- ggplot(tabla456(),aes(x =Provincia,y = Licenciatarios ,fill = Estado)) + 
        geom_bar(stat = "identity",position = position_dodge())+
        labs(x = "") +
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position = "top")+
        # scale_x_discrete(limits = month.abb) +
        geom_text(aes(label = as.numeric(Licenciatarios)),
                  position = position_dodge(width = 0.9), 
                  hjust=100,
                  size = 3)+
        scale_fill_manual("", values = hcl.colors(3,palette = "TealGrn",rev = F))
    
    ggplotly(a,tooltip = c("Provincia","Licenciatarios","Estado")) %>%  layout(legend = list(orientation = "h", x = 0, y = 10))

})

output$plot7 <- renderPlotly({
    
    if (!is.null(input$provincias)) {
        a <- ggplot(grafico7(),aes(x =Tipo,y = Licenciatarios ,fill = `Estado Actual`)) + 
            geom_bar(stat = "identity",position = position_dodge())+
            labs(x = "") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position = "top")+
            
            geom_text(aes(label = as.numeric(Licenciatarios)),
                      position = position_dodge(width = 0.9), 
                      hjust=100,
                      size = 3)+
            scale_fill_manual("", values = hcl.colors(3,palette = "TealGrn",rev = F))+
            facet_grid(~Provincia)+
            facet_wrap(~Provincia,ncol = 2 ,nrow = 12)
            
        ggplotly(a,tooltip = c("Estado Actual","Licenciatarios")) %>%  layout(legend = list(orientation = "v", x = 10, y = 0))
        
    }
    
})

output$plot8 <- renderPlotly({
    
    if (!is.null(input$provincias)) {
        a <- ggplot(grafico8(),aes(x =Tipo,y = Hectáreas ,fill = `Estado Actual`)) + 
            geom_bar(stat = "identity",position = position_dodge())+
            labs(x = "") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position = "top")+
            
            geom_text(aes(label = as.numeric(Hectáreas)),
                      position = position_dodge(width = 0.9), 
                      hjust=100,
                      size = 3)+
            scale_fill_manual("", values = hcl.colors(6,palette = "Earth",rev = T))+
            facet_grid(~Provincia)+
            facet_wrap(~Provincia,ncol = 2 ,nrow = 12)
        
        ggplotly(a,tooltip = c("Estado Actual","Hectáreas")) %>%  layout(legend = list(orientation = "v", x = 10, y = 0))
        
    }
    
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
                    selectizeInput("provincias","Provincias:",choices=NULL,size=7,multiple=TRUE,width = 200),
                    selectizeInput("cantones","Cantones:",choices=NULL,size=7,multiple=TRUE,width = 200)
                )
                
            }
            
        }
        
    })
    
    
    output$u_i3 <- renderUI({   #ui de box de acuerdo a opcion de filtro
        if(!is.null(input$filtro)){
            if (input$filtro!="Nacional") {
                fluidRow(
                    box(id="box1",
                        title = "Licenciatarios Por Estado Y Tipo De Licencia",status = 'success',
                        plotlyOutput("plot7", height = 700),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE),
                    box(id="box2",
                        title = "Superficie Destinada Por Estado Y Tipo De Licencia",status = "warning",
                        plotlyOutput("plot8", height = 500),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE),
                    box(id="box3",
                        title = "Licencias Inactivas Por Provincia",status = 'info',width = 12,
                        plotlyOutput("plot9", height = 700),solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE)
                    
                ) 
            }else{
                fluidPage(
                    
                    fluidRow(
                        box(
                            title = "Licenciatarios Por Provincia",status = 'success',
                            plotlyOutput("plot1", height = 600),solidHeader = TRUE,collapsible = TRUE,width = 4),
                        box(
                            title = "Licencias Activas",status = 'success',solidHeader = TRUE,collapsible = TRUE,width = 8,
                            fluidRow(
                                column(plotlyOutput("plot2",height = 600),width = 6),
                                column(plotOutput("plot3",height = 600),width = 6))),    
                    fluidRow(
                            box(
                                title = "Licencias Inactivas Por Estado",status = 'success',solidHeader = TRUE,collapsible = TRUE,width = 12,
                                fluidRow(
                                    column(plotlyOutput("plot456",height = 600),width = 12)))
                            )
                    )
                    
                )
                
            }
            
        }
    })
    
# Observe de cantones
    
    observeEvent(input$filtro,{
        if(input$filtro=="Provincias"){
            updateSelectizeInput(session,'provincias',
                                 choices=unique(nacional()$Provincia))
        }
    })
    
    observeEvent(input$provincias,{
        updateSelectizeInput(session,'cantones',
                             choices=sort(unique(datos$Cantón[datos$Provincia %in% input$provincias])))
    })
    #Observe de colpaso de boxes
    observeEvent(input$provincias,{
        if(!is.null(input$provincias))
            {
            
            js$expandBox("box1")    
            
            
        }else{
            js$collapseBox("box1")    
        }
             })
    
    observeEvent(input$provincias,{
        if(!is.null(input$provincias))
        {
            
            js$expandBox("box2")    
            
            
        }else{
            js$collapseBox("box2")    
        }
    })
    observeEvent(input$provincias,{
        if(!is.null(input$provincias))
        {
            
            js$expandBox("box3")    
            
            
        }else{
            js$collapseBox("box3")    
        }
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
