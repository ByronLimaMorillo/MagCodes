library(shiny)
library(shinydashboard)
source('global.R')
# COMPROBACION DE UN NUMERO NATURAL
# testInteger <- function(x){
#   test <- all.equal(x, as.integer(x), check.attributes = FALSE)
#   a <- x>0
#   w <- unique(a)
#   if(length(w)==2){
#     j <- FALSE
#   }else{
#     j <- TRUE
#   }
#   if(test == TRUE & j== TRUE ){ return(TRUE) }
#   else { return(FALSE) }
# }


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



ui <- dashboardPage(title="Cáñamo-MAG",
    dashboardHeader(

      title = HTML("<div style>
                                  <img src = 'https://iili.io/K1d1J1.png' width='160' height = '90'  >
                                  </div>")),
    dashboardSidebar(
        sidebarMenu(id = "menu",
                    menuItem("REPORTEADOR", tabName = "indicadores",icon = icon("fas fa-chart-bar")),
                    menuItem("BUSCADOR", tabName = "buscador", icon = icon("search")),
                    menuItem("ACERCA DE",tabName = "acerca",icon = icon("question"), selected = TRUE)),
        hr(),
        uiOutput("u_i1")
        
    ),
    dashboardBody(
      #Icono en tab de browser
      tags$link(rel = "icon", type = "image/gif", href = "https://iili.io/K12u2I.png"),
      
      # Letras de error color blanco
      tags$head(tags$style(".shiny-output-error{color: white;}")),
      
      
      #Estilo de numeric input (cedula) como caja de texto
      tags$style(HTML("
                                       input[type=number] {
                                       -moz-appearance:textfield;
                                       }
                                       input[type=number]::{
                                       -moz-appearance:textfield;
                                       }
                                       input[type=number]::-webkit-outer-spin-button,
                                       input[type=number]::-webkit-inner-spin-button {
                                       -webkit-appearance: none;
                                       margin: 0;
                                       }
                                       ")),
        
        tags$head(tags$style(HTML('
                           
          .form-control {
            border-radius: 4px 4px 4px 4px;
        }

        

        
                          
                          
                          /* Imagen de header */
                              .skin-blue .main-header .navbar {
                              height: 100px;
                              background-image:url("https://iili.io/K1dc0B.jpg");
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
                    fluidRow(
                      uiOutput("u_i5"),
                      
                    )
                    ),
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

grafico9 <- reactive({
    graph9 <- nacional() %>% filter(Cantón %in% input$cantones) %>% 
        group_by(Cantón,`Estado Actual`,`Tipo de licencia (1-7)`) %>% 
        summarise(Licenciatarios=n()) %>% 
        mutate(Tipo=paste0("Licencia Tipo ",`Tipo de licencia (1-7)`))
    graph9 <- as.data.frame(graph9)
    graph9
})


grafico10 <- reactive({
    graph10 <- nacional() %>% filter(Cantón %in% input$cantones) %>% 
        group_by(Cantón,`Estado Actual`,`Tipo de licencia (1-7)`) %>% 
        summarise(Hectáreas=sum(`Área Destinada`)) %>% 
        mutate(Tipo=paste0("Licencia Tipo ",`Tipo de licencia (1-7)`))
    graph10 <- as.data.frame(graph10)
    graph10
})


#Reactive para validación de cédula
dni <- reactive({
  validate(need(is.numeric(input$cedula),"             Ingrese solo números"))})
output$mensaje1 <- renderText({dni()})

#Reactive para tabla consulta de licenciatarios

consulta <- reactive({
    search <- nacional() %>% filter(grepl(input$nombres,`Nombre, razón social`, ignore.case = TRUE)) %>% select(`Nombre, razón social`,Provincia,Cantón,`Estado Actual`,`Tipo de licencia (1-7)`,`Número de licencia`,Estado)
    search <- as.data.frame(search)
    search  
  
})

consulta_cedula <- reactive({
  search_cedula <- nacional() %>% filter(RUC==input$cedula) %>% select(`Nombre, razón social`,Provincia,Cantón,`Estado Actual`,`Tipo de licencia (1-7)`,`Número de licencia`,Estado,RUC)
  search_cedula <- as.data.frame(search_cedula)
  search_cedula
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

output$plot9 <- renderPlotly({
    
    if (!is.null(input$cantones)) {
        a <- ggplot(grafico9(),aes(x =Tipo,y = Licenciatarios ,fill = `Estado Actual`)) + 
            geom_bar(stat = "identity",position = position_dodge())+
            labs(x = "") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position = "top")+
            
            geom_text(aes(label = as.numeric(Licenciatarios)),
                      position = position_dodge(width = 0.9), 
                      hjust=100,
                      size = 3)+
            scale_fill_manual("", values = hcl.colors(3,palette = "TealGrn",rev = F))+
            facet_grid(~Cantón)+
            facet_wrap(~Cantón,ncol = 2 ,nrow = 12)
        
        ggplotly(a,tooltip = c("Estado Actual","Licenciatarios")) %>%  layout(legend = list(orientation = "v", x = 10, y = 0))
        
    }
    
})

output$plot10 <- renderPlotly({
    
    if (!is.null(input$cantones)) {
        a <- ggplot(grafico10(),aes(x =Tipo,y = Hectáreas ,fill = `Estado Actual`)) + 
            geom_bar(stat = "identity",position = position_dodge())+
            labs(x = "") +
            theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),legend.position = "top")+
            
            geom_text(aes(label = as.numeric(Hectáreas)),
                      position = position_dodge(width = 0.9), 
                      hjust=100,
                      size = 3)+
            scale_fill_manual("", values = hcl.colors(6,palette = "Earth",rev = T))+
            facet_grid(~Cantón)+
            facet_wrap(~Cantón,ncol = 2 ,nrow = 12)
        
        ggplotly(a,tooltip = c("Estado Actual","Hectáreas")) %>%  layout(legend = list(orientation = "v", x = 10, y = 0))
        
    }
    
})

output$busqueda <- renderDataTable({
  if(input$nombres!=""){
  datatable(select(consulta(),1:5), escape=FALSE,rownames = FALSE,style = "bootstrap4",selection = "single",options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:4))
  ))
  }
})   
    
#Desarrollo de Ui's

 
    
output$u_i1<- renderUI({        #UI de menú con radiobuttons
        if (input$menu=="indicadores") {
            fluidPage(
                style = "position: fixed; overflow: visible;",
                h5(icon("fas fa-chart-bar"),"Reporteador:"),
                radioButtons("filtro","Filtros:",choices = c("Nacional"="Nacional","Provincial"="Provincias","Cantonal"="Cantones"),inline = FALSE,selected = character(0)),
                uiOutput("u_i2"),
                br()
            )
        } else{
            if (input$menu=="buscador") {
                fluidPage(
                  h5(icon("fas fa-search"),"Buscador:"),
                  style = "position: fixed; overflow: visible;",
                  radioButtons("filtro2","Búsqueda Por:",choices = c("Nombres"="Nombres","Cédula"="Cedula"),inline = FALSE,selected = character(0)),
                    uiOutput("u_i4")
                    
                )
            }
        }
    })
    
    output$u_i2 <- renderUI({   #UI de filtro de nacional, provincial y cantonal (Reporteador)
        if (!is.null(input$filtro)) {
            if (input$filtro!="Nacional") {
                if(input$filtro!="Provincias"){
                    fluidPage(
                        selectizeInput("provincias","Provincias:",choices=NULL,size=7,multiple=TRUE,width = 200),
                        selectizeInput("cantones","Cantones:",choices=NULL,size=7,multiple=TRUE,width = 200)
                    )    
                }else{
                    fluidPage(
                        selectizeInput("provincias","Provincias:",choices=NULL,size=7,multiple=TRUE,width = 200),
                        
                    )    
                }
                
                
            }
            
        }
        
    })
    
    
    output$u_i3 <- renderUI({   #UI ligado con ui2 para mostrar gráficos de Menú Reporteador
        if(!is.null(input$filtro)){
            if (input$filtro!="Nacional") {
                if (input$filtro!="Provincias") {
                    fluidRow(
                        box(id="box3",
                            title = "Licenciatarios Por Estado Y Tipo De Licencia",status = 'warning',
                            plotlyOutput("plot9", height = 700),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE),
                        box(id="box4",
                            title = "Superficie Destinada Por Estado Y Tipo De Licencia",status = "info",
                            plotlyOutput("plot10", height = 500),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE)
                       
                        
                    )  
                }else{
                    fluidRow(
                        box(id="box1",
                            title = "Licenciatarios Por Estado Y Tipo De Licencia",status = 'warning',
                            plotlyOutput("plot7", height = 700),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE),
                        box(id="box2",
                            title = "Superficie Destinada Por Estado Y Tipo De Licencia",status = "info",
                            plotlyOutput("plot8", height = 500),solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE)
                        
                        
                    )   
                }
               
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
    
    output$u_i4 <- renderUI({  #UI de filtro de cedula y nombres (Buscador)
      if (!is.null(input$filtro2)) {
        if (input$filtro2!="Nombres") {
          
            fluidPage(
              numericInput("cedula","Cédula:",min = 0,value = NULL,width = 200),
              textOutput("mensaje1")
            )    
          
        }else{
          textInput("nombres","Nombres/Apellidos:",value = NULL,width = 220)
          
        }
        
      }
      
      
    })
    
    
    
    
    output$u_i5 <- renderUI({   #UI ligado con ui4 para mostrar consulta de nombre y cedula ligado a UI3
      
      if(!is.null(input$filtro2)){
        if (input$filtro2!="Nombres") {
          
          fluidRow(
            box(title = "Búsqueda De Licenciatarios Por Cédula",status = 'success',solidHeader = TRUE,collapsible = FALSE,width = 12,
              splitLayout(
                fluidPage(
                  box(title = "Datos De Licenciatario",status = "warning",solidHeader = FALSE,collapsible = FALSE,width = 10)
                ),
                
                  box(title = "Detalle De Licencia",status = "warning",solidHeader = FALSE,collapsible = FALSE,width = 12,
                      verticalLayout(
                        
                        column(valueBoxOutput("num_licencia2"),offset = 2,width = 12),
                        column(valueBoxOutput("estado_licencia2"),offset = 2,width = 12),  
                        column(valueBoxOutput("tipo_licencia2"),offset = 2,width = 12),
                        column(valueBoxOutput("inac_licencia2"),offset = 2,width = 12 )
                  )
                    
                  
                  
                )
                
              )
              
              
            )
            
            
          )
        }else{
          fluidRow(
                  box(
                    title = "Busqueda De Licenciatarios Por Nombres",status = 'success',solidHeader = TRUE,collapsible = FALSE,width = 12,
                    uiOutput("u_i6"),
                    column(dataTableOutput("busqueda"),width = 12))

                  )
          
        }
        
        }
      
      
    })
    
    
    output$u_i6 <- renderUI({  # UI ligado a UI5 para mostrar el detalle de la busqueda por nombre
      fila <- input$busqueda_rows_selected
      if (length(fila)) {
        box(
          title = "Detalle De Licencia",status = "success",solidHeader = FALSE,collapsible = TRUE,width = 12,
          valueBoxOutput("num_licencia"),
          valueBoxOutput("estado_licencia"),
          valueBoxOutput("tipo_licencia"),
          column(valueBoxOutput("inac_licencia"),width = 11,offset = 4)    
        )
      }
      
    })
    
    
  
    
    
# Observe de cantones
    
    observeEvent(input$filtro,{
            
        if(input$filtro=="Provincias" |input$filtro=="Cantones"){
            
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
    observeEvent(input$cantones,{
        if(!is.null(input$cantones))
        {
            
            js$expandBox("box3")    
            
            
        }else{
            js$collapseBox("box3")    
        }
    })
    
    observeEvent(input$cantones,{
        if(!is.null(input$cantones))
        {
            
            js$expandBox("box4")    
            
            
        }else{
            js$collapseBox("box4")    
        }
    })
  
#Observe para validación de cédula
    
    observeEvent(input$cedula, {
      updateNumericInput(session,"cedula", value = ({
        if(!is.numeric(input$cedula))
        {""}
        else if(!(is.null(input$cedula) || is.na(input$cedula))){
          if(input$cedula < 0){
            ""
          }else{
            if(input$cedula>as.numeric(2499999999)){
            ""  
            }else{
              return (input$cedula)
            }
            
          }
        }
        else{1}
      })
      )
    })
    
#Render de Valuebox
    
    output$total_lic <- renderValueBox({
        if(!is.null(input$filtro)){
            if(input$filtro!="Nacional"){
                if (input$filtro!="Provincias") {
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
                if (input$filtro!="Provincias") {
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
                if (input$filtro!="Provincias") {
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
    
    
    output$num_licencia <- renderValueBox({
      valueBox(consulta()$`Número de licencia`[input$busqueda_rows_selected],subtitle = h4("Número De Licencia"),icon = icon("far fa-hashtag"),color = "yellow") 
    })
    
    output$estado_licencia <- renderValueBox({
      if (consulta()$`Estado Actual`[input$busqueda_rows_selected]=="Activa") {
        valueBox(consulta()$`Estado Actual`[input$busqueda_rows_selected],subtitle = h4("Estado De Licencia"),icon = icon("far fa-id-badge"),color = "green") 
      }else{
        valueBox(consulta()$`Estado Actual`[input$busqueda_rows_selected],subtitle = h4("Estado De Licencia"),icon = icon("far fa-ban"),color = "red")   
      }
      
    })
    
    output$tipo_licencia <- renderValueBox({
      valueBox(consulta()$`Tipo de licencia (1-7)`[input$busqueda_rows_selected],subtitle = h4("Tipo De Licencia"),icon = icon("far fa-chalkboard-teacher"),color = "teal") 
    })
    
    output$inac_licencia <- renderValueBox({
      
        
          if (consulta()$Estado[input$busqueda_rows_selected]!="Activa") {
            valueBox(consulta()$Estado[input$busqueda_rows_selected],subtitle = h4("Tipo De Inactividad"),icon = icon("far fa-chalkboard-teacher"),color = "maroon")   
          }    
          
    })
    
    output$num_licencia2 <- renderValueBox({
      if (!is.null(input$cedula)) {
        if (!is_empty(consulta_cedula()$`Número de licencia`)) {
        valueBox(consulta_cedula()$`Número de licencia`,subtitle = h4("Número De Licencia"),icon = icon("far fa-hashtag"),color = "yellow")   
      }else{
        valueBox("Sin Número",subtitle = h4("Número De Licencia"),icon = icon("far fa-hashtag"),color = "yellow")   
      }}
      
      
    })
    
    output$estado_licencia2 <- renderValueBox({
      if (!is.null(input$cedula)) {
        if (!is_empty(consulta_cedula()$`Estado Actual`)) {
          if (consulta_cedula()$`Estado Actual`=="Activa") {
            valueBox(consulta_cedula()$`Estado Actual`,subtitle = h4("Estado De Licencia"),icon = icon("far fa-id-badge"),color = "green") 
          }else{
            valueBox(consulta_cedula()$`Estado Actual`,subtitle = h4("Estado De Licencia"),icon = icon("far fa-ban"),color = "red")   
          }  
        }else{
          valueBox("Sin Estado",subtitle = h4("Estado De Licencia"),icon = icon("far fa-ban"),color = "red")   
        }  
        }
        
    })
    
    output$tipo_licencia2 <- renderValueBox({
      if (!is.null(input$cedula)) {
        if (!is_empty(consulta_cedula()$`Tipo de licencia (1-7)`)) {
      valueBox(consulta_cedula()$`Tipo de licencia (1-7)`,subtitle = h4("Tipo De Licencia"),icon = icon("far fa-chalkboard-teacher"),color = "teal") 
        }else{
          valueBox("Sin Tipo",subtitle = h4("Tipo De Licencia"),icon = icon("far fa-chalkboard-teacher"),color = "teal")
        }}
          })
    
    output$inac_licencia2<- renderValueBox({
      if (!is.null(input$cedula)) {
        if (!is_empty(consulta_cedula()$Estado)) {
          if (consulta_cedula()$Estado!="Activa") {
            valueBox(consulta_cedula()$Estado,subtitle = h4("Tipo De Inactividad"),icon = icon("far fa-chalkboard-teacher"),color = "maroon") 
          } 
      }
      }
         
        
    })
    
    
    
}

shinyApp(ui = ui, server = server)
