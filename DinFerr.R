library(shiny); library(shinyWidgets); library(shinydashboard); library(DT); library(dplyr)
library(ggplot2); library(data.table); library(extrafont)

ui <- 
  dashboardPage(skin = "red",
                dashboardHeader(title = "DinFerr"),
                dashboardSidebar(
                  withMathJax(),
                  tags$h5(tags$strong('Características da Via'), align='center'),
                  sliderInput('i','Rampa: \\(i\\) (\\(m/100 m\\))',0,4,0,0.1),
                  sliderInput('r','Raio de curvatura: \\(r\\) (\\(m\\))',50,5000,3000,10),
                  sliderInput('f_a','Coeficiente de aderência: \\(f_a\\)',0,0.33,0.22,0.01)
                ),
                
                dashboardBody(
                  withMathJax(),
                  tags$head(tags$style(HTML('
        .form-group, .selectize-control {
             padding-top: 0px;
             margin-bottom: 10px;
        }
        .box-body {
            padding-top: 0px;
            padding-bottom: 0px;
        }'))),
                  
                  # FluidRow para tabela de paineis e gráfico (Início)
                  fluidRow(
                    
                    tabBox(
                      title = '', id = 'tabbox', height = 400, width = 6,
                      
                      # TabPanel Locomotiva (Início)
                      tabPanel(
                        tags$h5('Locomotiva', align = 'center'),
                        tags$table(style="width:100%",
                                   tags$tr(width = "50%",
                                           tags$td(width = "50%",
                                                   sliderInput('P','Potência nominal: \\(P\\) (\\(kW\\))',800,3000,2800,20, width = '95%')),
                                           tags$td(width = "50%",
                                                   sliderInput('eta','Eficiência da Transmissão: \\(\\eta\\)',0.7,0.95,0.82,0.01, width = '95%'))
                                   )
                        ),
                        tags$table(style="width:100%",
                                   tags$tr(width = "50%",
                                           tags$td(width = "50%",
                                                   sliderInput('G_l','Peso: \\(G_l\\) (\\(kN\\))',1200,2100,1800,100, width = '95%')),
                                           tags$td(width = "50%",
                                                   sliderInput('A_l','Área Frontal: \\(A_l\\) (\\(m^2\\))',10,15,12,0.1, width = '95%'))
                                   )
                        ),
                        tags$table(style="width:100%",
                                   tags$tr(width = "70%",align = 'center',
                                           tags$td(width = "15%", numericInput('x_l','Número de eixos: \\(x_l\\)',6,4,8,2,width = '60%')),
                                           tags$td(width = "15%", numericInput('VMOC','VMOC (\\(km/h\\))',18,17,20,1,width = '60%'))
                                   )
                        )
                        
                      ), #TabPanel Locomotiva (Fim)
                      
                      # TabPanel Vagão (Início)
                      tabPanel(
                        tags$h5('Vagão', align = 'center'),
                        tags$table(style="width:100%",
                                   tags$tr(width = "50%",
                                           tags$td(width = "50%",
                                                   sliderInput('n_v','Número de Vagões',0,50,30,1, width = '95%')),
                                           tags$td(width = "50%",
                                                   sliderInput('G_v','Peso Bruto: \\(G_l\\) (\\(kN\\))',800,1500,1000,100, width = '95%'))
                                   )
                        ),
                        tags$table(style="width:100%",
                                   tags$tr(width = "50%",
                                           tags$td(width = "50%",align = 'center',
                                                   numericInput('x_v','Número de eixos: \\(x_v\\)',2,4,8,2,width = '60%')),
                                           tags$td(width = "50%",
                                                   sliderInput('A_v','Área Frontal: \\(A_v\\) (\\(m^2\\))',7,13,9,0.1, width = '95%'))
                                   )
                        )
                        
                      ) #TabPanel Vagão (Fim)
                      
                    ), #tabBox
                    
                    column(width = 6,
                           plotOutput('diagr')
                    )
                  ), # FluidRow para tabela de paineis e gráfico (Fim)
                  
                  # FluidRow para valores importantes (Início)
                  fluidRow(
                    valueBoxOutput("F_max"),
                    valueBoxOutput("vmin"),
                    valueBoxOutput("veq")
                    
                  ) # FluidRow para valores importantes (Fim)
                  
                ) #dashboardBody
  ) #dashboardPage



server <- function(input, output) {
  
  F_VMOC <- reactive({
    input$eta*3.6*input$P/input$VMOC
  })
  
  F_max <- reactive({
    input$f_a*input$G_l
  })
  
  v_min <- reactive({
    3.6*input$eta*input$P/F_max()
  })
  
  F_90 <- reactive({
    input$eta*3.6*input$P/90
  })
  
  v <- reactive({
    v1 <- seq(0, input$VMOC, length.out = 20)
    v2 <- seq(input$VMOC, v_min(), length.out = 20)
    v3 <- seq(v_min() + 0.5, 90, length.out = 100)
    v4 <- seq(90 + 0.5, 100, length.out = 20)
    c(v1,v2,v3,v4)
    
  })
  
  veq <- reactive({
    c <- ((0.046*input$A_l) + input$n_v*(0.009*input$A_v))/1000
    b <- ((0.009*input$G_l) + input$n_v*(0.013*input$G_v))/1000
    a <- ((0.65*input$G_l + 125*input$x_l) + (input$n_v*(0.65*input$G_v + 125*input$x_v)) +
            (698*input$G_l/input$r + 10*input$G_l*input$i) + 
            (input$n_v*(698*input$G_v/input$r + 10*input$G_v*input$i)))/1000
    cte <- - input$eta*3.6*input$P
    polyroot(c(cte,a,b,c))
  })
  
  output$diagr <- renderPlot({
    Rn_l <- 0.65*input$G_l + 125*input$x_l + 
      0.009*input$G_l*v() + 0.046*input$A_l*v()^2
    Ro_l <- 698*input$G_l/input$r + 10*input$G_l*input$i
    R_l <- (Rn_l + Ro_l)/1000 #Somando e convertendo de N -> kN
    Rn_v <- input$n_v*(0.65*input$G_v + 125*input$x_v + 
                         0.013*input$G_v*v() + 0.009*input$A_v*v()^2)
    Ro_v <- input$n_v*(698*input$G_v/input$r + 10*input$G_v*input$i)
    R_v <- (Rn_v + Ro_v)/1000 #Somando e convertendo de N -> kN
    R <- R_l + R_v
    F_t <- ifelse(v() <= v_min(), F_max(),
                  ifelse(v() <= 90, input$eta*3.6*input$P/v(), NA))
    if(input$VMOC <= v_min()){
      F_tmc <- ifelse(v() < input$VMOC, NA, 
                      ifelse(v() <= v_min(), input$eta*3.6*input$P/v(), NA))
    } else{F_tmc <- rep(NA, length(v()))}
    
    ggplot(reshape2::melt(data.frame(v = v(), R = R, F_t = F_t), id.vars = 'v'),
           aes(x = v, y = value, color = variable)) +
      geom_segment(aes(x = 90, xend = 90, 
                       y = 0, yend = input$eta*3.6*input$P/90), 
                   color = 'blue', size = 1.2) +
      geom_path(size = 1.2) +
      scale_color_manual(values = c('red','blue'),
                         labels = c('R',expression(F[t]))) +
      ylim(0,500) +
      labs(x = 'Velocidade (km/h)', y = expression(paste(F[t],' e R (kN)'),),
           linetype = 'Tipo:', color = 'Tipo:') +
      theme_classic() +
      theme(
        text = element_text(
          family = 'Calibri Light',
          size = 20,
          hjust = 0.5
        ),
        legend.position="bottom"
      )
    
  })
  
  #Box do Esforço Trator Disponível:
  output$F_max <- renderValueBox({
    valueBox(paste(round(F_max(),0),'kN'),"Esforço Trator Disponível",
             icon = icon("bolt"), color = 'yellow')
    
  })
  
  #Box da Velocidade Mínima (dada pela Aderência)
  output$vmin <- renderValueBox({
    valueBox(paste(round(v_min(),1), 'km/h'),"Velocidade Mínima (Aderência)",
             icon = icon("tachometer-alt"), color = 'red')
  })
  
  #Box da Velocidade de Equilíbrio:
  output$veq <- renderValueBox({
    valueBox(paste(round(as.numeric(veq()[1]),1),'km/h'),"Velocidade de Equilíbrio",
             icon = icon("balance-scale"), color = 'green')
    
  })
  
  
}

shinyApp(ui, server)