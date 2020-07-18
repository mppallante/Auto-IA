# Pacotes & Global --------------------------------------------------------

library(dplyr)
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinybusy)
library(h2o)
library(readxl)
library(data.table)
library(formattable)
library(DT)
library(prophet)
library(dygraphs)
library(lubridate)
library(factoextra)
library(cluster)
library(tm)
library(echarts4r)
library(googleVis)
library(lda)
library(LDAvis)
library(word2vec)
library(textplot)
h2o.init(nthreads = -1) # Iniciar e / ou conectar-se a uma instância de H2O

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = TRUE,
  enable_preloader = TRUE,
  loading_duration = 5,
  loading_background = "#DCDCDC",
  # Nome do Dashboard
  title = "AutoML",
  # Menu Superior
  navbar = bs4DashNavbar(
    skin = "dark"
  ),
  # Menu Lateral Esquerdo
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "AutoML",
    src = 'https://xplain.co/wp-content/uploads/2019/10/artificial-intelligence-256x256.jpg',
    brandColor = "gray-light",
    bs4SidebarMenu(
      # Páginas do Dashboard
      bs4SidebarHeader("Modelos de Predição"),
      bs4SidebarMenuItem(
        startExpanded = T,
        tabName = "AutoML",
        icon = "robot",
        text = "Machine Learning"
      ),
      bs4SidebarMenuItem(
        tabName = "Time",
        icon = "chart-line",
        text = "Série Temporal"
      ),
      # Análise de Padrões
      bs4SidebarHeader("Análise de Padrões"),
      bs4SidebarMenuItem(
        tabName = "Cluster",
        icon = "project-diagram",
        text = "Cluster (Agrupamento)"
      ),
      # MAnálise de Texto
      bs4SidebarHeader("Análise de Texto"),
      bs4SidebarMenuItem(
        tabName = "Text",
        icon = "align-justify",
        text = "Análise de Texto"
      ),
      # Sobre o Dashboard
      bs4SidebarHeader("Sobre a Aplicação"),
      bs4SidebarMenuItem(
        tabName = "How",
        icon = "question",
        text = "Como utilizar?"
      ),
      bs4SidebarMenuItem(
        tabName = "App",
        icon = "github",
        text = "Aplicação"
      )
    )
  ),
  # Footer
  footer = bs4DashFooter(
    copyrights = a(
      href = "https://github.com/mppallante/AutoML/", 
      target = "_blank", "Projeto disponível para visualização no Github - MPPallante."
    ),
    right_text = lubridate::year(Sys.time())
  ), 
  # Corpo do Dahboard
  body = bs4DashBody(
    bs4TabItems(
      # Machine Learning (AutoML)
      bs4TabItem(
        tabName = 'AutoML',
        fluidRow(
          # Configuração do Modelo
          column(width = 4,
                 bs4Card(title = 'CONFIGURAÇÕES DO MODELO - MACHINE LEARNING (AUTOML)', height = 400, 
                         status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                         # importar dados
                         fileInput(inputId = "ArquivoML", 
                                   label = "Importar dados (Treino):",
                                   multiple = FALSE,
                                   accept = c(".xlsx"), 
                                   width = "100%",
                                   placeholder = "Nenhum arquivo carregado",
                                   buttonLabel = 'Importar'),
                         # Escolher a Coluna Target 
                         fluidRow(
                           column(width = 8,
                                  pickerInput(
                                    inputId = "TargetML",
                                    label = "Coluna para predição:", 
                                    choices = c("Selecione um arquivo !"),
                                    options = list(
                                      `live-search` = TRUE)
                                  )),
                           column(width = 4,
                                  radioButtons(inputId = "Model", 
                                               label = "Tipo do modelo:",
                                               choices = c("Classificação", "Regressão"),
                                               selected = "Classificação")
                           )
                         ),
                         # Tempo de Treinamento do Modelo
                         fluidRow(
                           column(width = 8,
                                  sliderInput(
                                    inputId = "TempoML",
                                    label = "Tempo de treinamento (Minutos)", 
                                    min = 1,
                                    max = 5,
                                    value = 1
                                  )),
                           # Botão para Treinar o Modelo
                           column(width = 4,
                                  actionButton(inputId = "TreinarML",
                                               label = "EXECUTAR"))
                         )
                 ),
                 bs4Card(title = 'CLASSIFICAÇÃO E PREVISÃO DE NOVOS DADOS', height = 400, 
                         status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                         # Arquivo com novos dados para predição
                         fileInput(inputId = "Arquivo2ML", 
                                   label = "Importar dados:",
                                   multiple = FALSE,
                                   accept = c(".xlsx"), 
                                   width = "100%",
                                   placeholder = "Nenhum arquivo selecionado",
                                   buttonLabel = 'Importar'),
                         # Botão para Prever os novos dados
                         actionButton(inputId = "PredizerML",label = "EXECUTAR")
                 )),
          column(width = 8,
                 bs4Card(title = 'ESTATÍSTICAS DOS MODELOS GERADOS (AutoML)', height =  400,
                         status = "secondary", width = NULL, closable = F, maximizable = F, collapsible = F,
                         DTOutput(outputId = "DadosML", height = '100%', width = 'auto')
                 ),
                 bs4Card(title = 'RESULTADO DA CLASSIFICAÇÃO / PREVISÃO', height = 400, 
                         status = "secondary", width = NULL, closable = F, maximizable = F, collapsible = F,
                         DTOutput(outputId = "PrevML", height = '100%', width = 'auto')
                 )),
        )
      ),
      # Cluster (Agrupamento)
      bs4TabItem(
        tabName = 'Cluster',
        fluidRow(
          column(width = 4,
                 # Importar Arquivo
                 bs4Card(title = 'CONFIGURAÇÕES DO MODELO - AGRUPAMENTO (CLUSTERIZAÇÃO)', height = 400, 
                         status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                         # Arquivo de Treino do Modelo
                         fileInput(inputId = "ArquivoCL", 
                                   label = "Importar dados:",
                                   multiple = FALSE,
                                   accept = c(".xlsx"), 
                                   width = "100%",
                                   placeholder = "Nenhum arquivo selecionado",
                                   buttonLabel = 'IMPORTAR'),
                         # Escolher Numero de Cluster
                         sliderInput(inputId = "numCL", 
                                     label = 'Número de clusters (grupos):', 
                                     value = 3,
                                     min = 2,
                                     max = 10,
                                     width = "100%"),
                         # Botão para Treinar e Prever os Dados
                         actionButton(inputId = "PrevisaoCL",label = "EXECUTAR")
                 ),
                 # Tabela de Otimização de Numeros de Clusters
                 bs4Card(title = 'OTIMIZAÇÃO DE NÚMERO DE CLUSTERS', height =  400,
                         status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                         plotOutput(outputId = "otimCL", height = '100%', width = 'auto')
                 )
          ),
          column(width = 8,
                 bs4Card(title = 'CLUSTERS GERADOS PELO MODELO', height =  400,
                         status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                         plotOutput(outputId = "ClusterCL", height = '100%', width = 'auto')
                 ),
                 bs4Card(title = 'TABELA COM O RESULTADO DA CLUSTERIZAÇÃO (AGRUPAMENTO)', height =  400,
                         status = "secondary", width = NULL, closable = F, maximizable = F, collapsible = F,
                         DTOutput(outputId = "TableCL", height = '100%', width = 'auto')
                 )
          )
        )
      ),
      # Série Temporal
      bs4TabItem(
        tabName = 'Time',
        fluidRow(
          column(width = 4,
                 # Importar Arquivo
                 bs4Card(
                   title = 'CONFIGURAÇÕES DO MODELO - SÉRIES TEMPORAIS', height = 1340, 
                   status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                   # Arquivo de Treino do Modelo
                   fileInput(inputId = "ArquivoTS", 
                             label = "Importar dados:",
                             multiple = FALSE,
                             accept = c(".xlsx"), 
                             width = "100%",
                             placeholder = "Nenhum arquivo selecionado",
                             buttonLabel = 'IMPORTAR'),
                   # Escolher o formado de Data
                   pickerInput(
                     inputId = "TimeFormatTS",
                     label = "Padrão do campo data:", 
                     choices = c("Ano-Mês-dia" = "ymd",
                                 "Ano-Mês-dia Hora" = "ymd_h",
                                 "Ano-Mês-dia Hora-Minuto" = "ymd_hm",
                                 "Ano-Mês-dia Hora-Minuto-Segundo" = "ymd_hms",
                                 "Ano-Dia-Mês" = 'ydm',
                                 "Ano-Dia-Mês Hora" = 'ydm_h',
                                 "Ano-Dia-Mês Hora-Minuto" = 'ydm_hm',
                                 "Ano-Dia-Mês Hora-Minuto-Segundo" = 'ydm_hms',
                                 "Mês-Dia-Ano" = 'mdy',
                                 "Mês-Dia-Ano Hora" = 'mdy_h',
                                 "Mês-Dia-Ano Hora-Minuto" = 'mdy_hm',
                                 "Mês-Dia-Ano Hora-Minuto-Segundo" = 'mdy_hms',
                                 "Mês-Ano-Dia" = 'myd',
                                 "Mês-Ano-Dia Hora" = 'myd_h',
                                 "Mês-Ano-Dia Hora-Minuto" = 'myd_hm',
                                 "Mês-Ano-Dia Hora-Minuto-Segundo" = 'myd_hms',
                                 "Dia-Mês-Ano" = "dmy",
                                 "Dia-Mês-Ano Hora" = "dmy_h",
                                 "Dia-Mês-Ano Hora-Minuto" = "dmy_hm",
                                 "Dia-Mês-Ano Hora-Minuto-Segundo" = "dmy_hms",
                                 "Dia-Ano-Mês" = 'dym',
                                 "Dia-Ano-Mês Hora" = 'dym_h',
                                 "Dia-Ano-Mês Hora-Minuto" = 'dym_hm',
                                 "Dia-Ano-Mês Hora-Minuto-Segundo" = 'dym_hms'
                     ),selected = "dmy"
                   ),
                   # Quantidade de Dias para Previsão
                   sliderInput(inputId = "DiasTS", 
                               label = "Periodo para previsão (Dias)", 
                               min = 7, 
                               max = 30, 
                               value = 14,
                               width = '100%'),
                   # Botão para Treinar e Prever os Dados
                   actionButton(inputId = "PrevisaoTS",label = "EXECUTAR")
                 )
          ),
          column(width = 8,
                 # Previsão do Modelo Gerado
                 bs4Card(
                   title = 'GRÁFICO DA PREVISÃO GERADA PELO MODELO', height = 400, 
                   status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                   dygraphOutput(outputId = 'TS1', width = '100%', height = '100%')
                 ),
                 # Previsão do Modelo Gerado (Tendência e Sazonalidade)
                 bs4Card(
                   title = 'GRÁFICO DE TENDÊNCIA E SAZONALIDADE', height = 400, 
                   status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                   plotOutput(outputId = "TS2", width = '100%', height = '100%')
                 ),
                 # Previsão do Modelo Gerado (Tabela com Valores Gerados)
                 bs4Card(
                   title = 'TABELA COM O RESULTADO DA PREVISÃO', height = 400, 
                   status = "secondary", width = NULL, closable = F, maximizable = F, collapsible = F,
                   DTOutput(outputId = "TableTS", height = '100%', width = 'auto')
                 )
          )
        )
      ),
      # Análise de Texto (Mineração)
      bs4TabItem(
        tabName = 'Text',
        fluidRow(
          column(width = 4,
                 # Importar Arquivo
                 bs4Card(title = 'INSERIR DADOS', height = 1600, 
                         status = "dark", width = NULL, closable = F, maximizable = F, collapsible = F,
                         # Arquivo de Texto
                         fileInput(inputId = "ArquivoTX", 
                                   label = "Importar dados:",
                                   multiple = FALSE,
                                   accept = c(".xlsx"), 
                                   width = "100%",
                                   placeholder = "Nenhum arquivo selecionado",
                                   buttonLabel = 'Importar'),
                         # Botão para Fazer a Análise do Texto
                         actionButton(inputId = "PrevisaoTX",label = "EXECUTAR")
                 )
          ),
          column(width = 8,
                 # Nuvem de Palavras
                 bs4Card(title = 'NUVEM DE PALAVRAS', height =  400,
                         status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                         echarts4rOutput(outputId = "CloudTX", height = '100%', width = "100%")
                 ),
                 # Palavras mais frequentes
                 bs4Card(title = 'TOP 10 - PALAVRAS FREQUENTES', height =  400,
                         status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                         plotOutput(outputId = "WordTX", height = '100%', width = 'auto')
                 ),
                 # Palavras mais frequentes
                 bs4Card(title = 'ASSOCIAÇÃO DE PALAVRAS', height = 650,
                         status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                         htmlOutput(outputId = "view")
                 )
          )
        ),
        fluidPage(
          # Análise de tópicos textuais
          bs4Card(title = 'ANÁLISE DE TÓPICOS TEXTUAIS', 
                  status = "secondary", width = NULL, closable = F, maximizable = T, collapsible = F,
                  visOutput(outputId = 'vis')
          )
        )
      ),
      # Documentação do Dashboard
      bs4TabItem(
        tabName = 'How',
        fluidPage(
          bs4Card(width = NULL, height = 700, closable = F, maximizable = T, status = "secondary",
                  title = "DOCUMENTAÇÃO DA APLICAÇÃO",
                  tags$iframe(style='height:100%; width:100%; scrolling=yes',
                              src='https://github.com/mppallante/AutoML/blob/master/Documentacao.pdf')
          )
        )
      ),
      # Sobre o Projeto
      bs4TabItem(
        tabName = 'App',
        fluidPage(
          bs4Jumbotron(
            title = "AutoML",
            lead = "Projeto disponível para visualização no GitHub",
            status = "info",
            btn_name = 'AutoML',
            href = "https://github.com/mppallante/AutoML/"
          )
        )
      )
    )
  ) 
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) ({
  
  ##### AUTOML (MACHINE LEARNING) #####
  ## Evento de Carregar o Arquivo
  observeEvent(input$ArquivoML, {
    # Recebe o Arquivo Carregado
    file1ML <- input$ArquivoML
    # Carrega os Dados
    impML <- read_xlsx(file1ML$datapath)
    # Update do campo de seleção ->> Nome das Colunas
    updatePickerInput(
      session = session,
      inputId = "TargetML",
      choices = colnames(impML),
      selected = colnames(impML[length(impML)])
    )
  })
  ## Evento de criação do modelo
  observeEvent(input$TreinarML, {
    # Recebe o Arquivo Carregado
    file1ML <- input$ArquivoML
    # Carrega os Dados
    impML = read_xlsx(file1ML$datapath)
    # Pega o nome da classe
    yML = input$TargetML
    # Transforma para objeto do h20
    dadosML = as.h2o(impML)
    # Divide em treino e teste
    dadosML = h2o.splitFrame(data = dadosML, ratios = 0.7)
    treinoML = dadosML[[1]]
    testeML = dadosML[[2]]
    # Trata fator para Classificação
    if(input$Model == 'Classificação') {
      treinoML[, yML] <- as.factor(treinoML[, yML])
      testeML[, yML] <- as.factor(testeML[, yML])
    } else {}
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Cria modelo de Auto Machine Learning
    modeloML <<- h2o.automl(y = yML,
                            training_frame = treinoML, 
                            max_runtime_secs = input$TempoML * 60 )
    # Remove a roda de Carregamento
    remove_modal_spinner()
    # Tabela dos Melhores Modelos Criados
    lbML = as.data.table(modeloML@leaderboard)
    names(lbML)[1] <- c("Modelos Gerados")
    #lbML[,2:7] <- round(lbML[,2:7],2)
    output$DadosML <- renderDT(
      datatable(lbML, 
                extensions = 'Buttons',
                options = list(scrollX = T,
                               pageLength = 5,
                               lengthChange = F,
                               searching = F,
                               dom = 'Bfrtip',
                               buttons = c('excel')),
                class = 'cell-border stripe') %>% formatPercentage(2:length(lbML), 2)
    )
  })
  ## Evento para a previsão de novos dados	
  observeEvent(input$PredizerML, {
    # Recebe o Arquivo Carregado
    file2ML <- input$Arquivo2ML
    # Carrega dados para previsão
    impML = read_xlsx(file2ML$datapath)
    size = length(impML) + 1
    # Transforma para objeto do h20
    impML = as.h2o(impML)
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Faz a previsão e exibe o resultado
    previsaoML <- h2o.predict(modeloML@leader, impML)
    previsaoML = as.data.frame(previsaoML)
    # Cria a tabela com dados e previsão
    previsaoML <- cbind(as.data.frame(impML),previsaoML)
    # Remove a roda de Carregamento
    remove_modal_spinner()
    # Tabela com os Resultados da Predição
    output$PrevML <- renderDT({
      aML = datatable(previsaoML, 
                      extensions = 'Buttons',
                      options = list(scrollX = T, 
                                     pageLength = 5,
                                     lengthChange = F,
                                     searching = T,
                                     dom = 'Bfrtip',
                                     buttons = c('excel')),
                      class = 'cell-border stripe') 
      if(input$Model == 'Classificação') {
        aML = aML %>% formatPercentage((size+1):length(previsaoML), 2) %>%
          formatStyle(names(previsaoML[, size:length(previsaoML)]),  color = '#000000', backgroundColor = '#FFD700', fontWeight = 'bold')
        return(aML)
      } else {
        aML = aML %>% formatRound(length(previsaoML):length(previsaoML), 3) %>%
          formatStyle(names(previsaoML[length(previsaoML)]),  color = '#000000', backgroundColor = '#FFD700', fontWeight = 'bold')
        return(aML)
      }
    })
  })
  
  ##### SÉRIE TEMPORAL #####
  ## Evento para Gerar o Modelo e Gráficos
  observeEvent(input$PrevisaoTS, {
    # Recebe o Arquivo Carregado
    fileTS <- input$ArquivoTS
    # Carrega os Dados
    impTS = read_xlsx(fileTS$datapath)
    # Renomeia as Colunas
    names(impTS)[1:2] <- c("ds", "y")
    # Converte o campo hora
    switch (input$TimeFormatTS,
            ymd = {impTS$ds = ymd(impTS$ds)},
            ymd_h = {impTS$ds = ymd_h(impTS$ds)},
            ymd_hm = {impTS$ds = ymd_hm(impTS$ds)},
            ymd_hms = {impTS$ds = ymd_hms(impTS$ds)},
            ydm = {impTS$ds = ydm(impTS$ds)},
            ydm_h = {impTS$ds = ydm_h(impTS$ds)},
            ydm_hm = {impTS$ds = ydm_hm(impTS$ds)},
            ydm_hms = {impTS$ds = ydm_hms(impTS$ds)},
            mdy = {impTS$ds = mdy(impTS$ds)},
            mdy_h = {impTS$ds = mdy_h(impTS$ds)},
            mdy_hm = {impTS$ds = mdy_hm(impTS$ds)},
            mdy_hms = {impTS$ds = mdy_hms(impTS$ds)},
            myd = {impTS$ds = myd(impTS$ds)},
            myd_h = {impTS$ds = myd_h(impTS$ds)},
            myd_hm = {impTS$ds = myd_hm(impTS$ds)},
            myd_hms = {impTS$ds = myd_hms(impTS$ds)},
            dmy = {impTS$ds = dmy(impTS$ds)},
            dmy_h = {impTS$ds = dmy_h(impTS$ds)},
            dmy_hm = {impTS$ds = dmy_hm(impTS$ds)},
            dmy_hms = {impTS$ds = dmy_hms(impTS$ds)},
            dym = {impTS$ds = dym(impTS$ds)},
            dym_h = {impTS$ds = dym_h(impTS$ds)},
            dym_hm = {impTS$ds = dym_hm(impTS$ds)},
            dym_hms = {impTS$ds = dym_hms(impTS$ds)}
    )
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Cria o Modelo de Série Temporal
    m <- try(prophet(impTS), silent = T)
    future <- try(make_future_dataframe(m, periods = input$DiasTS), silent = T)
    forecast <- try(predict(m, future),silent = T)
    # Gráfico do Modelo Gerado
    output$TS1 <- renderDygraph({
      dyplot.prophet(m, forecast)
    })
    # Gráfico do Modelo Gerado (Tendência e Sazonalidade)
    output$TS2 <- renderPlot({
      prophet_plot_components(m, forecast)
    })
    # Tabela do Modelo Gerado
    output$TableTS <- renderDT({
      forecast <- forecast[c('ds' , 'yhat_lower', 'yhat', 'yhat_upper')]
      colnames(forecast) <- c('Data', 'Margem Baixa', 'Predição', 'Margem Alta')
      forecast$Data <- paste0(day(forecast$Data),'/',month(forecast$Data),'/',year(forecast$Data))
      forecast <- forecast[(length(forecast$Data)-(input$DiasTS-1)):length(forecast$Data),]
      datatable(forecast, 
                extensions = 'Buttons',
                options = list(scrollX = T, 
                               pageLength = 5,
                               lengthChange = F,
                               searching = T,
                               dom = 'Bfrtip',
                               buttons = c('excel')),
                class = 'cell-border stripe') 
    })
    # Remove a roda de Carregamento
    remove_modal_spinner()
  })
  
  ##### CLUSTERIZAÇÃO (AGRUPAMENTO) #####
  ## Evento de Carregar o Arquivo & Gráfico de Otimização (K-Means)
  observeEvent(input$ArquivoCL, {
    # Recebe o Arquivo Carregado
    file1CL <- input$ArquivoCL
    # Carrega os Dados
    impCL = read_xlsx(file1CL$datapath, col_names = T)
    impCL <- data.frame(impCL, stringsAsFactors = T)
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Gráfico de Otimização de Numero de Clusters
    output$otimCL <- renderPlot(
      fviz_nbclust(x = scale(impCL), 
                   FUNcluster = kmeans, 
                   method = "gap_stat",
                   verbose = T)
    )
    # Remove a roda de Carregamento
    remove_modal_spinner()
  })
  ## Evento para Executar e Clusterizar
  observeEvent(input$PrevisaoCL, {
    # Recebe o Arquivo Carregado
    file1CL <- input$ArquivoCL
    # Carrega os Dados
    impCL = read_xlsx(file1CL$datapath, col_names = T)
    impCL <- data.frame(impCL, stringsAsFactors = T)
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Modelo K-Means
    km <- kmeans(x = scale(impCL), 
                 centers = input$numCL, 
                 iter.max = 100,
                 nstart = 10)
    # Gráfico dos Clusters Gerados
    output$ClusterCL <- renderPlot({
      # Gráfico dos Clusters Gerados
      fviz_cluster(km, data = impCL,
                   palette = "Tableau 10",
                   ggtheme = theme_minimal(),
                   main = "Partitioning Clustering Plot"
      )
    })
    # Tabela com os Dados Clusterizados
    output$TableCL <- renderDT({
      # Clusterizar os dados
      impCL2 <- impCL
      impCL2$Cluster <- km$cluster
      datatable(impCL2, 
                extensions = 'Buttons',
                options = list(scrollX = T, 
                               pageLength = 5,
                               lengthChange = F,
                               searching = F,
                               dom = 'Bfrtip',
                               buttons = c('excel')),
                class = 'cell-border stripe') %>%
        formatStyle("Cluster",  color = '#000000', backgroundColor = '#FFD700', fontWeight = 'bold')
    })
    # Remove a roda de Carregamento
    remove_modal_spinner()
  })
  
  ##### CLASSIFICAÇÃO DE TEXTO #####
  ## Evento para executar o modelo & Analisar o texto
  observeEvent(input$PrevisaoTX , {
    # Recebe o Arquivo Carregado
    file1TX <- input$ArquivoTX
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Carrega os Dados
    impTX <- read_xlsx(file1TX$datapath)
    impTX <- data.frame(impTX, stringsAsFactors = F)
    ## Mineração de Texto
    docs <- Corpus(VectorSource(impTX))
    # Converte o texto para Lower Case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove números
    docs <- tm_map(docs, removeNumbers)
    # Remove stopwords em Português
    docs <- tm_map(docs, removeWords, stopwords("portuguese"))
    # Remove Pontuações
    docs <- tm_map(docs, removePunctuation)
    # Remove Espaços
    docs <- tm_map(docs, stripWhitespace)
    # Text stemming
    docs <- tm_map(docs, stemDocument)
    # Transformação de formato
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing = TRUE)
    d <- data.frame(word = names(v),freq = v)
    # Nuvem de Palavras
    output$CloudTX <- renderEcharts4r({
      d %>% 
        e_color_range(freq, color) %>% 
        e_charts() %>% 
        e_cloud(word, freq, color, shape = "circle") %>% 
        e_title("Nuvem de Pavalvras") %>%
        e_tooltip()
    })
    # Gráfico de Palavras Frequentes (TOP 10)
    output$WordTX <- renderPlot({
      barplot(d[1:10,]$freq, 
              las = 2, 
              names.arg = d[1:10,]$word,
              col = "#1E90FF", 
              main = "Palavras mais Frequentes",
              ylab = "Frequência")
    })
    ## Teste GoogleVis
    output$view <- renderGvis({
      gvisWordTree(impTX, textvar = names(impTX)[1], options = list(height = 600))
    })
    ## Modelo de Tópico
    output$vis <- renderVis({
      # Carregar StopWords:
      reviews <- impTX
      stop_words <- stopwords("pt")
      # Pré-Processamento:
      reviews <- gsub("'", "", reviews)  # Remove apóstrofe
      reviews <- gsub("[[:punct:]]", " ", reviews)  # Substituir pontuação por espaço
      reviews <- gsub("[[:cntrl:]]", " ", reviews)  # Substituir "caracteres de controle" por espaço
      reviews <- gsub("^[[:space:]]+", "", reviews) # Remover espaço em branco - início do documento
      reviews <- gsub("[[:space:]]+$", "", reviews) # Remover espaço em branco - final do documento
      reviews <- tolower(reviews)  # Transforma o texto em minúsculo
      # Tokenize e saída como uma lista:
      doc.list <- strsplit(reviews, "[[:space:]]+")
      # Cria a tabela de termos:
      term.table <- table(unlist(doc.list))
      term.table <- sort(term.table, decreasing = TRUE)
      # Remova termos que são palavras de parada ou ocorram menos de 5 vezes:
      del <- names(term.table) %in% stop_words | term.table < 5
      term.table <- term.table[!del]
      vocab <- names(term.table)
      # Transforma no formato exigido pelo pacote LDA:
      get.terms <- function(x) {
        index <- match(x, vocab)
        index <- index[!is.na(index)]
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
      }
      documents <- lapply(doc.list, get.terms)
      # Calcule algumas estatísticas relacionadas ao conjunto de dados:
      D <- length(documents)  # Tamanho do documento
      W <- length(vocab)  # Número de termos no vocabulário
      doc.length <- sapply(documents, function(x) sum(x[2, ]))  # Número de tokens por documento
      N <- sum(doc.length)  # Número total de tokens nos dados
      term.frequency <- as.integer(term.table)  # Frequências de termos no corpus
      # MCMC e parâmetros de ajuste do modelo:
      K <- 5
      G <- 1000
      alpha <- 0.02
      eta <- 0.02
      # Ajuste de modelo:
      set.seed(2020)
      t1 <- Sys.time()
      fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                         num.iterations = G, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
      t2 <- Sys.time()
      t2 - t1  
      theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
      phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
      MovieReviews <- list(phi = phi,
                           theta = theta,
                           doc.length = doc.length,
                           vocab = vocab,
                           term.frequency = term.frequency)
      # cria o objeto JSON para alimentar a visualização:
      J = createJSON(phi = MovieReviews$phi, 
                     theta = MovieReviews$theta, 
                     doc.length = MovieReviews$doc.length, 
                     vocab = MovieReviews$vocab, 
                     term.frequency = MovieReviews$term.frequency)
      J
    })
    # Remove a roda de Carregamento
    remove_modal_spinner()
  })
  
 ##### SISTEMA DE RECOMENDAÇÃO #####
  ## Evento para executar o modelo
  observeEvent(input$PrevisaoRC, {
    # Recebe o Arquivo Carregado
    file1RC <- input$ArquivoRC
    # Mostra a roda de carregamento
    show_modal_spinner(spin = "semipolar", text = 'Processando')
    # Carrega os Dados
    impRC <- read_xlsx(file1RC$datapath)
    impRC <- data.frame(title = impRC[1],
                        rating = impRC[2], 
                        stringsAsFactors = F)
    # Agrupamento de Dados
    baseRC = movie_ratings %>% 
      group_by(title = title) %>% 
      summarise(rating = mean(rating, na.rm = T))
    # Crosstalk da base de Dados
    # Tabela 1
    
    # Tabela 2
  })
})  

# Shiny -------------------------------------------------------------------

shinyApp(ui = ui, server = server)
