#################################
# Autor:  Matheus Pina Pallante #
# Nome:   Auto IA               #
# Versão: 1.0                   #
# Ano:    2020                  #
#################################

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
#h2o.init(nthreads = -1) # Iniciar e / ou conectar-se a uma instância de H2O

# User Interface ----------------------------------------------------------

ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = TRUE,
  enable_preloader = TRUE,
  loading_duration = 5,
  loading_background = "#DCDCDC",
  # Nome do Dashboard
  title = "Auto IA",
  # Menu Superior
  navbar = bs4DashNavbar(
    skin = "dark"
  ),
  # Menu Lateral Esquerdo
  sidebar = bs4DashSidebar(
    skin = "dark",
    status = "primary",
    title = "Auto IA",
    src = 'https://github.com/mppallante/Auto-IA/blob/master/Imagens/Logo.png',
    brandColor = "gray-light",
    bs4SidebarMenu(
      # Páginas do Dashboard
      bs4SidebarHeader("Deep Learning"),
      bs4SidebarMenuItem(
        startExpanded = T,
        tabName = "Classification",
        icon = "font",
        text = "Classificação"
      ),
      bs4SidebarMenuItem(
        tabName = "Regression",
        icon = "chart-line",
        text = "Regressão"
      ),
      # Série Temporal
      bs4SidebarHeader("Série Temporal"),
      bs4SidebarMenuItem(
        tabName = "Times",
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
      # Corpus Textual
      bs4SidebarHeader("Corpus Textual"),
      bs4SidebarMenuItem(
        tabName = "Text",
        icon = "align-justify",
        text = "Análise de Texto"
      ),
      # Sobre o Dashboard
      bs4SidebarHeader("Sobre"),
      # Link para o Github
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
      target = "_blank", "Auto IA - Projeto disponível para visualização no Github. MPPallante"
    ),
    right_text = lubridate::year(Sys.time())
  ), 
  # Corpo do Dahboard
  body = bs4DashBody(
    bs4TabItems(
      # Deep Learning - Classificação
      bs4TabItem(
        tabName = 'Classification',
        fluidPage(
          
        )
      ),
      # Deep Learning - Regressão
      bs4TabItem(
        tabName = 'Regression',
        fluidPage(
          
        )
      ),
      # Série Temporal
      bs4TabItem(
        tabName = 'Times',
        fluidPage(
          
        )
      ),
      # Cluster (Agrupamento)
      bs4TabItem(
        tabName = 'Cluster',
        fluidPage(
          
        )
      ),
      # Análise de Texto
      bs4TabItem(
        tabName = 'Text',
        fluidPage(
          
        )
      ),
      # Aplicação
      bs4TabItem(
        tabName = 'App',
        fluidPage(
          bs4Jumbotron(
            title = "Auto IA",
            lead = "Projeto e documentação disponíveis para visualização no GitHub",
            status = "info",
            btn_name = 'Auto IA - GitHub',
            href = "https://github.com/mppallante/AutoML/"
          )
        )
      )
    )
  ) 
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) ({
  
  
  # Avisos de Sistema -------------------------------------------------------
  
  
  # Deep Learning - Classificação -------------------------------------------
  
  
  # Deep Learning - Regressão -----------------------------------------------
  
  
  # Série Temporal ----------------------------------------------------------
  
  
  # Clusterização -----------------------------------------------------------
  
  
  # Análise de Texto --------------------------------------------------------
  
  
})  

# Shiny -------------------------------------------------------------------

shinyApp(ui = ui, server = server)
