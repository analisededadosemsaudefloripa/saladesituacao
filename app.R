########################################################################################### 
#pacotes 
library(shiny)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(leaflet)
library(magrittr)
library(htmltools)
library(plyr)
library(readr)
library(stringr)
library(dplyr)
library(DT)
library(ggvis)




#Banco de dados para o mapa
acesso_dat <- read_csv("acesso_dat_final.csv", 
                       col_types = cols(BIENIO = col_factor(levels = c("2013/2014", 
                                                                       "2015/2016")), DISTRITO = col_factor(levels = c("Centro", 
                                                                                                                       "Continente", "Norte", "Sul"))))
########################################################################################### 
#Banco de dados de acesso
#dados de pacientes diferentes por trimestre - florianópolis
pc_florianopolis <- read_csv("base_de_dados/preparadas/pacientes diferentes/pc_florianopolis.csv", 
                             col_types = cols(VALOR = col_integer()))
pc_florianopolis$ESPECIALIDADE <- as.factor(pc_florianopolis$ESPECIALIDADE)
pc_florianopolis$TRIMESTRE <- as.factor(pc_florianopolis$TRIMESTRE)

#dados de pacientes diferentes por trimestre - distritos
pc_distrito <- read_csv("base_de_dados/preparadas/pacientes diferentes/pc_distrito.csv", 
                        col_types = cols(VALOR = col_integer()))
pc_distrito$ESPECIALIDADE <- as.factor(pc_distrito$ESPECIALIDADE)
pc_distrito$TRIMESTRE <- as.factor(pc_distrito$TRIMESTRE)

#dados de pacientes diferentes por trimestre - cs
pc_cs <- read_csv("base_de_dados/preparadas/pacientes diferentes/pc_cs.csv", 
                  col_types = cols(VALOR = col_integer()))
pc_cs$ESPECIALIDADE <- as.factor(pc_cs$ESPECIALIDADE)
pc_cs$TRIMESTRE <- as.factor(pc_cs$TRIMESTRE)

#dados de pacientes diferentes por trimestre - esf
pc_esf <- read_csv("base_de_dados/preparadas/pacientes diferentes/pc_esf.csv", 
                   col_types = cols(VALOR = col_integer()))
pc_esf$ESPECIALIDADE <- as.factor(pc_esf$ESPECIALIDADE)
pc_esf$AREA <- as.factor(pc_esf$AREA)
pc_esf$TRIMESTRE <- as.factor(pc_esf$TRIMESTRE)
########################################################################################### 
#Banco de dados de longitudinalidade
###########################################################################################
gestantes_florianopolis <- read_csv("base_de_dados/preparadas/gestantes/gestantes_florianopolis.csv")
gestantes_florianopolis$TRIMESTRE_PARTO <- as.factor(gestantes_florianopolis$TRIMESTRE_PARTO)
gestantes_florianopolis$TIPO <- as.factor(gestantes_florianopolis$TIPO)
gestantes_distrito <- read_csv("base_de_dados/preparadas/gestantes/gestantes_distrito.csv")
gestantes_distrito$DISTRITO <- as.factor(gestantes_distrito$DISTRITO)
gestantes_distrito$TRIMESTRE_PARTO <- as.factor(gestantes_distrito$TRIMESTRE_PARTO)
gestantes_distrito$TIPO <- as.factor(gestantes_distrito$TIPO)
gestantes_cs <- read_csv("base_de_dados/preparadas/gestantes/gestantes_cs.csv")
gestantes_cs$DISTRITO <- as.factor(gestantes_cs$DISTRITO)
gestantes_cs$UNIDADE <- as.factor(gestantes_cs$UNIDADE)
gestantes_cs$TRIMESTRE_PARTO <- as.factor(gestantes_cs$TRIMESTRE_PARTO)
gestantes_cs$TIPO <- as.factor(gestantes_cs$TIPO)
gestantes_esf <- read_csv("base_de_dados/preparadas/gestantes/gestantes_esf.csv", 
                          col_types = cols(VALOR = col_number()))
gestantes_esf$AREA <- as.factor(gestantes_esf$AREA)

########################################################################################### 
#Banco de dados de integralidade
###########################################################################################
encam_med_florianopolis <- read_csv("base_de_dados/preparadas/encaminhamentos/encam_med_florianopolis.csv",
                                    col_types = cols(VALOR = col_double()))
encam_med_distrito <- read_csv("base_de_dados/preparadas/encaminhamentos/encam_med_distrito.csv", 
                               col_types = cols(VALOR = col_double()))
encam_med_cs <- read_csv("base_de_dados/preparadas/encaminhamentos/encam_med_cs.csv", 
                         col_types = cols(VALOR = col_double()))
encam_med_esf <- read_csv("base_de_dados/preparadas/encaminhamentos/encam_med_esf.csv", 
                          col_types = cols(VALOR = col_double()))
encam_med_esf$AREA <- as.factor(encam_med_esf$AREA)
########################################################################################### 
ui <- dashboardPage(skin = "blue",
                    ########################################################################################### 
                    dashboardHeader(title = "Sala de Situação da APS - Versão para Teste", titleWidth = 550),
                    ########################################################################################### 
                    dashboardSidebar(
                      ########################################################################################### 
                      sidebarMenu(
                        menuItem("Apresentação", tabName = "apresentacao", icon = icon("heartbeat")),
                        menuItem("Florianóplis", icon = icon("dashboard"),  
                                 menuSubItem("Acesso", tabName = "acesso_forianopolis"),
                                 menuSubItem("Longitudinalidade", tabName = "longitudinalidade_forianopolis"),
                                 menuSubItem("Integralidade", tabName = "integralidade_forianopolis")),
                        menuItem("Distritos", icon = icon("dashboard"),   
                                 menuSubItem("Acesso", tabName = "acesso_distrito"),
                                 menuSubItem("Longitudinalidade", tabName = "longitudinalidade_distrito"),
                                 menuSubItem("Integralidade", tabName = "integralidade_distrito")),
                        menuItem("Centros de Saúde", icon = icon("dashboard"), 
                                 menuSubItem("Acesso", tabName = "acesso_cs"),
                                 menuSubItem("Longitudinalidade", tabName = "longitudinalidade_cs"),
                                 menuSubItem("Integralidade", tabName = "integralidade_cs")),
                        menuItem("Equipes", icon = icon("dashboard"), 
                                 menuSubItem("Acesso", tabName = "acesso_esf"),
                                 menuSubItem("Longitudinalidade", tabName = "longitudinalidade_esf"),
                                 menuSubItem("Integralidade", tabName = "integralidade_esf")),
                        menuItem("Instruções", icon = icon("question-circle"),
                                  href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/wiki/Instru%C3%A7%C3%B5es-para-Utiliza%C3%A7%C3%A3o-das-Salas-de-Situa%C3%A7%C3%A3o-em-Sa%C3%BAde"),
                        menuItem("Dados", tabName = "dados", icon = icon("database")),
                        menuItem("Código-fonte", icon = icon("code"), 
                                 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/app.R"),
                        menuItem("Licença de Uso", icon = icon("cc"), 
                                 href = "https://github.com/analisededadosemsaudefloripa/saladesituacao/blob/atencao_primaria/LICENSE")
                      )
                    ),
                    ########################################################################################### 
                    dashboardBody(
                      tabItems(
                        ########################################################################################### 
                        #Apresentação
                        tabItem(tabName = "apresentacao", h2("Atenção Primária em Florianópolis"),
                                fluidRow(
                                  mainPanel(
                                    p("A atenção primária é uma estratégia de organização da atenção à saúde voltada para responder de forma regionalizada, contínua e sistematizada à maior parte das necessidades de saúde de uma população."),
                                    p("No Brasil, seu principal efetor são as equipes de saúde da família."),
                                    p("Florianópolis destaca-se nacionalmente por ser a única capital com 100% de cobertura de atenção primária e por possuir os melhores resultados no PMAQ."),
                                    p("Ela é composta por Centros de Saúde, dividios em distritos sanitários. Cada 
                                      Centro de Saúde, por sua vez, é formado por uma ou mais Equipes de Saúde da Família.
                                      Informações básicas sobre os centros de saúde podem ser vistas no mapa a seguir.")
                                    )
                                    ),
                                fluidRow(
                                  box(title = "Mapa", status = "primary", width=12, solidHeader = T, collapsible = T, leafletOutput("localizacao_cs", height = 400))
                                ),
                                fluidRow(
                                  valueBoxOutput("infoPopulação"),
                                  valueBoxOutput("infoNumEquipes"),
                                  valueBoxOutput("infoCobertura")
                                )
                                  ),
                        ###########################################################################################
                        #Florianópolis
                        ###########################################################################################                         
                        
                        ###########################################################################################                         
                        #Dashboard Florianópolis Acesso                                                           
                        ###########################################################################################
                        tabItem(tabName = "acesso_forianopolis", h2("Acesso à Atenção Primária em Florianópolis"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="acesso_florianopolis_especialidade",
                                    label="Selecione o Tipo de Atendimento:",
                                    choices=list("Médico" = "Médico", "Enfermeiro" = "Enfermeiro", "Odontólogo" = "Odontólogo",
                                                 "Outros" = "Outros", "Médico e Enfermeiro" = "Médico e Enfermeiro", "Total" = "Total"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  valueBoxOutput("infoAcesso_florianopolis", width = 12)
                                ),
                                
                                
                                
                                fluidRow(
                                  tabBox(title = "Pacientes Diferentes por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "acesso_florianopolis_plot")),
                                         tabPanel("Dados", dataTableOutput("acesso_florianopolis_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("acesso_florianopolis_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Florianópolis Longitudinalidade
                        ###########################################################################################
                        tabItem(tabName = "longitudinalidade_forianopolis", h2("Longitudinalidade na Atenção Primária em Florianópolis"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="gestantes_florianopolis_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Total de Mulheres no Terceiro Trimestre de Gestação" = "TOTAL_GESTANTES", "Total de Gestantes com Pré-natal nos Três Trimestres" = "PRENATAL_TRES_TRI", "Percentual de Gestantes com Pré-natal nos Três Trimestres" = "PERCE_PRENATAL_TRES_TRI"),
                                    selected= "Percentual de Gestantes com Pré-natal nos Três Trimestres"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  valueBoxOutput("gestantes_florianopolis_valuebox", width = 12)
                                ),
                                
                                
                                
                                fluidRow(
                                  tabBox(title = "Mulheres no Terceiro Trimestre de Gestação", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "gestantes_florianopolis_plot")),
                                         tabPanel("Dados", dataTableOutput("gestantes_florianopolis_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("gestantes_florianopolis_info"))
                                  )
                                )
                        ),                        
                        
                        
                        ###########################################################################################
                        #Dashboard Florianópolis Integralidade
                        ###########################################################################################
                        tabItem(tabName = "integralidade_forianopolis", h2("Integralidade na Atenção Primária em Florianópolis"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="encam_med_florianopolis_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Consulta Médica" = "Consulta", "Encaminhamento Médico" = "Encaminhamento", "Percentual de Encaminhamento Médico" = "Percentual de Encaminhamento"),
                                    selected="Percentual de Encaminhamento Médico"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  valueBoxOutput("encam_med_florianopolis_valuebox", width = 12)
                                ),
                                
                                
                                
                                fluidRow(
                                  tabBox(title = "Consultas/Encaminhamentos Médicos por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "encam_med_florianopolis_plot")),
                                         tabPanel("Dados", dataTableOutput("encam_med_florianopolis_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("encam_med_florianopolis_info"))
                                  )
                                )
                        ),
                        
                        ################################################################################ 
                        #Distritos
                        ###########################################################################################
                        
                        ###########################################################################################
                        #Dashboard Distritos Acesso
                        ###########################################################################################
                        tabItem(tabName = "acesso_distrito", h2("Acesso nos Distritos"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="acesso_distrito_especialidade",
                                    label="Selecione o Tipo de Atendimento:",
                                    choices=list("Médico" = "Médico", "Enfermeiro" = "Enfermeiro", "Odontólogo" = "Odontólogo",
                                                 "Outros" = "Outros", "Médico e Enfermeiro" = "Médico e Enfermeiro", "Total" = "Total"),
                                    selected="Total"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Pacientes Diferentes por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "acesso_distrito_plot")),
                                         tabPanel("Dados", dataTableOutput("acesso_distrito_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("acesso_distrito_info"))
                                  )
                                )
                        ),
                        ###########################################################################################
                        #Dashboard Distritos Longitudinalidade
                        ###########################################################################################
                        tabItem(tabName = "longitudinalidade_distrito", h2("Longitudinalidade nos Distritos"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="gestantes_distrito_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Total de Mulheres no Terceiro Trimestre de Gestação" = "TOTAL_GESTANTES", "Total de Gestantes com Pré-natal nos Três Trimestres" = "PRENATAL_TRES_TRI", "Percentual de Gestantes com Pré-natal nos Três Trimestres" = "PERCE_PRENATAL_TRES_TRI"),
                                    selected= "Percentual de Gestantes com Pré-natal nos Três Trimestres"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Mulheres no Terceiro Trimestre de Gestação", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "gestantes_distrito_plot")),
                                         tabPanel("Dados", dataTableOutput("gestantes_distrito_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("gestantes_distrito_info"))
                                  )
                                )
                        ),
                        
                        
                        ###########################################################################################
                        #Dashboard Distritos Integralidade
                        ###########################################################################################
                        tabItem(tabName = "integralidade_distrito", h2("Integralidade nos Distritos"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="encam_med_distrito_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Consulta Médica" = "Consulta", "Encaminhamento Médico" = "Encaminhamento", "Percentual de Encaminhamento Médico" = "Percentual de Encaminhamento"),
                                    selected="Percentual de Encaminhamento Médico"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Consultas/Encaminhamentos Médicos por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "encam_med_distrito_plot")),
                                         tabPanel("Dados", dataTableOutput("encam_med_distrito_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("encam_med_distrito_info"))
                                  )
                                )
                        ),
                        ########################################################################################### 
                        #Centros de Saúde
                        ###########################################################################################
                        
                        ###########################################################################################
                        #Dashboard Centros de Saúde Acesso
                        ###########################################################################################
                        tabItem(tabName = "acesso_cs", h2("Acesso nos Centros de Saúde"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="acesso_cs_especialidade",
                                    label="Selecione o Tipo de Atendimento:",
                                    choices=list("Médico" = "Médico", "Enfermeiro" = "Enfermeiro", "Odontólogo" = "Odontólogo",
                                                 "Outros" = "Outros", "Médico e Enfermeiro" = "Médico e Enfermeiro", "Total" = "Total"),
                                    selected="Total"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="acesso_cspordistrito",
                                    label="Selecione um Distrito:",
                                    choices=list("Centro" = "Centro", "Continente" = "Continente", "Norte" = "Norte",
                                                 "Sul" = "Sul"),
                                    selected="Centro"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Pacientes Diferentes por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "acesso_cs_plot")),
                                         tabPanel("Dados", dataTableOutput("acesso_cs_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("acesso_cs_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Centros de Saúde Longitudinalidade
                        ###########################################################################################
                        tabItem(tabName = "longitudinalidade_cs", h2("Longitudinalidade nos Centros de Saúde"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="gestantes_cs_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Total de Mulheres no Terceiro Trimestre de Gestação" = "TOTAL_GESTANTES", "Total de Gestantes com Pré-natal nos Três Trimestres" = "PRENATAL_TRES_TRI", "Percentual de Gestantes com Pré-natal nos Três Trimestres" = "PERCE_PRENATAL_TRES_TRI"),
                                    selected= "Percentual de Gestantes com Pré-natal nos Três Trimestres"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="longitudinalidade_cspordistrito",
                                    label="Selecione um Distrito:",
                                    choices=list("Centro" = "Centro", "Continente" = "Continente", "Norte" = "Norte",
                                                 "Sul" = "Sul"),
                                    selected="Centro"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Mulheres no Terceiro Trimestre de Gestação", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "gestantes_cs_plot")),
                                         tabPanel("Dados", dataTableOutput("gestantes_cs_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("gestantes_cs_info"))
                                  )
                                )
                        ),
                        
                        
                        ###########################################################################################
                        #Dashboard Centros de Saúde Integralidade
                        ###########################################################################################
                        tabItem(tabName = "integralidade_cs", h2("Integralidade nos Centros de Saúde"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="encam_med_cs_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Consulta Médica" = "Consulta", "Encaminhamento Médico" = "Encaminhamento", "Percentual de Encaminhamento Médico" = "Percentual de Encaminhamento"),
                                    selected="Percentual de Encaminhamento Médico"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="integralidade_cspordistrito",
                                    label="Selecione um Distrito:",
                                    choices=list("Centro" = "Centro", "Continente" = "Continente", "Norte" = "Norte",
                                                 "Sul" = "Sul"),
                                    selected="Centro"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Consultas/Encaminhamentos Médicos por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "encam_med_cs_plot")),
                                         tabPanel("Dados", dataTableOutput("encam_med_cs_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("encam_med_cs_info"))
                                  )
                                )
                        ),
                        ###########################################################################################
                        #Equipes
                        ###########################################################################################
                        #Dashboard Equipes Geral
                        ###########################################################################################
                        ###########################################################################################
                        #Dashboard Equipes de Saúde Acesso
                        ###########################################################################################
                        tabItem(tabName = "acesso_esf", h2("Acesso nas Equipes de Saúde da Família"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="acesso_esf_especialidade",
                                    label="Selecione o Tipo de Atendimento:",
                                    choices=list("Médico" = "Médico", "Enfermeiro" = "Enfermeiro", "Odontólogo" = "Odontólogo",
                                                 "Outros" = "Outros", "Médico e Enfermeiro" = "Médico e Enfermeiro", "Total" = "Total"),
                                    selected="Total"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="acesso_esfporcs",
                                    label="Selecione um Centro de Saúde:",
                                    choices=list("CS ABRAÃO" = "CS ABRAÃO" ,  "CS AGRONÔMICA"  = "CS AGRONÔMICA",  "CS ALTO RIBEIRÃO" = "CS ALTO RIBEIRÃO",  
                                                 "CS ARMAÇÃO"  =  "CS ARMAÇÃO",  "CS BALNEÁRIO" =  "CS BALNEÁRIO",  "CS BARRA DA LAGOA" = "CS BARRA DA LAGOA",  
                                                 "CS CAMPECHE" =  "CS CAMPECHE",  "CS CANASVIEIRAS" = "CS CANASVIEIRAS",  "CS CANTO DA LAGOA" = "CS CANTO DA LAGOA",  
                                                 "CS CAPOEIRAS"  =  "CS CAPOEIRAS",  "CS CARIANOS"  =  "CS CARIANOS",  "CS CENTRO" = "CS CENTRO",  
                                                 "CS COLONINHA" = "CS COLONINHA",  "CS COQUEIROS"  = "CS COQUEIROS",  "CS CÓRREGO GRANDE" = "CS CÓRREGO GRANDE",  
                                                 "CS COSTA DA LAGOA" =  "CS COSTA DA LAGOA",  "CS COSTEIRA DO PIRAJUBAÉ"="CS COSTEIRA DO PIRAJUBAÉ",  
                                                 "CS ESTREITO" = "CS ESTREITO",  "CS FAZENDA DO RIO TAVARES" = "CS FAZENDA DO RIO TAVARES", "CS INGLESES"= "CS INGLESES",  
                                                 "CS ITACORUBI"= "CS ITACORUBI",  "CS JARDIM ATLÂNTICO"= "CS JARDIM ATLÂNTICO",  "CS JOÃO PAULO"= "CS JOÃO PAULO",  
                                                 "CS LAGOA DA CONCEIÇÃO"= "CS LAGOA DA CONCEIÇÃO", "CS MONTE CRISTO"= "CS MONTE CRISTO",  "CS MONTE SERRAT"= "CS MONTE SERRAT",  
                                                 "CS MORRO DAS PEDRAS"= "CS MORRO DAS PEDRAS",  "CS NOVO CONTINENTE"= "CS NOVO CONTINENTE",  "CS PANTANAL" =  "CS PANTANAL",  
                                                 "CS PONTA DAS CANAS"=  "CS PONTA DAS CANAS",  "CS PRAINHA"=  "CS PRAINHA",  "CS RATONES"=  "CS RATONES",  
                                                 "CS RIBEIRÃO DA ILHA"="CS RIBEIRÃO DA ILHA",  "CS RIO TAVARES"=  "CS RIO TAVARES",  "CS RIO VERMELHO"= "CS RIO VERMELHO",  
                                                 "CS SACO DOS LIMÕES"= "CS SACO DOS LIMÕES",  "CS SACO GRANDE"= "CS SACO GRANDE",  "CS SANTINHO"=  "CS SANTINHO",  
                                                 "CS TAPERA"=  "CS TAPERA",  "CS TRINDADE"= "CS TRINDADE",  "CS VARGEM GRANDE"= "CS VARGEM GRANDE",  
                                                 "CS VARGEM PEQUENA"=  "CS VARGEM PEQUENA",  "CS VILA APARECIDA" = "CS VILA APARECIDA"),
                                    selected="CS ABRAÃO"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Pacientes Diferentes por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "acesso_esf_plot")),
                                         tabPanel("Dados", dataTableOutput("acesso_esf_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("acesso_esf_info"))
                                  )
                                )
                        ),
                        
                        ###########################################################################################
                        #Dashboard Equipes de Saúde Longitudinalidade
                        ###########################################################################################
                        tabItem(tabName = "longitudinalidade_esf", h2("Longitudinalidade nas Equipes de Saúde da Família"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="gestantes_esf_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Total de Mulheres no Terceiro Trimestre de Gestação" = "TOTAL_GESTANTES", "Total de Gestantes com Pré-natal nos Três Trimestres" = "PRENATAL_TRES_TRI", "Percentual de Gestantes com Pré-natal nos Três Trimestres" = "PERCE_PRENATAL_TRES_TRI"),
                                    selected= "Percentual de Gestantes com Pré-natal nos Três Trimestres"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="longitudinalidade_esfporcs",
                                    label="Selecione um Centro de Saúde:",
                                    choices=list("CS ABRAÃO" = "CS ABRAÃO" ,  "CS AGRONÔMICA"  = "CS AGRONÔMICA",  "CS ALTO RIBEIRÃO" = "CS ALTO RIBEIRÃO",  
                                                 "CS ARMAÇÃO"  =  "CS ARMAÇÃO",  "CS BALNEÁRIO" =  "CS BALNEÁRIO",  "CS BARRA DA LAGOA" = "CS BARRA DA LAGOA",  
                                                 "CS CAMPECHE" =  "CS CAMPECHE",  "CS CANASVIEIRAS" = "CS CANASVIEIRAS",  "CS CANTO DA LAGOA" = "CS CANTO DA LAGOA",  
                                                 "CS CAPOEIRAS"  =  "CS CAPOEIRAS",  "CS CARIANOS"  =  "CS CARIANOS",  "CS CENTRO" = "CS CENTRO",  
                                                 "CS COLONINHA" = "CS COLONINHA",  "CS COQUEIROS"  = "CS COQUEIROS",  "CS CÓRREGO GRANDE" = "CS CÓRREGO GRANDE",  
                                                 "CS COSTA DA LAGOA" =  "CS COSTA DA LAGOA",  "CS COSTEIRA DO PIRAJUBAÉ"="CS COSTEIRA DO PIRAJUBAÉ",  
                                                 "CS ESTREITO" = "CS ESTREITO",  "CS FAZENDA DO RIO TAVARES" = "CS FAZENDA DO RIO TAVARES", "CS INGLESES"= "CS INGLESES",  
                                                 "CS ITACORUBI"= "CS ITACORUBI",  "CS JARDIM ATLÂNTICO"= "CS JARDIM ATLÂNTICO",  "CS JOÃO PAULO"= "CS JOÃO PAULO",  
                                                 "CS LAGOA DA CONCEIÇÃO"= "CS LAGOA DA CONCEIÇÃO", "CS MONTE CRISTO"= "CS MONTE CRISTO",  "CS MONTE SERRAT"= "CS MONTE SERRAT",  
                                                 "CS MORRO DAS PEDRAS"= "CS MORRO DAS PEDRAS",  "CS NOVO CONTINENTE"= "CS NOVO CONTINENTE",  "CS PANTANAL" =  "CS PANTANAL",  
                                                 "CS PONTA DAS CANAS"=  "CS PONTA DAS CANAS",  "CS PRAINHA"=  "CS PRAINHA",  "CS RATONES"=  "CS RATONES",  
                                                 "CS RIBEIRÃO DA ILHA"="CS RIBEIRÃO DA ILHA",  "CS RIO TAVARES"=  "CS RIO TAVARES",  "CS RIO VERMELHO"= "CS RIO VERMELHO",  
                                                 "CS SACO DOS LIMÕES"= "CS SACO DOS LIMÕES",  "CS SACO GRANDE"= "CS SACO GRANDE",  "CS SANTINHO"=  "CS SANTINHO",  
                                                 "CS TAPERA"=  "CS TAPERA",  "CS TRINDADE"= "CS TRINDADE",  "CS VARGEM GRANDE"= "CS VARGEM GRANDE",  
                                                 "CS VARGEM PEQUENA"=  "CS VARGEM PEQUENA",  "CS VILA APARECIDA" = "CS VILA APARECIDA"),
                                    selected="CS ABRAÃO"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Mulheres no Terceiro Trimestre de Gestação", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "gestantes_esf_plot")),
                                         tabPanel("Dados", dataTableOutput("gestantes_esf_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("gestantes_esf_info"))
                                  )
                                )
                        ), 
                        
                        ###########################################################################################
                        #Dashboard Equipes de Saúde Integralidade
                        ###########################################################################################
                        tabItem(tabName = "integralidade_esf", h2("Integralidade nas Equipes de Saúde da Família"),
                                
                                fluidRow(
                                  box(selectInput(
                                    inputId="encam_med_esf_opcao",
                                    label="Selecione o Tipo:",
                                    choices=list("Consulta Médica" = "Consulta", "Encaminhamento Médico" = "Encaminhamento", "Percentual de Encaminhamento Médico" = "Percentual de Encaminhamento"),
                                    selected="Percentual de Encaminhamento Médico"),
                                    width = 12, status = "primary"),
                                  
                                  box(selectInput(
                                    inputId="integralidade_esfporcs",
                                    label="Selecione um Centro de Saúde:",
                                    choices=list("CS ABRAÃO" = "CS ABRAÃO" ,  "CS AGRONÔMICA"  = "CS AGRONÔMICA",  "CS ALTO RIBEIRÃO" = "CS ALTO RIBEIRÃO",  
                                                 "CS ARMAÇÃO"  =  "CS ARMAÇÃO",  "CS BALNEÁRIO" =  "CS BALNEÁRIO",  "CS BARRA DA LAGOA" = "CS BARRA DA LAGOA",  
                                                 "CS CAMPECHE" =  "CS CAMPECHE",  "CS CANASVIEIRAS" = "CS CANASVIEIRAS",  "CS CANTO DA LAGOA" = "CS CANTO DA LAGOA",  
                                                 "CS CAPOEIRAS"  =  "CS CAPOEIRAS",  "CS CARIANOS"  =  "CS CARIANOS",  "CS CENTRO" = "CS CENTRO",  
                                                 "CS COLONINHA" = "CS COLONINHA",  "CS COQUEIROS"  = "CS COQUEIROS",  "CS CÓRREGO GRANDE" = "CS CÓRREGO GRANDE",  
                                                 "CS COSTA DA LAGOA" =  "CS COSTA DA LAGOA",  "CS COSTEIRA DO PIRAJUBAÉ"="CS COSTEIRA DO PIRAJUBAÉ",  
                                                 "CS ESTREITO" = "CS ESTREITO",  "CS FAZENDA DO RIO TAVARES" = "CS FAZENDA DO RIO TAVARES", "CS INGLESES"= "CS INGLESES",  
                                                 "CS ITACORUBI"= "CS ITACORUBI",  "CS JARDIM ATLÂNTICO"= "CS JARDIM ATLÂNTICO",  "CS JOÃO PAULO"= "CS JOÃO PAULO",  
                                                 "CS LAGOA DA CONCEIÇÃO"= "CS LAGOA DA CONCEIÇÃO", "CS MONTE CRISTO"= "CS MONTE CRISTO",  "CS MONTE SERRAT"= "CS MONTE SERRAT",  
                                                 "CS MORRO DAS PEDRAS"= "CS MORRO DAS PEDRAS",  "CS NOVO CONTINENTE"= "CS NOVO CONTINENTE",  "CS PANTANAL" =  "CS PANTANAL",  
                                                 "CS PONTA DAS CANAS"=  "CS PONTA DAS CANAS",  "CS PRAINHA"=  "CS PRAINHA",  "CS RATONES"=  "CS RATONES",  
                                                 "CS RIBEIRÃO DA ILHA"="CS RIBEIRÃO DA ILHA",  "CS RIO TAVARES"=  "CS RIO TAVARES",  "CS RIO VERMELHO"= "CS RIO VERMELHO",  
                                                 "CS SACO DOS LIMÕES"= "CS SACO DOS LIMÕES",  "CS SACO GRANDE"= "CS SACO GRANDE",  "CS SANTINHO"=  "CS SANTINHO",  
                                                 "CS TAPERA"=  "CS TAPERA",  "CS TRINDADE"= "CS TRINDADE",  "CS VARGEM GRANDE"= "CS VARGEM GRANDE",  
                                                 "CS VARGEM PEQUENA"=  "CS VARGEM PEQUENA",  "CS VILA APARECIDA" = "CS VILA APARECIDA"),
                                    selected="CS ABRAÃO"),
                                    width = 12, status = "primary")
                                ),
                                
                                
                                fluidRow(
                                  tabBox(title = "Consultas/Encaminhamentos Médicos por Trimestre", width=12,
                                         tabPanel("Gráfico", plotlyOutput(outputId = "encam_med_esf_plot")),
                                         tabPanel("Dados", dataTableOutput("encam_med_esf_tab")),
                                         tabPanel("Sobre o Indicador", htmlOutput("encam_med_esf_info"))
                                  )
                                )
                        )  
                                )
                      )
)
########################################################################################### 
server <- function(input, output, session) {
  ########################################################################################### 
  #Página inicial
  #Mapa das unidades
  output$localizacao_cs<- renderLeaflet({
    leaflet(data = acesso_dat) %>% addTiles() %>%
      addMarkers(~X, ~Y, popup = ~htmlEscape(DESCRICAO))
  })
  ###########################################################################################
  #Florianópolis
  ###########################################################################################
  
  ###########################################################################################
  #Dashboard Florianópolis Acesso
  ###########################################################################################
  #valueBox de pessoas diferentes atendidas por especialidade e trimestre
  
  output$infoAcesso_florianopolis<- renderValueBox({
    
    florianopolis_acesso <- pc_florianopolis %>% 
      filter(ESPECIALIDADE == input$acesso_florianopolis_especialidade) %>% 
      filter(TRIMESTRE == "2017 Q2")
    
    valueBox(
      value= tags$p(florianopolis_acesso$VALOR, style = "font-size: 100%"),
      subtitle = tags$p("pacientes diferentes, segundo trimestre 2017", style = "font-size: 100%"),
      icon = icon("child")
      
    )
  })
  
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$acesso_florianopolis_plot <- renderPlotly({
    
    florianopolis_acesso <- pc_florianopolis %>% 
      filter(ESPECIALIDADE == input$acesso_florianopolis_especialidade)
    florianopolis_acesso$TRIMESTRE <- as.factor(florianopolis_acesso$TRIMESTRE)
    
    a<-ggplot(florianopolis_acesso, aes(x = TRIMESTRE)) + 
      geom_col(aes(y = VALOR, fill = VALOR))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$acesso_florianopolis_tab <- renderDataTable({
    
    florianopolis_acesso <- pc_florianopolis %>% 
      filter(ESPECIALIDADE == input$acesso_florianopolis_especialidade) 
    
    florianopolis_acesso
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do acesso - Florianópolis
  output$acesso_florianopolis_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  
  ###########################################################################################
  #Dashboard Florianópolis Longitudinalidade
  ###########################################################################################
  #valueBox de consultas e encaminhamentos e trimestre
  
  output$gestantes_florianopolis_valuebox<- renderValueBox({
    
    florianopolis_longitudinalidade <- gestantes_florianopolis %>% 
      filter(TIPO == input$gestantes_florianopolis_opcao) %>% 
      filter(TRIMESTRE_PARTO == "2017 Q2")
    
    valueBox(
      value= tags$p(round(florianopolis_longitudinalidade$VALOR,2), style = "font-size: 100%"),
      subtitle = tags$p("Mulheres no Terceiro Trimestre de Gestação, no segundo trimestre de 2017", style = "font-size: 100%"),
      icon = icon("line-chart")
      
    )
  })
  
  #gráfico de consultas e encaminhamentos e trimestre
  output$gestantes_florianopolis_plot <- renderPlotly({
    
    florianopolis_longitudinalidade <- gestantes_florianopolis %>% 
      filter(TIPO == input$gestantes_florianopolis_opcao)
    
    a<-ggplot(florianopolis_longitudinalidade, aes(x = TRIMESTRE_PARTO)) + 
      geom_col(aes(y = VALOR, fill = VALOR))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  
  #tabela de longitudinalidade - Florianópolis
  output$gestantes_florianopolis_tab <- renderDataTable({
    
    florianopolis_longitudinalidade <- gestantes_florianopolis %>% 
      filter(TIPO == input$gestantes_florianopolis_opcao)
    
    florianopolis_longitudinalidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do longitudinalidade - Florianópolis
  output$gestantes_florianopolis_info <- renderText({
    
    paste("<b>B.01)Total de mulheres no terceiro trimestre de gestação</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de mulheres no terceiro trimestre de gestação no período" , "<br>",
          "<br>",
          "<b>B.02)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período", "<br>",
          "<br>",
          "<b>B.03)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período/Somatório do número de mulheres no terceiro trimestre" , "<br>",
          "de gestação no período *100 ", "<br>",
          "<br>")
  })
  
  ###########################################################################################
  #Dashboard Florianópolis Integralidade
  ###########################################################################################
  #valueBox de consultas e encaminhamentos e trimestre
  
  output$encam_med_florianopolis_valuebox<- renderValueBox({
    
    florianopolis_integralidade <- encam_med_florianopolis %>% 
      filter(TIPO == input$encam_med_florianopolis_opcao) %>% 
      filter(TRIMESTRE == "2017 Q2")
    
    valueBox(
      value= tags$p(round(florianopolis_integralidade$VALOR,2), style = "font-size: 100%"),
      subtitle = tags$p("consultas/encaminhamentos segundo trimestre 2017", style = "font-size: 100%"),
      icon = icon("retweet")
      
    )
  })
  
  #gráfico de consultas e encaminhamentos e trimestre
  output$encam_med_florianopolis_plot <- renderPlotly({
    
    florianopolis_intergralidade <- encam_med_florianopolis %>% 
      filter(TIPO == input$encam_med_florianopolis_opcao)
    florianopolis_intergralidade$TRIMESTRE <- as.factor(florianopolis_intergralidade$TRIMESTRE)
    
    a<-ggplot(florianopolis_intergralidade, aes(x = TRIMESTRE)) + 
      geom_col(aes(y = VALOR, fill = VALOR))+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela de integralidade - Florianópolis
  output$encam_med_florianopolis_tab <- renderDataTable({
    
    florianopolis_intergralidade <- encam_med_florianopolis %>% 
      filter(TIPO == input$encam_med_florianopolis_opcao)
    
    florianopolis_intergralidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do integralidade - Florianópolis
  output$encam_med_florianopolis_info <- renderText({
    
    paste("<b>C.01)Número de consultas médicas realizadas</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de consultas médicas realizadas no período" , "<br>",
          "<br>",
          "<b>C.02)Número de encaminhamentos médicos realizados</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período", "<br>",
          "<br>",
          "<b>C.03)Número de encaminhamentos médicos</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período/Somatório do número de consultas médicas realizadas no período *100", "<br>")
    
  })
  
  
  ###########################################################################################       
  #Distritos
  ###########################################################################################
  
  ###########################################################################################
  #Dashboard Distritos Acesso
  ###########################################################################################
  
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$acesso_distrito_plot <- renderPlotly({
    
    distrito_acesso <- pc_distrito %>% 
      filter(ESPECIALIDADE == input$acesso_distrito_especialidade)
    distrito_acesso$TRIMESTRE <- as.factor(distrito_acesso$TRIMESTRE)
    
    a<-ggplot(distrito_acesso, aes(x = TRIMESTRE, y = VALOR, group = DISTRITO, colour = DISTRITO)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$acesso_distrito_tab <- renderDataTable({
    
    distrito_acesso <- pc_distrito %>% 
      filter(ESPECIALIDADE == input$acesso_distrito_especialidade) 
    
    distrito_acesso
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  #informações do acesso - Florianópolis
  output$acesso_distrito_info <- renderText({
    
    paste("<b>Fórmula: </b>", "alhos/bugalhos", "<br>", "<b>Observações: </b>", "bom indicador")
    
  })
  
  #informações do acesso - Distritos
  output$acesso_distrito_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  ###########################################################################################
  #Dashboard Distritos Longitudinalidade
  ###########################################################################################
  #gráfico de consultas e encaminhamentos e trimestre
  output$gestantes_distrito_plot <- renderPlotly({
    
    distrito_longitudinalidade <- gestantes_distrito %>% 
      filter(TIPO == input$gestantes_distrito_opcao)
    
    
    a<-ggplot(distrito_longitudinalidade, aes(x = TRIMESTRE_PARTO, y = VALOR, group = DISTRITO, colour = DISTRITO)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(a)
    
  })
  
  #tabela de integralidade - Florianópolis
  output$gestantes_distrito_tab <- renderDataTable({
    
    distrito_longitudinalidade <- gestantes_distrito %>% 
      filter(TIPO == input$gestantes_distrito_opcao)
    
    distrito_longitudinalidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do integralidade - Distritos
  output$gestantes_distrito_info <- renderText({
    
    paste("<b>B.01)Total de mulheres no terceiro trimestre de gestação</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de mulheres no terceiro trimestre de gestação no período" , "<br>",
          "<br>",
          "<b>B.02)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período", "<br>",
          "<br>",
          "<b>B.03)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período/Somatório do número de mulheres no terceiro trimestre" , "<br>",
          "de gestação no período *100 ", "<br>",
          "<br>")
  })  
  
  
  
  ###########################################################################################
  #Dashboard Distritos Integralidade
  ###########################################################################################
  
  #gráfico de consultas e encaminhamentos e trimestre
  output$encam_med_distrito_plot <- renderPlotly({
    
    distrito_intergralidade <- encam_med_distrito %>% 
      filter(TIPO == input$encam_med_distrito_opcao)
    
    distrito_intergralidade$TRIMESTRE <- as.factor(distrito_intergralidade$TRIMESTRE)
    
    a<-ggplot(distrito_intergralidade, aes(x = TRIMESTRE, y = VALOR, group = DISTRITO, colour = DISTRITO)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(a)
    
  })
  
  #tabela de integralidade - Florianópolis
  output$encam_med_distrito_tab <- renderDataTable({
    
    distrito_intergralidade <- encam_med_distrito %>% 
      filter(TIPO == input$encam_med_distrito_opcao)
    
    distrito_intergralidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do integralidade - Distritos
  output$encam_med_distrito_info <- renderText({
    
    paste("<b>C.01)Número de consultas médicas realizadas</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de consultas médicas realizadas no período" , "<br>",
          "<br>",
          "<b>C.02)Número de encaminhamentos médicos realizados</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período", "<br>",
          "<br>",
          "<b>C.03)Número de encaminhamentos médicos</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período/Somatório do número de consultas médicas realizadas no período *100", "<br>")
    
  })  
  
  
  ###########################################################################################       
  #Centros de Saúde
  ########################################################################################### 
  ########################################################################################### 
  
  ###########################################################################################
  #Dashboard Centros de Saúde Acesso
  ###########################################################################################
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$acesso_cs_plot <- renderPlotly({
    
    cs_acesso <- pc_cs %>% 
      filter(ESPECIALIDADE == input$acesso_cs_especialidade) %>%
      filter(DISTRITO == input$acesso_cspordistrito)
    
    a<-ggplot(cs_acesso, aes(x = TRIMESTRE, y = VALOR, group = UNIDADE, colour = UNIDADE)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$acesso_cs_tab <- renderDataTable({
    
    cs_acesso <- pc_cs %>% 
      filter(ESPECIALIDADE == input$acesso_cs_especialidade)%>%
      filter(DISTRITO == input$acesso_cspordistrito)
    
    cs_acesso
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do acesso - Centros de Saúde
  output$acesso_cs_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  ###########################################################################################
  #Dashboard Centros de Saúde Longitudinalidade
  ###########################################################################################
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$gestantes_cs_plot <- renderPlotly({
    
    cs_longitudinalidade <- gestantes_cs %>% 
      filter(TIPO == input$gestantes_cs_opcao) %>%
      filter(DISTRITO == input$longitudinalidade_cspordistrito)
    
    a<-ggplot(cs_longitudinalidade, aes(x = TRIMESTRE_PARTO, y = VALOR, group = UNIDADE, colour = UNIDADE)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$gestantes_cs_tab <- renderDataTable({
    
    cs_longitudinalidade <- gestantes_cs %>% 
      filter(TIPO == input$gestantes_cs_opcao) %>%
      filter(DISTRITO == input$longitudinalidade_cspordistrito)
    
    cs_longitudinalidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de integralidade - Centros de Saúde
  output$gestantes_cs_info <- renderText({
    
    paste("<b>B.01)Total de mulheres no terceiro trimestre de gestação</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de mulheres no terceiro trimestre de gestação no período" , "<br>",
          "<br>",
          "<b>B.02)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período", "<br>",
          "<br>",
          "<b>B.03)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período/Somatório do número de mulheres no terceiro trimestre" , "<br>",
          "de gestação no período *100 ", "<br>",
          "<br>") 
  })
  
  
  
  ###########################################################################################
  #Dashboard Centros de Saúde Integralidade
  ###########################################################################################
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$encam_med_cs_plot <- renderPlotly({
    
    cs_integralidade <- encam_med_cs %>% 
      filter(TIPO == input$encam_med_cs_opcao) %>%
      filter(DISTRITO == input$integralidade_cspordistrito)
    
    a<-ggplot(cs_integralidade, aes(x = TRIMESTRE, y = VALOR, group = UNIDADE, colour = UNIDADE)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$encam_med_cs_tab <- renderDataTable({
    
    cs_integralidade <- encam_med_cs %>% 
      filter(TIPO == input$encam_med_cs_opcao) %>%
      filter(DISTRITO == input$integralidade_cspordistrito)
    
    cs_integralidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de integralidade - Centros de Saúde
  output$encam_med_cs_info <- renderText({
    
    paste("<b>C.01)Número de consultas médicas realizadas</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de consultas médicas realizadas no período" , "<br>",
          "<br>",
          "<b>C.02)Número de encaminhamentos médicos realizados</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período", "<br>",
          "<br>",
          "<b>C.03)Número de encaminhamentos médicos</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período/Somatório do número de consultas médicas realizadas no período *100", "<br>")
    
  })
  ###########################################################################################
  #Equipes
  ###########################################################################################
  ###########################################################################################  
  #Dashboard Equipes Geral
  ###########################################################################################
  ###########################################################################################
  #Dashboard Equipes de Saúde Acesso
  ###########################################################################################
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$acesso_esf_plot <- renderPlotly({
    
    esf_acesso <- pc_esf %>% 
      filter(ESPECIALIDADE == input$acesso_esf_especialidade) %>%
      filter(UNIDADE == input$acesso_esfporcs)
    
    a<-ggplot(esf_acesso, aes(x = TRIMESTRE, y = VALOR, group = AREA, colour = AREA)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  
  #tabela do acesso - esf
  output$acesso_esf_tab <- renderDataTable({
    
    esf_acesso <- pc_esf %>% 
      filter(ESPECIALIDADE == input$acesso_esf_especialidade) %>%
      filter(UNIDADE == input$acesso_esfporcs)
    
    esf_acesso
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações do acesso - Equipes
  output$acesso_esf_info <- renderText({
    
    paste("<b>A.01)Pessoas diferentes atendidas em consultas médicas e de enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas e de enfermagem" , "<br>",
          "<b>Observações:</b> Soma de pessoas diferentes atendidas em qualquer dessas consultas." , "<br>",
          "Não foram considerados atividades coletivas e Atualização de prontuário", "<br>",
          "<b>A.02)Pessoas diferentes atendidas em consultas médicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas médicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.03)Pessoas diferentes atendidas em consultas enfermagem</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas de enfermagem", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.04)Pessoas diferentes atendidas em consultas odontológicas</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas odontológicas", "<br>",
          "<b>Observações:</b> Não foram consideradas pessoas atendidas em atividades coletivas ou que tenham a consulta finalizada como Atualização de prontuário", "<br>",
          "<b>A.05)Pessoas diferentes atendidas em todos os serviços</b>", "<br>",
          "<b>Fórmula:</b> Somatório de pessoas diferentes atendidas em consultas, procedimentos, dispensação de medicamentos e atividades coletivas nos últimos 2 anos")
    
  })
  ###########################################################################################
  #Dashboard Equipes de Saúde Longitudinalidade
  ###########################################################################################
  #gráfico de pessoas diferentes atendidas por especialidade e trimestre
  output$gestantes_esf_plot <- renderPlotly({
    
    esf_longitudinalidade <- gestantes_esf %>% 
      filter(TIPO == input$gestantes_esf_opcao) %>%
      filter(UNIDADE == input$longitudinalidade_esfporcs)
    
    a<-ggplot(esf_longitudinalidade, aes(x = TRIMESTRE_PARTO, y = VALOR, group = AREA, colour = AREA)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(a)
    
  })
  
  #tabela do acesso - Florianópolis
  output$gestantes_esf_tab <- renderDataTable({
    
    esf_longitudinalidade <- gestantes_esf %>% 
      filter(TIPO == input$gestantes_esf_opcao) %>%
      filter(UNIDADE == input$longitudinalidade_esfporcs)
    
    esf_longitudinalidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações de integralidade - Centros de Saúde
  output$gestantes_esf_info <- renderText({
    
    paste("<b>B.01)Total de mulheres no terceiro trimestre de gestação</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de mulheres no terceiro trimestre de gestação no período" , "<br>",
          "<br>",
          "<b>B.02)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período", "<br>",
          "<br>",
          "<b>B.03)Total de gestantes com pré-natal nos três trimestres</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de gestantes no terceiro trimestre que realizaram ao menos" , "<br>",
          "uma consulta em cada trimestre no período/Somatório do número de mulheres no terceiro trimestre" , "<br>",
          "de gestação no período *100 ", "<br>",
          "<br>")    
  })
  
  
  ###########################################################################################
  #Dashboard Equipes de Saúde Integralidade
  ########################################################################################### 
  output$encam_med_esf_plot <- renderPlotly({
    
    esf_integralidade <- encam_med_esf %>% 
      filter(TIPO == input$encam_med_esf_opcao) %>%
      filter(UNIDADE == input$integralidade_esfporcs)
    
    a<-ggplot(esf_integralidade, aes(x = TRIMESTRE, y = VALOR, group = AREA, colour = AREA)) + 
      geom_line()+ 
      ylab("  ")+
      xlab("  ")+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    ggplotly(a)
    
  })
  
  
  #tabela do acesso - esf
  output$encam_med_esf_tab <- renderDataTable({
    
    esf_integralidade <- encam_med_esf %>% 
      filter(TIPO == input$encam_med_esf_opcao) %>%
      filter(UNIDADE == input$integralidade_esfporcs)
    
    esf_integralidade
    
  }, extensions = 'Buttons',
  options = list(
    "dom" = 'T<"clear">lBfrtip',
    buttons = list('copy', 'csv', 'pdf', 'print')))
  
  
  #informações integralidade - Equipes
  output$encam_med_esf_info <- renderText({
    
    paste("<b>C.01)Número de consultas médicas realizadas</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de consultas médicas realizadas no período" , "<br>",
          "<br>",
          "<b>C.02)Número de encaminhamentos médicos realizados</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período", "<br>",
          "<br>",
          "<b>C.03)Número de encaminhamentos médicos</b>", "<br>",
          "<b>Fórmula:</b> Somatório do número de encaminhamentos médicos realizados no período/Somatório do número de consultas médicas realizadas no período *100", "<br>")
    
  })
  
}    

###########################################################################################
shinyApp(ui, server)
