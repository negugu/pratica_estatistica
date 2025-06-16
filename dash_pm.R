library(shiny)
library(shinydashboard)
library(ggplot2)
library(sf)
library(readxl)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggspatial)
library(lubridate)
library(BlandAltmanLeh)
library(plotly)
library(patchwork)



ui <- dashboardPage(
  header<-dashboardHeader(title = " PM 2.5  Sergipe"),
  sidebar<-dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon=icon("bar-chart"),
               menuSubItem("Gráfico de Linha", tabName="subitem1"),
               menuSubItem("Boxplot", tabName="subitem2"),
               menuSubItem('Mapa', tabName = 'subitem3'),
               menuSubItem('Dispersão', tabName = 'subitem4'),
               menuSubItem('Altman-Bland', tabName = 'subitem5')))
  ),
  body<-dashboardBody(
    tabItems(
      tabItem("subitem1",
              plotOutput("distPlot")),
      tabItem("subitem2", plotOutput("distPlot2")),
      tabItem('subitem3' , plotOutput('distPlot3')),
      tabItem('subitem4' , plotOutput('distPlot4')),
      tabItem('subitem5' , plotOutput('distPlot5'))
    )
  )
)


server <- function(input, output) {
  #Lendo e manipulando a base de dados
  
  dados_totais = read_xlsx("dados_completos_consolidado_donkelar.xlsx") 
  Se_totais = dados_totais %>% 
    filter(SIGLA_UF == 28) %>% 
    mutate(CD_MUN = as.character(CD_MUN))
  
  dados=read.csv("PM2.5_diario_2023.csv")
  dados$Data=ymd(dados$Date)
  dados$mes=month(dados$Data)
  dados$ano=year(dados$Data)
  dados$wday=weekdays(dados$Data)
  dados$UF=substr(dados$Cod,1,2)
  
  #sergipe
  
  Se = dados %>% 
    filter(UF == 28)
  
  #agupar por municipio
  Se_mun = Se %>% 
    group_by(Cod) %>% 
    summarise(media = mean(PM2.5,na.rm = T))
  
  
  # agrupar por municipio e mes
  Se_mun_mes = Se %>% 
    group_by(Cod , mes) %>% 
    summarise(media = mean(PM2.5,na.rm = T))
  
  # agrupar por mes
  Se_mes = Se %>% 
    group_by(mes) %>% 
    summarise(media = mean(PM2.5,na.rm = T))
  
  
  Se_mun_mes$Cod = as.character(Se_mun_mes$Cod)
  
  
  mapa = read_sf("SE_Municipios_2024.shp")
  Se_mun$Cod = as.character(Se_mun$Cod)
  
  mapa=left_join(mapa,Se_mun,join_by("CD_MUN"=="Cod"))
  mapa2 = left_join(mapa,Se_totais[c(1,7)],by = "CD_MUN")
  
  min_val <- min(min(mapa$media, na.rm = TRUE), min(mapa2$Media_PM25, na.rm = TRUE))
  max_val <- max(max(mapa$media, na.rm = TRUE), max(mapa2$Media_PM25, na.rm = TRUE))
  
  # 2. Define o gradiente comum
  fill_scale <- scale_fill_gradient(
    name = "Média",
    low = "yellow",
    high = "red",
    limits = c(min_val, max_val)
  )
  
  
  p1=ggplot(mapa)+
    geom_sf(aes(fill=media))+
    fill_scale+
    #annotation_scale(location="br")+
    #annotation_north_arrow(location="br",
                           #height = unit(1.8,"cm"),
                           #width = unit(1.8,"cm"))+
    ggtitle('SISAM')
  p3 = ggplotly(p1)
  
  
  p2=ggplot(mapa2)+
    geom_sf(aes(fill= Media_PM25))+
    fill_scale+
    #annotation_scale(location="br")+
    #annotation_north_arrow(location="br",
                           #height = unit(1.8,"cm"),
                           #width = unit(1.8,"cm"))+
    ggtitle('DONKLEAR')
  
  p4 = ggplotly(p2)
  medias_cop=Se_mun_mes %>%
    group_by(mes)%>% 
    summarise(media_c = mean(media))
  medias_don=Se_totais %>% 
    group_by(Mes) %>% 
    summarise(media_d = mean(Media_PM25))
  
  medias = left_join(medias_cop,medias_don, join_by('mes'== 'Mes')) %>% 
    pivot_longer(cols = starts_with('media'), names_to = 'grupo', values_to = 'media')
  
  
  
  med_ano = left_join(Se_mun_mes , Se_totais[c(1,7,12)] , join_by('Cod' == 'CD_MUN', 'mes' == 'Mes'))
  
  
  med_ano_long = med_ano %>% 
    pivot_longer(cols = c(3,4) , names_to = 'fonte' , values_to = 'media')
  
  
  med_ano_long$mes <- factor(
    med_ano_long$mes,
    levels = 1:12,
    labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  )
  
  
  
  
  output$distPlot <- renderPlot({
    ggplot(data = medias,aes(x = mes, y = media ,color = grupo ))+
      geom_line(lwd = 2) +
      scale_x_continuous(breaks = 1:12, limits = c(1,12) , labels = month.abb)+
      labs(y = "Variação PM2.5") +
      scale_color_manual(
        values = c("media_c" = "blue", "media_d" = "darkgreen"),
        labels = c("media_c" = "SISAM", "media_d" = "DONKLEAR")
      ) +
      theme_bw() +
      theme(panel.border = element_blank(),
            legend.title = element_blank())+
      ggtitle(label = 'Variação PM 2.5 em 2023')
  })
  
  
  output$distPlot2<-renderPlot({
    ggplot(med_ano_long ,aes(x= mes , y = media ,fill = fonte))+
      geom_boxplot(position = position_dodge(width = 0.8), width = 0.6)+
      scale_fill_manual(
        values = c("media" = "blue", "Media_PM25" = "darkgreen"),
        labels = c("media" = "SISAM", "Media_PM25" = "DONKLEAR")
      ) +
      theme_bw() +
      theme(panel.border = element_blank(),
            legend.title = element_blank())+
      ggtitle('Médias mensais')+
      xlab(label = 'Mês')
    
  })
  
  output$distPlot3<-renderPlot({
    p1 + p2
    
  })
  
  output$distPlot4<-renderPlot({
    med_ano %>% 
      ggplot(aes(x = media , y = Media_PM25))+
      geom_point()+
      geom_smooth(method = "lm",  se = FALSE, col = "blue" , lwd = 1.8)+
      ylab(label = 'media SISAM PM_25')+
      xlab('media DONKLEAR PM_25')+
      theme_bw()
    
  })
  output$distPlot5<-renderPlot({
    bland.altman.plot(med_ano$media, med_ano$Media_PM25,
                      graph.sys = "base",
                      xlab = "Média das duas medidas", 
                      ylab = "Diferença das duas medidas")
    
  })
  
  
}

shinyApp(ui, server)