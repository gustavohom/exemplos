# Mourao 2021
# Corrida de Barras

# Instalando e carregando pacotes -----------------------------------------

# remotes::install_github("gustavohom/gmourao")

pkg <- c("ggplot2", "gganimate", "tidyverse", "magrittr", "gifski", "lubridate")

gmourao::m_load(pkg)


# Carregando dados --------------------------------------------------------

dados <-read.csv(file = "dados-2006_2019.csv", 
                                   header = TRUE, sep = ";", dec = ".")

# Corrigindo e resumindo os dados -----------------------------------------


dados$Ano %<>% dmy %>% year(.)

dados %<>% drop_na (.) %>%
  as.tibble(.) %>% 
  filter(Estado != "Não informado") %>% 
  group_by(Estado, Ano)%>% 
  summarise(area = sum(Área..ha.))


# Ranquendo os dados ------------------------------------------------------


dados %<>%
  group_by(Ano) %>%
  mutate(rank = rank(-area),
         area_lbl = paste0(" ",area)) %>%
  group_by(Estado) %>%
  filter(rank <=10)%>%
  ungroup()


# Criando plot ------------------------------------------------------------


staticplot = ggplot(dados, aes(rank, group = Estado,
                                       fill = as.factor(Estado), color = as.factor(Estado))) +
  geom_tile(aes(y = area/2,
                height = area,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Estado, " ")), vjust = 0.2, hjust = 1, size = 10) +
  geom_text(aes(y=area,label = area_lbl, hjust=0), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=35, hjust=0.5, face="bold", colour="black", vjust=-1),
        plot.subtitle=element_text(size=22, hjust=0.5, face="italic", color="black"),
        plot.caption =element_text(size=22, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,5, 2, 10, "cm")
        )


# Criando animação --------------------------------------------------------


anim = staticplot + transition_states(Ano, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Florestas Plantadas no Brasil no ano de: {closest_state}',
       subtitle  =  "Eucalipto e Pinus (Valor em hectares)",
       caption  = "Feito por Gustavo Mourão (https://mourao.netlify.app) | Fonte dos Dados: Ibá (2017, 2018, 2020)")


# Sainda em gif e mp4 -----------------------------------------------------


animate(anim, 200, fps = 30,  width = 2000, height = 1200,
        renderer = gifski_renderer("gganim.gif"))

animate(anim, 200, fps = 20,  width = 1200, height = 1000,
        renderer = ffmpeg_renderer()) -> for_mp4anim_save("animation.mp4", animation = for_mp4 )
