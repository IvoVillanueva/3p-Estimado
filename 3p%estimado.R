library(rvest)
library(tidyverse)
library(htmltab)
library(ggimage)
library(rsvg)
library(ggthemes)
library(lubridate)
library(ggtext)
library(nbastatR)
library(scales)
library(gt)



tresp <- "https://www.basketball-reference.com/leagues/NBA_2021_totals.html"
tresp_data <- htmltab(reference1, which = 1, rm_nodata_cols = F)%>%
  janitor::clean_names() %>% 
  select(player, tm, x3p, x3pa, x3p_percent)

tresp_data [is.na(tresp_data )] <- .000
tresp_data <- tresp_data [tresp_data$player!="Player",]
tresp_data [, c(3:5)] <- sapply(tresp_data[,c(3:5)],as.numeric)

expected3p <- mean(tresp_data$x3p_percent)
# EXPECTED 3P% = (FG3M + X * LEAGUE AVERAGE 3P%) / (FG3A + X)

hoy <- today()
asp_ratio <- 1.618

tresp_data <- tresp_data %>% 
  mutate(x3pEstimado = (x3p + 242 * expected3p) / (x3pa + 242) )

tresp_data <- tresp_data [tresp_data$x3pEstimado!="Inf",]

tresp_data <- tresp_data %>% group_by(player) %>% arrange(desc(x3pEstimado)) 

tresp_data <- tresp_data  %>% mutate(tm=ifelse(player=="James Harden"& tm=="BRK","NON",tm),
                                tm=ifelse(player=="James Harden"& tm=="HOU","NON",tm),
                                tm=ifelse(player=="James Harden"& tm=="TOT","BRK",tm),
                                tm=ifelse(player=="Victor Oladipo"& tm=="IND","NON",tm),
                                tm=ifelse(player=="Victor Oladipo"& tm=="HOU","NON",tm),
                                tm=ifelse(player=="Victor Oladipo"& tm=="TOT","HOU",tm),
                                player=ifelse(player == "Marcus Morris", "Marcus Morris Sr.", player)) %>% filter(tm!="NON")

tresp_data <- tresp_data [c(1:30), ]

# Incluyendo los logos ----------------------------------------------------

teams <- nba_teams() %>% select(nameTeam, idTeam, logos = urlThumbnailTeam)
players <- nba_players() %>% left_join(teams, by =c("idTeam" = "idTeam")) %>% select(isActive, namePlayer, urlPlayerHeadshot, logos, nameTeam)

players <- players %>%
  filter(isActive == TRUE) %>%
  select(namePlayer, urlPlayerHeadshot, logos, nameTeam) %>%
  rename("player" = "namePlayer")

tresp_data <-tresp_data %>%fuzzyjoin::stringdist_left_join (players, by =c("player" = "player"))
tresp_data <- tresp_data  %>% select(player = player.x,tm ,x3p_percent, x3pEstimado, cabezas = urlPlayerHeadshot, logos) 


# Haciendo la Gráfica -----------------------------------------------------

# scale_x_continuous(labels = percent_format(1), breaks = seq(0, .45, .05)) + 
#   scale_y_continuous(labels = percent_format(.1), breaks = seq(.45, .60, .025)) +


tresp_data_plot <-  ggplot(tresp_data, aes( x=x3p_percent, y=x3pEstimado, label= player))+
  geom_image(aes(image = logos), size = 0.05, by = "width", asp = asp_ratio,nudge_y =0.003) +
  scale_x_continuous(labels = percent_format(1), limit = c(.37, .51), breaks = seq(0, .51, .01))+
  scale_y_continuous(labels = percent_format(1), limit = c(.34, .41), breaks = seq(0, .41, .01))+
  labs(x = "Porcentaje Actual de 3 Puntos",
       y = "Porcentaje Esperado de 3 Puntos \n (FG3M + 242 * League Average 3P%) / (FG3A + 242)",
       title = expression(paste( bold("Porcentaje Actual de 3 Puntos vs. Porcentaje Esperado de 3 Puntos"))),
       subtitle =glue::glue(" TOP 30 3P% Expected NBA hasta {hoy}"),
       caption =expression(paste( bold("Datos:"), "@bball_ref",bold( ' Grafico:')," Ivo Villanueva")))+
  
  theme_bw() + #theme(text=element_text(family="Arial Rounded MT Bold", size=14))+
  
  
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12* asp_ratio),
    axis.text = element_text(size = 5* asp_ratio),
    axis.title.x = element_text(size = 7 * asp_ratio, face = "bold", vjust = -2),
    axis.title.y = element_text(size = 7 * asp_ratio, face = "bold", vjust = 3),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 7))+
  # Second step
  theme(aspect.ratio = 1/asp_ratio)

dtresp_data_plot  <-tresp_data_plot  +
  
  geom_image(aes(image = cabezas),
             size = 0.07,
             by = "width", asp = asp_ratio,nudge_y =0)+
  ggrepel::geom_label_repel(size=2, label.size = 0.03, segment.colour = NA, point.padding =0.9, nudge_y = 0)+
  theme(aspect.ratio = 1/asp_ratio)


ggsave("dtresp_data_plot.png",dtresp_data_plot  ,
       # make the width equivalent to the aspect.ratio
       height = 10, width = 10 * asp_ratio, dpi = "retina")


# ##################################################GT TABLE--------

tresp_dataGt <- tresp_data [c(1:10), ]






# Tabla -------------------------------------------------------------------


tresp_data_Gt <- tresp_dataGt  %>% select( cabezas, players = player, logos, x3p_percent, x3pEstimado) %>%
  ungroup() %>%
  mutate(x3p_percent = percent(x3p_percent, .1), 
           x3pEstimado = percent(x3pEstimado, .1)) %>% 
  gt() %>%
  tab_header(
    title = md(" **Porcentaje Esperado de 3 Puntos en la**  <br>  <img src='https://seeklogo.com/images/N/nba-logo-41668C66DB-seeklogo.com.png' style='height:30px;'> "),
    subtitle = md(glue::glue("Hasta el {hoy}")))%>%
  cols_label(cabezas = (" "), 
            players = md ("Player"), 
             logos= ("Team"),
             x3p_percent= md("3P% Actual"),
             x3pEstimado = md("3P% Estimado<sup>1</sup>")) %>% 
tab_row_group(
     group = "Top 10",
  rows = 1:10) %>% 
  cols_align(
    align = "left",
    columns = 2
  )  %>%
  cols_align(
    align = "center",
    columns = 3:5
  )  %>%
  
  text_transform(
    locations = cells_body(vars(cabezas, logos)),
    fn = function(x) {
      web_image(url = x) 
    }
  ) %>% 
  cols_width(2~ px(60)) %>%
  opt_row_striping() %>%
  tab_options(
    table.background.color = " #f4f4f4",
    column_labels.font.size = 11.5,
    column_labels.font.weight = "bold",
    row_group.font.weight = NULL,
    row_group.background.color = "#f4f4f4",
    table.font.size = 9,
    heading.title.font.size = 18,
    heading.subtitle.font.size = 12.5,
    table.font.names = "Chivo", 
    data_row.padding = px(2)) %>% 
  tab_source_note(
    source_note = md(" <sup>1</sup>3P% Esperado = (FG3M + 242 * League Average 3P%) / (FG3A + 242) <br>
                      **Source**: 'NBA Stabilization Rates and the Padding Approach' por Kostya Medvedovsky <br> 
                      **Datos por**: <i>https://www.basketball-reference.com/<i>   <br> 
                      **Grafica**: <i>Ivo Villanueva<i>"))
tresp_data_Gt %>% gtsave("tresp_data_Gt.html")

