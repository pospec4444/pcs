library(dplyr)
library(here)
library(stringr)

load(here('data/examples/rider_records_giro2020.rda'))

if (! exists("rider_records_giro2020"))
{
  source("R/data-raw/functions.R")
  # Get rider IDs from Giro 2020 startlist
  rider_ids <-
    get_rider_urls_sl('https://www.procyclingstats.com/race/giro-d-italia/2020/gc/startlist')

  # Get profiles and results for each rider
  pcs_data <- get_pcs_data(rider_ids, c(2020))

  rider_profiles_giro2020 <- pcs_data$profiles
  rider_records_giro2020 <- pcs_data$results %>%
    subset(race == "Giro d'Italia (2.UWT)")

  usethis::use_data(rider_profiles_giro2020,
                    rider_records_giro2020,
                    overwrite = TRUE)
}
################################################################################

#
# Example 01:
# UCI points scored by each rider
score_uci <- rider_records_giro2020 %>%
  group_by(rider) %>%
  summarise(pointsuci = sum(pointsuci, na.rm = TRUE)) %>%
  arrange(desc(pointsuci))

#
# Example 02:
# GC place chart
# (Requires `plotly` package!)

library(plotly)
# GC results
gc <- rider_records_giro2020 %>%
  subset(stage == "General classification")

# top 3 riders in each team
top3_by_team <- gc %>% group_by(team) %>% slice_min(order_by = result, n = 3)

# take stages 1-20
stages20 <- rider_records_giro2020 %>%
  subset(date < '2020-10-25')

# stage 21 has empty column `gc_result_on_stage`
stage21 <- rider_records_giro2020 %>%
  subset(date == '2020-10-25') %>%
  mutate(gc_result_on_stage = gc$result) # use result from `gc`

# to be removed:
data <- stages20

# combine both data frames
data <- full_join(stages20, stage21) %>%
  select(rider, date, gc_result_on_stage, result, team, stage) %>%
  # include final GC
  #merge(., select(gc, rider, result), by = "rider", all.x = TRUE) %>%
  #rename(gc_result = result) %>%
  # rename stages to alphabetical order (so aes works)
  mutate(stage = sub('^Stage (\\d) ', '0\\1 ', stage)) %>%
  mutate(stage = sub('^Stage ', '', stage))
  # shorten long names
  #mutate(stage = str_replace(stage,
  #                           fixed("10 - île d'Oléron (Le Château-d'Oléron) › Île de Ré (Saint-Martin-de-Ré)"),
  #                           "10 - île d'Oléron › Île de Ré"))

# use only top 3 riders from each team
# data <- semi_join(data, top3_by_team, by = "rider")

chart <- ggplot(
  data,
  aes(
    x = stage,
    y = gc_result_on_stage,
    group = rider,
    colour = team,
    # customize tooltip
    text = paste(rider, team,
                 paste("GC:", gc_result_on_stage),
                 paste("Stage:", result),
#                 paste("Final GC:", gc_result),
                 sep = '\n')
  )) +
  geom_line() +
  # custom team colors
  scale_colour_manual(values = c(
    c("AG2R La Mondiale" = "#8c5206"),
    c("Androni Giocattoli - Sidermec" = "#fc2c03"),
    c("Astana Pro Team" = "#1cf7ff"),
    c("Bahrain - McLaren" = "#ffb300"),
    c("Bardiani-CSF-Faizanè" = "#a63080"),
    c("BORA - hansgrohe" = "#2a6143"),
    c("CCC Team" = "#ff7b00"),
    c("Cofidis, Solutions Crédits" = "#ff8066"),
    c("Deceuninck - Quick Step" = "#4081c7"),
    c("EF Pro Cycling" = "#ff00ee"),
    c("Groupama - FDJ" = "#b77efc"),
    c("INEOS Grenadiers" = "#4d0000"),
    c("Israel Start-Up Nation" = "#82c5ff"),
    c("Lotto Soudal" = "#c91100"),
    c("Mitchelton-Scott" = "#a7e009"),
    c("Movistar Team" = "#1fa2ff"),
    c("NTT Pro Cycling" = "#0a33ff"),
    c("Team Arkéa Samsic" = "#e3001e"),
    c("Team Jumbo-Visma" = "#ebe302"),
    c("Team Sunweb" = "#000000"),
    c("Team Total Direct Energie" = "#8dcbeb"),
    c("Trek - Segafredo" = "#9e6767"),
    c("UAE-Team Emirates" = "#07732b"),
    c("Vini Zabù - KTM" = "#2b5561")
  )) +
  scale_y_reverse(breaks = c(1, seq(10, 170, by = 10), 176)) +
  labs(title = "Giro d'Italia 2020: GC standing by stage", x = "Stage", y = "GC") +
  theme(axis.text.x = element_text(angle = 65))

ggplotly(chart, tooltip = "text")
