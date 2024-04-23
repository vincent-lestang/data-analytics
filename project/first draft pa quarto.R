install.packages("plotly")
install.packages ("tidyverse")
library
library(plotly)
map_world <- map_data("world")

unicef_full <- full_join(unicef_indicator_1,unicef_metadata, by =c("time_period" ="year" ))
unicef_test3 <- full_join(unicef_indicator_1, unicef_metadata, by =c("country", "time_period" ="year"))
unicef_graph <- left_join(unicef_indicator_1, unicef_metadata, by =c("country"))
unicef_test6 <- right_join(unicef_indicator_1, unicef_metadata, by =c("country"))

unicef_indic <- unicef_test4
unicef_time <-unicef_test5



#map 2 final 


data_jointotal <- unicef_indicator_1 %>%
  filter (sex == "Total") 

map_data_jointotal <- full_join(data_jointotal, map_world, by =c("country"="region"))

p <- ggplot(map_data_jointotal) + 
  aes(x = long, y = lat, group = group, fill = obs_value, 
      text = paste("Country: ", country, "<br>", 
                   "% of children with exactly 4 deprivations: ", round(obs_value,2), "%")) +
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") +
  labs(
    title = "% of children with exactly 4 deprivations",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source: Unicef",
    x = "Longitude",
    y = "Latitude",
    fill = "% of children with exactly 4 deprivations"
  ) +
  theme_bw()

p <- ggplotly(p, tooltip = "text")

print(p)
 

# scatter plot final 
data_jointotal3 <- unicef_test3 %>%
  filter(sex == "Total" & !is.na(obs_value) & !is.na(lifeExp) & obs_value > 0 & lifeExp > 0)

p <- ggplot(data_jointotal3) +
  aes(obs_value, lifeExp, color = obs_value, text = paste("Country :", country, "<br>",
                                                          "Life expectancy :", lifeExp, "<br>",
                                                          "% of children with 4D :", round(obs_value, 2))) +
  scale_color_gradient(low = "yellow", high = "red", na.value = "grey") +
  geom_point()
 
ggplotly(p, tooltip ="text")
 
# time series final
mean_gdp_per_capita <- unicef_graph %>%
  group_by(year) %>%
  summarise(mean_gdp_per_capita = mean(gdpPCap, na.rm = TRUE))

# Graphique de l'évolution de la moyenne du PIB par habitant de tous les pays au fil des années
p <- ggplot(mean_gdp_per_capita, aes(x = year, y = mean_gdp_per_capita)) +
  geom_line(color = "yellow") +
  geom_point(color = "orange") +
  labs(x = "Year", y = "Mean GDP", title =  "Evolution of countries' mean GDP per Capita over the years") +
  theme_minimal()

# Affichage du graphique
print(p)

#bar chart 

#deuxième test 
p <- ggplot(unicef_graph, aes(x = reorder(country,pop), y = pop, fill = obs_value,
                              text = paste("Country: ", country, "<br>",
                                           "Pop: ", pop, "<br>",
                                           "% of 4D: ", round(obs_value,2)))) +
  geom_col() +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey") + 
  labs(x = "Country", y = "Population,2022", title = "Population and % of Children with 4D") +
  theme_minimal() +
    theme(axis.text.x = element_blank())
  

# Conversion en ggplotly avec tooltip contenant les informations demandées
p <- ggplotly(p, tooltip = "text") %>% 


# Affichage du graphique
print(p)

