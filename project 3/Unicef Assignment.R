#installs
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("plotly")

#libraries
library(tidyverse)
library(ggplot2)
library(plotly)

#data
continents <- read_csv("continents_data.csv")
unicef_indicator_1_3 <- read_csv("unicef_indicator1.csv")
gapminder <- read_tsv("gapminder.tsv")
map_world <- map_data("world")

#transformations
unicef_continent_join <- inner_join(unicef_indicator_1_3, continents, by = c("country" = "Entity"))
data_joinUniGap <- inner_join(unicef_indicator_1_3, gapminder, by = c("country" = "country"))
map_data_join <- full_join(unicef_continent_join, map_world, by = c("country" = "region"))
map_data_joinUniGap <- full_join(data_joinUniGap, map_world, by = c("country" = "region"))
data_joinPlot <- full_join(unicef_indicator_1_3, gapminder, by = c("country" = "country"))

#Map Country
map_world <- map_data("world")

Country_map <- ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value, text = paste("Country:", map_data_join$country, "\nValue:", obs_value)) +
  geom_polygon() +
  scale_fill_gradient(low = "yellow", high = "darkorange", na.value = "grey80", limits = c(NA, NA)) +
  labs(title = "% Children Suffering a Deprivation per Country") +
  theme(text = element_text(family = "serif"), 
        plot.title = element_text(size = 14, face = "bold"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggplotly(Country_map)

#Map Continents
unicef_total <- unicef_indicator_1_3 %>% filter(sex == "total")

continent_aggregate <- unicef_continent_join %>%
  group_by(Continent) %>%
  summarise(mean_indicator = mean(obs_value, na.rm = TRUE))

ggplot(map_data_joinUniGap) +
  aes(x = long, y = lat, group = group, fill = continent) +  
  geom_polygon() +
  scale_fill_manual(values = c("yellow", "orange", "lightgreen", "lightblue"), na.value = "grey80") + 
  labs(title = "% Children Suffering a Deprivation per Continent") +
  guides(fill = "none") +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#Pie Chart Continents
filtered_data <- unicef_continent_join %>%
  filter(sex == "Total")

continent_aggregate <- unicef_continent_join %>%
  group_by(Continent) %>%
  summarise(mean_indicator = mean(obs_value, na.rm = TRUE)) %>%
  mutate(percentage = mean_indicator / sum(mean_indicator) * 100,
         Continent = case_when(Continent == "Europe" ~ "EU", Continent == "North America" ~ "N Am", TRUE ~ Continent))

average_percentage <- mean(continent_aggregate$mean_indicator)

pie_chart <- ggplot(continent_aggregate, aes(x = "", y = mean_indicator, fill = Continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("theta" = "y") +
  geom_label(aes(label = sprintf("%s: %.1f%%", Continent, percentage)), position = position_stack(vjust = 0.3), size = 2.5) +
  scale_fill_manual(values = c("yellow", "lightpink", "orange", "lightblue", "lightgreen", "beige")) +
  labs(title = "Mean Observed Value of Children Suffering by Continent") +
  theme_void() + 
  theme(plot.title = element_text(size = 14, face = "bold"), legend.position = "none")

print(pie_chart)

#Bar Chart - sex
data_aggregated <- unicef_continent_join %>%
  filter(sex %in% c("Male", "Female")) %>%  
  group_by(Continent, sex) %>%
  summarise(mean_obs_value = mean(obs_value, na.rm = TRUE), .groups = 'drop')

barchart <- ggplot(data_aggregated, aes(x = Continent, y = mean_obs_value, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "lightpink")) +
  labs(x = "Continent", y = "Average % of Children Suffering at least 1 deprivation", title = "Comparison of Children Suffering by Gender in Each Continent") +
  theme_minimal(base_family = "serif") +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 45), panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggplotly(barchart)


#Scatter Plot 
data_joinPlot <- full_join(unicef_indicator_1_3, gapminder, by = "country")

scatter_plot_1 <- ggplot(data_joinPlot, aes(x = obs_value, y = gdpPercap, color = continent)) +
  geom_point(alpha = 1) + 
  scale_y_continuous(limits = c(0, 25000)) +
  scale_color_manual(values = c("yellow", "lightpink", "orange", "lightblue", "lightgreen", "beige")) +
  labs(title = "Relationship between the % of Children suffering from a Deprivation & Countries GDP", x = "% Children Suffering at least 1 deprivation", y = "GDP per Capita", color = "Continent") +
  theme(text = element_text(family = "serif"), title = element_text(size = 8.5, face = "bold"))

print(scatter_plot_1)
scatter_plotly_1 <- ggplotly(scatter_plot_1)
print(scatter_plotly_1)

#Scatter Plot 2
scatter_plot_2 <- data_joinPlot %>%
  ggplot(aes(x = obs_value, y = lifeExp, color = continent)) +
  geom_point(alpha = .8) +
  scale_color_manual(values = c("yellow", "lightpink", "orange", "lightblue", "lightgreen", "beige")) +
  labs(title = "Relationship between: % of Children suffering from a Deprivation & Countries Life Expectancy", x = "% Children Suffering at least 1 deprivation", y = "Life Expectency", color = "Continent", size = "Sex") +
  theme(text = element_text(family = "serif"), title = element_text(size = 8.5, face = "bold")) +
  guides(size = "none")

scatter_plotly2 <- ggplotly(scatter_plot_2)
print(scatter_plotly2)

