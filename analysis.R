library(ggplot2)
library(tidyverse)
library(stringr)
library(maps)
library(usdata)

incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
selected_columns <- select(incarceration_data, year, 1:33)
summary_information <- list()
test <- max(selected_columns$total_jail_pop, na.rm = TRUE)
item1 <- filter(selected_columns, total_jail_pop == test) 
summary_information$max_jail_pop <- select(item1,year, county_name)  


summary_information$max_black_jail_pop <-
  filter(incarceration_data, black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  
  pull(county_name, year)

summary_information$num_b_jail_pop <- incarceration_data %>%
  
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>%
  
  pull(black_jail_pop)

summary_information$num_w_jail_pop <- incarceration_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(white_jail_pop)

summary_information$max_white_jail_pop <- incarceration_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = T)) %>%
  pull(county_name, year)

  yawp <- filter(selected_columns, year == '2018')
  yawp <- filter(yawp, county_name == "Los Angeles County") 
  summary_information$black_difference <- summary_information$num_b_jail_pop - yawp$black_jail_pop
  
  summary_information$white_difference <- summary_information$num_w_jail_pop - yawp$white_jail_pop
# A chart that shows trends over time for a variable of your choice

# Incarceration by Race in LA County over time
new_chart <- incarceration_data[str_detect(incarceration_data$county_name, "Los Angeles County"), ]
chart_1_columns_na <- select(new_chart, year, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop)
chart_1_columns <- na.omit(chart_1_columns_na)
line_chart <- ggplot(data = chart_1_columns) +
  geom_line(mapping = aes(x = year, y = aapi_jail_pop, color = "red")) +
  geom_line(mapping = aes(x = year, y = black_jail_pop, color = "orange")) +
  geom_line(mapping = aes(x = year, y = latinx_jail_pop, color = "yellow")) +
  geom_line(mapping = aes(x = year, y = native_jail_pop, color = "green")) +
  geom_line(mapping = aes(x = year, y = white_jail_pop, color = "blue")) +
    scale_color_manual(
    name = "Races:",
    values = c("red", "orange", "yellow", "green", "blue"),
    labels = c("Asian American and Pacific Islander", "Black", "Latinx", "Native American", "White")
  
  ) +
  ggtitle("Incarcerated Population by Race in Los Angeles County") +
    labs(x = "Year", y = "Jail Population")
line_chart
# A chart that compares two variables to one another


# Total jail population vs. total black jail population in 2018

new_chart_2 <- select(incarceration_data, year, state, total_jail_pop, black_jail_pop)
jail_pop_data_2018 <- filter(new_chart_2, year == 2018)
jail_pop_data <- na.omit(jail_pop_data_2018)
total_jail_population_2018 <- sum(jail_pop_data$total_jail_pop)
total_black_jail_pop_2018 <- sum(jail_pop_data$black_jail_pop)

jail_pop_df <- data.frame(total_black_jail_pop_2018, total_jail_population_2018, color = c("blue", "pink"))
bar_chart <- ggplot(data = jail_pop_df) +
  geom_bar(stat = "identity", aes(x = "Total Black Jail Population (2018)", y = total_black_jail_pop_2018, fill = "blue")) +
  geom_bar(stat = "identity", aes(x = "Total Jail Population (2018)", y = total_jail_population_2018, fill = "pink")) +
  scale_fill_manual(
    name = "Population Type",
    values = c("blue", "pink"),
    labels = c("Black Jail Population", "Total Jail Population")
  ) +
  ggtitle("Total Jail Population vs. Total Black Jail Population in 2018") +
  labs(x = "", y = "Jail Population")

bar_chart


# A map that shows how your measure of interest varies geographically

# Incarcerated black population by county

map_dataframe <- incarceration_data %>%
  filter(year == 2018) %>%
  select(fips, state, county_name, black_jail_pop)
map_dataframe$state <- abbr2state(map_dataframe$state)
colnames(map_dataframe)[4] <- "Population"
map_shape <- map_data("county")
county.fips
uniting_counties <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",")
joined_counties <- uniting_counties %>%
  left_join(county.fips, by = "polyname")
final_df <- joined_counties %>%
  left_join(map_dataframe, by = "fips")
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

black_incarceration_map <- ggplot(final_df) +
   geom_polygon(
     mapping = aes(x = long, y = lat, group = group, fill = Population),
    color = "black", size = 0.3
   ) +
   coord_map() +
   scale_fill_continuous(limits = c(0, max(final_df$Population)), na.value = "white", low = "blue", high = "green") +
   blank_theme +
   ggtitle("Black Incarceration Population in 2018 by County")


