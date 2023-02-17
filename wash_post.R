# Load required packages
library(tidyverse)
library(sf)
library(transformr)
library(gganimate)
library(tigris)


# Pull down the data on school shootings compiled by the Washington Post
data <- read_csv("https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv")


# How often does this happen in private school settings? 
data %>%
  group_by(school_type, year) %>%
  tally() %>%
ggplot(mapping = aes(fill = school_type, x = school_type, y = n)) + 
  geom_bar(stat = "identity", alpha = 0.7) + 
  scale_fill_manual(values = c("dodgerblue", "orange"), labels = c("Private", "Public"), name ="") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), text=element_text(size=20)) + 
  geom_text(aes(x = 1, y = 20, label = 21, vjust = 1.5), color = "white") + 
  geom_text(aes(x = 2, y = 315, label = 317, vjust = 1.5), color = "white")   

ggsave("overall.png")


# What about the trend for public versus private schools?
data %>%
  group_by(school_type, year) %>%
  tally() %>%
ggplot(mapping = aes(color = school_type, x = year, y = n, group = school_type)) + 
  geom_line(stat = "identity", size = 1.25, alpha = 0.7) + 
  scale_color_manual(values = c("dodgerblue", "orange"), labels = c("Private Schools", "Public Schools"), name ="") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), text=element_text(size=20))

ggsave("by_year.png", width = 12, height = 8)



# How often is the perpetrator a student? 
knitr::kable(data %>%
  filter(is.na(shooter_relationship1) == FALSE) %>%
  group_by(shooter_relationship1) %>%
  tally() %>%
  arrange(-n))


# Visualize the number of fatalities for the worst incident each year
data %>%
  group_by(year) %>%
  summarize(maxdeaths = max(killed)) %>%
ggplot(mapping = aes(x = year, y = maxdeaths)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "orange") + 
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), text=element_text(size=20)) + 
  labs(caption = "Note that there were 7 incidents in 2015 but no fatalities.") +
  ggtitle("Maximum Number of Casualities", subtitle = "Per Individual Incident") +
  geom_text(aes(x = 1999, y = 10, label = "Columbine", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2012, y = 10, label = "Sandy \n Hook", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2018, y = 10, label = "Parkland", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2022, y = 10, label = "Uvalde", vjust = 1.5), color = "grey30") 

ggsave("worst_by_year.png", width = 12, height = 8)



# Visualize the total number of fatalities by year
data %>%
  group_by(year) %>%
  summarize(totdeaths = sum(killed)) %>%
  ggplot(mapping = aes(x = year, y = totdeaths)) + 
  geom_bar(stat = "identity", alpha = 0.7, fill = "dodgerblue") + 
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), text=element_text(size=20)) + 
  labs(caption = "Note that there were 7 incidents in 2015 but no fatalities.") +
  ggtitle("Total Number of Casualities", subtitle = "All Incidents") +
  geom_text(aes(x = 1999, y = 10, label = "Columbine", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2012, y = 10, label = "Sandy \n Hook", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2018, y = 10, label = "Parkland", vjust = 1.5), color = "grey30") + 
  geom_text(aes(x = 2022, y = 10, label = "Uvalde", vjust = 1.5), color = "grey30") 

ggsave("total_by_year.png", width = 12, height = 8)



# Map the location of these incidents
data_no_miss <- data %>% drop_na(long) 
data_sf <- st_as_sf(data_no_miss, coords = c("long", "lat"), crs = 4326)

states_0 <- states(cb = TRUE, resolution = "500k")
states <- st_transform(states_0, crs = 4326) %>%
  filter(!NAME %in% c("American Samoa", "Commonwealth of the Northern Mariana Islands", 
                      "Guam", "Puerto Rico", "United States Virgin Islands" ))

ggplot() +
  geom_sf(data = states, color="white", fill = "grey", alpha = 0.5) +
  geom_sf(data = data_sf, mapping = aes(size = killed), color = "dodgerblue", alpha = 0.4) + 
  coord_sf(crs = 5070) +
  scale_size_continuous(name = "Number Killed", range = c(3, 15)) +
  theme_void() + 
  theme(legend.position="bottom")
  
ggsave("mapped.png", width = 12, height = 8)

data_sf_1 <- data_sf %>% filter(year < 2011)
data_sf_2 <- data_sf %>% filter(year > 2010)

ggplot() +
  geom_sf(data = states, color="white", fill = "grey", alpha = 0.5) +
  geom_sf(data = data_sf_1, mapping = aes(size = killed), color = "dodgerblue", alpha = 0.4) + 
  coord_sf(crs = 5070) +
#  scale_size_continuous(name = "Number Killed", breaks = c(1, 5, 10, 15, 20, 25), range = c(3, 15)) +
  scale_size_continuous(name = "Number Killed", breaks = c(1, 5, 10, 15), range = c(3, 10)) +
  theme_void() + 
  theme(legend.position="bottom") + 
  facet_wrap(~year, ncol = 3)

ggsave("facet_wrapped_1.png", width = 12, height = 12)


# Create animated visualization of the mapped data
the_animation <- ggplot() +
  geom_sf(data = states, color="white", fill = "grey", alpha = 0.5) +
  geom_sf(data = data_sf, mapping = aes(size = killed), color = "dodgerblue", alpha = 0.4) + 
  coord_sf(crs = 5070) +
  scale_size_continuous(name = "Number Killed", breaks = c(0, 5, 10, 15, 20, 25), range = c(3, 15)) +
  theme_void() + 
  theme(legend.position="bottom") + 
labs(title = 'Year: {frame_time}', x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5, size=22)) +
  transition_time(as.integer(year))


# the_animation_15_duration <- animate(the_animation, fps = 1, duration = 15)
# the_animation_30_duration <- animate(the_animation, nframes = 23, fps = 0.33, height = 8, width = 6, units = "in", res = 150)
the_animation_45_duration <- animate(the_animation, nframes = 23, fps = 0.33, height = 12, width = 8, units = "in", res = 150)

# anim_save(filename = "animated_slow.gif", animation = the_animation_30_duration) 
anim_save(filename = "animated_slower.gif", animation = the_animation_45_duration) 


# --------------------------------------------------------------------------------------------------------------------
# End of the script
# --------------------------------------------------------------------------------------------------------------------