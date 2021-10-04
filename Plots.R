# Read in data
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# Creating plot1
plot1 <- ufo_sightings %>% 
  filter(!is.na(state)) %>%
  mutate(state = str_to_upper(state)) %>%
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, n), y = n, fill = state)) + 
  geom_col() + 
  coord_flip() +
  guides(fill = "none") + 
  labs(title = "Top 10 States for Ufo Sightings",
       x = NULL,
       y = NULL) +
  ylim(0, 11000) +
  theme_minimal() +
  theme(text = element_text(size = 15))

#Tidying data
tidied_ufo <- ufo_sightings %>% 
  filter(country == "us") %>%
  filter (latitude > 24 & latitude <50)

#Creating plot 2
plot2 <- tidied_ufo %>%
  ggplot(aes(x = longitude, y = latitude)) + 
  geom_point(size = .5, alpha = .25) +
  theme_void() +
  coord_cartesian() +
  labs(title = "Sites of UFO Sightings in the US") +
  theme(text = element_text(size = 15))
