
library(readr)
library(dplyr)
library(ggplot2)

# Read in the csv files
cancer_incidence_data_scotland <- read_csv("data/opendata_inc9418_scotland.csv")
cancer_network_region <- read_csv("data/opendata_inc9418_region.csv") # Contains no exclusive info on Borders 
scottish_borders <- read_csv("data/scottish-borders-hscp.csv")
geography_codes <- read_csv("data/geography_codes_and_labels_hb2014_01042019.csv") # This contains the Borders geography code
incidence_health_board <- read_csv("data/opendata_inc9418_hb.csv") # This contains NHS Borders Cancer data

# Explore the datasets.

cancer_network_region %>%
  distinct(Region)

cancer_incidence_data_scotland %>%
  distinct(Region)

south_east_cancer_network <- cancer_network_region %>%
  filter(Region == "South East of Scotland")

south_east_cancer_network %>%
  distinct(CancerSite)

borders_incidence_hb <- incidence_health_board %>%
  filter(HB == "S08000016")

colnames(south_east_cancer_network)

colnames(borders_incidence_hb)


# INCIDENCES BY YEAR PLOT
borders_incidence_hb %>%
  filter(CancerSite == "All cancer types") %>%
  ggplot() +
  #geom_bar(aes(x = Year, y = IncidencesAllAges, fill = Sex), position = "dodge", stat = "identity") +
  aes(x = Year, y = IncidencesAllAges, colour = Sex, fill = Sex) +
  #geom_line(alpha = 0.25) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE) + # using linear model and no CI 
  theme_minimal() +
  labs(
  x = "\nYear",
  y = "Incidences",
  title = "Incidences by Year",
  subtitle = "All Ages\n",
  fill = "Sex"
  ) +
  theme(legend.text = element_text(size=15), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 16))


# CRUDE RATE BY YEAR PLOT
borders_incidence_hb %>%
  filter(CancerSite == "All cancer types") %>%
  ggplot() +
  #geom_bar(aes(x = Year, y = IncidencesAllAges, fill = Sex), position = "dodge", stat = "identity") +
  aes(x = Year, y = CrudeRate, colour = Sex, fill = Sex) +
  #geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  theme_minimal() +
  labs(
    x = "\nYear",
    y = "Crude Rate",
    title = "Crude Rate by Year",
    subtitle = "The crude rate is calculated per 100,000 person-years at risk\n",
    fill = "Sex"
  ) +
  theme(legend.text = element_text(size=15), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 16))


# CANCER TYPES PLOT
borders_incidence_hb %>%
  filter(CancerSite != "All cancer types") %>%
  filter(Sex != "All") %>%
  #filter(CrudeRate > 10) %>%
  group_by(Year) %>%
  arrange(desc(IncidencesAllAges)) %>%
  slice(seq_len(9)) %>%
  ggplot() +
  aes(x = Year, y = IncidencesAllAges, group = CancerSite, colour = Sex, fill = Sex) +
  geom_point() +
  #geom_smooth() +
  geom_smooth(aes(group=Sex), size=0.5)+
  facet_wrap(~ CancerSite, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "\nYear",
    y = "Incidences",
    title = "Top 8 Cancer Types by Gender",
    subtitle = "Incidences by Year",
    fill = "Sex"
  ) +
  theme(legend.text = element_text(size=15), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 16))




cancer_types <- south_east_cancer_network %>%
  group_by(CancerSite) %>%
  summarise(count = n())

# DISTRIBUTION PLOT

top_cancer <- borders_incidence_hb %>%
  #filter(CancerSite != "All cancer types") %>%
  group_by(CancerSite) %>%
  summarise(count = sum(IncidencesAllAges)) %>%
  arrange(desc(count)) #%>%


top_cancer_stats <- borders_incidence_hb %>%
  filter(CancerSite == "All cancer types", Sex == "All") %>% 
  summarise(
    num = sum(IncidencesAllAges),
    mean = mean(IncidencesAllAges),
    sd = sd(IncidencesAllAges)
  )  




borders_incidence_hb %>%
  filter(CancerSite == "All cancer types", Sex == "All") %>%
  ggplot() +
  aes(x = IncidencesAllAges) +
  geom_histogram(aes(y = ..density..), bins = 30, col = "white", fill = "light blue") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = top_cancer_stats$mean,
      sd = top_cancer_stats$sd
    ),
    col = "red"
  ) +
  theme_minimal() +
  labs(
    x = "\n Incidences - (1994 to 2018)",
    y = "Desnity",
    title = "Distribution of Incidences as a Probability Density",
    subtitle = "Normal Distribution Fitted",
    fill = "Sex"
  ) +
  theme(legend.text = element_text(size=15), 
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        title = element_text(size = 16))








