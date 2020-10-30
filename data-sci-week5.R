# TASK 1:
# Reading the file in:
hdi <- read.csv("Human-development-index.csv")

# tidying the hdi data:
hdi2 <- hdi %>% 
  pivot_longer(names_to = "Years",
               values_to = "Index",
               cols = -c(Country,HDI.Rank..2018.))

# filtering the dataset to exclude the missing observations:
hdi3 <- hdi2[!is.na(hdi2$Index),]

# summarising the dataset with NAs removed
hdi3_summary <- hdi3 %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(Index),
            n = length(Index))

hdi3_summary <- hdi3 %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(Index),
            n = length(Index),
            sd = sd(Index),
            se = sd/sqrt(n))

# filtering the summary to get just the ten countries with the lowest mean HDI:
hdi4_summary <- hdi3_summary %>% 
  filter(rank(mean_index) < 11)
hdi4_summary

# plotting the data:
hdi4_summary %>% 
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se,
                    ymax = mean_index + se)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

# TASK 2:
file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)
buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)
?scan

