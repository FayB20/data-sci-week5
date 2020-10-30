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



