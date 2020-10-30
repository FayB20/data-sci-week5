# Reading the file in:
hdi <- read.csv("Human-development-index.csv")

# tidying the hdi data:
hdi2 <- hdi %>% 
  pivot_longer(names_to = "Years",
               values_to = "Index",
               cols = -c(Country,HDI.Rank..2018.))

# filtering the dataset to exclude the missing observations:
hdi3 <- hdi2[!is.na(hdi2$Index),]
hdi4 <- hdi3[!is.na(hdi3$HDI.Rank..2018.),]

