library(tidyverse)
#Argentine Ant Survival

dat <- read.csv("Arena fight and venom usage Argentine ant Survival.csv") #loading the CSV file 
str(dat) #checking the structure of the file 

unique(dat$Species) #checking which species we have 
unique(dat$ArgAntDensity) #checking how many argentine densities we have

#filtering our data to include argentine ant density over 0
#grouping our data by species and argentine ant density 
#calculating the total number of dead ants in each group and then changing that value into a proportion of the total argentine ants present
dat2 <- dat %>% filter(ArgAntDensity >= 1) %>% group_by(Species, ArgAntDensity) %>% 
  summarise(Total_Dead = sum(Status..Alive.Dead.)) %>% 
  mutate(Proportion_Dead = Total_Dead/case_when(ArgAntDensity == 20 ~ 120,
                                                                ArgAntDensity == 40 ~ 240,
                                                                ArgAntDensity == 80 ~ 480,
                                                                ArgAntDensity == 160 ~ 960))

#plotting the data
ggplot(dat2, aes(x = ArgAntDensity, y = Proportion_Dead, colour = Species)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("M_antarcticum" = "#910a29","M_antipodum" = "#f1ab1f","M_smithii" = "#1764b3", "M_sydneyense" = "black"),
                     labels = c("M. antarcticum", "M. antipodum", "M. smithii", "M. sydneyense")) +
  scale_x_continuous(limits = c(0,160), breaks = seq(0,160,40)) +
  labs(x = "Argentine Ant Density", y = "Proportion Dead") +
  theme_classic() 


#conducting a generalized linear model for species and argentine ant density 
GLMod <- glm(Status..Alive.Dead.~Species*ArgAntDensity, family = binomial, data = dat) 
summary(GLMod)









