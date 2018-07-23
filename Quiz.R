
rm(list = ls())

#update r version
# install.packages("installr")
# library(installr)
# updateR()



install.packages("pacman")
library(pacman)
p_isloaded("pacman")


p_load(dplyr, tidyverse)


p_isloaded(tidyverse, dplyr)
p_isinstalled("tidyverse")
p_isinstalled("dplyr")

#install.packages("dplyr")
#library(dplyr)

#install.packages("tidyverse")
#library(tidyverse)



#fuel efficiency - hwy
#car type - drv
# - f - front wheel
# - 4 - 4wd

front_cars = mpg %>% 
    filter(drv=="f")

four_cars = mpg %>% 
  filter(drv=="4")

diffmeans_t = function(group1, group2) {
    m1 = mean(group1)
    m2 = mean(group2)
    s1 = sd(group1)
    s2 = sd(group2)
    n1 = length(group1)
    n2 = length(group2)
    
    rval = (m1-m2) / sqrt((s1^2/n1) + (s2^2/n2))
    return(rval)
}

diffmeans_t(front_cars$hwy, four_cars$hwy)

t.test(front_cars$hwy, four_cars$hwy)


#-----------------------------
starwars %>% mutate(BMI:=mass/((height/100)^2)) -> newStarwars
newStarwars

summary(newStarwars)

#remove outlier:
newStarWars2 = newStarwars %>% 
  filter(mass!=1358.00)

summary(newStarWars2)

ggplot(data = newStarWars2, mapping = aes(x = height, y = BMI)) + 
  geom_point() +
  geom_smooth(method="lm", se=TRUE)

cor(newStarWars2$height, newStarWars2$BMI)

cor.test( ~ newStarWars2$height + newStarWars2$BMI,
          data=newStarWars2,
          method = "pearson",
          conf.level = 0.95)

newStarWars2 %>% view

