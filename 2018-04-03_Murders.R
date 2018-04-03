library(fivethirtyeight)
library(tidyverse)

set.seed(20130810)
theme_set(theme_minimal())

names(murder_2015_final)

murders_gathered <- murder_2015_final %>% gather(murder_year, murders, murders_2014:murders_2015, na.rm = TRUE)
glimpse(murders_gathered)

murders_arranged <- murders_gathered %>% arrange(state, city)
glimpse(murders_arranged)

murders_separate <- murders_arranged %>% separate(murder_year, c("text", "year")) # default is a match of any non-alphanumeric values
glimpse(murders_separate)

murders_spread <- murders_separate %>% spread(year, murders) %>% arrange(state, city)
glimpse(murders_spread)

murders_final <- murders_spread %>% unite(city_state, city, state) %>% 
                                    arrange(city_state) %>% 
                                    select(-text)  
glimpse(murders_final)
