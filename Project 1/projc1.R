library(dplyr)
library(ggplot2)
library(maps)

armed_func = function() {
  
}
police_vio_15 = read.csv('2015.csv')
police_vio_16 = read.csv('2016.csv')
state_abb = data.frame(full = state.name,state = state.abb)
police_vio_total = rbind(police_vio_15, police_vio_16)
police_vio_total = inner_join(police_vio_total,state_abb, by = 'state')
police_vio_total = mutate(police_vio_total, age.numeric = as.numeric( as.character(police_vio_total$age) ))
police_vio_total = mutate(police_vio_total, full.char = as.character(police_vio_total$full))
total_2015 = as.numeric(summarise(filter(police_vio_total, year == 2015), total = sum(n())) )
total_2016 = as.numeric(summarise(filter(police_vio_total, year == 2016), total = sum(n())) )
group_by_race = group_by(police_vio_total,raceethnicity, year)
group_by_armed = group_by(police_vio_total,armed, year)
group_by_classification = group_by(police_vio_total, classification, year)
race_sum = summarise(group_by_race,total = sum(n()))
armed_sum = summarise(group_by_armed, total = sum(n()))
classification_sum = summarise(group_by_classification, total = sum(n()))
race_sum = mutate(race_sum, percentage = ifelse(year == 2015, (total/total_2015)*100, (total/total_2016)*100 ))
armed_sum = mutate(armed_sum, percentage = ifelse(year == 2015, (total/total_2015)*100, 
                                                  (total/total_2016)*100 ))
classification_sum =  mutate(classification_sum, percentage = ifelse(year == 2015, (total/total_2015)*100, 
                                                            (total/total_2016)*100 ))
by_state = summarise(group_by(police_vio_total, full.char), total = 
                       sum(n()))
gg1 = ggplot(race_sum, aes(x = raceethnicity, y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                                                  stat = 'identity', position =
                                                                                    'dodge')
gg2 = ggplot(armed_sum, aes(x = armed, y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                                     stat = 'identity', position =
                                                                       'dodge')
gg3 = ggplot(classification_sum, aes(x = classification, y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                              stat = 'identity', position =
                                                                'dodge')
gg4 = ggplot(filter(police_vio_total, year == 2015), aes(x = reorder(raceethnicity,age.numeric,median), 
                                   y = age.numeric, fill = raceethnicity )) + geom_boxplot()

gg5 = ggplot(filter(police_vio_total, year == 2016), aes(x = reorder(raceethnicity,age.numeric,median), 
                                  y = age.numeric, fill = raceethnicity )) + geom_boxplot() 