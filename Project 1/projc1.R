library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
counties = map_data('county')
states = map_data('state')

armed_func = function() {
  
}
police_vio_15 = read.csv('2015.csv')
police_vio_16 = read.csv('2016.csv')
state_abb = data.frame(full = state.name,state = state.abb)
police_vio_total = rbind(police_vio_15, police_vio_16)
police_vio_total = inner_join(police_vio_total,state_abb, by = 'state')
police_vio_total = mutate(police_vio_total, age.numeric = as.numeric( as.character(police_vio_total$age) ))
police_vio_total = mutate(police_vio_total, region = as.character(police_vio_total$full))
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
by_state = summarise(group_by(police_vio_total, region), total = 
                       sum(n()))
by_state$region = tolower(by_state$region)
by_state_merge = inner_join(states, by_state, by = 'region')
gg1 = ggplot(race_sum, aes(x = reorder(raceethnicity,total,median), y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                                                  stat = 'identity', position =
                                                                                    'dodge')
gg1 = gg1 + ggtitle('Police Violence in 2015 & 2016 - By Race') + xlab('Ethnicity') + ylab('Total killed') + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
gg2 = ggplot(armed_sum, aes(x = reorder(armed,total,median), y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                                     stat = 'identity', position =
                                                                       'dodge')
gg2 = gg2 + ggtitle("Police Violence in 2015 & 2016 - By Weapon") + xlab("Weapon of use") + ylab("Total Killed") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
gg3 = ggplot(classification_sum, aes(x = reorder(classification,total, median), y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                              stat = 'identity', position =
                                                                'dodge')
gg3 = gg3 + ggtitle("Police Violence in 2015 & 2016 - by Classification") + xlab("Classification") + ylab("Total Killed") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
gg4 = ggplot(filter(police_vio_total, year == 2015), aes(x = reorder(raceethnicity,age.numeric,median), 
                                   y = age.numeric, fill = raceethnicity )) + geom_boxplot()
gg4 = gg4 + ggtitle("Police Violence in 2015 - Grouped by Race") + xlab('Ethnicity') + ylab("Total Killed") + 
  theme(legend.position = 'bottom') + scale_fill_discrete(name = 'Ethnicity')

gg5 = ggplot(filter(police_vio_total, year == 2016), aes(x = reorder(raceethnicity,age.numeric,FUN = mean), 
                                  y = age.numeric, fill = raceethnicity )) + geom_boxplot() 
gg5 = gg5 + ggtitle("Police Violence in 2016 - Grouped by Race") + xlab('Ethnicity') + ylab("Total Killed") + 
  theme(legend.position = 'bottom') + scale_fill_discrete(name = 'Ethnicity')

gg6 = ggplot(by_state_merge) + geom_polygon(aes(x = long, y = lat, fill = total, group = group), color = "black") + 
  scale_fill_gradient(low = 'yellow', high = 'red', name = "Total Killed")
gg6 = gg6 + ggtitle("Police Violence in 2015 & 2016 - By State") + xlab('Longitude') + ylab("Latitude") + 
  theme(legend.position = 'bottom') 
temp = summarise(group_by(by_state_merge,region), long = mean(long), lat = mean(lat))
gg6 + geom_text(data = tem, aes(x = long, y = lat, label = region), size = 5)
