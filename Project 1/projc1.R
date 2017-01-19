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
by_state = summarise(group_by(police_vio_total, region, state, year), total = 
                       sum(n()))
by_state$region = tolower(by_state$region)
by_state_merge = inner_join(states, by_state, by = 'region')
gg1 = ggplot(race_sum, aes(x = reorder(raceethnicity,total,median), y = total)) + geom_bar(aes(fill = as.factor(year)),
                                                                                  stat = 'identity', position =
                                                                                    'dodge')
gg1 = gg1 + ggtitle('Police Violence in 2015 & 2016 - By Race') + xlab('Ethnicity') + ylab('Total killed') + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
data1 = group_by(filter(police_vio_total, raceethnicity %in% c("White", "Black", "Hispanic/Latino")), raceethnicity, armed)
gg2 = ggplot(data = data1) +
  geom_bar(aes(x = armed, fill = raceethnicity), position = 'dodge') + facet_grid(. ~ year)

gg2 = gg2 + ggtitle("Police Violence in 2015 & 2016 - By Weapon") + xlab("Weapon of use") + ylab("Total Killed") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
data2 = group_by(filter(police_vio_total, raceethnicity %in% c("White", "Black", "Hispanic/Latino")), raceethnicity, classification)
gg3 = ggplot(data = data2) +
  geom_bar(aes(x = classification, fill = raceethnicity), position = 'dodge') + facet_grid(. ~ year)

gg3 = gg3 + ggtitle("Police Violence in 2015 & 2016 - by Classification") + xlab("Classification") + ylab("Total Killed") + 
  theme(legend.position = "bottom") + scale_fill_discrete(name = "Year")
gg4 = ggplot(data = police_vio_total) +
  geom_boxplot(aes(x = reorder(raceethnicity, age.numeric, median), y = age.numeric, fill = raceethnicity), na.rm = TRUE, varwidth = TRUE, notch = FALSE) + 
  facet_grid(. ~ year) 
gg4 = gg4 + ggtitle("Police Violence in 2015 - Grouped by Race") + xlab('Ethnicity') + ylab("Age") + 
  theme(legend.position = 'bottom') + scale_fill_discrete(name = 'Ethnicity')

gg5 = ggplot(by_state_merge) + geom_polygon(aes(x = long, y = lat, fill = total, group = group), color = "black") + 
  scale_fill_gradient(low = 'yellow', high = 'red', name = "Total Killed")
gg5 = gg5 + ggtitle("Police Violence in 2015 & 2016 - By State") + xlab('Longitude') + ylab("Latitude") + 
  theme(legend.position = 'bottom') 
temp = summarise(group_by(by_state_merge,region,state), long = mean(long), lat = mean(lat))
gg5 + geom_text(data = temp, aes(x = long, y = lat, label = state), size = 5) + facet_grid(. ~ year)
#ggplot(data = filter(police_vio_total, raceethnicity %in% c("White","Black","Hispanic/Latino"), classification %in% c("Gunshot"))) + 
#  geom_bar(aes(x = raceethnicity, fill = raceethnicity), position = 'dodge') + facet_grid(armed ~ classification)
#ggplot(data = filter(police_vio_total, raceethnicity %in% c("White","Black","Hispanic/Latino"))) + 
#  geom_bar(aes(x = raceethnicity, fill = raceethnicity), position = 'dodge') + facet_grid(armed ~ classification)
#ggplot(data = filter(police_vio_total, raceethnicity %in% c("White", "Black", "Hispanic/Latino"), gender != "Non-conforming")) + 
#  geom_bar(aes(x = raceethnicity, fill = raceethnicity), position = 'dodge') + facet_grid(gender ~ year)
