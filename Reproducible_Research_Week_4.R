#Load the raw data

storm <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))

dim(storm)

# translate all letters to lowercase
event <- tolower(storm$EVTYPE)

# replace all punct. characters with a space
event <- gsub("[[:blank:][:punct:]+]", " ", event)

## length(unique(event))

# update the data frame
storm$EVTYPE <- event

#load library
library(plyr)
library(dplyr)
library(ggplot2)


#1. Dangerous Events with respect to Population Health 

# To find the event types that are most harmful to population health, 
# the number of casualties are aggregated by the event type.


casualties <- ddply(storm, .(EVTYPE), summarize, fatalities = sum(FATALITIES),injuries = sum(INJURIES))

# Find events that caused most death and injury (Top 15)

#Death
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 15)


# Top 15 events that caused largest number of deaths are

#                EVTYPE fatalities injuries
# 758           tornado       5633    91346
# 116    excessive heat       1903     6525
# 138       flash flood        978     1777
# 243              heat        937     2100
# 418         lightning        816     5230
# 779         tstm wind        504     6957
# 154             flood        470     6789
# 524       rip current        368      232
# 320         high wind        248     1137
# 19          avalanche        224      170
# 888      winter storm        206     1321
# 525      rip currents        204      297
# 245         heat wave        172      379
# 125      extreme cold        162      231
# 685 thunderstorm wind        133     1488

#Diagram of top 15 events that caused largest number of deaths

p_fatal <-ggplot(data=fatal_events,aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Total number of fatalities") +
  xlab("Event type") +
  theme(legend.position="none")


p_fatal


#Injury
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 15)


#                EVTYPE fatalities injuries
# 758           tornado       5633    91346
# 779         tstm wind        504     6957
# 154             flood        470     6789
# 116    excessive heat       1903     6525
# 418         lightning        816     5230
# 243              heat        937     2100
# 387         ice storm         89     1975
# 138       flash flood        978     1777
# 685 thunderstorm wind        133     1488
# 212              hail         15     1361
# 888      winter storm        206     1321
# 372 hurricane/typhoon         64     1275
# 320         high wind        248     1137
# 274        heavy snow        127     1021
# 875          wildfire         75      911



#Diagram of top 15 events that caused largest number of injury
p_injury<-ggplot(data=injury_events,aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  ylab("Total number of injuries") +
  xlab("Event type") +
  theme(legend.position="none")


p_injury


# Tornadoes cause most number of deaths and injuries among all event types. 
# There are more than 5,000 deaths and more than 10,000 injuries in the last 60 years in US, due to tornadoes. 
# The other event types that are most dangerous with respect to population health are excessive heat and flash floods.



#2. Economic impact of weather events


# The crop damage is represented using two fields, CROPDMG and CROPDMGEXP. 
# The first step in the analysis is to calculate the property and crop damage for each event.

exp_transform <- function(e) {
  # h -> hundred, k -> thousand, m -> million, b -> billion
  if (e %in% c('h', 'H'))
    return(2)
  else if (e %in% c('k', 'K'))
    return(3)
  else if (e %in% c('m', 'M'))
    return(6)
  else if (e %in% c('b', 'B'))
    return(9)
  else if (!is.na(as.numeric(e))) # if a digit
    return(as.numeric(e))
  else if (e %in% c('', '-', '?', '+'))
    return(0)
  else {
    stop("Invalid exponent value.")
  }
}

prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)

# Compute the economic loss by event type
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss (Top 15)
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 15)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 15)

#Top 15 events that caused most property damage 

prop_dmg_events[, c("EVTYPE", "prop_dmg")]

#                 EVTYPE     prop_dmg
# 138        flash flood 6.820237e+13
# 711 thunderstorm winds 2.086532e+13
# 758            tornado 1.078951e+12
# 212               hail 3.157558e+11
# 418          lightning 1.729433e+11
# 154              flood 1.446577e+11
# 372  hurricane/typhoon 6.930584e+10
# 168           flooding 5.920825e+10
# 599        storm surge 4.332354e+10
# 274         heavy snow 1.793259e+10
# 363          hurricane 1.186832e+10
# 772     tropical storm 7.703891e+09
# 888       winter storm 6.688597e+09
# 320          high wind 5.270248e+09
# 529        river flood 5.118946e+09


#Diagram of top 15 events that caused most property damage 

p_property <- ggplot(data=prop_dmg_events, aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
  geom_bar(stat="identity") +
  coord_flip() +
  xlab("Event type") +
  ylab("Total Property damage in dollars") +
  theme(legend.position="none")


p_property


# Top 15 events that caused biggest crop damage are
crop_dmg_events[, c("EVTYPE", "crop_dmg")]

#                EVTYPE    crop_dmg
# 84            drought 13972566000
# 154             flood  5661968450
# 529       river flood  5029459000
# 387         ice storm  5022113500
# 212              hail  3025974480
# 363         hurricane  2741910000
# 372 hurricane/typhoon  2607872800
# 138       flash flood  1421317100
# 125      extreme cold  1312973000
# 187      frost/freeze  1094186000
# 254        heavy rain   733399800
# 772    tropical storm   678346000
# 320         high wind   638571300
# 779         tstm wind   554007350
# 116    excessive heat   492402000



#Diagram of top 15 events that caused most property damage in dollars
p_crop <- ggplot(data=crop_dmg_events, aes(x=reorder(EVTYPE, crop_dmg), y=crop_dmg, fill=crop_dmg)) +
  geom_bar(stat="identity") +
  coord_flip() + 
  xlab("Event type") +
  ylab("Total Crop damage in dollars") + 
  theme(legend.position="none")

p_crop

# Property damages are given in logarithmic scale due to large range of values. 
# The data shows that flash floods and thunderstorm winds cost the largest property damages among weather-related natural diseasters. 
# Note that, due to untidy nature of the available data, type flood and flash flood are separate values and should be merged for more accurate data-driven conclusions.
# The most severe weather event in terms of crop damage is the drought. In the last half century, the drought has caused more than 10 billion dollars damage. Other severe crop-damage-causing event types are floods and hails.
