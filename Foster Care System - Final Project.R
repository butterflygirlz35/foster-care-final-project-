### Loading libraries
library(dplyr)
library(tidyr)
library(rcompanion)

### Data Wrangling
## Exiting the Foster Care System dataset
Exiting_FC = Children_exiting_foster_care_by_age_group
Exiting_FC = filter(Exiting_FC, DataFormat != 'Percent')
Exiting_FC = Exiting_FC %>% mutate(Data = as.numeric(Data))
Exiting_fc = select(Exiting_FC, c('Location', 'Age group', 'TimeFrame', 'Data'))
Exiting_fc1 = Exiting_fc %>% filter(Location != 'United States') # Using for Tableau
Exiting_age = names(Exiting_fc)[names(Exiting_fc) == "Age group"] <- "AgeGroup"

# Subsetting United States' Total data
Exiting_USA = Exiting_fc %>% filter((Location == 'United States') & (AgeGroup != 'Total') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Exiting_USA1 = aggregate(Data~Location + AgeGroup, Exiting_USA, sum)

# Subsetting the 10 States' data
Exiting_States = Exiting_fc %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')) & (AgeGroup == 'Total') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Exiting_States1 = aggregate(Data~Location + AgeGroup, Exiting_States, sum)

## Youth incarceration dataset
Juveniles = Youth_residing_in_juvenile_detention_correctional_and_or_residential_facilities
Juveniles = Juveniles %>% mutate(Data = as.numeric(Data))
Juveniles = filter(Juveniles, DataFormat != 'Rate per 100,000')
Juveniles_IN = select(Juveniles, c('Location', 'TimeFrame', 'Data'))
Juveniles_IN = Juveniles_IN %>% mutate(Data = as.numeric(Data))

# Subsetting United States' Total data
Juveniles_USA = Juveniles_IN %>% filter((Location == 'United States') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Juveniles_USA1 = aggregate(Data~Location, Juveniles_USA, sum)

# Adding blank AgeGroup column and reordering the columns
Juveniles_USA1$AgeGroup = " "
Juveniles_usa = Juveniles_USA1[, c(1, 3, 2)]

# Subsetting the 10 States' data
Juveniles_States = Juveniles_IN %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')) & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Juveniles_States = Juveniles_States %>% mutate(Data = as.numeric(Data))
Juveniles_States1 = aggregate(Data~Location, Juveniles_States, sum)

# Adding blank AgeGroup column and reordering the columns
Juveniles_States1$AgeGroup = " "
Juveniles_states = Juveniles_States1[, c(1, 3, 2)]

## Combine the datasets together by rows
# USA datesets 
Foster_USA = rbind(Exiting_USA1, Juveniles_usa)

# States datasets
Foster_States = rbind(Exiting_States1, Juveniles_states)

## Combine the datasets together by columns
# USA datasets
Foster_USA1 = merge(Exiting_USA1, Juveniles_usa, by=c('Location'))

# States datasets
Foster_States1 = merge(Exiting_States1, Juveniles_states, by=c('Location'))


