### Loading libraries
library(dplyr)
library(tidyr)
library(rcompanion)


### Data Wrangling
## Exiting the Foster Care System dataset
Exiting_FC = Children_exiting_foster_care_by_age_group
Exiting_FC = na.omit(Exiting_FC)
Exiting_FC = filter(Exiting_FC, DataFormat != 'Percent')
Exiting_fc = select(Exiting_FC, c('Location', 'Age group', 'TimeFrame', 'Data'))
Exiting_age = names(Exiting_fc)[names(Exiting_fc) == "Age group"] <- "AgeGroup"

# Subsetting United States' Total data
Exiting_USA = Exiting_fc %>% filter((Location == 'United States') & (AgeGroup == 'Total') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Exiting_USA = Exiting_USA %>% mutate(Data = as.numeric(Data))
Exiting_USA1 = aggregate(Data~Location, Exiting_USA, sum)

# Subsetting the 10 States' data
Exiting_States = Exiting_fc %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'New Jersey', 'Ohio', 'Texas', 'Utah', 'Vermont')) & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Exiting_States = Exiting_States %>% mutate(Data = as.numeric(Data))
Exiting_States1 = aggregate(Data~Location + AgeGroup, Exiting_States, sum)

## Youth incarceration dataset
Juveniles = Youth_residing_in_juvenile_detention_correctional_and_or_residential_facilities
Juveniles = na.omit(Juveniles)
Juveniles = filter(Juveniles, DataFormat != 'Rate per 100,000')
Juveniles_IN = select(Juveniles, c('Location', 'TimeFrame', 'Data'))

# Subsetting United States' Total data
Juveniles_USA = Juveniles_IN %>% filter((Location == 'United States') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Juveniles_USA = Juveniles_USA %>% mutate(Data = as.numeric(Data))
Juveniles_USA1 = aggregate(Data~Location, Juveniles_States, sum)

# Subsetting the 10 States' data
Juveniles_States = Juveniles_IN %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'New Jersey', 'Ohio', 'Texas', 'Utah', 'Vermont')) & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
Juveniles_States = Juveniles_States %>% mutate(Data = as.numeric(Data))
Juveniles_States1 = aggregate(Data~Location, Juveniles_States, sum)