### Load in necessary Libraries
library(dplyr)
library(tidyr)
library(gmodels)
library(rcompanion)
library(readxl)

### Load in the datasets for evaluation
Exiting_FC = read_excel('C:/Users/13612/Desktop/Course Final Project/Assignments/Datasets/Children exiting foster care by age group.xlsx')
Juveniles = read_excel('C:/Users/13612/Desktop/Course Final Project/Assignments/Datasets/Youth residing in juvenile detention, correctional and_or residential facilities.xlsx')
Juveniles_2 = read.csv('C:/Users/13612/Desktop/Course Final Project/Assignments/Datasets/ezacjrp_export.csv')

##### Shared Data wrangling for the Evaluation Questions

## Exiting the Foster Care System dataset
Exiting_FC = filter(Exiting_FC, DataFormat != 'Percent') # Getting rid of the percent rows in the DataFormat column
Exiting_FC = Exiting_FC %>% mutate(Data = as.numeric(Data)) # Changing the Data from a character to a numeric format

## Juvenile incarceration dataset
Juveniles = filter(Juveniles, DataFormat != 'Rate per 100,000') # Dropping the Rate per 100,000 rows in the DataFormat column
Juveniles = Juveniles %>% mutate(Data = as.numeric(Data)) # Changing the Data from a character to a numeric format


### Shared Data Exploration graphs
Tableau_Exiting = select(Exiting_FC, c('Location', 'Age group', 'TimeFrame', 'Data')) # Selecting the four columns wanted for the graphs
# Dropping the United States and Puerto Rico rows from the Location column and selecting the desired years in the TimeFrame column
Tableau_Exiting_Graph = Tableau_Exiting %>% filter(Location != 'United States')
Tableau_Exiting_Graph = Tableau_Exiting %>% filter((Location != 'Puerto Rico') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
# Selecting the 10 States we chose for Eval Question 1
Tableau_Exiting_Graph2 = Tableau_Exiting %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))
# Exporting the csv files to be used in Tableau for graphing purposes
write.csv(Tableau_Exiting_Graph, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Exiting 50 States.csv', row.names=TRUE)
write.csv(Tableau_Exiting_Graph2, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Exiting 10 States.csv', row.names=TRUE)


Tableau_Juveniles = select(Juveniles, c('Location', 'TimeFrame', 'Data')) # Selecting the three columns wanted for the graphs
# Dropping the United States and Puerto Rico rows from the Location column and selecting the desired years in the TimeFrame column
Tableau_Juveniles_Graph = Tableau_Juveniles %>% filter(Location != 'United States')
Tableau_Juveniles_Graph = Tableau_Juveniles %>% filter((Location != 'Puerto Rico') & (TimeFrame %in% c('2001', '2003', '2006', '2007', '2010', '2011', '2013', '2015', '2017', '2019')))
# Selecting the 10 States we chose for Eval Question 1
Tableau_Juveniles_Graph2 = Tableau_Juveniles %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))
# Exporting the csv files to be used in Tableau for graphing purposes
write.csv(Tableau_Juveniles_Graph, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Juveniles 50 States.csv', row.names=TRUE)
write.csv(Tableau_Juveniles_Graph2, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV Dataframes\\Juveniles 10 States.csv', row.names=TRUE)



#### Evaluation Question 1: "Do 10 States with children leaving foster care have an influence on the juvenile incarceration numbers?"

### Data Wrangling

## Exiting the Foster Care System dataset
Exiting_FC1 = select(Exiting_FC, c('Location', 'Data')) # Selecting the two columns needed for analysis
# Selecting the 10 States we chose
Exiting_States = Exiting_FC1 %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))
Exiting_States1 = aggregate(Data~Location, Exiting_States, sum) # Getting the sum of the Data column for all years combined

## Youth Incarceration dataset
Juveniles_IN = select(Juveniles, c('Location', 'Data')) # Selecting the two columns needed for analysis
# Selecting the 10 States we chose
Juveniles_States = Juveniles_IN %>% filter((Location %in% c('Arizona', 'California', 'Colorado', 'Connecticut', 'Georgia', 'Indiana', 'Ohio', 'Texas', 'Utah', 'Vermont')))
Juveniles_States1 = aggregate(Data~Location, Juveniles_States, sum) # Getting the sum of the Data column for all years combined

### Data Analysis

## Testing assumptions
CrossTable(Exiting_States1$Data, Juveniles_States1$Data, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format='SPSS')

## Conclusions
# The p-value is .02313417, which is greater than .05 making this analysis not significant. Also no cells have any values > 5
# There is no influence from children leaving foster care by state to the incarceration numbers.


#### Evaluation Question 2: " Does the age group of children exiting foster care have an influence on juvenile incarceration numbers?"

### Data Wrangling

## Exiting the Foster Care System dataset
Exiting_Age = names(Exiting_FC)[names(Exiting_FC) == 'Age group'] <- "AgeGroup" # Changing the Age Group column so avoid confusion with codes
Exiting_FC2 = select(Exiting_FC, c('Location', 'AgeGroup', 'Data')) # Selecting the two columns needed for analysis
Exiting_USA = Exiting_FC2 %>% filter((Location == 'United States') & (AgeGroup != 'Total')) # Filtering only United States from the Location column and dropping the Total rows from the AgeGroup columns
Exiting_USA1 = aggregate(Data~Location + AgeGroup, Exiting_USA, sum) # Getting the sum of the Data column for all years combined
# Exporting the csv file to recode the dataset in Excel
write.csv(Exiting_USA1, 'C:\\Users\\13612\\Desktop\\Course Final Project\\CSV DataFrames\\Exiting_USA1.csv', row.names=TRUE)
Exiting_USA2 <- read.csv("C:/Users/13612/Desktop/Course Final Project/Assignments/Datasets/Exiting_USA2.csv") # Reading in the csv file that has been recoded

## Youth incarceration dataset
Juveniles_USA = select(Juveniles_2, c('AgeGroup', 'Data')) #Keeping only the two needed columns for analysis
Juveniles_USA$Location = 'United States' # Adding a the Location column
Juveniles_USA = na.omit(Juveniles_USA) # Dropping any na rows
Juveniles_USA1 = Juveniles_USA[, c(3,1,2)] # Reorganizing the columns to match Exiting_USA2 dataframe

### Data Analysis

## Testing assumptions
CrossTable(Exiting_USA2$AgeGroup, Juveniles_USA1$Data, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format='SPSS')

## Conclusions
# The p-value is .1572992, which is < .05, making this analysis not significant.  Also no cells have any values > 5
# There is no influence from children leaving foster care by age group to the incarceration numbers
