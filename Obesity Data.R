Obesity_data <- read.csv("Share_Adults_Obese.csv")
Overweight_data <- read.csv("Share_Adults_Overweight.csv")
Macronutrient_data <- read.csv("Macronutrient_Diet.csv")
Commodity_data <- read.csv("Commodity_Diet.csv")
Animal_Protein_data <- read.csv("Animal_Protein_Diet.csv")

#Identify which country has an overweight rate of 87.9%--turns out its a small island called Nauru and I fact-checked-true

max(Overweight_data$`Overweight Rate`)
which(grepl(87.9, Overweight_data$`Overweight Rate`))

library(tidyverse)
# Filter for years after 1974
Macro_Data <- Macronutrient_data %>%
  filter(Year >= 1975)

Commodity_Data <- Commodity_data %>%
  filter(Year >= 1975)

Animal_Protein_Data <- Animal_Protein_data %>%
  filter(Year >= 1975)

names(Overweight_data)[4] <- "Overweight Rate"
names(Obesity_data)[4] <- "Obesity Rate"

#Add obesity rate and overweight rate to Commodity Data

Obesity_Rate <- Obesity_data$`Obesity Rate`
Overweight_Rate <- Overweight_data$`Overweight Rate`

#Inner Join Commodity Data to Overweight Data to eliminate incomplete observations using Year and Entity (Country)
install.packages("sqldf")
library(sqldf)

Commodity_Obesity_Data <- sqldf("SELECT *
              FROM Commodity_Data
              JOIN Obesity_data USING(Entity, Year)")

#Inner Join New Dataframe to Overweight Data to eliminate further observations
Commodity_Obesity_Overweight_Data <- sqldf("SELECT *
                                            FROM Commodity_Obesity_Data
                                            JOIN Macro_Data USING(Entity, Year)")
#Inner Join New Dataframe to Macronutrient Data

Commodity_Obesity_Overweight_Data <- sqldf("SELECT *
                                            FROM Commodity_Obesity_Data
                                           JOIN Overweight_data USING(Entity, Year)")
#Inner Join New Dataframe to Animal vs. Plant Protein Data
Commodity_Obesity_Overweight_Data <- sqldf("SELECT *
                                            FROM Commodity_Obesity_Data
                                           JOIN Animal_Protein_data USING(Entity, Year)")
#Remove column 14 (Repeat column)
Commodity_Obesity_Overweight_Data <- Commodity_Obesity_Overweight_Data[,  -14]
Commodity_Obesity_Overweight_Data <- Commodity_Obesity_Overweight_Data[,  -15]

#Rename final dataframe
Health_Diet_Data <- Commodity_Obesity_Overweight_Data 

#Rename Columns
colnames(Health_Diet_Data)[4] <- "Other Calories"
colnames(Health_Diet_Data)[5] <- "Sugar Calories"
colnames(Health_Diet_Data)[6] <- "Oils/Fats Calories"
colnames(Health_Diet_Data)[7] <- "Meat Calories"
colnames(Health_Diet_Data)[8] <- "Dairy/Eggs Calories"
colnames(Health_Diet_Data)[9] <- "Fruits/Vegetables Calories"
colnames(Health_Diet_Data)[10] <- "Starchy Roots Calories"
colnames(Health_Diet_Data)[11] <- "Pulses Calories"
colnames(Health_Diet_Data)[12] <- "Cereals/Grains Calories"
colnames(Health_Diet_Data)[13] <- "Alcohol Calories"

Health_Diet_Data <-       sqldf("SELECT *
                                  FROM Health_Diet_Data
                                  JOIN Macro_Data USING(Entity, Year)")

colnames(Health_Diet_Data)[19] <- "Animal Protein Calories"
colnames(Health_Diet_Data)[20] <- "Plant Protein Calories"
colnames(Health_Diet_Data)[21] <- "Fats Calories"
colnames(Health_Diet_Data)[22] <- "Carbs Calories"

#Make new column combining macronutrient calories to get total calories
Health_Diet_Data$Total_Calories <- Health_Diet_Data$`Animal Protein Calories`+ Health_Diet_Data$`Plant Protein Calories` +
  Health_Diet_Data$`Carbs Calories`+ Health_Diet_Data$`Fats Calories`

Health_Diet_Data <- sqldf("SELECT *
                            FROM Health_Diet_Data
                            JOIN Overweight_data USING(Entity+  Year)")
Health_Diet_Data <- Health_Diet_Data[,  -24]

#Combine 4 Macronutrients (Animal Protein+  Plant Protein+  Fats+  Carbs) into one column called Macronutrients
library(tidyr)
library(dplyr)

Health_Diet_Data <- gather(Health_Diet_Data, Macronutrient_Calories, Calories_from_Macronutrient, `Animal Protein Calories`, `Plant Protein Calories`, `Fats Calories`, `Carbs Calories`)

#Make new column combining 10 Food Groups to get total calories from each
Health_Diet_Data <- gather(Health_Diet_Data, Food_Group_Calories, Calories_from_Food_Group,  
                           `Other Calories`, `Sugar Calories`, `Oils/Fats Calories`,
                           `Meat Calories`,  `Dairy/Eggs Calories`,  `Fruits/Vegetables Calories`,  
                           `Starchy Roots Calories`,  `Pulses Calories`,  `Cereals/Grains Calories`,  
                           `Alcohol Calories`)

# Load New Dataframe to Desktop Documents to use in Tableau
write.csv(Health_Diet_Data+  "Health_Diet_Data.csv")

Death_causes <- read.csv("Causes_of_death.csv")

#Filter to only Year 2013
Death_causes$Year <- as.integer(Death_causes$Year)

#Make Executions column from factor to integer
Death_causes$Execution..deaths. <- as.integer(Death_causes$Execution..deaths.)
str(Death_causes)

Death_causes <- Death_causes %>%
  filter(Year == 2013)

#Make new column of Total Deaths from Major Diseases

Death_causes$Total_deaths <- Death_causes$Dementia + Death_causes$Cardiovascular.diseases + Death_causes$Kidney.disease + Death_causes$Respiratory.disease +
  Death_causes$Liver.disease + Death_causes$Diabetes..blood.and.endocrine.disease +
  Death_causes$Digestive.disease + Death_causes$Hepatitis + Death_causes$Cancers +
  Death_causes$Parkinson.s.disease + Death_causes$Fire +  Death_causes$Malaria +  Death_causes$Drowning +
  Death_causes$Homicide + Death_causes$HIV.AIDS + Death_causes$Drug.disorder + Death_causes$Tuberculosis +
  Death_causes$Road.incidents +  Death_causes$Maternal.deaths +
  Death_causes$Neonatal.deaths +  Death_causes$Alcohol.disorder +  Death_causes$Natural.disasters +
  Death_causes$Diarrheal.diseases + Death_causes$Heat.related.deaths..hot.or.cold.exposure. +
  Death_causes$Nutritional.deficiencies + Death_causes$Suicide +  Death_causes$Meningitis..deaths. +
  Death_causes$Lower.respiratory.infections..deaths. +  Death_causes$Intestinal.infectious.diseases..deaths. +
  Death_causes$Protein.energy.malnutrition..deaths. +  Death_causes$Conflict..deaths. + Death_causes$Terrorism..deaths.

Death_causes$Total_deaths <- as.integer(Death_causes$Total_deaths)

#Combine causes of death into one column called Major Causes of Death
Death_causes <- gather(Death_causes, Major_Cause_of_Death,  Deaths_from_Major_Cause, Dementia,
                       Cardiovascular.diseases, Kidney.disease,
                       Respiratory.disease, Liver.disease,
                       Diabetes..blood.and.endocrine.disease, Digestive.disease,
                       Hepatitis, Cancers,
                       Parkinson.s.disease, Fire, Malaria, Drowning, 
                       Homicide, HIV.AIDS, Drug.disorder, Tuberculosis, 
                       Road.incidents, Road.incidents, Maternal.deaths,
                       Neonatal.deaths, Alcohol.disorder, Natural.disasters,
                       Diarrheal.diseases, Heat.related.deaths..hot.or.cold.exposure.,
                       Nutritional.deficiencies, Suicide, Meningitis..deaths., 
                       Lower.respiratory.infections..deaths., Intestinal.infectious.diseases..deaths.,
                       Protein.energy.malnutrition..deaths., Conflict..deaths., Terrorism..deaths.)

#Filter Health_Diet_Data to only year 2013

Health_Diet_Data_2013 <- Health_Diet_Data %>%
  filter(Year == 2013)

Health_Diet_Data_2013 <- Health_Diet_Data_2013[, -8:-14]
Health_Diet_Data_2013 <- Health_Diet_Data_2013[, -5:-6]
Health_Diet_Data_2013 <- Health_Diet_Data_2013[-166:-6600, ]

#Merge dataframes
Combination_df <- merge(Death_causes, Health_Diet_Data)

#Delete Code Column
Combination_df <- Combination_df[, -12]
# Find incidence of specific diseases by dividing the population of a country by 1000, then divide number of deaths from major cause from the result of the population per thousands

colnames(Combination_df)[11] <- "Population"

Combination_df$Population_per_thousand <- Combination_df$Population/1000
Combination_df$Incidence_of_death <- Combination_df$Deaths_from_Major_Cause/Combination_df$Population_per_thousand

# Load New Dataframe to Desktop Documents to use in Tableau
write.csv(Combination_df, "Combination_df.csv")





