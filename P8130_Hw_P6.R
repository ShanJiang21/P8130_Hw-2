################################################################
#             Biostatistical Methods I: Homework P6            #
#                                                              #
#           Author: Shan Jiang; Date: Sept 22, 2018            #
################################################################

# Import Data from xlsx file and detect the NAs in the data
library(tidyverse)
Migraine <- read_excel("~/Downloads/CUMC Y1/Biostat Method/Hw 2/Migraine.xlsx")
any(is.na(Migraine))

# Find out the NAs in each column and Rename the dataframe
Migraine_NA = Migraine
sum(is.na(Migraine_NA))
colSums(is.na(Migraine_NA))


# Exploring the dataframe
names(Migraine_NA)
head(Migraine_NA)
tail(Migraine_NA)




# Access the data with name  
Migraine_df %>% 
  group_by(Migraine_df$Migraine) %>% 
  summarize(Avg_ABNAS = mean(Migraine_df$`ABNAS language`, na.rm = TRUE), 
            count = n()) 
Migraine_df %>% 
  group_by(Migraine_df$Migraine) %>% 
  summarize(Avg_ABNAS = mean(Migraine_df$`ABNAS language`, na.rm = TRUE), 
            count = n()) 

           

# Summarize three variables for with migraine and without migraine
library(tidyverse)
as.numeric(Migraine_df$CESD)
# The sample size for all 3 variables  with migraine and without migraine

summarize(Migraine_df, 
          Avg_CESD = mean(as.numeric(Migraine_df$CESD)), 
          Avg_NDDIE = mean(Migraine_df$NDDIE),
          Avg_Amemo = mean(Migraine_df$`ABNAS memory`),  
          Avg_Alang = mean(Migraine_df$`ABNAS language`))
View(Migraine_df)
library(reshape2)