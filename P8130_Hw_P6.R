################################################################
#             Biostatistical Methods I: Homework P6            #
#                                                              #
#           Author: Shan Jiang; Date: Sept 22, 2018            #
################################################################
# Prolem 2(c)
(6000 - 650)/6000 * (1-(85/100)) + 650/6000 * (75/100) 
75/100*650/6000/0.215
# Prolem 3(a)
dbinom(5, size = 10, prob = 0.08) 
## It can also be written as the function 
factorial(10)/(factorial(5) * factorial(10 - 5)) * (1 - 0.08)^{5}* 0.08^(10 - 5)

# Prolem 3(b) For women, the distribution probability will be changed. 
dbinom(5, size=10, prob=0.3) 

# Prolem 4(a)
dpois(30, lambda=8.55*5/1000000) 
# Prolem 4(b)
dpois(30, lambda=8.55*6.02*0.446/1000000) 
dpois(30, lambda=8.55*0.31*0.251/1000000)
dpois(30, lambda=8.55*0.39*0.118/1000000) 


# Prolem 6(c)
# Import Data from xlsx file and detect the NAs in the data
library(tidyverse)
Migraine <- readxl::read_excel("~/Downloads/CUMC Y1/Biostat Method/Hw 2/Migraine.xlsx")
any(is.na(Migraine))
nrow(Migraine)

# Find out the NAs in each column and Rename the dataframe
Migraine_NA = Migraine
sum(is.na(Migraine_NA))
colSums(is.na(Migraine_NA))
mean()

# Removing NAs in R dataframes
Migraine_clean = na.omit(Migraine_NA)
View(Migraine_clean)
nrow(Migraine_clean)
colSums(is.na(Migraine_clean))
 
# Exploring the dataframe after data cleaning

names(Migraine_clean)
head(Migraine_clean)
tail(Migraine_clean)

# Access the data with name  
Migraine_df = na.omit(Migraine_NA)
Migraine_df %>% 
  group_by(Migraine_df$Migraine) %>% 
  summarize(Avg_ABNAS = mean(Migraine_df$`ABNAS language`, na.rm = TRUE), 
            count = n()) 
Migraine_df %>% 
  summarize(Avg_ABNAS = mean(Migraine_df$`ABNAS language`, na.rm = TRUE), 
            count = n()) 
Migraine_table1 <- table(Avg_ABNAS, Migraine_df$`ABNAS memory`) 
Migraine_table1          

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