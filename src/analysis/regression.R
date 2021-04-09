#reading input
airbnb <- read.csv("../../data/data.csv", sep=",")

#install.packages("fastDummies")
library(fastDummies)
library(dplyr)

#creating a dummy for covid yes or no
class(airbnb$date)
airbnb$date <- as.Date(airbnb$date)
airbnb <- airbnb %>% 
            mutate(covid = ifelse(date >= "2020-03-01", 1, 0))

airbnb <- dummy_cols(airbnb, select_columns = "covid")

#creating a dummy for neighbourhood
airbnb <- dummy_cols(airbnb, select_columns = "neighbourhood")

#creating datasets for the linear analysis
neighbourhood <- airbnb[10:31]
covid <- airbnb[8:9]

mdl_airbnb <- lm(num_reviews ~ month + covid + neighbourhood, data=airbnb)

neigbourhood_label <- (colnames(neighbourhood))
reg_name <- substr(neigbourhood_label, 15, nchar(neigbourhood_label))

# Where to safe the outcome
dir.create("../../gen")
dir.create("../../gen/analysis")
dir.create("../../gen/analysis/output")

library(stargazer)
stargazer(mdl_airbnb, 
          title = "Influence of several factors on reviews in Amsterdam ",
          dep.var.caption = "Reviews",  
          dep.var.labels = "",  
          covariate.labels = c("Month", "Covid", reg_name),  
          notes.label = "Significance levels",  
          type="html",
          out="../../gen/analysis/output/regression.html"  
)







