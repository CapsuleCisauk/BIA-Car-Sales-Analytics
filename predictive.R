# Exploring a dataset that I found on https://www.kaggle.com/datasets/missionjee/car-sales-report

setwd("C:/Users/set working directory")

# Data Wrangling & Plotting
library(tidyverse)
library(data.table)


# Get Data
cars <- fread("car_data.csv", sep=",", header= TRUE, encoding = 'Latin-1')

# Clean Data
colnames(cars)[colnames(cars) == "Price ($)"] <- "Price"
cars$Date <- as.Date(cars$Date, format = "%m/%d/%Y")


# Revenue per Day
data <- cars %>%
  group_by(Date) %>%
  summarise(revenue = sum(Price, na.rm = TRUE)) %>%
  ungroup()

ggplot(data) +
  geom_line(aes(x=Date, y=revenue), color = "red") +
  theme_set(theme_bw()) +
  labs(x = "Date", y = "Sales in $") +
  ggtitle("Revenue per Day") +
  theme(plot.title = element_text(size = 12, face = 4, hjust = 1))+
  scale_y_continuous(labels = scales::comma)  


# linear regression
lmSales <- lm(data = cars, Price ~ Date)
summary(lmSales)
head(cars, 3)

ggplot(data, aes(x = Date, y = revenue)) +
  geom_line(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  theme_bw() +
  labs(x = "Date", y = "Sales in $") +
  ggtitle("Revenue per Day") +
  theme(plot.title = element_text(size = 12, face = 4, hjust = 1)) +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", 
           x = max(data$Date), 
           y = max(data$revenue), 
           label = paste("Prediction Accuracy (R-squared):", round(summary(lmSales)$r.squared, 6)), 
           hjust = 1)

# The R-squared value of 1.1e-05 indicates limited model explanatory power, possibly due to the small dataset with only two years of information.
# Consideration of additional data could enhance model performance by capturing a broader range of patterns and variations.
# Further evaluation or exploration of alternative modeling approaches may be beneficial.




