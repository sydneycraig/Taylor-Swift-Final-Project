## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
sd(data$song_duration)
sd(data$popularity)

mean(data$song_duration)
mean(data$popularity)

table(data$tone)
table(data$genre)
table(data$tone, data$genre)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
ggplot(data, aes(x = tone, y = `song_duration` )) +
  geom_boxplot() +
  labs(title = "Song Tone and Its Duration",
       x = "tone",
       y = "song_duration") +
  theme_minimal() 
anova <- aov(song_duration ~ tone, data = data)
summary(anova)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$song_duration, data$popularity)
print(linear_plot)

meany <- mean(data$song_duration)
meanx <- mean(data$popularity)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")

linear_relationship <- lm(song_duration ~ popularity, data = data)
summary (linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
plot(data$song_duration, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$tone,data$genre)
chisq.test(data$tone,data$genre)

