library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
#resetting RStudio Environment
rm(list = ls())
#set working directory
setwd('C:/Users/nicho/Documents/r_project/student')

course <- read_excel('Course.xlsx', .name_repair = 'universal')
student <- read_excel('Student.xlsx', .name_repair = 'universal')
registration <- read_excel('Registration.xlsx', .name_repair = 'universal')

student_registration <- left_join(student, registration, by = c('Student.ID'))
df <- left_join(student_registration, course, by = c('Instance.ID'))

df_majors <- df %>%
  group_by(Title) %>%
  summarize(count = n())

ggplot(df_majors, aes(x = Title, y = count, fill = Title)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1)) +
  labs(title = "Majors")

df[c('DOB.Year', 'DOB.Month', 'DOB.Date')] <- str_split_fixed(df$'Birth.Date', '-', 3)

birthyear_df <- df %>%
  group_by(DOB.Year) %>%
  summarize(count = n())

ggplot(birthyear_df, aes(x = DOB.Year, y = count)) +
  geom_col() + 
  theme(axis.text = element_text(angle = 90, vjust = .5, hjust = 1))

major_totalcost <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(sum(Total.Cost))

balancedue_major <- df %>%
  group_by(Title) %>%
  summarize(sum(Balance.Due))

major_totalcost <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(total_cost = sum(Total.Cost))

ggplot(major_totalcost, aes(x = Title, y = total_cost, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Cost per Major Segmented by Payment Plan", x = "Major", y = "Total Cost") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank())

balance_due_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(total_balance_due = sum(Balance.Due, na.rm = TRUE), .groups = "drop")

ggplot(balance_due_major, aes(x = Title, y = total_balance_due, fill = Payment.Plan)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Balance Due per Major Segmented by Payment Plan", x = "Major", y = "Total Balance Due") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title = element_blank())
