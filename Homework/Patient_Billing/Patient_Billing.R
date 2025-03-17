library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(viridis)
library(hrbrthemes)
#resetting RStudio Environment
rm(list = ls())
#set working directory
setwd('C:/Users/nicho/Documents/r_project/data/patient_billing')

billing <- read_excel("Billing.xlsx")
patient <- read_excel("Patient.xlsx")
visit <- read_excel("Visit.xlsx")

patient_visitjoin <- left_join(patient, visit, by = "PatientID")

#create Data
df <- left_join(patient_visitjoin, billing, by = "VisitID")

df <- df %>%
  mutate(VisitMonth = lubridate::month(VisitDate, label = TRUE))

#work for Reason for visit segmented by Month of the Year
reason_visit_count <- df %>%
  group_by(VisitMonth, Reason) %>%
  summarise(visit_count = n()) %>%
  ungroup()

reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypertension", "Hypertension monitoring"), "Hypertension", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Laceration of left hand", "Laceration of right calf", "Laceration of right foot"), "Laceration", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypotension", "Hypotension monitoring"), "Hypotension", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Dermatitis", "Dermatitis follow-up"), "Dermatitis", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Rhinitis", "Rhinitis follow-up"), "Rhinitis", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Allergic reaction", "Allergic reaction follow-up"), "Allergic reaction", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Bronchitis", "Bronchitis follow-up"), "Bronchitis", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Migraine", "Migraine follow-up"), "Migraine", Reason))
reason_visit_count <- reason_visit_count %>%
  mutate(Reason = ifelse(Reason %in% c("Fracture of left fifth metacarpel", "Fracture of right tibia"), "Fracture", Reason))

#creat grapgh for Reason for visit segmented by Month of the Year
ggplot(reason_visit_count, aes(x = VisitMonth, y = visit_count, fill = Reason)) +
  geom_bar(stat = "identity") +
  labs(title = "Reasons for Visit Segmented by Month of the Year",
       x = "Month",
       y = "Number of Visits",
       fill = "Reason for Visit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  guides(fill = guide_legend(ncol = 2)) 

#filter for visit segmented by Walk-in Status
reason_walkin_count <- df %>%
  group_by(Reason, WalkIn) %>%
  summarise(visit_count = n()) %>%
  ungroup()

#work for visit segmented by Walk-in Status
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypertension", "Hypertension monitoring"), "Hypertension", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Laceration of left hand", "Laceration of right calf", "Laceration of right foot"), "Laceration", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypotension", "Hypotension monitoring"), "Hypotension", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Dermatitis", "Dermatitis follow-up"), "Dermatitis", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Rhinitis", "Rhinitis follow-up"), "Rhinitis", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Allergic reaction", "Allergic reaction follow-up"), "Allergic reaction", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Bronchitis", "Bronchitis follow-up"), "Bronchitis", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Migraine", "Migraine follow-up"), "Migraine", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("Fracture of left fifth metacarpel", "Fracture of right tibia"), "Fracture", Reason))
reason_walkin_count <- reason_walkin_count %>%
  mutate(Reason = ifelse(Reason %in% c("UTI", "UTI follow-up"), "UTI", Reason))

#graph for visit segmented by Walk-in Status
ggplot(reason_walkin_count, aes(x = Reason, y = visit_count, fill = WalkIn)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reasons for Visit Segmented by Walk-In Status",
       x = "Reason for Visit",
       y = "Number of Visits",
       fill = "Walk-In") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

#filter for visit segmented by city/state or zip code
city_reason_count <- df %>%
  group_by(City, Reason) %>%
  summarise(visit_count = n()) %>%
  ungroup()

#work for visit segmented by city/state or zip code
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypertension", "Hypertension monitoring"), "Hypertension", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Laceration of left hand", "Laceration of right calf", "Laceration of right foot"), "Laceration", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Hypotension", "Hypotension monitoring"), "Hypotension", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Dermatitis", "Dermatitis follow-up"), "Dermatitis", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Rhinitis", "Rhinitis follow-up"), "Rhinitis", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Allergic reaction", "Allergic reaction follow-up"), "Allergic reaction", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Bronchitis", "Bronchitis follow-up"), "Bronchitis", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Migraine", "Migraine follow-up"), "Migraine", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("Fracture of left fifth metacarpel", "Fracture of right tibia"), "Fracture", Reason))
city_reason_count <- city_reason_count %>%
  mutate(Reason = ifelse(Reason %in% c("UTI", "UTI follow-up"), "UTI", Reason))

#graph for visit segmented by city/state or zip code test
ggplot(city_reason_count, aes(x = City, y = visit_count, fill = Reason)) +
  geom_bar(stat = "identity") +
  labs(title = "Reasons for Visit Segmented by City/State or Zip Code",
       x = "City/State or Zip Code",
       y = "Number of Visits",
       fill = "Reason for Visit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  guides(fill = guide_legend(ncol = 2))

#filter for total invoice amount segmented by reason for visit and payment status
reason_payment_amount <- df %>%
  group_by(Reason, InvoiceAmt) %>%
  summarise(total_amount = sum(InvoiceAmt)) %>%
  ungroup()

#graph for total invoice amount segmented by reason for visit and payment status
ggplot(reason_payment_amount, aes(x = Reason, y = total_amount, fill = InvoiceAmt)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Invoice Amount Segmented by Reason for Visit and Payment Status",
       x = "Reason for Visit",
       y = "Total Invoice Amount",
       fill = "Payment Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_y_continuous(labels = scales::dollar)

#busiest month filter
busiest_month <- df %>%
  group_by(VisitMonth) %>% 
  summarize(Count = n ()) %>%
  mutate(VisitMonth = month.abb[as.numeric(VisitMonth)])

#plot for busiest month
ggplot(busiest_month, aes(fill=VisitMonth, y=Count, x=VisitMonth)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Busiest Month") +
  theme_ipsum() +
  xlab("")


