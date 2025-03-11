library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(here) 
library(kableExtra)
#reset
rm(list = ls())

setwd('C:/Users/nicho/Documents/r_project/fish')

fish <- read_csv("fish.csv")
kelp_abur <- read_excel("kelp_fronds.xlsx", sheet = "abur")

fish_garibaldi <- fish %>% 
  filter(common_name == "garibaldi")

fish_mohk <- fish %>% 
  filter(site == "mohk")

fish_over50 <- fish %>% 
  filter(total_count >= 50)

fish_3sp <- fish %>% 
  filter(common_name == "garibaldi" | 
         common_name == "blacksmith" | 
         common_name == "black surfperch")

fish_3sp <- fish %>% 
  dplyr::filter(common_name %in% c("garibaldi", "blacksmith", "black surfperch"))

fish_gar_2016 <- fish %>% 
  dplyr::filter(year == 2016 | common_name == "garibaldi")

aque_2018 <- fish %>% 
  dplyr::filter(year == 2018, site == "aque")

aque_2018 <- fish %>% 
  dplyr::filter(year == 2018 & site == "aque")

aque_2018 <- fish %>% 
  dplyr::filter(year == 2018) %>% 
  dplyr::filter(site == "aque")

fish_bl <- fish %>% 
  dplyr::filter(str_detect(common_name, pattern = "black"))

low_gb_wr <- fish %>% 
  dplyr::filter(common_name %in% c("garibaldi", "rock wrasse"), 
         total_count <= 10)

fish_it <- fish %>% 
  dplyr::filter(str_detect(common_name, pattern = "it"))

abur_kelp_fish <- kelp_abur %>% 
  full_join(fish, by = c("year", "site")) 

kelp_fish_left <- kelp_abur %>% 
  left_join(fish, by = c("year","site"))

kelp_fish_injoin <- kelp_abur %>% 
  inner_join(fish, by = c("year", "site"))

my_fish_join <- fish %>% 
  filter(year == 2017, site == "abur") %>% 
  left_join(kelp_abur, by = c("year", "site")) %>% 
  mutate(fish_per_frond = total_count / total_fronds)

kable(my_fish_join)

my_fish_join %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE)