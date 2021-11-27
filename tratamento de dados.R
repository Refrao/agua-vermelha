library(tidyverse)

phs <- read_csv("~/Estudos de R/phs_2021_1.csv")

phs$stat_amount <- round(phs$stat_amount,2)

a<- phs %>% select(stat_name,team_name, hero_name, stat_amount) %>% 
        filter(stat_name %in% c("All Damage Done","Healing Done")) %>% 
        group_by(stat_name, hero_name,team_name) %>% summarise(soma = sum(stat_amount)) %>% 
        ungroup() %>% droplevels(.) %>% 
        pivot_wider(names_from = c(hero_name), values_from = soma)