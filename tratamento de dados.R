library(tidyverse)

phs <- read_csv("~/Estudos de R/phs_2021_1.csv")

phs$stat_amount <- round(phs$stat_amount,2)

tabela_stats <- phs %>% select(stat_name,team_name, hero_name, stat_amount) %>% 
        filter(stat_name %in% c("All Damage Done","Healing Done")) %>% 
        group_by(stat_name, hero_name,team_name) %>% summarise(soma = sum(stat_amount)) %>% 
        ungroup() %>% droplevels(.) %>% 
        pivot_wider(names_from = c(hero_name), values_from = soma)


tabela_stats <- phs %>% select(stat_name,team_name, hero_name, stat_amount) %>% 
                filter(stat_name == "All Damage Done" & hero_name != "All Heroes")%>% 
               group_by(hero_name,team_name) %>% summarise(soma = sum(stat_amount)) %>% 
               arrange(desc(soma)) %>% 
               ungroup() %>% droplevels(.) %>% 
               group_by(team_name) %>% 
               mutate(nrow = 1:n()) %>% 
               filter(nrow <= 5) %>% 
               tibble()
  
               
             


tabela_stats %>% group_by(hero_name) %>% summarise(n())