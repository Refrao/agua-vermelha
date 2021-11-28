library(tidyverse)

#Ler arquivo

phs <- read_csv("~/Estudos de R/phs_2021_1.csv")

#alterar 
phs$stat_amount <- round(phs$stat_amount,2)

tabela_stats <- phs %>% select(stat_name,team_name, hero_name, stat_amount) %>% 
        filter(stat_name %in% c("All Damage Done","Healing Done")) %>% 
        group_by(stat_name, hero_name,team_name) %>% summarise(soma = sum(stat_amount)) %>% 
        ungroup() %>% droplevels(.) %>% 
        pivot_wider(names_from = c(hero_name), values_from = soma)

# 

tabela_stats <- phs %>% select(stat_name,team_name, hero_name, stat_amount) %>% 
                filter(stat_name == "All Damage Done" & hero_name != "All Heroes")%>% 
               group_by(hero_name,team_name) %>% summarise(soma = sum(stat_amount)) %>% 
               arrange(team_name,desc(soma)) %>% 
               ungroup() %>% droplevels(.) %>% 
               group_by(team_name) %>% 
               mutate(nrow = 1:n()) %>% 
               filter(nrow <= 5) %>% 
               tibble()


# 

t1 <- tabela_stats %>% group_by(hero_name) %>% summarise(qtd = n(), soma= sum(soma))
t2 <- phs %>% filter(stat_name == "All Damage Done" & hero_name != "All Heroes") %>% 
              group_by(hero_name) %>% summarise(qtd = n(),soma = sum(stat_amount)) %>% 
              ungroup() %>% droplevels(.) %>% 
              tibble()

write.csv(tabela_stats, "C:\\Projeto\\a.csv")

tfinal <- t1 %>% inner_join(t2, by= "hero_name")
tfinal <- tfinal %>% rename(soma = 3, qtd = 2, qtdt = 4, somt = 5) %>% arrange(desc(qtd)) %>% head(5)
tfinal$somt <- tfinal$somt*10^3

library(plotly)

ggplotly(
tfinal %>% ggplot(mapping = aes(x = qtdt, y=somt, size = qtd)) + geom_point(aplha = 0.5, colour = "blue"))
)


phs <- phs %>% separate(start_time, into= c("data", "hora"), sep = " ") 

phs$data <- as.Date(phs$data)

library(janitor)

phs_1 <- phs %>% filter(hero_name != "All Heroes") %>% 
                 group_by(data, map_name) %>% 
                 summarise(soma = sum(stat_amount)) %>% 
                 ungroup() %>%  droplevels(.) %>% 
                 pivot_wider(names_from = map_name, values_from = soma) %>% 
                 adorn_totals()
                 




        


