library(tidyverse)
library(dplyr)
library(lpSolve)

players <- as.tibble(read.csv('../data/2021-22/cleaned_players.csv'))
players_old <- as.tibble(read.csv('../data/2020-21/cleaned_players.csv')) %>% select('first_name', 'second_name')
colnames(players)
teams <- read.csv('../data/2021-22/teams.csv') %>% select(name)
fixtures <- read.csv('../data/2021-22/fixtures.csv')
head(players)
players <- players %>% inner_join(players_old, by=c('first_name','second_name')) %>% 
  select('first_name', 'second_name', 'now_cost', 'element_type')

gws = as.tibble(read.csv('../data/2020-21/gws/merged_gw.csv')) %>% filter("GW" > 30) %>%
  group_by(name) %>% summarise( avg_points = mean(total_points), team=last(team) ) %>% 
  select('name', 'avg_points', 'team')

players['name'] <- with(players, paste0(first_name, " ", second_name))
players <- players %>% inner_join(gws, by='name') 

pos_gk <- as.integer(players["element_type"]=='GK')
pos_def <- as.integer(players["element_type"]=='DEF')
pos_mid <- as.integer(players["element_type"]=='MID')
pos_fwd <- as.integer(players["element_type"]=='FWD')
values = players['now_cost']
injured = 1-1*sapply(players["name"], `%in%`, c('James Ward-Prowse'))

avg_points = players['avg_points']*injured
nplayers = dim(players)[1]
ones <- rep(1, nplayers)
team_constraints <- 1*(players %>% 
  select(team) %>% 
  apply(c(1, 2), function(x){x==teams})  %>%
  matrix(nrow=20))

constraints = matrix ( unlist(c(pos_gk, pos_def, pos_mid, pos_fwd, values, ones)), byrow=TRUE, nrow=6)
constraints <- rbind(constraints, team_constraints)
comp = c(c('<=','<=','<=','<=','<=','<='), rep('<=', 20))
rhs = c(as.double(c(2, 5, 5, 3, 1000, 15)), rep(3, 20))

solns <- lp ('max', avg_points, constraints, comp, rhs, all.bin=TRUE, num.bin.solns=1)
inds = unlist(which(solns$solution!=0))
sum(solns$solution)

sum(solns$solution * values)
players[inds,] %>% arrange(element_type) 
