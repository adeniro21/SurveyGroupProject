---
  title: "R Notebook"
output: html_notebook
---
  


library(corrgram)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

hockey_data <- read_csv("hockey_data.csv")

#intial exploration
head(hockey_data)
dim(hockey_data)
which(is.na(hockey_data))

cor(hockey_data$gf, hockey_data[,c(3:21,24:29,32:43)])


View(hockey_data)

#separating columns to extract the just the score, creating second and third databases 
hockey_data2 <- separate(col=game, into=c("date", "score"), sep=" - ", remove=TRUE, data = hockey_data)
head(hockey_data2)
View(hockey_data2)

# change dataset name 
hockey_data3_0 <- separate(col = score, into = c("score1", "score2"), sep = ", ", remove = T, data = hockey_data2)
hockey_data3<-hockey_data3_0
#View(hockey_data3)

## add code
for (i in 1:7544){
hockey_data3$score1[i]<-str_remove_all(hockey_data3$score1[i], "[ 0123456789]")
hockey_data3$score2[i]<-str_remove_all(hockey_data3$score2[i], "[ 0123456789]")
}
#check
library("stringi")
hockey_data3
stri_length(hockey_data3$score1[1])

# find the team which match long team name
library(tidyverse)
df<-hockey_data3 %>% 
  group_by(hockey_data3$score1) %>% 
  summarize(n=n()) # just for knowing all long team name


change_longteamname_into_team<-function(df){
  df %>% 
  mutate(change_longteamname_into_team=case_when(
    score1 =="Avalanche"~"col",
    score1 =="Blackhawks" ~ "chi",
    score1 =="BlueJackets" ~ "cbj",
    score1 =="Blues" ~ "stl",
    score1 =="Bruins" ~ "bos",
    score1 =="Canadiens" ~ "mtl",
    score1 =="Canucks" ~ "van",
    score1 =="Capitals" ~ "wsh",
    score1 =="Coyotes" ~ "ari",
    score1 =="Devils" ~ "njd",
    score1 =="Ducks" ~ "ana",
    score1 =="Flames" ~ "cgy",
    score1 =="Flyers" ~ "phi",
    score1 =="GoldenKnights" ~ "vgk",
    score1 =="Hurricanes" ~ "car",
    score1 =="Islanders" ~ "nyi",
    score1 =="Jets" ~ "wpg",
    score1 =="Kings" ~ "lak",
    score1 =="Lightning" ~ "tbl",
    score1 =="MapleLeafs" ~ "tor",
    score1 =="Oilers" ~ "edm",
    score1 =="Panthers" ~ "fla",
    score1 =="Penguins" ~ "pit",
    score1 =="Predators" ~ "nsh",
    score1 =="Rangers" ~ "nyr",
    score1 =="RedWings" ~ "det",
    score1 =="Sabres" ~ "buf",
    score1 =="Senators" ~ "ott",
    score1 =="Sharks" ~ "sjs",
    score1 =="Stars" ~ "dal",
    score1 =="Wild" ~ "min",
    TRUE ~"Trouble"             #Trouble is to check whether something is ignored
    
    
  ))
}

hockey_data3_new<-change_longteamname_into_team(hockey_data3)
hockey_data3_new

# check Trouble
n=0
hockey_data3_new %>% 
  for (i in 1:7544)
  if(change_longteamname_into_team=="Trouble")
    n=n+1
n    # you can find n is still equal to 0. Thus no one observation is ignored.

numextract <- function(string)
{ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
out<-rep(NA,7544)
for(i in 1:7544){
  if (hockey_data3$team[i]==hockey_data3_new$change_longteamname_into_team[i])
    out[i] <- numextract(hockey_data3_0$score1[i])
  else
    out[i]<-numextract(hockey_data3_0$score2[i])
}

out  # all the number such as 1 2 in out, they are characters.

# combine out into hockey_data3_new
hockey_data3_score_matched_to_team<-hockey_data3_new%>% 
  mutate(score_matched_to_team=out)

# create hockey_data3_score_matched_to_team 
write.csv(x=hockey_data3_score_matched_to_team,file="hockey_data3_score_matched_to_team.csv")




