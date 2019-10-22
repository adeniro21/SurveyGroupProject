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

hockey_data3 <- separate(col = score, into = c("score1", "score2"), sep = ", ", remove = T, data = hockey_data2)
#View(hockey_data3)


numextract <- function(string)
{ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

hockey_data3$score1 <- numextract(hockey_data3$score1)
#View(hockey_data3)

hockey_data3$score2 <- numextract(hockey_data3$score2)
#View(hockey_data3)

#dim(hockey_data3)

#mapping win or loss based on the the scores
hockey_data3$outcome <- NA_character_
which(is.na(hockey_data3$score2))

for(i in 1:7544)
{
  if(i %% 2 != 0)
  {
    if(hockey_data3$score1[[i]] > hockey_data3$score2[[i]])
    {
      hockey_data3$outcome[[i]] = "L"
      hockey_data3$outcome[[(i + 1)]] = "W"
    }
    else if(hockey_data3$score1[[i]] < hockey_data3$score2[[i]])
    {
      hockey_data3$outcome[[i]] = "W"
      hockey_data3$outcome[[(i + 1)]] = "L"
    }
  }
}


View(hockey_data3)

hockey_data4 <- separate(col = score, into = c("score1", "score2"), sep = ", ", remove = T, data = hockey_data2)
#View(hockey_data4)

# I realized here that the two score columns won't consistently map to the order of the teams, making the values in correct and 
# impossible to do in a forloop


# instead focusing on just the gf and ga column to determine an outcome. This might make sense since it is a feature within our data 
hockey_data4$outcome <- hockey_data3$outcome
hockey_data4$outcome2 <- NA_character_

for(i in 1:7544)
{
  if(i %% 2 != 0)
  {
    if(hockey_data4$gf[[i]] > hockey_data4$ga[[i]])
    {
      hockey_data4$outcome[[i]] = "W"
      hockey_data4$outcome[[(i + 1)]] = "L"
    }
    else
    {
      hockey_data4$outcome[[i]] = "L"
      hockey_data4$outcome[[(i + 1)]] = "W"
    }
  }
}
View(hockey_data4)

hockey_data5 <- hockey_data4[,c("gf", "ga", "outcome")]
View(hockey_data5)
