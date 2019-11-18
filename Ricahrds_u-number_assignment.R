# Byronie Richards u-number

library(dplyr)
library(ggplot2)

videogames <- read.delim("./input/videogames.txt", stringsAsFactors = FALSE)

################ Question #1 ################
colNames <- colnames(videogames)

onlyGlobalSalesVideogames <- videogames

onlyGlobalSalesVideogames[ , grepl('Sales', names(onlyGlobalSalesVideogames), fixed=TRUE) & names(onlyGlobalSalesVideogames) != 'Global_Sales'] <- NULL

# str(onlyGlobalSalesVideogames)
# colnames(onlyGlobalSalesVideogames)
# onlyGlobalSalesVideogames["Global_Sales"][2,1]
# nrow(onlyGlobalSalesVideogames["Global_Sales"])

answer1 <- onlyGlobalSalesVideogames[onlyGlobalSalesVideogames$Global_Sales >= 1, ]

# print(answer1[answer1$Global_Sales < 1, ])


################ Question #2 ################

videogamesWithAVGScore <- videogames
videogamesWithAVGScore$Average_Score <- (videogamesWithAVGScore$Critic_Score + videogamesWithAVGScore$User_Score) / 2
# str(videogamesWithAVGScore)

answer2 <- videogamesWithAVGScore

################ Question #3 ################

PlaformsAndGlobalSales <- videogames[ , names(videogames) == 'Name' | names(videogames) == 'Global_Sales'] 

PlaformsAndGlobalSales$Num_Platforms <- transform(PlaformsAndGlobalSales, Num_Platforms = ave(seq(nrow(PlaformsAndGlobalSales)), Name, FUN=length))

# str(PlaformsAndGlobalSales$Num_Platforms) 

answer3 <- PlaformsAndGlobalSales

################ Question #4 ################

NoKAVideogames <- videogames
NoKAVideogames$Rating[NoKAVideogames$Rating == 'K-A'] <- 'E10+'

answer4 <- NoKAVideogames

################ Question #5 ################

WiiUScores <- videogames[ videogames$Platform == 'WiiU' , names(videogames) == 'Platform' | names(videogames) == 'Critic_Score' | names(videogames) == 'User_Score']

str(WiiUScores)

answer5 <- ggplot() + geom_point(data = videogames, mapping = aes(y = Critic_Score, x = User_Score, color = Critic_Score, size = User_Score)) + labs(title="WiiU Critic Scores by User Scores", x="User's Scores", y="Critic's Scores")

answer5

