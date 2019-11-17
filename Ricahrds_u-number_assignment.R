# Byronie Richards u-number

require('dplyr')

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

str(PlaformsAndGlobalSales$Num_Platforms) 

################ Question #4 ################



