# Byronie Richards u-572445

library(dplyr)
library(ggplot2)

videogames <- read.delim("./input/videogames.txt", stringsAsFactors = FALSE)

################ Question #1 ################
colNames <- colnames(videogames)

onlyGlobalSalesVideogames <- videogames

onlyGlobalSalesVideogames[ , grepl('Sales', names(onlyGlobalSalesVideogames), fixed=TRUE) & names(onlyGlobalSalesVideogames) != 'Global_Sales'] <- NULL

answer1 <- onlyGlobalSalesVideogames[onlyGlobalSalesVideogames$Global_Sales >= 1, ]

# print(answer1[answer1$Global_Sales < 1, ])


################ Question #2 ################

videogamesWithAVGScore <- videogames
videogamesWithAVGScore$Average_Score <- (videogamesWithAVGScore$Critic_Score + videogamesWithAVGScore$User_Score) / 2
# str(videogamesWithAVGScore)

answer2 <- videogamesWithAVGScore
# str(answer2)

################ Question #3 ################

PlaformsAndGlobalSales <- videogames[ , names(videogames) == 'Name' | names(videogames) == 'Global_Sales'] 

PlaformsAndGlobalSales$Num_Platforms <- transform(PlaformsAndGlobalSales, Num_Platforms = ave(seq(nrow(PlaformsAndGlobalSales)), Name, FUN=length))

# str(PlaformsAndGlobalSales$Num_Platforms) 

answer3 <- PlaformsAndGlobalSales
# str(answer3)

################ Question #4 ################

NoKAVideogames <- videogames
NoKAVideogames$Rating[NoKAVideogames$Rating == 'K-A'] <- 'E10+'

answer4 <- NoKAVideogames
# print(answer4[answer4$Rating == 'K-A', ])

################ Question #5 ################

WiiUScores <- videogames[ videogames$Platform == 'WiiU' , names(videogames) == 'Platform' | names(videogames) == 'Critic_Score' | names(videogames) == 'User_Score']

# str(WiiUScores)

answer5 <- ggplot() + geom_point(data = videogames, mapping = aes(y = Critic_Score, x = User_Score, color = Critic_Score, size = User_Score)) + labs(title="WiiU Critic Scores by User Scores", x="User's Scores", y="Critic's Scores")
# answer5

################ Question #6 ################

videogameRatings <- videogames[videogames$Rating != 'AO', names(videogames) == 'Name' | names(videogames) == 'Rating']
videogameRatings <- videogameRatings[videogameRatings$Rating != 'EC', ]
videogameRatings <- videogameRatings[videogameRatings$Rating != 'K-A', ]
videogameRatings <- videogameRatings[videogameRatings$Rating != 'RP', ]
videogameRatings <- na.omit(videogameRatings)

videogameRatings <- arrange(videogameRatings, Rating)
videogameRatings <- transform(videogameRatings, NumOfGames = ave(seq(nrow(videogameRatings)), Rating, FUN=length))

# str(videogameRatings)

videogameRatings <- videogameRatings %>% count(Rating, NumOfGames)


answer6 = ggplot(data = videogameRatings, mapping = aes(y = NumOfGames, x = Rating)) + geom_bar(stat="identity") + labs( x = 'Ratings', y = 'Number of Games', title = 'Number of Games per Rating')
# answer6

################ Question #7 ################

UbiAct <- videogames[videogames$Publisher == 'Ubisoft' | videogames$Publisher == 'Activision', names(videogames) == 'Genre' | names(videogames) == 'Publisher']
UbiAct <- na.omit(UbiAct)

UbiAct <- UbiAct %>% count(Genre, Publisher, name="NumOfPublishes")

answer7 <- ggplot(data = UbiAct, mapping = aes(y = NumOfPublishes, x = Genre, fill = Publisher)) + geom_col() + labs( x = 'Genre', y = 'Number of Publishes', title = "Number of Publishes per Genre for 'Ubisoft' and 'Activision'")
# answer7

################ Question #8 ################

GenreCritics <- videogames[ , names(videogames) == 'Genre' | names(videogames) == 'Critic_Score']

# str(GenreCritics)

answer8 <- ggplot(data = GenreCritics, mapping = aes( x = Genre, y = Critic_Score)) + geom_boxplot(outlier.colour="skyblue") + labs(x = 'Genre', y = 'Critic Scores', title = 'Distribution of Critic Scores by Genre')
# answer8

################ Question #9 ################

legoGames <- videogames[grepl('LEGO', videogames$Name, fixed=TRUE) , names(videogames) == 'Name' | names(videogames) == 'User_Score' | names(videogames) == 'Global_Sales']
legoGames <- legoGames %>% group_by(Name) %>% summarize(Global_Sales = sum(na.omit(Global_Sales)), User_Score = sum(na.omit(User_Score))) #legoGames[!duplicated(legoGames$Name), ]
legoGames$User_Score_Mean <- mean(na.omit(legoGames[['User_Score']]))
legoGames$Total_Global_Sales <- sum(na.omit(legoGames[['Global_Sales']]))

# str(legoGames)

answer9 <- legoGames
# str(answer9)

################ Question #10 ################

numGamesPerYear <- videogames[ , names(videogames) == 'Year_of_Release' | names(videogames) == 'Genre']

numGamesPerYear <- transform(numGamesPerYear, GamesPerYear = ave(seq(nrow(numGamesPerYear)), Year_of_Release, FUN=length))
numGamesPerYear <- numGamesPerYear %>% count(Year_of_Release, Genre, GamesPerYear)
# str(numGamesPerYear)

answer10 <- ggplot(data = numGamesPerYear , mapping = aes(x = Year_of_Release, y = GamesPerYear, fill=Genre, color=Genre)) + 
labs( x = 'Year of Release', y = 'Number of Games', title='') + 
geom_line() + geom_point()

# answer10

################ Question #11 ################

gameGenre <- videogames[ , names(videogames) == 'Name' | names(videogames) == 'Genre']

gameGenre <- transform(gameGenre, NumOfGames = ave(seq(nrow(gameGenre)), Genre, FUN=length))
gameGenre <- gameGenre %>% count(Genre, NumOfGames)


# str(gameGenre)

answer11 <- ggplot(data = gameGenre, mapping = aes(y = as(NumOfGames, 'integer'), x = Genre, fill=Genre)) + 
geom_col() + labs( x = "", y = "Number of Games", title="Videogame Hits Per Genre") + 
theme(legend.position="none", axis.text.y=element_blank()) + 
geom_text(aes(label=NumOfGames), position=position_dodge(width=0.9), vjust=-0.5)

# answer11


