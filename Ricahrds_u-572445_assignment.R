# Byronie Richards 
# u-572445

## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

## Load data -------------------------------------------------------------------
videogames <- read.delim("./input/videogames.txt", stringsAsFactors = FALSE)

## Exercise 1 -----------------------------------------------------------------
colNames <- colnames(videogames)

onlyGlobalSalesVideogames <- videogames

onlyGlobalSalesVideogames[ , grepl('Sales', names(onlyGlobalSalesVideogames), 
fixed=TRUE) & names(onlyGlobalSalesVideogames) != 'Global_Sales'] <- NULL

answer1 <- onlyGlobalSalesVideogames[onlyGlobalSalesVideogames$Global_Sales >= 1
, ]

# print(answer1[answer1$Global_Sales < 1, ])


## Exercise 2 -----------------------------------------------------------------

videogamesWithAVGScore <- videogames
videogamesWithAVGScore$Average_Score <- ((videogamesWithAVGScore$Critic_Score / 
10) + videogamesWithAVGScore$User_Score) / 2
# str(videogamesWithAVGScore)

answer2 <- videogamesWithAVGScore
# str(answer2)

## Exercise 3 -----------------------------------------------------------------

PlaformsAndGlobalSales <- videogames[ , names(videogames) == 'Name' | 
names(videogames) == 'Global_Sales'] %>% group_by(Name) %>%
                         summarise(Global_Sales = sum(Global_Sales), 
                         Num_Platforms = n())


answer3 <- PlaformsAndGlobalSales
# str(answer3)


## Exercise 4 -----------------------------------------------------------------

NoKAVideogames <- videogames
NoKAVideogames$Rating[NoKAVideogames$Rating == 'K-A'] <- 'E10+'

answer4 <- NoKAVideogames
# print(answer4[answer4$Rating == 'K-A', ])

## Exercise 5 -----------------------------------------------------------------

WiiUScores <- videogames[ videogames$Platform == 'WiiU' , 
names(videogames) == 'Platform' | names(videogames) == 'Critic_Score' | 
names(videogames) == 'User_Score' | names(videogames) == 'Critic_Count' | 
names(videogames) == 'User_Count']

# str(WiiUScores)

answer5 <- ggplot() + 
    geom_point(data = videogames, mapping = aes(y = Critic_Score, 
    x = User_Score, color = Critic_Count, size = User_Count)) + 
    labs(title="WiiU Critic Scores by User Scores", x="Users' Scores",
     y="Critics' Scores", color="Critic Count", size = "User Count") + 
    scale_color_continuous(low = "white", high = "black")
# answer5

## Exercise 6 -----------------------------------------------------------------

videogameRatings <- videogames[, names(videogames) == 'Name' | 
names(videogames) == 'Rating']

# str(videogameRatings)


answer6 = ggplot(data = videogameRatings, mapping = aes( x = Rating)) + 
            geom_bar() + 
            scale_x_discrete(limits = c("E", "E10+", "T", "M"), 
            labels = c("Everyone", "Everyone 10+", "Teen", "Mature")) + 
            labs( x = 'Ratings', y = 'Number of Games', 
            title = 'Number of Games per Rating')
# answer6

## Exercise 7 -----------------------------------------------------------------

UbiAct <- videogames[videogames$Publisher == 'Ubisoft' | 
videogames$Publisher == 'Activision', names(videogames) == 'Genre' | 
names(videogames) == 'Publisher']
UbiAct <- na.omit(UbiAct)

answer7 <- ggplot(data = UbiAct, mapping = aes( x = Publisher, fill = Genre)) + 
geom_bar(position = "dodge") + labs( y = 'Genre', x = 'Number of Publishes', 
title = "Number of Publishes per Genre for 'Ubisoft' and 'Activision'")
# answer7

## Exercise 8 -----------------------------------------------------------------

GenreCritics <- videogames[ , names(videogames) == 'Genre' | 
names(videogames) == 'Critic_Score']

# str(GenreCritics)

answer8 <- ggplot(data = GenreCritics, mapping = aes( x = Genre, 
y = Critic_Score)) + geom_boxplot(outlier.colour="skyblue") + 
labs(x = 'Genre', y = 'Critic Scores', 
title = 'Distribution of Critic Scores by Genre')
# answer8

## Exercise 9 -----------------------------------------------------------------

legoGames <- videogames[grepl('LEGO', videogames$Name, fixed=TRUE) , 
names(videogames) == 'Name' | names(videogames) == 'User_Score' | 
names(videogames) == 'Global_Sales'] %>% 
    group_by(Name) %>% summarize(Global_Sales = sum(na.omit(Global_Sales)), 
    User_Score = sum(na.omit(User_Score))) 
User_Score_Mean <- mean(na.omit(legoGames[['User_Score']]))
Total_Global_Sales <- sum(na.omit(legoGames[['Global_Sales']]))

# str(legoGames)

answer9 <- list(
        user_score_mean = User_Score_Mean, 
        total_global_sales = Total_Global_Sales,
        all_lego_games = legoGames[ , names(legoGames) == 'Name']
    )
# str(answer9)

## Exercise 10 -----------------------------------------------------------------

numGamesPerYear <- videogames[ videogames$Year_of_Release == 
max(videogames$Year_of_Release), 
                    names(videogames) == 'Year_of_Release' | 
                    names(videogames) == 'Genre'] %>%
                group_by(Genre) %>% 
                summarise( GamesPerYear = n())

# str(numGamesPerYear)

answer10 <- ggplot(data = videogames , mapping = aes(x = Year_of_Release, 
color=Genre)) + 
labs( x = 'Year of Release', y = 'Number of Games', title='') + 
scale_color_discrete(breaks = numGamesPerYear$Genre) +
geom_line(stat = "count")

# answer10

## Exercise 11 -----------------------------------------------------------------

gameGenre <- videogames[ , names(videogames) == 'Name' | 
names(videogames) == 'Genre']

gameGenre <- transform(gameGenre, NumOfGames = ave(seq(nrow(gameGenre)), Genre, 
FUN=length))
gameGenre <- gameGenre %>% count(Genre, NumOfGames)


# str(gameGenre)

answer11 <- ggplot(data = gameGenre, 
mapping = aes(y = as(NumOfGames, 'integer'), x = Genre, fill=Genre)) + 
geom_col() + labs( x = "", y = "Number of Games", 
title="Videogame Hits Per Genre") + 
theme_bw() +
theme(legend.position="none", panel.grid = element_blank(), 
axis.text.y=element_blank(), axis.ticks.y = element_blank()) + 
geom_text(aes(label=NumOfGames), position=position_dodge(width=0.9), vjust=-0.5)

# answer11


