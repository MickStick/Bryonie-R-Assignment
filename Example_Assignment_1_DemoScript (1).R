## Your name
## Your u-number

## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

## Load data -------------------------------------------------------------------
videogames <- read.delim("input/videogames.txt", stringsAsFactors = FALSE)

## Exercise 0a -----------------------------------------------------------------
ten_videogames <- slice(videogames, 1:10)
answer0a <- ten_videogames

## Exercise 0b -----------------------------------------------------------------
answer0b <- ggplot(videogames, aes(x = Platform)) +
  geom_bar()

## Note: Delete these demo's, and this comment, before handing in.