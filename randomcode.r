library(plsRglm)
library(ggplot2)
library(caret)

library(funModeling)
library(tidyverse)
library(skimr)
library(corrplot)
#importing the datasets into R 

red <- read.csv('winequality-red.csv', sep=";")
white <- read.csv('winequality-white.csv', sep=";")
#Adding a column for color since we're comparing quality

red['color'] <- 'red'
white['color'] <- 'white'

redandwhite <- rbind(red,white)

dim(redandwhite)
names(redandwhite)

RedQuality <- qplot(quality, data = red, fill = color, binwidth = 1) +
  scale_x_continuous(breaks = seq(3,10,1), lim = c(3,10)) +
  scale_y_sqrt()
print(RedQuality + ggtitle("Red Wine Quality"))


p <- ggplot(redandwhite, aes(x = density, y = count)) + geom_point()
p

ggplot(redandwhite, aes(x = fix.acidity,
                        fill = color)) +
  geom_histogram(alpha = 0.5, position = "identity",
                 binwidth = 0.0005) + scale_x_log10(breaks = seq(min(redandwhite$fixed.acidity),
                                                                 max(redandwhite$fixed.acidity),0.1))
ggplot(redandwhite, aes(x=density, fill = color)) + 
  geom_histogram(alpha =0.5, position = "identity", binwidth = 0.0002)+
  scale_x_log10(lim = c(min(redandwhite$density), 1.00370),
                breaks = seq(min(redandwhite$density), 1.00370, 0.005))

ggplot(redandwhite, aes(x =free.sulfur.dioxide, fill = color)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth =1) +
  scale_x_continuous(breaks = seq(min(redandwhite$free.sulfur.dioxide),
                                  max(redandwhite$free.sulfur.dioxide), 20))

ggplot(redandwhite, aes(x =total.sulfur.dioxide, fille = color))
  geom_histogram(alpha = 0.5, position = "identity", binwidth =1) +
    scale_x_continuous(breaks = seq(0,500,100), lim = c(0,500))
                                  