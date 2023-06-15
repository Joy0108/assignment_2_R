##########Q1########
?iris
data("iris")

boxplot(iris$Sepal.Length ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Sepal Length", main = "Boxplot of Sepal Length vs Species of iris")

boxplot(iris$Petal.Length ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Petal Length", main = "Boxplot of Petal Length vs Species of iris")

boxplot(iris$Sepal.Width ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Sepal Width", main = "Boxplot of Sepal Width vs Species of iris")

boxplot(iris$Petal.Width ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Sepal Width", main = "Boxplot of Sepal Width vs Species of iris")


plot(iris$Sepal.Length,iris$Petal.Length,main="Scatterplot of Sepal Length vs. Petal Length",
     xlab="Length of Sepals",ylab="Length of Petals",col=iris$Species,pch=19)
legend("topleft", legend =levels(iris$Species), col = 1:3,pch=19, 
       title = "Species")
##from this we can observe that the setosa species of iris has a very short petal as well as sepal compared to other species##


###q2############
flip <- function(image){
  img.mat<-as.matrix(image) 
  dims <- dim(img_mat)
  flipped_img.mat <- img.mat[dims[1]:1,1:dims[2],]
  flipped_img.img <- as.cimg(flipped_img.mat)
  plot(flipped_img.img)
}

#########q3###########
?ships
library(MASS)
data<-data.frame(ships)
str(data)
plot(data$type,data$incidents, main = "Plot representing ship damage incidents data",xlab="Ship type",ylab="No. of damage incidents")
plot(data$type,data$service, main = "Plot representing service months data",xlab="Ship type",ylab="No. of months in service")
#from the above plots we do observe that the ship B has met with the most no. of accidents but we also do have to keep in mind that B has given service for a much greater amount of time which speaks against the fact that the ship B is the least trusthworthy.

##q4##
library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
span <- c()
span <- html %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()
str(span)
mat <- matrix(span,nrow = 3,ncol=15)
mat <- t(mat)

votes<- mat[,1]
answers <- mat[,2]
views <- mat[,3]
ques<-html %>% html_elements(".s-link") %>% html_text()
ques<-ques[2:16]
df <- data.frame("Title of question"= ques,"number of views"=views,"number of answers"=answers, "number of votes"=votes)


###q5#######
get_half_tablet <- function() {
  count <- 0  
  n_whole <- 100
  n_half <- 0
  while (TRUE) {
    count <- count + 1
    pulled_item <- sample(c(rep("whole", n_whole), rep("half", n_half)), 1, replace = TRUE)
    if (pulled_item == "half") {
      return(count)
    }
    if (pulled_item == "whole") {
      n_whole <- n_whole - 1
      n_half <- n_half + 2
    }
  }
}

num_simulations <- 10000
total_days <- 0

for (i in 1:num_simulations) {
  total_days <- total_days + pull_half_tablet()
}

average_days <- total_days / num_simulations

cat("On average, it will take", average_days, "days to pull a half-tablet out of the bottle.")
