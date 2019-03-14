* Title: make-shots-data-script.R
* Description: This script is about data preparation to create a csv file.
* Input: andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, stephen-curry.csv
* Output: andre-iguodala-summary, draymond-green-summary, kevin-durnat-summary, klay-thompson-summary, shots-data-summary, stephen-curry-summary


library(dplyr)
library(plyr)

getwd()
setwd('/Users/east/desktop/hw-stat133/workout01/code')

colClass = c("character", "character", "integer", "integer", "integer", "integer", "factor", 
             "factor", "factor", "integer", "character", "integer", "integer" )

curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE, colClasses = colClass )
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE, colClasses = colClass )
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE, colClasses = colClass)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE, colClasses = colClass)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE, colClasses = colClass)

curry$name <- "Stephen Curry"
durant$name <- "Kevin Durant"
iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
thompson$name <- "Klay Thompson"


curry$shot_made_flag <- revalue(curry$shot_made_flag, 
                                c( "n" = "shot_no", "y" = "shot_yes"))
durant$shot_made_flag <- revalue(durant$shot_made_flag, 
                                 c( "n" = "shot_no", "y" = "shot_yes"))
iguodala$shot_made_flag <- revalue(iguodala$shot_made_flag, 
                                   c( "n" = "shot_no", "y" = "shot_yes"))
green$shot_made_flag <- revalue(green$shot_made_flag, 
                                c( "n" = "shot_no", "y" = "shot_yes"))
thompson$shot_made_flag <- revalue(thompson$shot_made_flag, 
                                   c( "n" = "shot_no", "y" = "shot_yes"))



curry$minute <- ((12 * curry$period) - (curry$minutes_remaining))
durant$minute <- ((12 * durant$period) - (durant$minutes_remaining))
iguodala$minute <- ((12 * iguodala$period) - (iguodala$minutes_remaining))
green$minute <- ((12 * green$period) - (green$minutes_remaining))
thompson$minute <- ((12 * thompson$period) - (thompson$minutes_remaining))


sink(file = "../output/stephen-curry-summary.txt")
summary(curry)
sink()

sink(file = "../output/kevin-durnat-summary.txt")
summary(durant)
sink()

sink(file = "../output/andre-iguodala-summary.txt")
summary(iguodala)
sink()

sink(file = "../output/draymond-green-summary.txt")
summary(green)
sink()

sink(file = "../output/klay-thompson-summary.txt")
summary(thompson)
sink()



shots_data <- rbind(curry, iguodala, green, thompson, durant)
write.csv(shots_data, "../data/shots-data.csv")

sink(file = "../output/shots-data-summary.txt")
summary(shots_data)
sink()