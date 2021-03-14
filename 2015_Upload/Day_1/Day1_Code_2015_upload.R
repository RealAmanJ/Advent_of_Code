############################################################# Day 1 ##############################################################

#https://adventofcode.com/2015/day/1

#Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - 
#the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows 
#the instructions one character at a time.

#An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), 
#means he should go down one floor.
 
############################################################# Start new Session ##############################################################

rm(list=ls())
dev.off()

############################################################# Load Libraries ##############################################################

#required packages 
library("readr")
library("dplyr")
library("stringr")


#Advent of Code folder (add your working directory)
setwd(ADD DIRECTORY HERE)

############################################################# Import files ##############################################################

#Import the puzzle input (add name of your input)
puzzle_input                                <- read_csv(ADD INPUT FILE HERE, col_names = F)

############################################################# Puzzle solution Part 1 ##############################################################

#Convert to vector to make it easier
vec_puzzle                                  <- puzzle_input$X1

#Count number of (
#Remember to escape "(" and ")"
floor_up                                    <- vec_puzzle                     %>%
  str_count(pattern = "\\(")

#Count number of )
floor_down                                  <- vec_puzzle                     %>%
  str_count(pattern = "\\)")

#Subtract floor up by floor down
part1_answer                                <- floor_up - floor_down
part1_answer

#Santa needs to go to floor 138

############################################################# Puzzle solution Part 2 ##############################################################

#Now, given the same instructions, find the position of the first character that 
#causes him to enter the basement (floor -1). The first character in the instructions 
#has position 1, the second character has position 2, and so on.

#Lets turn the vector into a dataframe

dat_puzzle                                  <- vec_puzzle                     %>%
  str_split(pattern = "")
dat_puzzle                                  <- as.data.frame(dat_puzzle)
#Rename the columns
colnames(dat_puzzle)                        <- c("Instructions")

#Change the values of the dataframe from "(" and ")" to 1 and -1
dat_puzzle$Instructions                     <- gsub("\\(", 1, dat_puzzle$Instructions)
dat_puzzle$Instructions                     <- gsub("\\)", -1, dat_puzzle$Instructions)

#Change column to numeric
dat_puzzle$Instructions                     <- as.numeric(dat_puzzle$Instructions)

#Lets mess about with loops

floor                                       <- 1
count                                       <- 0

for(x in dat_puzzle$Instructions){
  floor <- floor + x
  count <- count + 1
  
  if(floor == -1) {
    print(count -1)
    break;
  }
}

#That's the answer
#END OF CODE