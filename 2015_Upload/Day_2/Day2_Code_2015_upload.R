############################################################# Day 2 ##############################################################

#https://adventofcode.com/2015/day/2

#The elves are running low on wrapping paper, and so they need to submit an order 
#for more. They have a list of the dimensions (length l, width w, and height h) 
#of each present, and only want to order exactly as much as they need. 

#Fortunately, every present is a box (a perfect right rectangular prism), which 
#makes calculating the required wrapping paper for each gift a little easier: 
#find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also 
#need a little extra paper for each present: the area of the smallest side.

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

#Import the puzzle input
puzzle_input                                <- read_csv(ADD INPUT FILE HERE, col_names = F)

############################################################# Puzzle solution Part 1 ##############################################################

#Separate the rows to allow for an easier time
dat_puzzle                                  <- puzzle_input$X1                %>%
  str_split("x")

dat_puzzle                                  <- as.data.frame(dat_puzzle)
#Okay, lets just flip it
dat_puzzle                                  <- as.data.frame(t(dat_puzzle))

#Fix the row and col names
rownames(dat_puzzle)                        <- NULL
colnames(dat_puzzle)                        <- c("Length", "Width", "Height")

#Set the columns to be numeric
dat_puzzle$Length                           <- as.numeric(dat_puzzle$Length)
dat_puzzle$Width                            <- as.numeric(dat_puzzle$Width)
dat_puzzle$Height                           <- as.numeric(dat_puzzle$Height)

#Add a new column with the dimensions
dat_puzzle$Dimensions                       <-  2 * dat_puzzle$Length   * dat_puzzle$Width      + 
                                                2 * dat_puzzle$Width    * dat_puzzle$Height     + 
                                                2 * dat_puzzle$Height   * dat_puzzle$Length

#That's the easy part done
#How to find the smallest side?

#Lets create a new dataset 
#First create the vectors
vec_side1                                   <- dat_puzzle$Length  * dat_puzzle$Width
vec_side2                                   <- dat_puzzle$Width   * dat_puzzle$Height
vec_side3                                   <- dat_puzzle$Height  * dat_puzzle$Length

#Bind them as a dataframe (one to rule them all)
dat_sides                                   <- as.data.frame(cbind(vec_side1, vec_side2, vec_side3))
#Transpose to allow for ease of using lapply
dat_sides_t                                 <- as.data.frame(t(dat_sides))

#Find the minimum values
min_val_sides                               <- lapply(dat_sides_t, min)

#Add this as a column to dat_puzzle
#Remember to make sure it's numeric
dat_puzzle$Smallest_Side                    <- as.numeric(min_val_sides)

#Add another column called total
dat_puzzle$Total                            <- as.numeric(dat_puzzle$Dimensions + dat_puzzle$Smallest_Side)

#And now sum total
part1_answer                                <- sum(dat_puzzle$Total)
part1_answer

############################################################# Puzzle solution Part 2 ##############################################################

#The elves are also running low on ribbon. Ribbon is all the same width,
#so they only have to worry about the length they need to order, which 
#they would again like to be exact.

#The ribbon required to wrap a present is the shortest distance around 
#its sides, or the smallest perimeter of any one face. Each present also 
#requires a bow made out of ribbon as well; the feet of ribbon required 
#for the perfect bow is equal to the cubic feet of volume of the present. 
#Don't ask how they tie the bow, though; they'll never tell.

#Subset data puzzle for its original 3 values (L, W, H)
dat_puzzle_subset                           <- dat_puzzle[,1:3]
#Transpose it
dat_puzzle_subset_t                         <- as.data.frame(t(dat_puzzle_subset))

#Create new function to find the second lowest value that also does not equal the max value
func_second_lowest                          <- function(x){
  nth(x, 2, descending = F)
}

#Create vectors with the smallest side and the second smallest
min_val_subset_1                            <- sapply(dat_puzzle_subset_t, min)
min_val_subset_2                            <- sapply(dat_puzzle_subset_t, func_second_lowest)

#Bind them into a dataframe and trunc it
dat_minimum                                 <- as.data.frame(rbind(min_val_subset_1, min_val_subset_2))
dat_minimum                                 <- as.data.frame(t(dat_minimum))

#Rename columns
colnames(dat_minimum)                       <- c("Value1", "Value2")

#Nice! Now we can do the final part
#add the perimeter
dat_minimum$Perimeter                       <- dat_minimum$Value1 + dat_minimum$Value1 + 
                                               dat_minimum$Value2 + dat_minimum$Value2  

dat_minimum$Volume                          <- dat_puzzle_subset$Length                * 
                                               dat_puzzle_subset$Width                 * 
                                               dat_puzzle_subset$Height  

dat_minimum$Total                           <- dat_minimum$Perimeter + dat_minimum$Volume

#Now sum up total
part2_answer                                <- sum(dat_minimum$Total)
part2_answer

#END OF CODE