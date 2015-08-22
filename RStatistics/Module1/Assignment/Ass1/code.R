tab <- read.csv('../msleep_ggplot2.csv') #creates a data.frame
head(tab) #returns the first and last part of a data.frame, vector, list etc
dim(tab) #retrieve or set the dimension of the object
?read.csv #read function also has an argument as header such that if set to "TRUE" then it assumes that the 
#first line has the names of the variables (headers of column)
#Question1
tab$sleep_total[1] #tab$column_name returns a vector

#Manipulating Vectors
c(tab$sleep_total, 1000) #Adds 1000 to sleep_total

#plot
plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log="x")

#Question2
summary(tab$sleep_total)[5] #in general see summary(tab)

#Question3
tab[ tab$sleep_total > 18, ]

which(tab$sleep_total > 18) #which gives us the number of the rows which are true for a given property

#Question4
which(tab$sleep_total > 18 & tab$sleep_rem < 3)

#sort(vector) just sorts the vector
#order gives us the indexes in the original array of the sorted values

#Question5
order(tab$sleep_total)[1]

rank(c(10,2,2,3)) # gives the appropriate rank or position to numbers

#Question6
rank(tab$sleep_total)[1]

match(c("Cow","Owl monkey","Cheetah"), tab$name) #Match gives us the indexes of the matches of first vector
#in second vector you could also say it is find

#Question 7
match(c("Cotton rat"), tab$name)

#Factors in R are a way to turn character vectors with repeating values into a class of object that 
#recognizes the repeated values
#What R is doing internally is keeping track of character values using integers,where the integer refers 
#to the unique values, or "levels" of the factor. But R shows you the character vector when you print
#the factor. 
vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)

fac2 = factor(vec, levels=c("blue","green","yellow","orange","red"))
fac2
levels(fac2)

#Question 8
table(tab$order)[["Rodentia"]] #table in general counts the number of repetitions

s = split(tab$sleep_total, tab$order) #It builds a list of a vector by grouping it according to a factor
#We can pull out a single vector by using the name of the Order or number that it occurs in the list
#Always use double brackets while indexing lists

#Question 9
mean(s[["Rodentia"]])

#sapply applies a function to all the elements of list/vector and get a vector back
#lapply applies a function to all the elements of list/vector and get a list back

#tapply applies sapply and split together

#A bit on merging
rats <- data.frame(id = paste0("rat", 1:10), 
                   sex = factor(rep(c("female", "male"), each = 5)),
                   weight = c(2, 4, 1, 11, 18, 12, 7, 12, 19, 20),
                   length = c(100, 105, 115, 130, 95, 150, 165, 180, 190, 175))   

ratsTable <- data.frame(id = paste0("rat", c(6, 9, 7, 3, 5, 1, 10, 4, 8, 2)), 
                        secretID = 1:10)
cbind #Takes 2 class objects and bind thems w.r.t columns or rows
cbind(rats[match(ratsTable$id, rats$id), ], ratsTable)

ratsMerged <- merge(rats, ratsTable, by.x = "id", by.y = "id")
ratsMerged[order(ratsMerged$secretID), ]

#Another way of grouping
#use package dplyr
library(dplyr)
sexes <- group_by(rats, sex)
summarise(sexes, ave = mean(weight))
