# Importing Data
# Exercise 4:
 
data_reg <- read.table("session1.dat", header = TRUE)

data_reg$curric
attach(data_reg) # Set data.reg as the default dataset
curric
detach(data_reg) # Eliminates the default
curric # This is no longer an exisitng element 


# Exercise 5:

data_reg <- read.table("session1.dat", header = TRUE)
dim(data_reg) #Find the dimension of the dataset
data_reg[1:10,] #Select the first 10 cases for all variables
data_reg[,5] #Select all cases for the fifth variable:
data_reg[1:10,2] #Select the first 10 cases for the second variable
data_reg[data_reg$sex==1,] #Select all girls

# Create a new dataset called data new containing only the first and third variables: 
data_new <- data_reg[,c(1,3)]

mathatt_mean <- (data_reg$mathatt2 + data_reg$mathatt1)/2

# Exercise 6
attach(data_reg)
summary(data_reg)
table(school) # Frequency tables
table(sex)
par(mfrow=c(2,2))# Define 1 by 3 multi-figure display - filled by rows
hist(curric) # Histogram for variable curric
hist(mathatt2)
hist(mathatt1)
plot.new()

#bi-variate plots
plot(curric, mathatt1)
par(mfcol=c(2,2)) # Define 2 by 2 multifigure display - filled by columns
plot(curric, mathatt2)
plot(mathatt1, mathatt2)
boxplot(mathatt2 ~ school) # Produces box-plots of mathatt2 by school
boxplot(mathatt2 ~ sex)
pairs(data_reg) # Scatterplot matrix between all variables in the dataset


# Exercise 9:
library(lattice)

f_sex <- as.factor(data_reg$sex)
levels(f_sex) <- c("boy", "girl")
xyplot(mathatt2~curric|f_sex, data=data_reg)

f_school <- as.factor(data_reg$school)
histogram(mathatt2~f_school, data=data_reg)
