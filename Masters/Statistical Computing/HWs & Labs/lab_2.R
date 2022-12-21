# Lab 2

# Question 1
# a)
a_ <- 2.8*(exp(c(1.2, -3.2, 0.5, 0.65, -1.89))) + 5.7*(sin(c(11.2, 2.3, -0.75, 1.85, 0.15))) 
a_

# b)
Xcol_1 <- c(3,8,6,2)
Xcol_2 <- c(8,11,5,7)
Xcol_3 <- c(6,11,10,1)
Xcol_4 <- c(5,5,9,1)
X_1 <- matrix(c(Xcol_1, Xcol_2, Xcol_3, Xcol_4), ncol = 4)
X_2 <- matrix(c(Xcol_1, Xcol_2, Xcol_3, Xcol_4), ncol = 4)

Ycol_1 <- c(8,9,5,11)
Ycol_2 <- c(8,1,10,2)
Ycol_3 <- c(4,7,9,9)
Ycol_4 <- c(1,10,6,8)
Y_1 <- matrix(c(Ycol_1, Ycol_2, Ycol_3, Ycol_4), nrow = 4)
Y_2 <- matrix(c(Ycol_1, Ycol_2, Ycol_3, Ycol_4), nrow = 4)

Zcol_1 <- c(5,5,1,11)
Zcol_2 <- c(6,7,7,2)
Zcol_3 <- c(5,5,4,11)
Zcol_4 <- c(11,7,8,6)
Z_1 <-  matrix(c(Zcol_1, Zcol_2, Zcol_3, Zcol_4), nrow = 4)
Z_2 <-  matrix(c(Zcol_1, Zcol_2, Zcol_3, Zcol_4), nrow = 4)

Z_1[c(2,2,3,3), c(1,1,4,4)] <- (3.6*X/Y)[c(2,2,3,3), c(1,1,4,4)]

for (i in c(2,3)) {
  for (j in c(1,4)) {
    Z_2[i,j] <- 3.6*(X[i,j]/Y[i,j])
  }
}
Z_1
Z_2

# c)
for (i in 1:nrow(X_2)) {
    X_2[i,] <- X_2[i,]/sum(X_2[i,])
}
X_2
X_1/rowSums(X_1)

for (i in 1:ncol(Y_1)) {
    Y_1[,i] <- Y_1[,i]/sum(Y_1[,i])
}
Y_1
Y_2/rowSums(Y_2)

# d)
diag(1/(Y*X))

# Question 2
# a) 
cars_df <- read.table(file = "cars.txt", sep = "\t")
View(cars_df)

# b)
dim(cars_df)

# c) 
str(cars_df)

# d)
colnames(cars_df) <- c("mpg", "cylinders", "displacement", "horsepower",
                      "weight", "acceleration", "year", "origin", "name")


# Question 3
# a)
mean(cars_df[cars_df$year == 70,]$mpg)

# b)
sd(cars_df[cars_df$weight >= 3000 & cars_df$weight <= 4000,]$mpg)

# c)
str(cars_df$origin)
cars_df$origin <- factor(cars_df$origin)

# d)
cars_df$acc_cut <- ifelse(cars_df$acceleration > 15.0, "fast", "slow")

# e)
cars_df$PowerWeight <- 1000*(cars_df$horsepower / cars_df$weight)

# f)
median(cars_df$PowerWeight)

# g)
cars_df$scaledWeight <- ((cars_df$weight - min(cars_df$weight)) /
                         (max(cars_df$weight) - min(cars_df$weight)))

# i)
summary(cars_df)
