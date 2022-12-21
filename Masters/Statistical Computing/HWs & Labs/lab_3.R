
library(lattice)
library(tidyverse)
library(broom)


cars_df <- read.table(file = "cars.txt", sep = "\t")
colnames(cars_df) <- c("mpg", "cylinders", "displacement", "horsepower",
                      "weight", "acceleration", "year", "origin", "name")


# Question 1
# a)
plot(x = cars_df$horsepower, y = cars_df$mpg,
    main = "Miles/Gallon versus Horsepower",
    xlab = "Horsepower", ylab = "Miles per gallon")
# b) 
plot(x=cars_df$mpg, y=cars_df$horsepower, 
    main = "Miles/Gallon versus Horsepower",
    xlab = "Horsepower", ylab = "Miles per gallon", 
    col= "red")
# c)
symbs <- c(rep(1,100), rep(5,100), rep(2,100), rep(3,98))

plot(x=cars_df$mpg, y=cars_df$horsepower, 
    main = "Miles/Gallon versus Horsepower",
    xlab = "Horsepower", ylab = "Miles per gallon",
    pch= symbs)

# d) / e)
plot(x = cars_df$horsepower, y = cars_df$mpg,
    main = "Miles/Gallon versus Horsepower",
    xlab = "Horsepower", ylab = "Miles per gallon", 
    col = "springgreen4",
    xlim=c(0,300))

abline(v = 125, col = "red")
abline(a = 35, b = -0.1, col = "blue")

# Question 2
# a)
help(pgamma)
help(pexp)

#b) 
x <- seq(0, 4, length.out = 1000)
ygam <- pgamma(q=x, shape=4, rate=2)
yexp <- pexp(q=x, rate=2)
plot(x, ygam,
    main = "CDF for Gamma and Exponential Distribution",
    xlab = "x", ylab = "Distribution function",
    col = "magenta", ylim = c(0, 1))
lines(x, yexp, lty = 2, col = "black", lwd = 2)
legend(2.0, 0.4, lty = c(1, 2), lwd = c(1, 2), col = c("magenta", "black"),
legend = c("Gamma (shape 4, rate 2)", "Exponential (rate 2)"))

# c)
par(mfrow = c(1, 2))
plot(x, ygam,
    main = "Gamma Distribution",
    sub = "shape = 4, rate = 2",
    xlab = "x", ylab = "CDF",
    col = "magenta", ylim = c(0,1))
plot(x, yexp, lty = 2,  
    col = "black", lwd=2,
    main = "Exponential Distribution",
    xlab = "x", ylab = "CDF",
    sub = "rate = 2")


# Exercice 3)

#a) & b)
par(mfrow = c(1, 1))
hist(cars_df$weight, probability = TRUE, main = "Histogram of weight",
xlab = "weight", col = "pink", border = "red")
lines(density(cars_df$weight))

# c)
qqnorm(cars_df$weight)
qqline(cars_df$weight, col = "red", lwd = 2)
