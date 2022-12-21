## Task 1 ##

# Clear R workspace
rm(list=ls())

# Question 1
setwd("...")

# Read and attach the data for Task 1
task1.data <- read.table("expenditure.txt", sep = " ", header = TRUE)
attach(task1.data)

# Part 1)
# distribution of expenditure
summary(expenditure)
exp_distribution <- hist(expenditure,
                        breaks=8,
                        col="pink1",
                        xlab="Household Expenditure ", 
                        main="Histogram with Normal Curve")

xfit<-seq(min(expenditure),max(expenditure),length=40)
yfit<-dnorm(xfit,mean=mean(expenditure),sd=sd(expenditure))
yfit <- yfit*diff(exp_distribution$mids[1:2])*length(expenditure)
lines(xfit, yfit, col="blue", lwd=2)

# expenditure and income
exp_inc <- plot(x = income, y = expenditure, 
                xlab="Income", ylab="Expenditure", col="#c37618", 
                main="graph 2: expenditure vs Income")

lexp_inc <- plot(x = income, y = log(expenditure),
                xlab="Income", ylab="Log(Expenditure)", col="#cd3a00", 
                main="graph 3: log(expenditure) vs Income")


# expenditure and house.ten
exp_ht <- boxplot(expenditure~house.ten,
                names= c("Public rented", "Private rented", "Owned"),
                xlab="Household tenure",
                ylab="Expenditure",
                col="#c14560",
                main="graph 4: Expenditure vs Household tenure")

#expenditure and sex.hh
exp_lf <- boxplot(expenditure~sex.hh,
                names= c("Male", "Female"),
                xlab="Sex of the Household Head",
                ylab="Expenditure",
                col="#ffb5c5e0",
                main="graph 5: Expenditure vs Sex of the household head")


# expenditure and lab.force
exp_lf <- boxplot(expenditure~lab.force,
                names= c("Full time working", 
                        "Part time working", 
                        "Unemployed", 
                        "Economically inactive"),
                xlab="Employment Status",
                ylab="Expenditure",
                col="#934d75",
                main="graph 6: Expenditure vs Employment status")

# expenditure and hh.size
exp_hs <- boxplot(expenditure~hh.size,
                names= c("1 person",
                        "2 persons",
                        "3 persons",
                        "4 persons",
                        "5 persons or more"),
                xlab="Household Size",
                ylab="Expenditure",
                col="#138990",
                main="graph 7: Expenditure vs Household Size")

# expenditure and hh.adults
exp_ha <- boxplot(expenditure~hh.adults,
                names= c("1 adult",
                        "2 adults",
                        "3 adults",
                        "4 adults or more"),
                xlab="Number of Adults in the Household",
                ylab="Expenditure",
                col="#1e49a1",
                main="graph 8: Expenditure 
                        vs Number of adults in the household")

par( mfrow = c(3,2) )

# Part 2)
model_1 <- lm(expenditure~income)
plot(model_1)
summary(model_1)

# Part 3)
income_sqr <- income^2
model_2 <- lm(expenditure ~ (income + income_sqr))

plot(model_2)
summary(model_2)

# Part 4)
model_3 <- lm(log(expenditure)~income)

plot(model_3)
summary(model_3)

# Part 5)
model_4 <- lm(log(expenditure)~ (income + income_sqr))
plot(model_4)
summary(model_4)

# Part 6)
AIC(model_1)
AIC(model_2)
AIC(model_3)
AIC(model_4)
 
# part 7
task1.data <- read.table("expenditure.txt", sep = " ", header = TRUE)
task1.data$house.ten <- factor(task1.data$house.ten, levels = c(1, 2, 3),
                               labels = c("Public-Rented", "Private-Rented", "Owned"))

task1.data$sex.hh <- factor(task1.data$sex.hh, levels = c(1, 2),
                            labels = c("Male", "Female"))

task1.data$lab.force <- factor(task1.data$lab.force,
                levels = c(1, 2, 3, 4),
                labels = c("Full time", "Part time", "Unemployed", "Economically Inactive"))

task1.data$hh.size <- factor(task1.data$hh.size,
                levels = c(1, 2, 3, 4, 5),
                labels = c("1 Persons", "2 Persons", "3 Persons", "4 Persons", "5 Persons or more"))

task1.data$hh.adults <- factor(task1.data$hh.adults,
                levels = c(1, 2, 3, 4),
                labels = c("1 Adult", "2 Adults", "3 Adults", "4 Adults or more"))

task1.data$log_exp <- log(task1.data$expenditure)
task1.data$income_sqr <- task1.data$income^2

my_model <- lm(log_exp ~
                    income +
                    income_sqr + 
                    lab.force +
                    house.ten +
                    sex.hh +
                    hh.adults +
                    income*lab.force + 
                    income*hh.adults
                    ,
                data = task1.data
            )


summary(my_model)
AIC(my_model)
plot(my_model)


## Task 2 ##

# Clear R workspace
rm(list=ls())

# Importing Data
setwd("...")

# Read and attach the data for Task 2
task2.data <- read.table("airline.txt", header=T)
attach(task2.data)

# Part 2
# Find MLE for B0 & B1

#Defining number of iterations and epsilon 
max_j <- 100
eps <-0.0001

#Defining the matrix of B0 & B1 and setting initial values
n <- nrow(task2.data)
beta_opti <- matrix(NA, max_j , dim(task2.data)[2])
beta_opti[1,] <- c(0.1, 0.1)

# defining explanatory and outcome vectors
x <- task2.data[,2]
y <- task2.data[,1]
k <- rep(1,n)

# defining a matrix of explanatory data
X <- matrix(c(k,x),nrow=n,ncol=2)

signal <- 0
j <- 1

# Loop of iterations
while (j < max_j & signal == 0) {
    #defining our iteration_mu
    mu <- exp(X %*% beta_opti[j,])
    # score 
    score <- t(X)%*%((y-mu)/mu)
    # information matrix
    inf_mat <- solve(t(X)%*%X)
    # new beta
    beta <- beta_opti[j,]+inf_mat%*%score
    # login the new beta 1 and beta 0 
    beta_opti[(j+1),1] <- beta[1,1]
    beta_opti[(j+1),2] <- beta[2,1]

    # checking for convergence
    if (abs(beta_opti[j,1]-beta_opti[j+1,1]) < eps 
        & abs(beta_opti[j,2]-beta_opti[j+1,2]) < eps) {

        signal <- 1
        }

    j <- j+1

}
beta_opti[j-1,]

# variance - covariance matrix
cov_mat <- solve(t(X)%*%X)
cov_mat

# t test
std_error <- sqrt(diag(solve(t(X)%*%X)))
t_test_b1 <- beta_opti[j-1,2]/std_error[2]
t_test_b1

