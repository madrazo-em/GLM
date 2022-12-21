# 
airline_df <- read.table(file = "airline.txt", sep = "\t", header = TRUE)

# Find MLE for B0 & B1

max_j <- 100
eps <-0.0001

n <- nrow(airline_df)
beta_opti <- matrix(NA, max_j , dim(airline_df)[2])
beta_opti[1,] <- c(0.1, 0.1)
x <- airline_df[,2]
y <- airline_df[,1]

k <- rep(1,n)
X <- matrix(c(k,x),nrow=n,ncol=2)

signal <- 0
j <- 1

while (j < max_j & signal == 0) {
    mu <- exp(X %*% beta_opti[j,])
    score <- t(X)%*%((y-mu)/mu)
    inf_mat <- solve(t(X)%*%X)
    beta <- beta_opti[j,]+inf_mat%*%score

    beta_opti[(j+1),1] <- beta[1,1]
    beta_opti[(j+1),2] <- beta[2,1]

    if (abs(beta_opti[j,1]-beta_opti[j+1,1]) < eps 
        & abs(beta_opti[j,2]-beta_opti[j+1,2]) < eps) {

        signal <- 1
        }

    j <- j+1

}
beta_opti[j-1,]


cov_mat <- solve(t(X)%*%X)
cov_mat


std_error <- sqrt(diag(solve(t(X)%*%X)))
t_test_b1 <- beta_opti[j-1,2]/std_error[2]
t_test_b1
