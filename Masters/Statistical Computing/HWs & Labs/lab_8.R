set.seed(33)

# Part 1
r_myNormal_c <- function(n) {
    X <- NULL
    count <- 0
    for(i in 1:n) {
        repeat {
            x <- rcauchy(1)
            y <- runif(1)
            count <- count+1
            if (y <= sqrt(exp(1))/2*(1+x^2)*exp(-x^2/2)) {
                X[i] <- x
            break
            }
        }
    }
    return(list(X=X, count=count))
}

result <- r_myNormal_c(n=1000)
mysample <- result$X
totalsamples <- result$count
# Accpetance rate
1000/totalsamples

plot(density(mysample), lwd=2, main=" Empirical and actual pdfs of N(0,1)" )
#add the true density of N(0,1)
curve(dnorm(x), add=T, lwd=2, lty=3, col="red")
#add legend
legend(x=2, y=0.3, legend=c("Empirical pdf","Actual pdf"),
lty=c(1,3), col=c("black", "red"), cex =0.7)

# Part 2
1 - pchisq(q = 10, df = 2)

samples <- c(100, 10^3, 10^4, 10^5)
for (n in samples){
    x <- rchisq(n, df=2) ## generate n values
    print(mean(x>10))
}


chisq_tailprob <- function(n) {
    # Step 1. samples from proposal density
    X <- rcauchy(n)

    # Step 2. Calculate the importance weights vector
    # cutting the negative values of X to be zero, 
    # to prevent producing NaN in exp(-X/2)
    X[X<=0] <- 0
    v.weights <- pi/2*(1 + X^2)*exp(-X/2)*(X > 0)

    # Step 3. Calculate the estimator
    p.t <- mean(v.weights*(X>10))
    return(p.t)
}

for (n in samples){
    print(chisq_tailprob(n)) ## print the tail probability
}
