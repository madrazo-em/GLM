set.seed(42)

# Part 1

r_myunif <- function(n, seed, min, max) {
    M <- 2^32
    a <- 1103515245
    b <- 12345

    X <- NULL

    for (i in 1:n) {
        if (i ==1){
            X[i] <- (a*seed + b) %% M
        }
        else {
            X[i] <- (a*X[i-1] + b) %% M
        }
    }
    # transfor uniform integer to unif(0,1)
    Y <- X/M*(max-min)

    return(Y)
}
my_sample <- r_myunif(n=10000, seed = 42, min = -8, max = 10)

plot(density(my_sample), lwd = 2, xlim = c(-8, 10),
    main = "Empirical and actual pdfs of Unif(-8, 10)" )
curve(dunif(x, min = -8, max = 10), 
    from = 0, to = 1, add = T, lwd = 2, lty = 3, col = "red")

legend(x = 0.75, y = 0.3, lty = c(1, 3),
col = c("black", "red"), cex = 0.7,
legend = c("Empirical pdf","Actual pdf"))


# Part 2

r_myBernoulli <- function(n, p) {
    X <- NULL
    for (i in 1:n) {
        u <- runif(1) # uniform random number
        if (u <= 1 - p) {
            x <- 0 
        }
        else {
            x <- 1
        }
        X[i] <- x
    }
    return(X)
}

my_sample <- r_myBernoulli(n=1000, p=0.6)
sum(my_sample)
