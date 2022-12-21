# Question 1
# a)
calcFiniteGeo <- function(a, r, n) {
    series_list <- c()
    for (i in 1:n) {
        i_term <- a*(r**(i-1)) 
        series_list <- c(series_list, i_term)
    }
    series_sum <- sum(series_list)
    return(series_sum)
}

# b)
calcFiniteGeo(0.5, 2, 5)

# Question 2
# a) 
geom_series <- calcFiniteGeo(0.75, 0.25, 7)
# b)
results <- pgeom(6, 0.75)
geom_series - results # = 0., hence it works

# Question 3
# a)
calcHn <- function(n) {
    series_list <- c()
    for (i in 1:n) {
        i_term <- 1/i
        series_list <- c(series_list, i_term)
    }
    series_sum <- sum(series_list)
    return(series_sum)
}
calcHn(5)

harmonicMean <- function(x) {
    series_list <- c()
    for (i in x) {
        i_term <- 1/i
        series_list <- c(series_list, i_term)
    }
    harmonic_result <- length(x)/sum(series_list)
    return(harmonic_result)
}
x <- c(5.8, 7.2, 3.6, 1.1, 6.2)
harmonicMean(x)
