df <- read.table(file = "ecoStudy.txt", sep = "\t", header = TRUE)

# Question 1: Wild Life Population Density
# part a)
    # i)
summary(df$density)

    # ii)
df_count_raw <- aggregate(df, by=list(df$habitat), FUN=length)
names(df_count_raw)[names(df_count_raw) == 'Group.1'] <- 'habitat'
names(df_count_raw)[names(df_count_raw) == 'density'] <- 'Observations'
df_count <- df_count_raw[, c(1,2)]
df_count

# part b)
df$logDensity <- log(df$density + 1)

    # i) 
par(mfrow=c(1,1))
b_i <- boxplot(df$density~df$habitat,
                xlab="Habitat",
                ylab="Density (count per km2)",
                col="pink2",
                main="Boxplot Densities"
                )


    # ii)
par(mfrow=c(1,1))
b_ii <- boxplot(df$logDensity~df$habitat,
                xlab="Habitat",
                ylab="Log Density",
                col="red4",
                main="Boxplot Log Densities"
                )


    # iii)
par(mfrow=c(2,1))
boxplot(df$density~df$habitat,
        xlab="Habitat",
        ylab="Density (count per km2)",
        col="pink2",
        main="Boxplot Densities"
        )
boxplot(df$logDensity~df$habitat,
        xlab="Habitat",
        ylab="Log Density",
        col="red4",
        main="Boxplot Log Densities"
        )


# Part c)
    # i)
K <- nrow(df_count)
N <- nrow(df)
y_bar <- mean(df$density) 

dividend <- c()
divisor <- c()

for (i in df_count$habitat) {
    habitat_i <- df[which(df$habitat == i),]
    N_i <- df_count[df_count$habitat == i, "Observations"]
    yi_bar <- mean(habitat_i$density)
    dividend_i <- (N_i*(yi_bar-y_bar)^2 )/(K-1)
    dividend <- append(dividend, dividend_i)

    for (j in habitat_i$density) {
        divisor_ij <- (j-yi_bar)^2 / (N-K)
        divisor <- append(divisor, divisor_ij)
    }
}
F <- sum(dividend)/sum(divisor)
signif(F, 3)
# F_score 10.5

    # ii)
p_value <- pf(F, K-1, N-K, lower.tail = FALSE)
signif(p_value, 3)
# F_score 4e-05

# part d)
K <- nrow(df_count)
N <- nrow(df)
y_bar <- mean(df$density) 

tapply_dividend <- function(x, y_bar, K, N) {
    length(x)*((mean(x)-y_bar)^2) / (K-1)
}
tapply_devisor <- function(x, y_bar, K, N) {
    sum(((x - mean(x))^2) / (N-K))
}
dividend <- tapply(df$density, df$habitat, FUN=tapply_dividend,
                                    y_bar=y_bar,
                                    K=K,
                                    N=N)
divisor <- tapply(df$density, df$habitat, FUN=tapply_devisor,
                                    y_bar=y_bar,
                                    K=K,
                                    N=N)

F <- sum(dividend)/sum(divisor)
signif(F, 3)

# part e)
par(mfrow=c(1,1))

residuals_f <- tapply(df$density, df$habitat, function(x) x - mean(x))

qqnorm(unlist(residuals_f), col="green4")
qqline(unlist(residuals_f), col="blue4")


# Question 2)
# part a) + part b)
calcLogMix2Gamma <- function(x, alpha, beta, p) {

    if ((class(x)!="numeric")) {
        stop("x is not an appropriate vector:
    x must be a *numeric* vector of positive values only")
    }
    if (sum(sign(x))<length(x)) {
        stop("x is not an appropriate vector:
    x must be a numeric vector of *positive* values only")
    }
    if ((sum(sign(alpha))<length(alpha)) |
        (length(alpha)<2) 
        )  {
        stop("alpha is not an appropriate vector:
    alpha must be a numeric vectors of *positive* values of length 2")
    }
    if ((sum(sign(beta))<length(beta)) |
        (length(beta)<2) 
        )  {
        stop("alpha is not an appropriate vector:
    alpha must be a numeric vectors of *positive* values of length 2")
    }
    if ( p<0 | p>1 ) {
        stop("p is not an appropriate probabiltity:
    p must be a single numeric value between 0 and 1 inclusive")
    }

    Mix2Gamma <- function(x, alpha, beta, p) {
        ((p *
        x^(alpha[1]-1) *
        exp(-beta[1]*x) *
        beta[1]^alpha[1]
            ) /
        gamma(alpha[1])
            ) +
        (((1-p) *
        x^(alpha[2]-1) *
        exp(-beta[2]*x) *
        beta[2]^alpha[2]
            ) /
        gamma(alpha[2])
            )
    }
    fx <- sapply(X=x, FUN=Mix2Gamma, alpha=alpha, beta=beta, p=p)
    log_joint <- log(prod(fx))
    return(log_joint)
}

# part c)
x <- c(0.06, 2.32, 4.81, 0.02, 2.33, 2.18, 0.83, 2.45, 2.10, 3.27)
alpha <- c(0.5, 6)
beta <- c(0.5, 2.5)
p <- 0.35

c2_answer <- round(calcLogMix2Gamma(x=x, alpha=alpha, beta=beta, p=p),3)
c2_answer
# -13.928 

# Question 3)
# part a)

smoothCurve <- function(x, alpha, beta, p, q, k) {
    g_x <-function(x, alpha, beta, p, q, k) {
        sin(
            (1/(2*alpha)) * 
            log(
                ((gamma(x+beta)^k) *
                (p^alpha) *
                (q^beta)
                    ) /
                ((gamma(alpha)^k) *
                (gamma(beta)^k)
                )
            )
        )
    }

    if (length(x) > 1) {
        sol <- sapply(X=x, FUN=g_x, alpha=alpha, beta=beta, 
                    p=p, q=q, k=k)
    }
    else {
        sol <- g_x(x=x, alpha=alpha, beta=beta, 
                    p=p, q=q, k=k)
    }
    return(sol)
}

# part b)
calcMidRiemannLoop <- function(xVec, alpha, beta, p, q, k) {
    sum_list <- c()
    for (i in c(2:length(xVec))) {
        x <- (xVec[i]+xVec[i-1])/2
        i_term <- abs((xVec[i]-xVec[i-1])*smoothCurve(x=x, alpha=alpha, beta=beta,
                                                p=p, q=q, k=k))
        sum_list <- append(sum_list, i_term)
    }
    area <- round(sum(sum_list),3)
    return(area)
}

# part c) + part d)
calcMidRiemann <- function(xVec, alpha, beta, p, q, k) {
    if (length(alpha)!=1 | alpha<0 |  length(beta)!=1 | beta<0) {
        stop("alpha and beta must be a single positive numeric value")
    }
    if (length(c(p,q,k))!=3 | 
        is.numeric(p)==FALSE | 
        is.numeric(q)==FALSE | 
        is.numeric(k)==FALSE) {
        stop("p, q, k must be a single numeric value")
    }
    if (is.unsorted(xVec) == TRUE) {
        stop("xVec must be is a numeric vector with ascending ordered elements")
    }

    sum_i <- function(x, xVec) {
        i <- match(x, xVec)
        x_i <- (xVec[i]+xVec[i-1])/2
        abs((xVec[i]-xVec[i-1])*smoothCurve(x=x_i, alpha=alpha, beta=beta,
                                                    p=p, q=q, k=k)
            )   
    }
    sum_list <- sapply(X=xVec[2:length(xVec)], FUN=sum_i, xVec=xVec)
    area <- round(sum(sum_list),3)
    return(area)
}

# part e)
xVec <- seq(from = 2, to = 8.5, by = 0.01)
alpha <- 2.1
beta <- 0.5
p <- 3
q <- 6
k <- 2

area <- calcMidRiemann(xVec=xVec, alpha=alpha, beta=beta, 
                        p=p, q=q, k=k)

area
# area = 4.805

# part f)
calcMidRiemannAreas <- function(xSeqList, alpha, beta, p, q, k) {
    area_list <- c()
    for (xi in xSeqList) {
        area_i <- calcMidRiemann(xVec=xi, alpha=alpha, beta=beta, 
                            p=p, q=q, k=k)
        area_list <- append(area_list, area_i)
    }
    return(area_list)
}


xSeqList <- list(seq(from = 3.5, to = 8, by = 0.01),
                seq(from = 2.0, to = 4.3, by = 0.1),
                seq(from = 1.070, to = 9.012, by = 0.001))
alpha <- 1.9
beta <- 0.75
p <- 2.15
q <- 7
k <- 1.65

area_vector <- calcMidRiemannAreas(xSeqList=xSeqList,
                                    alpha=alpha,
                                    beta=beta,
                                    p=p,
                                    q=q,
                                    k=k)
area_vector
# area vector = c(3.009 2.144 5.694)

# Question 4)
# part a)

weatherSeqProb <- function(weatherSeq, trProbs, initProbs) {
    dict_ <- c("s"=1, "c"=2, "r"=3)
    px1 <- initProbs[dict_[weatherSeq[1]]]
    probability_list <- c(px1)
    for (i in c(2:length(weatherSeq))) {
        weather_i <- dict_[weatherSeq[i]]
        weather_y <- dict_[weatherSeq[i-1]]
        cond_p <- trProbs[weather_y,weather_i]
        probability_list <- append(probability_list, cond_p)
    }
    prob <- prod(probability_list)
    return(log(prob))
}
# part b)
weatherSeq <- c("c", "s", "c", "r", "s", "s")
trProbs <- matrix(c(0.50, 0.33, 0.30,
                    0.40, 0.35, 0.30,
                    0.10, 0.32, 0.40),
                    nrow = 3)
initProbs <- c(0.45, 0.25, 0.3)
log_prob <- round(weatherSeqProb(weatherSeq=weatherSeq,
                            trProbs=trProbs,
                            initProbs=initProbs), 3)
log_prob
# log_prob = -6.448

# part c)
weatherColourProbs <- function(colourSeq, emitProbs, weatherSeq, 
                                trProbs, initProbs) {
    if (length(weatherSeq) != length(colourSeq)) {
        stop()
    }
    weather_dict <- c("s"=1, "c"=2, "r"=3)
    shirt_dict <- c("B"=1, "W"=2)

    py1 <- emitProbs[weather_dict[weatherSeq[1]], shirt_dict[colourSeq[1]]]
    shirt_list <- c(py1)

    for (i in c(2:length(colourSeq))) {
        weather_i <- weather_dict[weatherSeq[i]]
        shirt_i <- shirt_dict[colourSeq[i]]
        shirt_p <- emitProbs[weather_i,shirt_i]
        shirt_list <- append(shirt_list, shirt_p)
    }
    shirt_prob <- prod(shirt_list)
    weather_logprob <- weatherSeqProb(weatherSeq=weatherSeq,
                            trProbs=trProbs,
                            initProbs=initProbs)
    shirt_logprob <- log(shirt_prob)
    log_prob <- weather_logprob+shirt_logprob
    return(log_prob)

}


weatherSeq <- c("r", "s", "c", "r", "c", "r", "s", "s")
colourSeq <- c("B", "W", "W", "B", "B", "W", "W", "W")
emitProbs <- matrix(c(0.20, 0.55, 0.90,
                    0.80, 0.45, 0.10),
                    nrow = 3)
trProbs <- matrix(c(0.55, 0.25, 0.20,
                    0.25, 0.35, 0.15,
                    0.20, 0.40, 0.65),
                    nrow = 3)
initProbs <- c(0.35, 0.45, 0.20)


log_prob <- round(weatherColourProbs(colourSeq=colourSeq,
                            emitProbs=emitProbs,
                            weatherSeq=weatherSeq,
                            trProbs=trProbs,
                            initProbs=initProbs), 3)
log_prob
# log_prob  -15.121 










