librar

# Question 1
# a)
a_1 <- cos(pi/3) - exp(1.25)
print(a_1)

# b) 
b_1 <- 2.6^(4.6) + 2.9^(2.1)
print(b_1)

# c)
c_1 <- ((507+243)/6.25)+4.3*((9.3/2.5)-6)
print(c_1)

#Question 2
# a) done
# b)
q_2 <- qnorm(0.025, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
q_97 <- qnorm(0.975, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
print(q_2)
print(q_97)

# c)
c_2 <- pnorm(1.5, mean = -1.25, sd = 3.5, lower.tail = FALSE, log.p  = FALSE)
print(c_2)

# d)
d_2 <- dnorm(2.3, mean = -1.25, sd = 3, log = FALSE)
print(d_2)

# e)
quantiles <- qnorm(c(0.2, 0.4, 0.6, 0.8))
print(quantiles)

#Question 3
# a) done
# b)
X <- t(matrix(c(-1:-10, 1:10, 11:20), ncol = 3, nrow = 10))
X

# c) 
app <- c("TikTok", "WhatsApp", "Instagram")
owner <- c("ByteDance", "Facebook", "Facebook")
size <- c(45.2, 153.72, 125.63)
is.numeric(size)


# d)
size <- as.character(size)

# e)
Y <-  data.frame("App" = app,
                "Owner" = owner,
                "Size" = size)
Y

# f) Done?

# Question 4
# a) 
a_4.1 <- X[2,6]
a_4.1

a_4.2 <- X[3,]
a_4.2

a_4.3 <- X[,5]
a_4.3

a_4.4 <- X[1, c(3,5,7,10)]
a_4.4

# b)
b_4 <- X[3, ][X[3, ] > 13.5]
b_4

# c) 
c_4.1 <- Y$Owner
c_4.2 <- Y$Size[1]
c_4.3 <- Y$Owner[1:2]
c_4.2

# d) Done?

sdfghjk = function(a=TRUE, b=TRUE, c_=0){
    d <- a + b
    return(d)
}
sdfghjk()


for (i in 1:10) {
  print(i)
}



i = 1

while(i <=10 ){

  if(i%%2 == 1){

      print(i)
  } 

  i = i + 1

}
