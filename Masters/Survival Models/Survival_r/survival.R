# Survival Analysis, including Kaplan-Meier estimation and fitting
# proportional hazards is very straightforward in R.
# there are three basic functions that are required for
    # 1. Nonparametric estimation of survival functions;
    # 2. Estimating Cox proportional hazards models; and
    # 3. Estimating accelerated failure models

library(survival)
library(MASS)


df <- gehan

Surv(df$time, event = df$cens)

mean(leuk$wbc)
table(leuk$ag)

mean(gehan$time)


# Kaplan-Meier estimate of the survivor function S(t)

leuk_sf_1 <- survfit(Surv(time) ~ 1, data = leuk)
#  The ~1 is just telling R that a single Kaplan-Meier estimate 
    # is to be computed using the whole data. 
    # Replacing this with ~ag would result in different estimates for 
    # the different groups defined by the variable ag.
leuk_sf_ag <- survfit(Surv(time) ~ ag, data = leuk)

summary(leuk_sf_1)
summary(leuk_sf_ag)

plot(leuk_sf_1)
#plot(leuk_sf_ag)

# produce a Nelson-Aalen estimate of the survicor function
    # (also known as a Fleming-Harrington estimate)
leuk_na <- survfit(Surv(time) ~ 1, data = leuk, type = "fleming")
leuk_na

?survfit.object

gehan_sf_1 <- survfit(Surv(time, event=cens) ~ 1, data = gehan)
summary(gehan_sf_1)

gehan_sf_treat <- survfit(Surv(time, event=cens) ~ treat, data = gehan)
summary(gehan_sf_treat)

gehan_na_treat <- survfit(Surv(time, event=cens) ~ treat, 
                            data = gehan, 
                            type = "fleming",
                            conf.int = 0.99)
summary(gehan_na_treat)


leuk_km <- survfit(Surv(time) ~ ag, data = leuk)
# plotting the kaplan-meier hazard function 
plot(leuk_km, fun = "cumhaz") 
# plotting the kaplan-meier survival function on the log scale
plot(leuk_km, log = TRUE)  

# Cox regression (proportional hazards) models
?coxph

leuk_cox <- coxph(Surv(time) ~ ag + log(wbc), data = leuk)
# hT = exp(B1xi1 + B2xi2)h0(t) where xi1 = 1 if ag 0 else, xi2 = log(wbc)

summary(leuk_cox)

# Breslow Approximation for tied survival times
leuk_breslow <- coxph(Surv(time) ~ ag + log(wbc), data = leuk, ties = "breslow")
summary(leuk_breslow)

# Exact likelihood for tied survival times
leuk_exact <- coxph(Surv(time) ~ ag + log(wbc), 
                    data = leuk,
                    ties = "exact", 
                    method = "exact" )
summary(leuk_breslow)


leuk_cox2 <- coxph(Surv(time) ~ strata(ag)+log(wbc), data = leuk)
logmlog <- function(x){
    + log(-log(x))}

plot(survfit(leuk_cox2),
    fun=logmlog,
    xlab="t (weeks)",
    ylab="log(-log(S(t)))")


# Accelerated failure regression models

leuk_reg <- survreg(Surv(time) ~ ag + log(wbc), data = leuk)
# T ~ Weibull(a, exp(-B0 -B1xi1 -B2xi2)) # nolint
summary(leuk_reg)
leuk$log_quared <- log(leuk$wbc)^2
leuk_reg3 <- survreg(Surv(time) ~ ag + log(wbc) + log_quared, data = leuk)
summary(leuk_reg3)


leuk_loglog <- survreg(Surv(time) ~ ag + log(wbc) + log_quared,
                    data = leuk,
                    dist = "loglogistic")
summary(leuk_loglog)


scaled <- leuk$time/predict(leuk_reg)
S<-survfit(Surv(scaled)~1)
plot(log(S$time), log(-log(S$surv)), xlab = "log t", ylab= "log (-log S(t))")


