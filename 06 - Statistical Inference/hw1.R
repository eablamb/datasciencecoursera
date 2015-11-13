s1 <- function(x, y) {
    fit <- lm(y ~ x)
    m <- summary(fit)
    
    # find the coefficients using least-squares approach
    B0 <- m$coefficients[1,1]
    B1 <- m$coefficients[2,1]
    cat(paste("B0 = ", B0, "\n", sep=""))
    cat(paste("B1 = ", B1, "\n", sep=""))
    
    # plot the regression line
    plot(x,y)
    abline(fit)
    m
}

s2 <- function(m) {
    
    # Get Sum-Squared Error
    SSE <- sum(m$residuals^2)
    cat(paste("SSE = ", SSE, "\n", sep=""))
    
    # Get variance
    variance <- m$sigma^2
    cat(paste("variance = ", variance, "\n", sep=""))
}

ss.xx <- function(x, y) {
    sum((x - mean(x))^2)
}

ss.xy <- function(x, y) {
    sum((x - mean(x)) * (y - mean(y)))
}

beta.1 <- function(x, y) {
    # cor(x, y) * sd(y) / sd(x)
    ss.xy(x, y) / ss.xx(x, y)
}

beta.0 <- function(x, y) {
    mean(y) - beta.1(x, y) * mean(x)
}

ss.yy <- function(x, y) {
    sum((y - mean(y))^2)
}

sse <- function(x, y) {
    ss.yy(x, y) - beta.1(x, y) * ss.xy(x, y)
}

lsq.err.var <- function(x, y) {
    sse(x, y) / (length(x) - 2) 
}

lsq.err.stdev <- function(x, y) {
    sqrt(lsq.var(x, y))
}

lsq.b1.stdev <- function(x, y) {
    lsq.err.stdev(x, y) / sqrt(ss.xx(x, y))
}

t.beta.1 <- function(x, y, est = 0) {
    ( beta.1(x, y) - est ) / ( lsq.err.stdev(x, y) / sqrt(ss.xx(x, y)) )
}

## 9.1
x <- c(1,2,3,4,5,6)
y <- c(1,2,2,3,5,5)
m <- s1(x,y)

## 9.9 (a)
s2(m)

## 9.2
x <- c(-2,-1,0,1,2)
y <- c(4,3,3,1,-1)
m <- s1(x,y)

## 9.9 (b)
s2(m)

## 9.3
x <- c(1,1.5,2,2.5,3)
y <- c(3,3.8,5.4,6.9,8.4)
m <- s1(x,y)

## 9.9 (c)
s2(m)

## 9.10 (a,b)
x <- c(.1,.15,.2,.25,.3,.35,.4,.45,.5,.55)
y <- c(1,.95,.95,.9,.85,.7,.65,.6,.55,.4)
m <- s1(x,y)

## 9.10 (c)
s2(m)

## 9.11 
# Using the data in 9.1, use the T-test to show if there is sufficient evidence 
# to say that the slope of the line is significantly different from zero

x <- c(1,2,3,4,5,6)
y <- c(1,2,2,3,5,5)

t <- t.beta.1(x,y)
res <- t < qt(0.975, length(x) - 2) & t > (-1 * qt(0.975, length(x) - 2)) 
# Since t is in our rejection region, we reject the null hypothesis that the slope of the line is zero

## 9.12
# Calculate the 90% confidence interval for the estimate of b1

x <- c(-2,-1,0,1,2)
y <- c(4,3,3,1,-1)
fit <- lm(y ~ x)
m <- summary(fit)

b1 <- m$coefficients[2,1]
ans <- b1 + c(-1,1) * qt(0.95, df=3) * lsq.b1.stdev(x, y)

# OR
confint(fit, 'x', level=0.9)

# 9.25
x <- c(1,2,3,4,5,6)
y <- c(1,2,2,3,5,5)
fit <- lm(y ~ x)
a1 <- predict(fit, list(x=2), interval="confidence")
a2 <- predict(fit, list(x=2), interval="prediction")

# Example 10.1

x1 <- c(6490, 7244, 7943, 6478, 3138, 8747, 2020, 4090, 
        3230, 8786, 1986, 9653, 9429, 2782, 8444, 6316, 
        2363, 7915, 6928, 5526, 3077, 6600, 2732, 7014,
        8321, 2422, 9581, 9326, 6818, 4831, 9630, 2905,
        6308, 1908, 8542, 4750, 6056, 7052, 7794, 1701)

x2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

y <- c(121,169,172,116,53,177,31,94,72,171,23,177,178,65,146,129,40,167,115,123,
       44,73,8,90,71,37,111,89,72,35,86,40,44,36,78,47,56,75,46,6)

fit <- lm(y ~ x1 + x2)
