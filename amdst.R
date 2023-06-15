amd=read.csv("D:/Python-R/Databases/amd.csv")
plot(as.ts(amd))
amd$Date=as.Date(amd$Date)



par(mfrow = c(1, 2))
prix.ts <- ts(amd$High, frequency = 12)
acf(prix.ts, na.action = na.omit)
diff.prix.ts <- diff(prix.ts, lag = 1, differences = 1)
acf(diff.prix.ts, na.action = na.omit)



plot(prix.ts, col = "blue")
plot(diff.prix.ts, col = "orangered2")     



time <- c(1:nrow(amd))
amd$time <- time
reg <- lm(High ~ time + I(time^2) + I(time^3), data = amd)
par(mfrow = c(1, 2))
plot(amd$Date,amd$High, type = "l", xlab = "",
     ylab = "AMD High prices", col = "blue")
lines(amd$Date, reg$fitted, col = "red", lwd = 2)
plot(data$Prix - reg$fitted, type = "l",
     xlab = "", ylab = "High - detrend", col = "orangered2")



noyau <- ksmooth(amd$time, amd$High, kernel = c("normal"),
                 bandwidth = 10)
par(mfrow = c(1, 2))
plot(amd$Date, amd$High, type = "l", xlab = "",
     ylab = "AMD High prices", col = "blue")
lines(amd$Date, noyau$y, col = "red", lwd = 2)
plot(amd$Date, amd$High - noyau$y, type = "l",
     xlab = "", ylab = "High - detrend", col = "orangered2")



lo <- loess(High ~ time, data = amd, degree = 2, span = 0.7)
plot(amd$Date, amd$High, type = "l", xlab = "",
     ylab = "AMD High prices", col = "blue")
lines(amd$Date, lo$fitted, col = "orangered2", lwd = 2)




n <- 100
const <- rep(1, n)
f1 <- function(x) x
f2 <- function(x) pmax(x - 0.25, 0)
f3 <- function(x) pmax(x - 0.5, 0)
f4 <- function(x) pmax(x - 0.8, 0)
x <- seq(0, 1, length = n)
design <- as.matrix(data.frame(const = const, f1 = f1(x), f2 = f2(x),
                               f3 = f3(x), f4 = f4(x)))
matplot(x, y = design, type = "l", lty = 1, ylab = "", main = "truncated power functions q = 1")



set.seed(150)
coef <- runif(5, -1, 1)
f <- design %*% coef
plot(x, f, type = "l", col = "purple", lwd = 2)
abline(v = c(0.25, 0.5, 0.8), lty = "dashed")


library(mgcv)


g <- gam(High ~ s(time, k = 10), data = amd)
plot(amd$Date, amd$High, type = "l", xlab = "",
     ylab = "AMD High Prices", col = "blue",
     lwd = 2)
lines(amd$Date, g$fitted, col = "red", lwd = 2)

