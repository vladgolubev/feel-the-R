x <- 1:5
y <- c(3.3, 4.3, 2.9, 1.1, 1.4)

k <- (mean(x*y) - mean(x)*mean(y)) / (mean(x^2) - mean(x)^2)
b <- mean(y) - k * mean(x)

plot(x, y, main = "Лінійна регресія", pch = 16, xlim = c(0, 6), ylim = c(0, 6))
abline(lm(x ~ y))
grid()