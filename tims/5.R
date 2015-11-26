X <- c(1, 2, 3, 4, 5)
P <- c(.1, .3, .2, .3, .1)

par(mfrow = c(2, 1))

plot(x = X, y = P,
     type = "l",
     ylim = c(0, 0.35),
     main = "Багатокутник розподілу",
     xlab = "Значення величини X",
     ylab = "Ймовірність можливого значення")

points(X, P)
grid()


plot(ecdf(rbind(X, P)),
     main = "Графік функції розподілу F(x)")
grid()
