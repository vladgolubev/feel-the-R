# x <- c(rep(11, 3), rep(16, 7), rep(21, 17), rep(26, 54), rep(31, 16), rep(36, 3))
# y <- c(rep(10, 6), rep(16, 9), rep(22, 50), rep(28, 20), rep(34, 15))
x <- c(11, 16, 21, 26, 31, 36)
y <- c(10, 16, 22, 28, 34)

x.average <- mean(x) # 23.5
y.average <- mean(y) # 22

x.square.average <- mean(x^2) # 625.1667
y.square.average <- mean(y^2) # 556

sigma.x <- sqrt(x.square.average - x.average^2) # 8.539126
sigma.y <- sqrt(y.square.average - y.average^2) # 8.485281

r.в <- (84296 - (x.average * y.average * 100)) / (100 * sigma.x * sigma.y) # 4.498675

y.k <- y.average + r.в*(sigma.y/sigma.x)
y.b <- (y.average + r.в*(sigma.y/sigma.x)) * -x.average

x.k <- x.average + r.в*(sigma.x/sigma.y)
x.b <- (x.average + r.в*(sigma.x/sigma.y)) * -y.average

curve(x.k*x + x.b, from = 0, to = 50, add = TRUE, lty = 2) # Пунктиром
curve(y.k*x + y.b, from = 0, to = 50, add = TRUE)
grid()
legend(0, 700, legend = c("X від Y", "Y від X"), lty = c(1, 2))
