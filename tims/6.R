par(mfrow = c(2, 1))

# f(x)
curve(2*(3*x-2)/5,
      from = 1,
      to = 2,
      xlim = c(-2, 4),
      ylim = c(-0.2, 2),
      ylab = "",
      main = "f(x)")

curve(x*0, -2, 1, add = TRUE)
curve(x*0, 2, 3, add = TRUE)
points(1, 2/5, pch = 17)
points(2, 0, pch = 17)
grid()

# F(x)
curve((3*x^2-4*x+1) / 5,
      from = 1,
      to = 2,
      xlim = c(-3, 6),
      ylim = c(-0.2, 1.2),
      ylab = "",
      main = "F(x)")

curve(x*0, -2, 1, add = TRUE)
curve(x / x, 2, 5, add = TRUE)
grid()
