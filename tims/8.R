n <- c(3, 6, 15, 18, 5, 3)

# Вмістимо два графіка на екрані
par(mfrow = c(2, 1))

plot(type = "h",
     lwd = 10,
     x = seq(from = 7.2, to = 22.2, by = 3),
     y = n / 3,
     log = "y",
     xlim = c(0, 25),
     main = "Гістограма частот")
grid()
plot(n / sum(n) / 3,
     type = "h",
     lwd = 10,
     log = "y",
     main = "Гістограма відносних частот")
grid()