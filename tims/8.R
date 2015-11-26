n <- c(3, 6, 15, 18, 5, 3)

# Вмістимо два графіка на екрані
par(mfrow = c(2, 1))

barplot(n / 3,
        space = c(4.2 / 3, 0, 0, 0, 0, 0),
        width = c(3, 3, 3, 3, 3, 3),
        xlim = c(0, 25),
        main = "Гістограма частот")
axis(side = 1)
grid()

barplot(n / sum(n) / 3,
        space = c(4.2 / 3, 0, 0, 0, 0, 0),
        width = c(3, 3, 3, 3, 3, 3),
        xlim = c(0, 25),
        main = "Гістограма відносних частот")
axis(side = 1)
grid()
