x <- c(2, 3, 4, 5, 6, 7)
y <- c(0, 4, 8, 12, 6, 16)

data1 <- data.frame(x, y)

model1 <- lm(y ~ x, data = data1)

model1
summary(model1)

plot(x, y,
     pch = 19,
     xlab = "X",
     ylab = "Y",
     main = "Линейная регрессия")

abline(model1, lwd = 2)