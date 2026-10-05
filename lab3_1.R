library(multcomp)

data("waste", package = "multcomp")

waste
str(waste)

aggregate(waste$waste,
          by = list(Temperature = waste$temp),
          FUN = mean)

aggregate(waste$waste,
          by = list(Temperature = waste$temp),
          FUN = sd)

aggregate(waste$waste,
          by = list(Environment = waste$envir),
          FUN = mean)

aggregate(waste$waste,
          by = list(Environment = waste$envir),
          FUN = sd)

aggregate(waste$waste,
          by = list(
            Temperature = waste$temp,
            Environment = waste$envir
          ),
          FUN = mean)

aggregate(waste$waste,
          by = list(
            Temperature = waste$temp,
            Environment = waste$envir
          ),
          FUN = sd)

table(waste$temp, waste$envir)

model_temp <- aov(waste ~ temp, data = waste)
summary(model_temp)

model_envir <- aov(waste ~ envir, data = waste)
summary(model_envir)

model_full <- aov(waste ~ temp * envir, data = waste)
summary(model_full)