library(ggplot2)

set.seed(123)

n <- 100

data <- data.frame(
  id = 1:n,
  amount = round(rnorm(n, mean = 500, sd = 150), 2)
)

data$amount[data$amount < 0] <- abs(data$amount[data$amount < 0])

data$segment <- ifelse(data$amount < 400, "Low",
                       ifelse(data$amount <= 700, "Medium", "High"))

summary(data)

mean(data$amount)
median(data$amount)
sd(data$amount)

ggplot(data, aes(x = amount)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black")

ggplot(data, aes(x = segment, y = amount)) +
  geom_boxplot(fill = "orange")

table(data$segment)

aggregate(amount ~ segment, data = data, mean)

data2 <- data.frame(
  id = 1:n,
  amount = data$amount,
  visits = sample(1:20, n, replace = TRUE)
)

cor(data2$amount, data2$visits)

model <- lm(amount ~ visits, data = data2)

summary(model)

ggplot(data2, aes(x = visits, y = amount)) +
  geom_point() +
  geom_smooth(method = "lm")
