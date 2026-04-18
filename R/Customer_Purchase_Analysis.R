install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

library(ggplot2)
library(dplyr)
library(readr)

data <- read.csv(file.choose())

colnames(data) <- c(
  "CustomerID", "Age", "Gender", "ItemPurchased", "Category",
  "PurchaseAmount", "Location", "Size", "Color", "Season",
  "ReviewRating", "SubscriptionStatus", "ShippingType",
  "DiscountApplied", "PreviousPurchases", "PaymentMethod",
  "Frequency"
)
data

data <- data %>%
  mutate(
    PurchaseAmount = abs(PurchaseAmount),
    Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age),
    PreviousPurchases = ifelse(is.na(PreviousPurchases), 0, PreviousPurchases),
    Gender = as.factor(Gender),
    Category = as.factor(Category),
    Season = as.factor(Season),
    PaymentMethod = as.factor(PaymentMethod),
    SubscriptionStatus = as.factor(SubscriptionStatus)
  )
data <- data %>%
  mutate(
    PurchaseAmount = ifelse(is.na(PurchaseAmount), mean(PurchaseAmount, na.rm = TRUE), PurchaseAmount)
  )

data <- data %>%
  mutate(
    Segment = case_when(
      PurchaseAmount < quantile(PurchaseAmount, 0.33, na.rm = TRUE) ~ "Low",
      PurchaseAmount < quantile(PurchaseAmount, 0.66, na.rm = TRUE) ~ "Medium",
      TRUE ~ "High"
    )
  )

summary(data)

mean(data$PurchaseAmount)
median(data$PurchaseAmount)
sd(data$PurchaseAmount)

ggplot(data, aes(x = PurchaseAmount)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black")

ggplot(data, aes(x = Segment, y = PurchaseAmount, fill = Segment)) +
  geom_boxplot()

ggplot(data, aes(x = Age, y = PurchaseAmount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

cor(data$PurchaseAmount, data$Age)

ggplot(data, aes(x = PreviousPurchases, y = PurchaseAmount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

cor(data$PurchaseAmount, data$PreviousPurchases)

ggplot(data, aes(x = Category, y = PurchaseAmount, fill = Category)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data %>% group_by(Category) %>% summarise(avg = mean(PurchaseAmount)),
       aes(x = reorder(Category, avg), y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip()

ggplot(data, aes(x = Season, y = PurchaseAmount, fill = Season)) +
  geom_boxplot()

ggplot(data %>% group_by(Season) %>% summarise(avg = mean(PurchaseAmount)),
       aes(x = Season, y = avg, fill = Season)) +
  geom_bar(stat = "identity")

ggplot(data, aes(x = PaymentMethod, y = PurchaseAmount, fill = PaymentMethod)) +
  geom_boxplot()

ggplot(data %>% group_by(PaymentMethod) %>% summarise(avg = mean(PurchaseAmount)),
       aes(x = PaymentMethod, y = avg, fill = PaymentMethod)) +
  geom_bar(stat = "identity")

model <- lm(PurchaseAmount ~ Age + PreviousPurchases + Gender + Category + Season + PaymentMethod + SubscriptionStatus, data = data)

summary(model)

data$Predicted <- predict(model, data)

ggplot(data, aes(x = PurchaseAmount, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm")

aggregate(PurchaseAmount ~ Segment, data = data, mean)
table(data$Segment)

ggplot(data2, aes(x = visits, y = amount)) +
  geom_point() +
  geom_smooth(method = "lm")
