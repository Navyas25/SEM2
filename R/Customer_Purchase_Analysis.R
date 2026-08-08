

install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")



library(ggplot2)
library(dplyr)
library(readr)



data <- read.csv(file.choose())

data



colnames(data) <- c(
  "CustomerID",
  "Age",
  "Gender",
  "ItemPurchased",
  "Category",
  "PurchaseAmount",
  "Location",
  "Size",
  "Color",
  "Season",
  "ReviewRating",
  "SubscriptionStatus",
  "ShippingType",
  "DiscountApplied",
  "PromoCodeUsed",
  "PreviousPurchases",
  "PaymentMethod",
  "Frequency"
)


colnames(data)



data <- data %>%
  mutate(
    PurchaseAmount = abs(PurchaseAmount),
    
 
    Age = ifelse(
      is.na(Age),
      mean(Age, na.rm = TRUE),
      Age
    ),
    
   
    PreviousPurchases = ifelse(
      is.na(PreviousPurchases),
      0,
      PreviousPurchases
    ),

    Gender = as.factor(Gender),
    Category = as.factor(Category),
    Season = as.factor(Season),
    PaymentMethod = as.factor(PaymentMethod),
    SubscriptionStatus = as.factor(SubscriptionStatus),
    PromoCodeUsed = as.factor(PromoCodeUsed),
    DiscountApplied = as.factor(DiscountApplied),
    ShippingType = as.factor(ShippingType),
    Frequency = as.factor(Frequency),
    Size = as.factor(Size),
    Color = as.factor(Color),
    Location = as.factor(Location),
    ItemPurchased = as.factor(ItemPurchased)
  )



data <- data %>%
  mutate(
    PurchaseAmount = ifelse(
      is.na(PurchaseAmount),
      mean(PurchaseAmount, na.rm = TRUE),
      PurchaseAmount
    )
  )



data <- data %>%
  mutate(
    Segment = case_when(
      PurchaseAmount <
        quantile(PurchaseAmount, 0.33, na.rm = TRUE)
      ~ "Low",
      
      PurchaseAmount <
        quantile(PurchaseAmount, 0.66, na.rm = TRUE)
      ~ "Medium",
      
      TRUE ~ "High"
    )
  )

# Convert Segment to factor
data$Segment <- as.factor(data$Segment)



data



summary(data)


mean(data$PurchaseAmount)

median(data$PurchaseAmount)

sd(data$PurchaseAmount)

min(data$PurchaseAmount)

max(data$PurchaseAmount)



ggplot(data, aes(x = PurchaseAmount)) +
  geom_histogram(
    binwidth = 50,
    fill = "blue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Purchase Amount",
    x = "Purchase Amount (USD)",
    y = "Number of Customers"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = Segment,
    y = PurchaseAmount,
    fill = Segment
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Customer Segment",
    x = "Customer Segment",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = Age,
    y = PurchaseAmount
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "Age vs Purchase Amount",
    x = "Age",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()



cor(
  data$PurchaseAmount,
  data$Age,
  use = "complete.obs"
)


ggplot(
  data,
  aes(
    x = PreviousPurchases,
    y = PurchaseAmount
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "Previous Purchases vs Purchase Amount",
    x = "Previous Purchases",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


cor(
  data$PurchaseAmount,
  data$PreviousPurchases,
  use = "complete.obs"
)


ggplot(
  data,
  aes(
    x = Category,
    y = PurchaseAmount,
    fill = Category
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Category",
    x = "Category",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )


category_avg <- data %>%
  group_by(Category) %>%
  summarise(
    AveragePurchase = mean(PurchaseAmount, na.rm = TRUE)
  )

category_avg

ggplot(
  category_avg,
  aes(
    x = reorder(Category, AveragePurchase),
    y = AveragePurchase
  )
) +
  geom_bar(
    stat = "identity",
    fill = "blue"
  ) +
  coord_flip() +
  labs(
    title = "Average Purchase Amount by Category",
    x = "Category",
    y = "Average Purchase Amount (USD)"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = Season,
    y = PurchaseAmount,
    fill = Season
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Season",
    x = "Season",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


season_avg <- data %>%
  group_by(Season) %>%
  summarise(
    AveragePurchase = mean(PurchaseAmount, na.rm = TRUE)
  )

season_avg

ggplot(
  season_avg,
  aes(
    x = Season,
    y = AveragePurchase,
    fill = Season
  )
) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    title = "Average Purchase Amount by Season",
    x = "Season",
    y = "Average Purchase Amount (USD)"
  ) +
  theme_minimal()



ggplot(
  data,
  aes(
    x = PaymentMethod,
    y = PurchaseAmount,
    fill = PaymentMethod
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Payment Method",
    x = "Payment Method",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


payment_avg <- data %>%
  group_by(PaymentMethod) %>%
  summarise(
    AveragePurchase = mean(PurchaseAmount, na.rm = TRUE)
  )

payment_avg

ggplot(
  payment_avg,
  aes(
    x = PaymentMethod,
    y = AveragePurchase,
    fill = PaymentMethod
  )
) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    title = "Average Purchase Amount by Payment Method",
    x = "Payment Method",
    y = "Average Purchase Amount (USD)"
  ) +
  theme_minimal()



ggplot(
  data,
  aes(
    x = Gender,
    y = PurchaseAmount,
    fill = Gender
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Gender",
    x = "Gender",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = SubscriptionStatus,
    y = PurchaseAmount,
    fill = SubscriptionStatus
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Subscription Status",
    x = "Subscription Status",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = DiscountApplied,
    y = PurchaseAmount,
    fill = DiscountApplied
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Discount Applied",
    x = "Discount Applied",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


ggplot(
  data,
  aes(
    x = PromoCodeUsed,
    y = PurchaseAmount,
    fill = PromoCodeUsed
  )
) +
  geom_boxplot() +
  labs(
    title = "Purchase Amount by Promo Code Usage",
    x = "Promo Code Used",
    y = "Purchase Amount (USD)"
  ) +
  theme_minimal()


segment_avg <- data %>%
  group_by(Segment) %>%
  summarise(
    AveragePurchase = mean(PurchaseAmount, na.rm = TRUE)
  )

segment_avg



table(data$Segment)



model <- lm(
  PurchaseAmount ~
    Age +
    PreviousPurchases +
    Gender +
    Category +
    Season +
    PaymentMethod +
    SubscriptionStatus,
  data = data
)



summary(model)



data$Predicted <- predict(
  model,
  newdata = data
)


head(
  data[, c(
    "CustomerID",
    "PurchaseAmount",
    "Predicted"
  )]
)


ggplot(
  data,
  aes(
    x = PurchaseAmount,
    y = Predicted
  )
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    title = "Actual vs Predicted Purchase Amount",
    x = "Actual Purchase Amount (USD)",
    y = "Predicted Purchase Amount (USD)"
  ) +
  theme_minimal()


frequency_count <- data %>%
  group_by(Frequency) %>%
  summarise(
    Customers = n()
  )

frequency_count

ggplot(
  frequency_count,
  aes(
    x = Frequency,
    y = Customers,
    fill = Frequency
  )
) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    title = "Frequency of Purchases",
    x = "Purchase Frequency",
    y = "Number of Customers"
  ) +
  theme_minimal()

