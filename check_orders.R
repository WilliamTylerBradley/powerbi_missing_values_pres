library(ggplot2)

df <- read.csv("orders_small.csv")
df$date <- as.Date(df$date)
df$month <- months(df$date)

summation <- aggregate(order_number ~ product + month, data = df, FUN = length)
summation$month <- as.Date(paste0("01-", summation$month, "-2020"), "%d-%B-%Y")

ggplot(data = summation,
       aes(month, order_number, col = product)) +
  geom_line() +
  ylim(c(0, max(summation$order_number)))

summation <- aggregate(price ~ product + month, data = df, FUN = sum)
summation$month <- as.Date(paste0("01-", summation$month, "-2020"), "%d-%B-%Y")

ggplot(data = summation,
       aes(month, price, col = product)) +
  geom_line() +
  ylim(c(0, max(summation$price)))

# actual average per month
summation <- aggregate(price ~ product, data = df, FUN = sum)
summation$price / 12

