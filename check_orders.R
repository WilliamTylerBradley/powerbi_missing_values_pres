library(ggplot2)

df <- read.csv("orders_big.csv")
df$date <- as.Date(df$date)
df$month <- months(df$date)

summation <- aggregate(order_number ~ product + month, data = df, FUN = length)
summation$month <- as.Date(paste0("01-", summation$month, "-2020"), "%d-%B-%Y")

ggplot(data = summation,
       aes(month, order_number, col = product)) +
  geom_line() +
  geom_point() +
  ylim(c(0, max(summation$order_number))) +
  theme(legend.position = "none")

summation <- aggregate(dollar_amount ~ product + month, data = df, FUN = sum)
summation$month <- as.Date(paste0("01-", summation$month, "-2020"), "%d-%B-%Y")

ggplot(data = summation,
       aes(month, dollar_amount, col = product)) +
  geom_line() +
  geom_point() +
  ylim(c(0, max(summation$dollar_amount))) +
  theme(legend.position = "none")

# actual average per month
summation <- aggregate(dollar_amount ~ product, data = df, FUN = sum)
summation$dollar_amount / 12

summation[summation$product == 'IIJDJ', ]
nrow(df[df$product == 'IIJDJ', ])
