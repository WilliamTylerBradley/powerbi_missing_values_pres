set.seed(2021)

n_products <- 50

products <- data.frame(product = as.character(sample(seq(10000, 99999), n_products)))
products$product <- gsub('0', 'A', products$product)
products$product <- gsub('1', 'B', products$product)
products$product <- gsub('2', 'C', products$product)
products$product <- gsub('3', 'D', products$product)
products$product <- gsub('4', 'E', products$product)
products$product <- gsub('5', 'F', products$product)
products$product <- gsub('6', 'G', products$product)
products$product <- gsub('7', 'H', products$product)
products$product <- gsub('8', 'I', products$product)
products$product <- gsub('9', 'J', products$product)

products$price <- sample(seq(10, 275), n_products, replace = TRUE)

day_list <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="day")
start_month_list <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="months")
end_month_list <- seq(as.Date("2020-02-01"), as.Date("2021-01-01"), by="months") - 1

start_end_dates <- products
start_end_dates$start_month <- sample(start_month_list, n_products, replace = TRUE, prob = c(.3, rep((1 - .3) / 11, 11)))
start_end_dates$end_month <- sample(end_month_list, n_products, replace = TRUE, prob = c(rep((1 - .3) / 11, 11), .3))

# # Some checks
# start_end_dates[start_end_dates$start_month > start_end_dates$end_month, ]
# start_end_dates[start_end_dates$start_month == as.Date("2020-01-01") & start_end_dates$end_month == as.Date("2020-12-31"), ]

# Clean up ones that cross over December
cross_over_1 <- start_end_dates[start_end_dates$start_month > start_end_dates$end_month, ]
cross_over_1$start_month <- as.Date("2020-01-01")

cross_over_2 <- start_end_dates[start_end_dates$start_month > start_end_dates$end_month, ]
cross_over_2$end_month <- as.Date("2020-12-31")

start_end_dates <- start_end_dates[!(start_end_dates$start_month > start_end_dates$end_month), ]
start_end_dates <- rbind(start_end_dates, cross_over_1, cross_over_2)

# Create orders
products_probs <- data.frame(product = products$product,
                             price = products$price)

start_end_dates$difference <- start_end_dates$end_month - start_end_dates$start_month
summation <- aggregate(difference ~ product, data = start_end_dates, FUN = sum)

products_probs <- merge(products_probs, summation)
products_probs$difference <- as.numeric(products_probs$difference)

products_probs$n_orders = round(runif(n_products, products_probs$difference / 30, products_probs$difference * 4))

products_probs$trend <- sample(c(0, 1, 2), n_products, TRUE)
products_probs$shape1 <- ifelse(products_probs$trend == 0, 1,
                                ifelse(products_probs$trend == 1, sample(c(1, 2), n_products, replace = TRUE),
                                       5))

products_probs$shape2 <- ifelse(products_probs$trend == 0, 1,
                                ifelse(products_probs$trend == 1 & products_probs$shape1 == 1, 2,
                                       ifelse(products_probs$trend == 1 & products_probs$shape1 == 2, 1, 5)))

current_product <- products[1, 'product']

products_probs_extended <- data.frame(product = current_product,
                                      date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days"),
                                      prob = dbeta(seq(1, length(day_list)) / (length(day_list) + 1),
                                                   shape1 = products_probs[products_probs$product == current_product, 'shape1'],
                                                   shape2 = products_probs[products_probs$product == current_product, 'shape2']))
products_probs_extended$prob <- ifelse(products_probs[products_probs$product == current_product, 'trend'] == 2, 
                                       products_probs_extended$prob[(seq(1, nrow(products_probs_extended)) + round(runif(1, -150, 150))) %% nrow(products_probs_extended) + 1],
                                       products_probs_extended$prob)

products_probs_extended <- merge(products_probs_extended, start_end_dates)
products_probs_extended <- products_probs_extended[products_probs_extended$date >= products_probs_extended$start_month &
                                                     products_probs_extended$date <= products_probs_extended$end_month, ]

current <- data.frame(product = current_product,
                      price = products[products$product == current_product, 'price'],
                      date = sample(products_probs_extended$date, products_probs[products_probs$product == current_product, 'n_orders'],
                                    replace = TRUE, prob = products_probs_extended$prob))

orders <- data.frame(product = current_product,
                     price = products[products$product == current_product, 'price'],
                     date = sample(products_probs_extended$date, products_probs$n_orders[1],
                                   replace = TRUE, prob = products_probs_extended$prob))


for(i in seq(2, n_products)) {
  current_product <- products[i, 'product']
  
  products_probs_extended <- data.frame(product = current_product,
                                        trend = products_probs[products_probs$product == current_product, 'trend'],
                                        date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days"),
                                        prob = dbeta(seq(1, length(day_list)) / (length(day_list) + 1),
                                                     shape1 = products_probs[products_probs$product == current_product, 'shape1'],
                                                     shape2 = products_probs[products_probs$product == current_product, 'shape2']))
  products_probs_extended$prob <- ifelse(products_probs_extended[, 'trend'] == 2, 
                                         products_probs_extended$prob[(seq(1, nrow(products_probs_extended)) + round(runif(1, -150, 150))) %% nrow(products_probs_extended) + 1],
                                         products_probs_extended$prob)
  
  products_probs_extended <- merge(products_probs_extended, start_end_dates)
  products_probs_extended <- products_probs_extended[products_probs_extended$date >= products_probs_extended$start_month &
                                                       products_probs_extended$date <= products_probs_extended$end_month, ]
  
  current <- data.frame(product = current_product,
                        price = products[products$product == current_product, 'price'],
                        date = sample(products_probs_extended$date, products_probs[products_probs$product == current_product, 'n_orders'],
                                      replace = TRUE, prob = products_probs_extended$prob))
  
  orders <- rbind(orders, current)
}

cust_ids <- sample(seq(10000, 99999), 2/3*(nrow(orders)), replace = TRUE)
orders <- data.frame(order_number = sample(seq(1000, 999999), nrow(orders)),
                     product = orders$product,
                     customer_id = sample(cust_ids, replace = TRUE, nrow(orders)),
                     date = orders$date,
                     price = orders$price)

## Clear out weird outliers
summation <- aggregate(order_number ~ product + months(date), data = orders, FUN = length)
outliers <- summation[summation$order_number > 200, 'product']

orders[orders$product == outliers[1], ]
products_probs[products_probs$product == outliers[1], ]

nrow(orders[orders$product == products_probs[1, 'product'], ])
products_probs[1, ]


# orders <- orders[!(orders$product %in% outliers), ]

write.csv(orders, "orders_big.csv", row.names = FALSE)
