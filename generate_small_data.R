set.seed(2021)

date_probs <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="days"))

date_probs$prob_A <- 1 / nrow(date_probs)
date_probs$prob_B <- dbeta(x = cumsum(rep(1 / nrow(date_probs), nrow(date_probs))),
                             shape1 = 7,
                             shape2 = 7)
date_probs$prob_C <- 0
date_probs$prob_C <- ifelse(months(date_probs$date) == 'January', .2, date_probs$prob_C)
date_probs$prob_C <- ifelse(months(date_probs$date) == 'November', .3, date_probs$prob_C)
date_probs$prob_C <- ifelse(months(date_probs$date) == 'December', .5, date_probs$prob_C)

n_A <- 64
n_B <- 57 
n_C <- 36
n_total <- n_A + n_B + n_C
  
cust_ids <- sample(seq(10000, 99999), 2/3*(n_total))
orders <- data.frame(order_number = sample(seq(1000, 999999), n_total),
                     product = c(rep('A', n_A),
                                 rep('B', n_B),
                                 rep('C', n_C)),
                     customer_id = sample(cust_ids, replace = TRUE, n_total),
                     date = c(sample(date_probs$date, n_A, 
                                     replace = TRUE, date_probs$prob_A),
                              sample(date_probs$date, n_B, 
                                     replace = TRUE, date_probs$prob_B),
                              sample(date_probs$date, n_C, 
                                     replace = TRUE, date_probs$prob_C)),
                     price = c(rep('60', n_A),
                               rep('75', n_B),
                               rep('100', n_C)))

write.csv(orders, "orders_small.csv", row.names = FALSE)

