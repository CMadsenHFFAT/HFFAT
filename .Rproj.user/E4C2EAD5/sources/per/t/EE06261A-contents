library(plotly)
library(forecast)
library(TTR)

# basic plotting and summary----------------------------------------------------------

cur_data <- EURUSD_Minute15
cur_data["ma5"] <- ma(cur_data$close, 5)
cur_data["ma10"] <- ma(cur_data$close, 10)
cur_data["ma20"] <- ma(cur_data$close, 20)
cur_data["ma200"] <- ma(cur_data$close, 200)

returns <- log(c(cur_data$close/cur_data$open))

fig <- cur_data %>% plot_ly(x = cur_data$date,
                            type="candlestick", 
                            open = cur_data$open, close = cur_data$close,
                            high = cur_data$high, low = cur_data$low)

fig <- fig %>% layout(title="EURUSD", xaxis=list(rangeslider=list(visible=F)))

fig2 <- cur_data %>% plot_ly(type = "scatter", mode = "lines", x=cur_data$date, 
                             y=~ma5, name="5-period MA", line=list(color = "red"))
fig2 <- fig2 %>% add_trace(type = "scatter", mode = "lines",  x=cur_data$date,
                           y=~ma10, name="10-period MA", line=list(color = "blue"))
fig2 <- fig2 %>% add_trace(type = "scatter", mode = "lines", x=cur_data$date,
                           y=~ma20, name="20-period MA", line=list(color = "yellow"))
fig2 <- fig2 %>% add_trace(type = "scatter", mode = "lines", x=cur_data$date,
                           y=~ma200, name="200-period MA", line=list(color = "green"))


basic <- data.frame(standard_deviation = (sd(returns)), expected_return = mean(returns),
                    meadian_return = median(returns))
summ <- summary(returns)

# returns and bollinger bands statistics ----------------------------------

n_ <- length(cur_data$close)

fig3 <- barplot(returns)

bb_5_1 <- BBands(cur_data$close, n=5, sd = 1)
bb_5_2 <- BBands(cur_data$close, n=5, sd = 2)
bb_5_3 <- BBands(cur_data$close, n=5, sd = 3)

bb_10_1 <- BBands(cur_data$close, n=10, sd = 1)
bb_10_2 <- BBands(cur_data$close, n=10, sd = 2)
bb_10_3 <- BBands(cur_data$close, n=10, sd = 3)

bb_20_1 <- BBands(cur_data$close, n=20, sd = 1)
bb_20_2 <- BBands(cur_data$close, n=20, sd = 2)
bb_20_3 <- BBands(cur_data$close, n=20, sd = 3)

my_5 <- bb_5_1[,2][5:n_]
my_10 <- bb_10_1[,2][10:n_]
my_20 <- bb_20_1[,2][20:n_]

sd_5 <- data.frame((bb_5_1[,2]-bb_5_1[,1]), (bb_5_2[,2]-bb_5_2[,1]), (bb_5_3[,2]-bb_5_3[,1])) 
sd_10 <- data.frame((bb_10_1[,2]-bb_10_1[,1]), (bb_10_2[,2]-bb_10_2[,1]), (bb_10_3[,2]-bb_10_3[,1])) 
sd_20 <- data.frame((bb_20_1[,2]-bb_20_1[,1]), (bb_20_2[,2]-bb_20_2[,1]), (bb_20_3[,2]-bb_5_3[,1])) 

excur_max_5 <- max(cur_data$high[(5):n_]-my_5)
excur_min_5 <- min(my_5-cur_data$low[(5):n_])

excur_max_10 <- max(cur_data$high[10:n_]-my_10)
excur_min_10 <- min(my_10-cur_data$low[10:n_])

excur_max_20 <- max(cur_data$high[20:n_]-my_20)
excur_min_20 <- min(my_20-cur_data$low[20:n_])

excur_max <- c(excur_max_5, excur_max_10, excur_max_20)
excur_min <- c(excur_min_5, excur_min_10, excur_min_20)

c_5_1 <- sum(abs(na.omit(bb_5_1[, 4])) > 1)/(length(bb_5_1)-5)
c_5_2 <- sum(abs(na.omit(bb_5_2[, 4])) > 1)/(length(bb_5_1)-5)
c_5_3 <- sum(abs(na.omit(bb_5_3[, 4])) > 1)/(length(bb_5_1)-5)

c_10_1 <- sum(abs(na.omit(bb_10_1[, 4])) > 1)/(length(bb_5_1)-10)
c_10_2 <- sum(abs(na.omit(bb_10_2[, 4])) > 1)/(length(bb_5_1)-10)
c_10_3 <- sum(abs(na.omit(bb_10_3[, 4])) > 1)/(length(bb_5_1)-10)

c_20_1 <- sum(abs(na.omit(bb_20_1[, 4])) > 1)/(length(bb_5_1)-20)
c_20_2 <- sum(abs(na.omit(bb_20_2[, 4])) > 1)/(length(bb_5_1)-20)
c_20_3 <- sum(abs(na.omit(bb_20_3[, 4])) > 1)/(length(bb_5_1)-20)

one_sd <- c(1-c_5_1, 1-c_10_1, 1-c_20_1)
two_sd <- c(1-c_5_2, 1-c_10_2, 1-c_20_2)
three_sd <- c(1-c_5_3, 1-c_10_3, 1-c_20_3)

nms <- c("5-period MA", "10-period MA", "20-period MA")

sd_df <- data.frame(row.names = nms, pct_in_one_sd = one_sd, 
                    pct_in_two_sd = two_sd, pct_in_three_sd = three_sd,
                    max_excursion = excur_max, min_excursio = excur_min)

# norm statistics for returns---------------------------------------------------------

return_hist <- hist(returns, breaks = 200, main = "1H log returns")

qqnorm(returns, frame = FALSE, pch = 1)
qqline(returns, col = "red", lwd = 2)
shapiro.test(returns)

# box statistics and histogram ----------------------------------------------------------
boxplot(returns, xlab = "Return %", horizontal = TRUE, labels = fivenum(returns))
lbl <- c(round(fivenum(returns), digits = 3))
lbl_3 <- c(lbl[1], lbl[3], lbl[5])
lbl_2 <- c(lbl[2], lbl[4])
text(x = lbl_3, labels=lbl_3, y=1.25)
text(x = c(-0.007, 0.007), labels=lbl_2, y=0.75)

# auto-correlation data ---------------------------------------------------
n_corr <- acf(returns, main = "1H auto-correlation")

# momentum statistics -----------------------------------------------------

rnam <- c('high2high_5', 'high2high_10', 'high2high_20', 
          'high2close_5', 'high2close_10', 'high2close_20',
          'low2low_5', 'low2low_10', 'low2low_20',
          'low2close_5', 'low2close_10', 'low2close_20')
res <- data.frame(sigma1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                  sigma2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                  sigma3 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), row.names = rnam)
t1 <- c(1:12)
trick <- c(5, 10, 20)
for (m in 1:3)
{
  k <- trick[m]
  h1 <- which(cur_data$high[k:n_]-cur_data$open[k:n_] > sd_5[m][k:n_,1])
  h2 <- which(cur_data$high[k:n_]-cur_data$open[k:n_] > sd_10[m][k:n_,1])
  h3 <- which(cur_data$high[k:n_]-cur_data$open[k:n_] > sd_20[m][k:n_,1])
  c1 <- which(cur_data$close[k:n_]-cur_data$open[k:n_] > sd_5[m][k:n_,1])
  c2 <- which(cur_data$close[k:n_]-cur_data$open[k:n_] > sd_10[m][k:n_,1])
  c3 <- which(cur_data$close[k:n_]-cur_data$open[k:n_] > sd_20[m][k:n_,1])
  l1 <- which(cur_data$open[k:n_]-cur_data$low[k:n_] > sd_5[m][k:n_,1])
  l2 <- which(cur_data$open[k:n_]-cur_data$low[k:n_] > sd_10[m][k:n_,1])
  l3 <- which(cur_data$open[k:n_]-cur_data$low[k:n_] > sd_20[m][k:n_,1])
  cl1 <- which(cur_data$open[k:n_]-cur_data$close[k:n_] > sd_5[m][k:n_,1])
  cl2 <- which(cur_data$open[k:n_]-cur_data$close[k:n_] > sd_10[m][k:n_,1])
  cl3 <- which(cur_data$open[k:n_]-cur_data$close[k:n_] > sd_20[m][k:n_,1])
  len2 <- c(length(h1), length(h2), length(h3), length(c1), length(c2), length(c3),
            length(l1), length(l2), length(l3), length(cl1), length(cl2), length(cl3))
  len <- max(len2)
  holder <- data.frame(hi1 = c(h1, rep(NA, len-length(h1))), hi2 = c(h2, rep(NA, len-length(h2))),
                       hi3 = c(h3, rep(NA, len-length(h3))), ch1 = c(c1, rep(NA, len-length(c1))),
                       ch2 = c(c2, rep(NA, len-length(c2))), ch3 = c(c3, rep(NA, len-length(c3))),
                       lo1 = c(l1, rep(NA, len-length(l1))), lo2 = c(l2, rep(NA, len-length(l2))),
                       lo3 = c(l3, rep(NA, len-length(l3))), clo1 = c(cl1, rep(NA, len-length(cl1))), 
                       clo2 = c(cl2, rep(NA, len-length(cl2))), clo3 = c(cl3, rep(NA, len-length(cl3))))
  dat <- data.frame(cur_data$high[k:n_],cur_data$high[k:n_], cur_data$high[k:n_],
                    cur_data$close[k:n_], cur_data$close[k:n_], cur_data$close[k:n_],
                    cur_data$low[k:n_], cur_data$low[k:n_], cur_data$low[k:n_],
                    cur_data$close[k:n_], cur_data$close[k:n_], cur_data$close[k:n_])
  for (i in 1:12)
  {
    if(i > 6){
      llen <- na.omit(holder[i])
      check <- which(dat[i][(holder[i][1:len2[i],]+1),] < dat[i][(holder[i][1:len2[i],]), ])
      t1[i] <- (length(check)/length(na.omit(holder[i][1:n_, 1])))
    }
    else{
      
      check <- which(dat[i][(holder[i][1:len2[i],]+1),] > dat[i][(holder[i][1:len2[i],]), ])
      t1[i] <- (length(check)/length(na.omit(holder[i][1:n_, 1])))
    }
  }
  res[m] <- t1
  
}



# naive probability of up after up and down after down --------------------------------------

avg_high <- which(cur_data$close > cur_data$open)
up_after_up <- which(cur_data$close[(avg_high+1)] > cur_data$close[avg_high])
up_after_up_prob <- length(cur_data$close[up_after_up])/length(avg_high)
mean_high <- mean(cur_data$close[avg_high]-cur_data$open[avg_high])
mean_high_idx <- which(cur_data$open-cur_data$close > mean_high)
mean_high_prob <- which(cur_data$close[(mean_high_idx+1)] > cur_data$close[mean_high_idx])
sigma_up_prob <- length(mean_high_prob)/length(mean_high_idx)

avg_low <- which(cur_data$open > cur_data$close)
low_after_low <- which(cur_data$close[(avg_low+1)] < cur_data$close[avg_low])
low_after_low_prob <- length(low_after_low)/length(avg_low)
mean_low <- mean(cur_data$open[avg_low] - cur_data$close[avg_low])
mean_low_idx <- which(cur_data$open - cur_data$close > mean_low)
mean_low_prob <- which(cur_data$close[(mean_low_idx+1)] < cur_data$close[mean_low_idx])
