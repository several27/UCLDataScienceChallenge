setwd("~/dsschack/data")

library(mice)
library(zoo)
library(dplyr)

commodity <- read.csv("Commodity.csv")
currency <- read.csv("Currency.csv")
econ_ind <- read.csv("Economic_Indicator.csv")
equity <- read.csv("Equity.csv")
index <- read.csv("Index.csv")
prec_met <- read.csv("Precious_Metal.csv")

# Define functions

rollmean_k <- function(x) {
      return(rollmean(x, k = 9))
}

rollderiv <- function(x, k = 9) {
      deriv_vec <- numeric()
      for(i in 1:length(x)) {
            deriv_vec[i] <- (x[i+k] - x[i])/k
      }
      return(deriv_vec)
}

# # DATA CLEANING
# md.pattern(commodity)
# md.pattern(currency)
# md.pattern(econ_ind)
# md.pattern(equity)
# md.pattern(index)
# md.pattern(prec_met)

# # AAPL
# aapl <- equity[grep("aapl", equity$Ticker, ignore.case = TRUE),]
# rollmeans_aapl <- as.data.frame(apply(aapl[-c(1,2)], 2, rollmean_k))
# rownames(rollmeans_aapl) <- aapl$Date[9:nrow(aapl)]
# rollvars_aapl <- as.data.frame(rollapply(aapl[-c(1,2)], 2, var))
# rownames(rollvars_aapl) <- aapl$Date[2:nrow(aapl)]
# rollderivs_aapl <- as.data.frame(apply(aapl[-c(1,2)], 2, rollderiv))
# rownames(rollderivs_aapl) <- aapl$Date

# MSFT
msft <- equity[grep("msft", equity$Ticker, ignore.case = TRUE),]
rollmeans_msft <- as.data.frame(apply(msft[-c(1,2)], 2, rollmean_k))
rollmeans_msft$Date <- msft$Date[9:nrow(msft)]
rollvars_msft <- as.data.frame(rollapply(msft[-c(1,2)], 2, var))
rollvars_msft$Date <- msft$Date[2:nrow(msft)]
rollderivs_msft <- as.data.frame(apply(msft[-c(1,2)], 2, rollderiv))
rollderivs_msft$Date <- msft$Date

# Visualize
# rollderivs_aapl_melt <- melt(rollderivs_aapl[,-c(5:6)])

# Export to CSV
write.csv(rollmeans_aapl, "rollmeans_aapl.csv")
write.csv(rollvars_aapl, "rollvars_aapl.csv")
write.csv(rollderivs_aapl, "rollderivs_aapl.csv")
write.csv(rollmeans_msft, "rollmeans_msft.csv")
write.csv(rollvars_msft, "rollvars_msft.csv")
write.csv(rollderivs_msft, "rollderivs_msft.csv")

# Try time series
aapl_ts <- ts(aapl[-c(1,2)])
plot.ts(aapl_ts)
decompose(aapl_ts)

msft_ts <- ts(msft[-c(1,2)])
plot.ts(msft_ts)
decompose(msft_ts)


month_date[month_date == "January"] <- "01"
month_date[month_date == "February"] <- "02"
month_date[month_date == "March"] <- "03"
month_date[month_date == "April"] <- "04"
month_date[month_date == "May"] <- "05"
month_date[month_date == "June"] <- "06"
month_date[month_date == "July"] <- "07"
month_date[month_date == "August"] <- "08"
month_date[month_date == "September"] <- "09"
month_date[month_date == "October"] <- "10"
month_date[month_date == "November"] <- "11"
month_date[month_date == "December"] <- "12"

msft_events <- read.csv("Microsoft_Events.csv")
colnames(msft_events)[7] <- "Date"

# # Fix dates for timeline of MS products
# for(i in 1:nrow(ms_full_timeline)) {
#       if(length(strsplit(as.character(ms_full_timeline$Month.and.date[i]), split = " ")[[i]]) == 1) {
#             ms_full_timeline$date[i] <- paste0(ms_full_timeline$Year[i], "-", month_date[i], "-", "1", collapse = "-")
#       }
#       ms_full_timeline$date[i] <- paste0(ms_full_timeline$Year[i], "-", month_date[i], "-", strsplit(as.character(ms_full_timeline$Month.and.date[i]), split = " ")[[i]][2], collapse = "-")
# }


# ROLLDERIVS
# Volume distribution
events_join <- inner_join(rollderivs_msft, msft_events[,c(5,6,7)], by = "Date")

plot1 <- ggplot(data = events_join, aes(x = Volume)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of derivatives of Volume for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot2 <- ggplot(data = rollderivs_msft, aes(x = Volume)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of derivatives of Volume (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot1, plot2, ncol = 2)
t.test(x = events_join$Volume, y = rollderivs_msft$Volume, alternative = "two.sided")

# MarketCap distribution
plot3 <- ggplot(data = events_join, aes(x = MarketCap)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of derivatives of MarketCap for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot4 <- ggplot(data = rollderivs_msft, aes(x = MarketCap)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of derivatives of MArketCap (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot3, plot4, ncol = 2)
t.test(x = events_join$MarketCap, y = rollderivs_msft$MarketCap, alternative = "two.sided")


# ROLLVARS
# Volume distribution
events_join <- inner_join(rollvars_msft, msft_events[,c(5,6,7)], by = "Date")

plot5 <- ggplot(data = events_join, aes(x = Volume)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of variance of Volume for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot6 <- ggplot(data = rollvars_msft, aes(x = Volume)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of variance of Volume (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot5, plot6, ncol = 2)
t.test(x = events_join$Volume, y = rollvars_msft$Volume, alternative = "two.sided")

# MarketCap distribution
plot7 <- ggplot(data = events_join, aes(x = MarketCap)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of variance of MarketCap for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot8 <- ggplot(data = rollvars_msft, aes(x = MarketCap)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of variance of MarketCap (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot7, plot8, ncol = 2)
t.test(x = events_join$MarketCap, y = rollvars_msft$MarketCap, alternative = "two.sided")


# ROLLMEANS
# Volume distribution
events_join <- inner_join(rollmeans_msft, msft_events[,c(5,6,7)], by = "Date")

plot9 <- ggplot(data = events_join, aes(x = Volume)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of mean of Volume for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot10 <- ggplot(data = rollmeans_msft, aes(x = Volume)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of mean of Volume (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot9, plot10, ncol = 2)
t.test(x = events_join$Volume, y = rollmeans_msft$Volume, alternative = "two.sided")

# MarketCap distribution
plot11 <- ggplot(data = events_join, aes(x = MarketCap)) + geom_histogram(fill = "steelblue") + ggtitle("Distribution of mean of MarketCap for major events") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
plot12 <- ggplot(data = rollmeans_msft, aes(x = MarketCap)) + geom_histogram(fill = "steelblue2") + ggtitle("Distribution of mean of MarketCap (general)") + 
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
grid.arrange(plot11, plot12, ncol = 2)
t.test(x = events_join$MarketCap, y = rollmeans_msft$MarketCap, alternative = "two.sided")


# Lags
make_lag_df <- function(lag, type) {
      roll_lag <- data.frame(Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), 
                                      MarketCap = numeric(), Date = factor(), Event.Type = character(), Details = character())
      
      if(type == "rollmeans") {
            for(i in 1:nrow(rollmeans_msft)) {
                  for(j in 1:nrow(events_join)) {
                        if(rollmeans_msft$Date[i] == events_join$Date[j])
                              roll_lag <- rbind(roll_lag, rollmeans_msft[i+lag,])
                  }
            }
      }
      else if(type == "rollvars") {
            for(i in 1:nrow(rollvars_msft)) {
                  for(j in 1:nrow(events_join)) {
                        if(rollvars_msft$Date[i] == events_join$Date[j])
                              roll_lag <- rbind(roll_lag, rollvars_msft[i+lag,])
                  }
            }
      }
      else if(type == "rollderivs") {
            for(i in 1:nrow(rollderivs_msft)) {
                  for(j in 1:nrow(events_join)) {
                        if(rollderivs_msft$Date[i] == events_join$Date[j])
                              roll_lag <- rbind(roll_lag, rollderivs_msft[i+lag,])
                  }
            }
      }
      
      return(roll_lag)
}

make_plots <- function(roll_lag, lag, roll_nolag, feature_name, column) {
      events_join <- roll_lag
      plot1 <- ggplot(data = events_join, aes_string(x = column)) + geom_histogram(fill = "steelblue") + 
            ggtitle(paste("Distribution of", feature_name, "of", column, "for major events (lag", lag, ")")) + theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5))
      plot2 <- ggplot(data = roll_nolag, aes_string(x = column)) + geom_histogram(fill = "steelblue2") + 
            ggtitle(paste("Distribution of", feature_name, "of", column, "(general)")) + theme_minimal() + 
            theme(plot.title = element_text(hjust = 0.5))
      print(t.test(x = events_join[,column], y = roll_nolag[,column], alternative = "two.sided"))
      return(grid.arrange(plot1, plot2, ncol = 2))
}


# Lag +1 and -1
# Means
lag = 20
type = "rollvars"
plot_lag <- make_plots(roll_lag = make_lag_df(lag = lag, type = type), roll_nolag = rollmeans_msft, lag = lag, feature_name = "var", column = "Open")
plot_nolag <- make_plots(roll_lag = make_lag_df(lag = -1, type = type), roll_nolag = rollmeans_msft, lag = "-1", feature_name = "var", column = "Open")
grid.arrange(plot_lag, plot_nolag, nrow = 2)


# Variances
type = "rollvars"
make_plots(roll_lag = make_lag_df(lag = lag, type = type), roll_nolag = rollmeans_msft, lag = 1, feature_name = "mean", column = "Volume")
make_plots(roll_lag = make_lag_df(lag = -lag, type = type), roll_nolag = rollmeans_msft, lag = 1, feature_name = "mean", column = "Volume")
