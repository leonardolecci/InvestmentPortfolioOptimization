#################################
### Created by Leonardo Lecci ###
###       Jun 28 2022         ###
#################################

# Let's now install Yahoo Finance API to get real time data
#install.packages("quantmod")

library(quantmod)

# Step 1: pulling in pricing data and combining the data frames gathered

# Import portfolio of Renaissance Technology from csv
# Deleted all symbols with class type (.A .B .WS), they didn't hold a big share of total portfolio
# Deleted all the stock with a % of portfolio < 0.02, the stocks represented here amount to ~60% of the portfolio
portfolio <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Hult/Wealth Mngmt/InvestmentPortfolioOptimization/renaissance_Name_Symbol_SharesHeld.csv")
dim_portfolio <- dim(portfolio)
dim_portfolio <- dim_portfolio[1]
stock_vector <- c(NULL)

# Create vector with the biggest 855 stock held by the fund
for(p in 1:5){
  stock_vector <- append(stock_vector, portfolio[p,2])
}
length_vector <- length(stock_vector)

#initializing joined_prices df, and vector to calculate positions for pulling only adjusted prices
joined_prices <- NULL
adj_position <- c(NULL)


# loop to get alll the values of stock through quantmod
for(i in 1:length_vector){
  stock_values <- NULL
  stock_values <- getSymbols(stock_vector[i], auto.assign = FALSE)
  joined_prices <- cbind(joined_prices, stock_values)
# We know adj. price is in the 6th column for each stock
# so we multiply each cycle by 6 to get the positions for each stock
  adj_position <- append(adj_position, i*6)
}

# Step 2: pulling in only adjusted prices
joined_prices_only <- joined_prices[,adj_position]

#Step 3: calculate returns

# Version 1 (more complex and used in real life work)


joined_returns_loop <- as.data.frame(joined_prices_only)

# User Defined Function to add time window
window_returns <- function(x, t){
  compounded <- rep(NA, each = (t-1))
  for(i in t:length(x)){
    compounded[i] <- log(x[i]/x[i-t+1])
  }
  return(compounded)
}#closing the window_returns UDF

#calling the UDF
# t=25 for the trading days per month

joined_monthly_returns <- NULL
trading_days_month <- 25

# for cycle for combining all log returns in a single df
for(i in 1:length_vector){
  joined_monthly_returns <- cbind(joined_monthly_returns, window_returns(x=joined_returns_loop[,i], t=25))
}

joined_monthly_returns <- as.data.frame(joined_monthly_returns)

# Calculating portfolio return
# This is not precise for old values, protfolio allocation changes over time, this assumes a fiexd allocation for the past

# Getting share held in each stock
stock_held <- c(NULL)

# Create vector with the biggest 855 stock held by the fund
for(p in 1:length_vector){
  stock_held <- append(stock_held, portfolio[p,3])
}

tot_stock_held <- sum(stock_held)

# calculate portfolio allocation
alloc_vect <- c(NULL)
for(i in 1:length_vector){
  alloc_vect <- append(alloc_vect, stock_held[i]/tot_stock_held)
}

# creating a vector with proportional return for each stock

prop_returns <- c(NULL)

for(i in 1:length_vector){
  prop_returns <- append(prop_returns, joined_monthly_returns[i]*alloc_vect[i])
}

prop_returns <- as.data.frame(prop_returns)

# Add column for portfolio's monthly return and add all monthly returns
for(i in 1:nrow(joined_monthly_returns)){
  joined_monthly_returns$portfolio[i] <- rowSums(prop_returns[i,],na.rm=TRUE)
}


# INVESTMENT RISK

# Calculating sigma to show total risk for assets and portfolio

# Taking only every 25th elements of the monthly returns
one_montlhy_returns <- as.data.frame(NULL)
one_montlhy_returns = joined_monthly_returns[seq(25, nrow(joined_monthly_returns), 25), ]


time_index <- nrow(one_montlhy_returns)
sigma_vector <- c(NULL)

for(i in 1:(length_vector+1)){
  sigma_vector_12_months <- append(sigma_vector_12_months, sd(one_montlhy_returns[(time_index-11):time_index,i])*sqrt(12))
  sigma_vector_24_months <- append(sigma_vector_24_months, sd(one_montlhy_returns[(time_index-23):time_index,i])*sqrt(12))
}

sigma_df <- as.data.frame(sigma_vector_12_months)
sigma_df <- cbind(sigma_df, sigma_vector_24_months)

# TRACKING ERROR
# Adding NASDAQ:NDAQ as benchmark for Renaissance tech

NDAQ_adjusted <- as.data.frame(getSymbols("NDAQ", auto.assign = FALSE)[,6])
NDAQ <- window_returns(NDAQ_adjusted[,1], t=25)
joined_monthly_returns <- cbind(joined_monthly_returns, NDAQ)

