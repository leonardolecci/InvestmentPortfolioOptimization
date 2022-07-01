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
portfolio <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Hult/Wealth Mngmt/InvestmentPortfolioOptimization/RENAISSANCE-TECHNOLOGIES-LLC-transactions-Q1 2022.csv")
dim_portfolio <- dim(portfolio)
dim_portfolio <- dim_portfolio[1]
stock_vector <- c(NULL)

# Create vector with the biggest 855 stock held by the fund
for(p in 1:dim_portfolio){
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

stock_returns <- NULL
joined_returns <- NULL
for(i in 1:length_vector){
  stock_returns <- monthlyReturn(getSymbols(stock_vector[i], auto.assign = FALSE))
  joined_returns <- cbind(joined_returns, stock_returns)
  print(i)
}

