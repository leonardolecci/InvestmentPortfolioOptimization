#################################
### Created by Leonardo Lecci ###
###       Jun 28 2022         ###
#################################

# Let's now install Yahoo Finance API to get real time data
#install.packages("quantmod")

library(quantmod)

# Step 1: pulling in pricing data
#Well's Fargo
stock1 <- getSymbols("WFC", auto.assign = FALSE)
# S&P500 via SPY
stock2 <- getSymbols("SPY", auto.assign = FALSE)
# US bonds via AGG
fixed_income <- getSymbols("AGG", auto.assign = FALSE)

# Step 2: combining the data frames gathered
joined_prices <- merge.xts(stock1, stock2, fixed_income)

# Step 3: pulling in only adjusted prices
joined_prices_only <- joined_prices[,c(6,12,18)]
