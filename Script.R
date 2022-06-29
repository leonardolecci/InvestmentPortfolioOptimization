#################################
### Created by Leonardo Lecci ###
###       Jun 28 2022         ###
#################################

# Let's now install Yahoo Finance API to get real time data
#install.packages("quantmod")

library(quantmod)


# Step 1: pulling in pricing data and combining the data frames gathered
# Create loop for code efficiency
# Create vector with all the symbols we want to get

stocks_vector <- c("NVO", "TSLA", "KR")
length_vector <- length(stocks_vector)

# Creating the actual loop
#initializing joined_prices df

joined_prices <- NULL
for(i in 1:length_vector){
  stock_values <- NULL
  stock_values <- getSymbols(stocks_vector[i], auto.assign = FALSE)
  joined_prices <- cbind(joined_prices, stock_values)
}

# Step 2: pulling in only adjusted prices
joined_prices_only <- joined_prices[,c(6,12,18)]
