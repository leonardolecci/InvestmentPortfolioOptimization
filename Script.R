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

stocks_vector <- c("NVO", "TSLA", "KR", "GILD", "VRSN", "AAPL", "AMD", "TEAM", "ZM", "AMZN", "META", "HSY", "MRNA", "FNV", "GOOGL", "MCD", "MOH", "ABNB", "HD", "MNST", "VZ", "XOM", "UTHR", "ABMD", "VOD", "VRTX", "GSK", "CL", "DPZ")
length_vector <- length(stocks_vector)

# Creating the actual loop
#initializing joined_prices df, and vector to calculate positions for pulling only adjusted prices

joined_prices <- NULL
adj_position <- c(NULL)

for(i in 1:length_vector){
  stock_values <- NULL
  stock_values <- getSymbols(stocks_vector[i], auto.assign = FALSE)
  joined_prices <- cbind(joined_prices, stock_values)
# We know adj. price is in the 6th column for each stock
# so we multiply each cycle by 6 to get the positions for each stock
  adj_position <- append(adj_position, i*6)
}

# Step 2: pulling in only adjusted prices
joined_prices_only <- joined_prices[,adj_position]

# trying to commit on github
