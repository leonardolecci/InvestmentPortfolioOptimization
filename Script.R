#################################
### Created by Leonardo Lecci ###
###       Jun 28 2022         ###
#################################

# Let's now install Yahoo Finance API to get real time data
#install.packages("quantmod")

library(quantmod)

# Step 1: pulling in pricing data
# Novo-Nordisk A/S ADS (NVO)
stock1 <- getSymbols("NVO", auto.assign = FALSE)
# Tesla (TSLA)
stock2 <- getSymbols("TSLA", auto.assign = FALSE)
# Kroger Co. (KR)
stock3 <- getSymbols("KR", auto.assign = FALSE)

# Step 2: combining the data frames gathered
joined_prices <- merge.xts(stock1, stock2, stock3)

# Step 3: pulling in only adjusted prices
joined_prices_only <- joined_prices[,c(6,12,18)]
