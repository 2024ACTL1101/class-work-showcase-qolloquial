---
  title: "ACTL1101 Assignment Part A"
author: "Xavier Nath"
date: "2024 T2"
output:
  html_document:
  df_print: paged
pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
- Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
- You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
- If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]

amd_df <- amd_df[amd_df$date >= as.Date("2024-01-01") & amd_df$date <= as.Date("2024-05-17"), ]

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  if (i == 1) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    previous_price <- amd_df$close[i]
    amd_df$accumulated_shares[i] <- share_size
    
  } else if (amd_df$close[i] < amd_df$close[i-1] && i != nrow(amd_df)) {
        amd_df$trade_type[i] <- 'buy'
        amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
        amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
        previous_price <- amd_df$close[i]
  } else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i - 1] * amd_df$close[i] 
    amd_df$accumulated_shares[i] <- 0
  } else {
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i - 1]
    amd_df$costs_proceeds[i] <- 0
  }
}

print(amd_df)
 
 
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
amd_df <- amd_df[amd_df$date >= as.Date("2024-01-01") & amd_df$date <= as.Date("2024-05-17"), ]
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$
  
```{r ROI}
 
 
 # Initialize all variables
total_profit1 <- 0
invested_capital1 <- 0

# Loop through each row of data frame
for (i in 1:nrow(amd_df)) {
# For the first row, initialize total_profit and invested_capital
  if (i == 1) { 
    total_profit1 <- amd_df$costs_proceeds[i]
    invested_capital1 <- -1 * amd_df$costs_proceeds[i]
  } else {
# Accumulate total_profit
    total_profit <- total_profit1 + amd_df$costs_proceeds[i]
  }
}

# Filter 'buy' transactions and calculate invested capital
buy_transactions <- amd_df[amd_df$trade_type == 'buy', ]

if (nrow(buy_transactions) > 0) {
  invested_capital1 <- -1 * sum(buy_transactions$costs_proceeds, na.rm = TRUE)
} else {
  invested_capital1 <- 1  # Prevent division by zero if there are no 'buy' transactions
}

# Calculate ROI
if (invested_capital1 != 0) {
  ROI_1 <- total_profit1 / invested_capital1 * 100
} else {
  ROI_1 <- NA  # Assign NA to ROI if invested_capital is zero to avoid division by zero
}

# Print ROI
print(paste("ROI:", ROI_1, "%"))

```


## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
# Implementing a Stop Loss mechanism
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- NA

# Initialize variables for tracking

amount_bought <- 0
average_price <- 0
accumulated_purchase <- 0


for (i in 1:nrow(amd_df)) {
  buy_price <- 0.66 * average_price
  if (i == 1) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    previous_price <- amd_df$close[i]
    amd_df$accumulated_shares[i] <- share_size
    accumulated_purchase <- share_size * amd_df$close[i]
    average_price <- amd_df$close[i]
    amount_bought <- 1
  } else if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i - 1] * amd_df$close[i] 
    amd_df$accumulated_shares[i] <- 0
  } else if (amd_df$close[i] <= buy_price) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$accumulated_shares[i - 1]/2 * amd_df$close[i] 
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i - 1]/2 
    accumulated_purchase <- accumulated_purch / 2
  } else if (amd_df$close[i] < amd_df$close[i-1] && i != nrow(amd_df)) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] + share_size
    amd_df$costs_proceeds[i] <- amd_df$close[i] * share_size * -1
    previous_price <- amd_df$close[i]
    amount_bought <- amount_bought + 1
    accumulated_purchase <- accumulated_purch + amd_df$close[i] * share_size
    average_price <- accumulated_purchase / amd_df$accumulated_shares[i]
  } else {
    amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i - 1]
    amd_df$costs_proceeds[i] <- 0
  }
}

print(amd_df)


 net_profit2 <- 0
 capital_invested2 <- 0
 
 for (i in 1:nrow(amd_df)) {
   if (i == 1) {
     net_profit2 <- amd_df$costs_proceeds[i]
     capital_invested2 <- amd_df$costs_proceeds[i]
   } else {
     net_profit2 <- net_profit2 + amd_df$costs_proceeds[i]
     if (!is.na(amd_df$trade_type[i]) && amd_df$trade_type[i] == "buy") {
       capital_invested2 <- capital_invested2 + amd_df$costs_proceeds[i]
     }
   }
 }
 ROI_2 <- (net_profit2 / capital_invested2 * -1) * 100
 print(paste("ROI:", ROI_2, "%"))
 
```

## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
start_date <- "2024-01-01"
end_date <- "2024-05-17"


# Calculate delta values
delta_pnl <- total_profit2 - total_profit1
delta_roi <- ROI_2 - ROI_1

# Create a data frame
results <- data.frame(
  Metric = c("Start Date", "End Date", "Average Buy Price", "Invested Capital (Default)", 
             "Total Profit/Loss (Default)", "ROI (Default)", "Invested Capital (Stop-Loss)",
             "Total Profit/Loss (Stop-Loss)", "ROI (Stop-Loss)", 
             "Δ Profit/Loss", "Δ ROI"),
  Value = c(start_date, end_date, average_price, capital_invested1, total_profit1, 
            paste0(ROI_1, "%"), capital_invested2, total_profit2, paste0(ROI_2, "%"),
            delta_pnl, paste0(delta_roi, "%"))
)

# Print the data frame
print(results)
```

Discussion:

In summary, as viewed from the results above, the second strategy (with the stop-loss mechanism) performed better in terms of the ROI and overall net balance during the trading period of the start of 2024 to the present value of the data set: 17th May 2024. The initial strategy yielded a negative ROI whilst with the Stop-loss mechanism implemented a substantial increase was seen. An increase of ROI of approx. 4.5% is a positive yield and a substantial improvement from the initial losses from the default strategy.

In February 2024, AMD CEO Lisa Su had completed it acquisition of Xilinx, an American based semiconductor company that supplied programmable logic devices, this allowed AMD to expanded their market, penetrating further into the data center market with their specialty chips. This deal worth $49B resulting in a sharp rise in AMD stock. This expansion into the data center market is also evident through a later deal during May as they completed their acquisition of Pensando Systems; an IT management company. Visually represented by the sharp spike from the plot (Step 1)

The stop loss strategy proved to output a greater ROI, than the first strategy; this is due to the volatility of AMD stock during this year, reaching an all time high in prices on March 8 2024, with the greatest rise in stock TTM this is also in conjunction to the sharp decline followed right after, the Stop-Loss mechanism allowed to maximise the profits but more beneficially, minimised the losses during the time of sharp decline. An ROI of approx. 34% is a very large ROI, but considering the trading period, which proved to be the most volatile period for AMD and hence is not unreasonable to extract a high return as such.





