---
title: "ACTL1101 Assignment Part B"
author: "Xavier Nath"
date: "2024 T2"
output:
  html_document:
    df_print: paged
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(quantmod)
library(ggplot2)
library(tidyverse)
```

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the Beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's Beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \Beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \Beta_i \) is the Beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
df$AMD_DailyReturn <- NA
df$GSPC_DailyReturn <- NA

for (i in 2:nrow(df)) {
  df$AMD_DailyReturn[i] <- (df$AMD[i] - df$AMD[i-1])/df$AMD[i-1]
  df$GSPC_DailyReturn[i] <- (df$GSPC[i] - df$GSPC[i-1])/df$GSPC[i-1]
  
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
df$Rf_Daily <- NA

for (i in 1:nrow(df)) {
  df$Rf_Daily[i] <- (1 + df$RF[i]/100)^(1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$AMD_ExcessReturns <- NA
df$GSPC_ExcessReturns <- NA

for (i in 2:nrow(df)) {
  df$AMD_ExcessReturns[i] <- df$AMD_DailyReturn[i] - df$Rf_Daily[i]
  df$GSPC_ExcessReturns[i] <- df$GSPC_DailyReturn[i] - df$Rf_Daily[i]
}

head(df)

```


- **Perform Regression Analysis**: Using linear regression, we estimate the Beta (\(\Beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}


CAPM <- lm(AMD_ExcessReturns ~ GSPC_ExcessReturns, data = df)

summary(CAPM)
```


#### Interpretation

What is your \(\Beta\)? Is AMD more volatile or less volatile than the market? 

**Answer:**
The \(\Beta\) value attained when running the linear regression model was 1.5699987. The \(\Beta\) value represents the gradient of the linearly-fitted line, and in the context of the CAPM model in question, represents the sensitivity of the AMD's stock return as compared to S&P500 (GSPC), which can be considered the returns of the market (a measure of volatility for AMD's stock). Thus we are essentially measuring the systematic risk compared to the market, exhibiting a value of 1.5699987 which suggests AMD's stock is more volatile as we see generally larger movement in the price of the stock of AMD (indicated by \(\Beta\) being greater than 1). The reason for this is because the \(\Beta\) value tells us how AMD's stock behaves compared to the market, where if the market were to go up  by about 1%, then AMD is predicted to go up about 1.5699987% and vice versa if the market stock goes down. 

Since the extent to which AMD varies is greater than the market, it is thus defined as more volatile than the market. The implications of this \(\Beta\) value on risk and potential returns are as follows, Due to the greater volatility of the asset there also carries a higher risk, thus investors expect higher fluctuations in price compared to GSPC but alongside this risk carries the higher potential returns in which the asset also carries the possibility to outperform the market.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
ggplot(data = df, aes(x = GSPC_ExcessReturns, y = AMD_ExcessReturns)) +
  geom_point(color = "blue", alpha = 0.8) + # Scatter plot
  geom_smooth(method = "lm", color = "green", se = FALSE) + # Regression line
  labs(title = "CAPM Model for GSPC vs. AMD Excess Returns",
       x = "GSPC (S&P 500) Excess Returns",
       y = "AMD Excess Returns")
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**
```{r pi}

MarketReturn_Daily <- 0.133/252
MarketReturn_Annual <- 0.133
Rf_Annual <- 0.05
Rf_Daily <- (1 + Rf_Annual)^(1/360) - 1
Beta <- 1.5699987
observations <- nrow(df)
StandardError <- 0.02567

AMD_forecast_return <- Rf_Annual + Beta * (MarketReturn_Annual - Rf_Annual)

X_mean <- mean(df$GSPC_ExcessReturns, na.rm = TRUE)
Xf <- (MarketReturn_Daily - Rf_Daily)
sum_of_square_error_mean <- sum((df$GSPC_ExcessReturns - X_mean)^2, na.rm = TRUE)
Xf_deviation_from_mean <- (Xf - X_mean)^2
sf_daily <- StandardError * sqrt(1 + 1/observations + Xf_deviation_from_mean / sum_of_square_error_mean)
sf_annual <- s_f_daily * sqrt(252)

print(sf_daily)
print(sf_annual)
print(Rf_Daily)

t_criticalvalue <- qt(0.95, df = observations - 2)
lower_bound_prediction <- AMD_forecast_return - t_criticalvalue * sf_annual
upper_bound_prediction <- AMD_forecast_return + t_criticalvalue * sf_annual

print(lower_bound_prediction)
print(upper_bound_prediction)

```
From the above analysis we evaluate the range of the annual expected return falls between -49% and 85%. this range guarantees a 90% prediction interval and hence this range is relatively large. This can be reasoned due to the higher volatility of the AMD stock that was calculated in the previous part. Additionally other factor that may affect the non-systematic risk of the stock pays a role in the larger interval. 

Several implications from the large interval suggests the following to investors:
- As mentioned previously the high volatility and hence risk of the asset, investors are less confident about the returns of the stock.
- As a carry-on of the last point this suggests the possibility to outperform the market or perform poorly compared to the market due to the larger price swings.
- The larger interval suggests that there are external factors that play a role in the performance of the asset, these include economic indicators, political events or any macroeconomic variables that contibute to the systematic risk of the stock.
