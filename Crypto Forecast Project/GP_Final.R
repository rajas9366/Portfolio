install.packages("tidyverse")
install.packages("tidyquant")
install.packages("tidymodels")
install.packages("timetk")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("plotly")
install.packages("modeltime")
install.packages("quantmod")
install.packages("forecast")
install.packages("PerformanceAnalytics")
install.packages("prophet")

library(tidyverse)
library(tidyquant)
library(tidymodels)
library(timetk)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(modeltime)
library(quantmod)
library(forecast)
library(PerformanceAnalytics)
library(prophet)


cryptos <- c("BTC-USD", "ETH-USD", "BNB-USD", "XRP-USD", "SOL-USD")

# Downloading historical data:
getSymbols(cryptos, src = "yahoo", from = "2021-11-01", to = Sys.Date())


# Obtaining price charts for the desired cryptocurrencies:

chartSeries(`BTC-USD`, TA = "addVo();addBBands();addCCI();addEMA(20, col = 'blue');addWMA(20, col = 'red')")

chartSeries(`ETH-USD`, TA = "addVo();addBBands();addCCI();addEMA(20, col = 'blue');addWMA(20, col = 'red')")

chartSeries(`BNB-USD`, TA = "addVo();addBBands();addCCI();addEMA(20, col = 'blue');addWMA(20, col = 'red')")

chartSeries(`XRP-USD`, TA = "addVo();addBBands();addCCI();addEMA(20, col = 'blue');addWMA(20, col = 'red')")

chartSeries(`SOL-USD`, TA = "addVo();addBBands();addCCI();addEMA(20, col = 'blue');addWMA(20, col = 'red')")


# Calculating monthly returns:

monthly_returns <- lapply(cryptos, function(sym) {
  monthlyReturn(Ad(get(sym)))
})

returns_df <- do.call(cbind, monthly_returns)
colnames(returns_df) <- cryptos

returns_df



# Calculating Standard Deviation (Volatility):

volatility <- apply(returns_df, 2, sd)


risk_summary_1 <- data.frame(
  Cryptocurrency = cryptos,
  Volatility = volatility
)

print(risk_summary_1)



risk_free_rate <- 0.0425

# Calculating average returns:

average_return <- apply(returns_df, 2, mean)


volatility <- apply(returns_df, 2, sd)

# Calculating Sharpe Ratio:
sharpe_ratio <- (average_return - risk_free_rate) / volatility


sharpe_summary <- data.frame(
  Cryptocurrency = cryptos,
  Sharpe_Ratio = sharpe_ratio
)

print(sharpe_summary)



# Calculating Value at Risk (VaR) at a 95% confidence level
var_95 <- apply(returns_df, 2, function(x) quantile(x, 0.05))

# Calculating Conditional Value at Risk (CVaR) at a 95% confidence level
cvar_95 <- apply(returns_df, 2, function(x) mean(x[x <= quantile(x, 0.05)]))

# Calculating Standard Deviation
std_dev <- apply(returns_df, 2, sd)

# Creating a data frame to summarize risk measures
risk_summary_2 <- data.frame(
  Cryptocurrency = cryptos,
  Volatility = volatility,
  VaR_95 = var_95,
  CVaR_95 = cvar_95
)

print(risk_summary_2)


#Converting returns_df to a data frame 

returns_df <- as.data.frame(returns_df)
returns_df$Month <- rownames(returns_df)


returns_long <- returns_df %>%
  pivot_longer(cols = -Month, names_to = "Cryptocurrency", values_to = "Returns")


interactive_plot <- plot_ly(returns_long, x = ~Month, y = ~Returns, color = ~Cryptocurrency, type = "bar") %>%
  layout(title = "Monthly Returns for Cryptocurrencies",
         xaxis = list(title = "Month"),
         yaxis = list(title = "Returns"))

interactive_plot







#---------------------

#Time Series Forecasting:


BTC_data <- read.csv("Bitcoin Historical Data.csv")
BNB_data <- read.csv("Binance Historical Data.csv")
ETH_data <- read.csv("Ethereum Historical Data.csv")
XRP_data <- read.csv("XRP Historical Data.csv")
SOL_data <- read.csv("Solana Historical Data.csv")


BTC_data



# Converting our data into tibble format:


BTC_tsibble <- BTC_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date to Date type
    Price = as.numeric(gsub(",", "", Price)),   # Remove commas and convert to numeric
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Vol. = as.numeric(gsub("K", "", gsub(",", "", Vol.))),  # Remove commas and 'K', convert to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) / 100  # Remove '%' and convert to numeric, divide by 100
  )

ETH_tsibble <- ETH_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date to Date type
    Price = as.numeric(gsub(",", "", Price)),   # Remove commas and convert to numeric
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Vol. = as.numeric(gsub("K", "", gsub(",", "", Vol.))),  # Remove commas and 'K', convert to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) / 100  # Remove '%' and convert to numeric, divide by 100
  )

BNB_tsibble <- BNB_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date to Date type
    Price = as.numeric(gsub(",", "", Price)),   # Remove commas and convert to numeric
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Vol. = as.numeric(gsub("K", "", gsub(",", "", Vol.))),  # Remove commas and 'K', convert to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) / 100  # Remove '%' and convert to numeric, divide by 100
  )

XRP_tsibble <- XRP_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date to Date type
    Price = as.numeric(gsub(",", "", Price)),   # Remove commas and convert to numeric
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Vol. = as.numeric(gsub("K", "", gsub(",", "", Vol.))),  # Remove commas and 'K', convert to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) / 100  # Remove '%' and convert to numeric, divide by 100
  )

SOL_tsibble <- SOL_data %>%
  mutate(
    Date = as.Date(Date, format = "%m/%d/%Y"),  # Convert Date to Date type
    Price = as.numeric(gsub(",", "", Price)),   # Remove commas and convert to numeric
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Vol. = as.numeric(gsub("K", "", gsub(",", "", Vol.))),  # Remove commas and 'K', convert to numeric
    Change.. = as.numeric(gsub("%", "", Change..)) / 100  # Remove '%' and convert to numeric, divide by 100
  )




BTC_tbl <- BTC_tsibble
ETH_tbl <- ETH_tsibble
BNB_tbl <- BNB_tsibble
XRP_tbl <- XRP_tsibble
SOL_tbl <- SOL_tsibble




#1) BTC:

btc_prophet <- BTC_tbl %>%
  select(ds = Date, y = Price)


model_prophet <- prophet(btc_prophet)


future <- make_future_dataframe(model_prophet, periods = 180)  # Adjust the number of days as needed


forecast_prophet <- predict(model_prophet, future)


interactive_plot1 <- plot_ly() %>%
  add_lines(x = btc_prophet$ds, y = btc_prophet$y, name = "Observed", type = "scatter", mode = "lines") %>%
  add_lines(x = forecast_prophet$ds, y = forecast_prophet$yhat, name = "Forecast", type = "scatter", mode = "lines")


interactive_plot1 <- interactive_plot1 %>%
  layout(title = "Bitcoin Price Forecast",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Price"))


interactive_plot1






#2)ETH:

ETH_prophet <- ETH_tbl %>%
  select(ds = Date, y = Price)


model_prophet <- prophet(ETH_prophet)


future <- make_future_dataframe(model_prophet, periods = 180)  # Adjust the number of days as needed


forecast_prophet <- predict(model_prophet, future)


interactive_plot2 <- plot_ly()


interactive_plot2 <- interactive_plot2 %>%
  add_markers(x = ETH_prophet$ds, y = ETH_prophet$y, name = "Observed", marker = list(color = 'red', size = 3))


interactive_plot2 <- interactive_plot2 %>%
  add_markers(x = forecast_prophet$ds, y = forecast_prophet$yhat, name = "Forecast", marker = list(color = 'white', size = 2))


interactive_plot2 <- interactive_plot2 %>%
  layout(title = "ETH Price Forecast",
         xaxis = list(title = "Date", showgrid = TRUE),
         yaxis = list(title = "Price", showgrid = TRUE),
         plot_bgcolor = 'black',
         paper_bgcolor = 'white')


interactive_plot2




#3)SOL:

SOL_prophet <- SOL_tbl %>%
  select(ds = Date, y = Price)


model_prophet <- prophet(SOL_prophet)


future <- make_future_dataframe(model_prophet, periods = 180)  # Adjust the number of days as needed


forecast_prophet <- predict(model_prophet, future)


interactive_plot3 <- plot_ly() %>%
  add_lines(
    x = SOL_prophet$ds, 
    y = SOL_prophet$y, 
    name = "Observed", 
    type = "scatter", 
    mode = "lines",
    line = list(color = "green")
  ) %>%
  add_lines(
    x = forecast_prophet$ds, 
    y = forecast_prophet$yhat, 
    name = "Forecast", 
    type = "scatter", 
    mode = "lines",
    line = list(color = "red", dash = "dash")
  )


interactive_plot3 <- interactive_plot3 %>%
  layout(
    title = "SOL Price Forecast",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Price"),
    legend = list(x = 0.8, y = 0.9)
  )


interactive_plot3




#4)XRP:

XRP_prophet <- XRP_tbl %>%
  select(ds = Date, y = Price)

model_prophet <- prophet(XRP_prophet)


future <- make_future_dataframe(model_prophet, periods = 180)  # Adjust the number of days as needed


forecast_prophet <- predict(model_prophet, future)


interactive_plot4 <- plot_ly() %>%
  add_lines(x = XRP_prophet$ds, y = XRP_prophet$y, name = "Observed", type = "scatter", mode = "lines") %>%
  add_lines(x = forecast_prophet$ds, y = forecast_prophet$yhat, name = "Forecast", type = "scatter", mode = "lines")


interactive_plot4 <- interactive_plot4 %>%
  layout(title = "XRP Price Forecast",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Price"))


interactive_plot4





#5)BNB:

BNB_prophet <- BNB_tbl %>%
  select(ds = Date, y = Price)


model_prophet <- prophet(BNB_prophet)

future <- make_future_dataframe(model_prophet, periods = 180)  # Adjust the number of days as needed


forecast_prophet <- predict(model_prophet, future)


interactive_plot5 <- plot_ly() %>%
  add_lines(x = BNB_prophet$ds, y = BNB_prophet$y, name = "Observed", type = "scatter", mode = "lines", line = list(color = 'purple')) %>%
  add_lines(x = forecast_prophet$ds, y = forecast_prophet$yhat, name = "Forecast", type = "scatter", mode = "lines", line = list(color = 'teal'))


interactive_plot5 <- interactive_plot5 %>%
  layout(title = "BNB Price Forecast",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Price"))


interactive_plot5



#-----------------------



