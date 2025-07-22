setwd("C:/Users/Ashmita/OneDrive/Desktop/PROJECTS/mutual_fund_project")
getwd()

library(readxl)
library(dplyr)
library(lubridate)

# Read from row 6 and select only NAV & Date
nav_icici <- read_excel("icici_largecap.xlsx", skip = 5) %>%
  select(1, 4) %>%  # Columns: Net Asset Value and NAV date
  rename(nav = 1, date = 2) %>%
  mutate(
    date = as.Date(date),
    nav = as.numeric(nav)
  ) %>%
  arrange(date) %>%
  filter(!is.na(nav)) %>%
  mutate(
    return = log(nav / lag(nav))
  )

# Preview cleaned data
head(nav_icici)


# Load packages
library(readxl)
library(dplyr)
library(lubridate)

# Read and clean HDFC Mid Cap Fund data
nav_midcap <- read_excel("hdfc_midcap.xlsx", skip = 5) %>%
  select(1, 4) %>%  # Columns: NAV and Date
  rename(nav = 1, date = 2) %>%
  mutate(
    date = as.Date(date),
    nav = as.numeric(nav)
  ) %>%
  arrange(date) %>%
  filter(!is.na(nav)) %>%
  mutate(
    return = log(nav / lag(nav))
  )

# Preview first few rows
head(nav_midcap)

library(readxl)
library(dplyr)
library(lubridate)

# Read and clean SBI Dynamic Bond Fund data
nav_debt <- read_excel("sbi_debt.xlsx", skip = 5) %>%
  select(1, 4) %>%  # Columns: NAV and Date
  rename(nav = 1, date = 2) %>%
  mutate(
    date = as.Date(date),
    nav = as.numeric(nav)
  ) %>%
  arrange(date) %>%
  filter(!is.na(nav)) %>%
  mutate(
    return = log(nav / lag(nav))
  )

# Preview cleaned data
head(nav_debt)


library(readxl)
library(dplyr)
library(lubridate)

# Read and clean HDFC Hybrid Fund NAV data
nav_hybrid <- read_excel("hdfc_hybrid.xlsx", skip = 5) %>%
  select(1, 4) %>%  # Columns: NAV and Date
  rename(nav = 1, date = 2) %>%
  mutate(
    date = as.Date(date),
    nav = as.numeric(nav)
  ) %>%
  arrange(date) %>%
  filter(!is.na(nav)) %>%
  mutate(
    return = log(nav / lag(nav))
  )

# Preview cleaned data
head(nav_hybrid)

library(readxl)
library(dplyr)
library(lubridate)

# Read and clean Axis ELSS Fund data
nav_elss <- read_excel("axis_elss.xlsx", skip = 5) %>%
  select(1, 4) %>%  # Columns: NAV and Date
  rename(nav = 1, date = 2) %>%
  mutate(
    date = as.Date(date),
    nav = as.numeric(nav)
  ) %>%
  arrange(date) %>%
  filter(!is.na(nav)) %>%
  mutate(
    return = log(nav / lag(nav))
  )

# Preview the cleaned data
head(nav_elss)

# Rename return columns for clarity
icici_ret  <- nav_icici %>% select(date, return) %>% rename(icici = return)
midcap_ret <- nav_midcap %>% select(date, return) %>% rename(midcap = return)
debt_ret   <- nav_debt %>% select(date, return) %>% rename(debt = return)
hybrid_ret <- nav_hybrid %>% select(date, return) %>% rename(hybrid = return)
elss_ret   <- nav_elss %>% select(date, return) %>% rename(elss = return)

library(dplyr)
library(purrr)

# Merge all five return datasets by date
returns_df <- reduce(
  list(icici_ret, midcap_ret, debt_ret, hybrid_ret, elss_ret),
  full_join,
  by = "date"
) %>% arrange(date)

# View the first few rows
head(returns_df)

# Load necessary packages
library(xts)                 # For converting data to time series format
library(PerformanceAnalytics) # For financial metrics like Sharpe, Beta

#Converting return data to a time-series xts object
# Excluding the date column and use it as row index
returns_xts <- xts(returns_df[,-1], order.by = returns_df$date)

# Defining a risk-free rate (e.g., 6% annual return converted to daily)
risk_free_rate <- 0.06 / 252  # Assuming that there are 252 trading days per year

# Calculating Annualized Standard Deviation (Volatility) for each fund
# Measures total risk or variability of returns
sd_annual <- apply(returns_xts, 2, function(x) {
  sd(x, na.rm = TRUE) * sqrt(252)  # Daily to annualized
})

# Calculating Annualized Sharpe Ratio for each fund
# Sharpe = (Average return - Risk-free rate) / Std Dev

sharpe_values <- sapply(colnames(returns_xts), function(fund) {
  SharpeRatio.annualized(R = returns_xts[, fund], Rf = risk_free_rate, scale = 252)
})


# Calculating Beta of each fund using ICICI fund as market proxy
# Beta > 1 = more volatile than market, Beta < 1 = less volatile
beta_values <- apply(returns_xts, 2, function(x) {
  cov(x, returns_xts[, "icici"], use = "complete.obs") / var(returns_xts[, "icici"], na.rm = TRUE)
})

#Creating a summary table of risk metrics
# Combining all metrics into one data frame for easy comparison
risk_summary <- data.frame(
  Fund = colnames(returns_xts),
  Std_Dev = round(sd_annual, 4),
  Sharpe = round(as.numeric(sharpe_values), 4),
  Beta = round(beta_values, 4)
)




# Display the final risk summary table
print(risk_summary)
  

#VISUALISATIONS
#Bar Chart of Standard Deviation
library(ggplot2)

ggplot(risk_summary, aes(x = Fund, y = Std_Dev, fill = Fund)) +
  geom_bar(stat = "identity") +
  labs(title = "Volatility (Standard Deviation) by Fund", y = "Annualized Std Dev") +
  theme_minimal()
# Interpretation of Standard Deviation Bar Chart:
# -----------------------------------------------
# This bar chart shows the annualized standard deviation (volatility) of each mutual fund category.
# 
# - Mid Cap fund shows the highest volatility (~0.16), indicating higher risk and return potential.
# - ELSS/Tax Saver also has high volatility (~0.15), due to equity exposure and market sensitivity.
# - ICICI Large Cap (~0.14) offers moderate risk with relatively stable large-cap equity exposure.
# - Hybrid fund (~0.11) balances equity and debt, offering medium volatility.
# - Debt fund has the lowest volatility (~0.02), reflecting low risk and stable returns.
# 
# Overall, higher standard deviation indicates greater fluctuations in NAV and higher investment risk.
#-----------------------------------------------------------------------------------------------------

#Lollipop Chart of Sharpe Ratio
# Highlights risk-adjusted performance with clearer fund comparisons than a bar chart.

# Lollipop Chart for Sharpe Ratio
ggplot(risk_summary, aes(x = reorder(Fund, Sharpe), y = Sharpe)) +
  geom_segment(aes(xend = Fund, y = 0, yend = Sharpe), color = "grey") +
  geom_point(size = 5, color = "pink") +
  coord_flip() +
  labs(title = "Sharpe Ratio by Fund",
       x = "Fund",
       y = "Sharpe Ratio") +
  theme_minimal()
# ──────────────────────────────────────────────────────────────
# Interpretation of Sharpe Ratio Lollipop Chart:
#
# ▸ Hybrid Fund shows the highest Sharpe Ratio (~1.58), 
#   indicating the best risk-adjusted performance among all funds.
#
# ▸ Mid Cap Fund also performs well (~1.56), suggesting strong returns 
#   per unit of risk, despite its relatively high volatility.
#
# ▸ ICICI Large Cap Fund (Sharpe ~1.20) offers moderate 
#   risk-adjusted returns with lower volatility than midcap.
#
# ▸ ELSS/Tax Saver has a lower Sharpe Ratio (~0.71), implying 
#   that its returns aren't as strong when adjusted for risk.
#
# ▸ Debt Fund has the lowest Sharpe Ratio (~0.25), reflecting 
#   its stable but low-return profile — suitable for risk-averse investors.
# ──────────────────────────────────────────────────────────────

#Bar Chart of Beta
# Beta Bar Chart by Fund
ggplot(risk_summary, aes(x = reorder(Fund, Beta), y = Beta, fill = Fund)) +
  geom_bar(stat = "identity") +
  labs(title = "Beta by Fund", x = "Fund", y = "Beta (Market Sensitivity)") +
  theme_minimal()

# -----------------------------------------------------------------------------
# Interpretation of Beta by Fund Chart
# - ICICI Large Cap (Beta = 1.00): Perfectly tracks the market, meaning it carries full market risk.
# - Mid Cap and ELSS Funds (Beta ~0.95): Slightly less sensitive than the market, indicating moderately high risk.
# - Hybrid Fund (Beta ~0.75): Has moderate market sensitivity due to a mix of equity and debt.
# - Debt Fund (Beta ~0.02): Has negligible market sensitivity, making it suitable for conservative investors.
#
# A higher beta indicates greater volatility in relation to the market,
# while a lower beta suggests more stability and lower responsiveness to market movements.
#--------------------------------------------------------------------------------------------
#Cumulative Returns of Mutual Funds over time
colnames(icici_ret)
colnames(midcap_ret)
colnames(debt_ret)
colnames(hybrid_ret)
colnames(elss_ret)

library(dplyr)
library(purrr)

returns_combined <- reduce(
  list(icici_ret, midcap_ret, debt_ret, hybrid_ret, elss_ret),
  full_join,
  by = "date"
) %>%
  arrange(date)

library(xts)

returns_xts <- xts(
  data.frame(lapply(returns_combined[, -1], as.numeric)),
  order.by = returns_combined$date
)

returns_xts_clean <- na.omit(returns_xts)

cumulative_returns <- cumprod(1 + returns_xts_clean)

library(reshape2)

cumulative_df <- data.frame(
  date = index(cumulative_returns),
  coredata(cumulative_returns)
)

cumulative_long <- melt(
  cumulative_df,
  id.vars = "date",
  variable.name = "Fund",
  value.name = "Cumulative_Return"
)

library(ggplot2)

ggplot(cumulative_long, aes(x = date, y = Cumulative_Return, color = Fund)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Cumulative Returns of Mutual Funds Over Time",
    x = "Date",
    y = "Cumulative Return (Growth of ₹1)"
  ) +
  theme_minimal()
#-------------------------------------------------------
  # Interpretation:
  # The chart visualizes the cumulative growth of ₹1 invested in each mutual fund from July 2020 to July 2025.
  # 
  # - The **midcap fund** significantly outperforms others, reaching a return of nearly ₹4, indicating high growth potential but likely higher volatility.
  # - **ICICI** and **hybrid funds** show steady and strong performance, with ICICI slightly ahead, both nearing 2.8x–3x returns.
  # - **ELSS** follows a similar trajectory but with more volatility, showing consistent but moderate long-term gains.
  # - The **debt fund** remains the most stable with minimal fluctuations and lowest return (~1.3x), ideal for risk-averse investors.
  # 
  # Overall, this plot helps assess the relative risk-return profile of each fund type over the 5-year period.
#-------------------------------------------------------------------  

#Constructing a Sample Portfolio
# We're creating a sample portfolio worth ₹1,00,000 with the following allocation:
# 40% in Hybrid, 30% in Midcap, 15% in ICICI, 10% in ELSS, 5% in Debt Fund
portfolio_weights <- c(
  hybrid = 0.40,
  midcap = 0.30,
  icici = 0.15,
  elss = 0.10,
  debt = 0.05
)

# Joining all individual fund return data by 'date'

library(dplyr)
returns_combined <- full_join(icici_ret, midcap_ret, by = "date") %>%
  full_join(debt_ret, by = "date") %>%
  full_join(hybrid_ret, by = "date") %>%
  full_join(elss_ret, by = "date") %>%
  arrange(date)

#Converting to time series
library(xts)
returns_xts <- xts(returns_combined[, -1], order.by = returns_combined$date)

# Remove rows with missing values (NAs)
returns_xts_clean <- na.omit(returns_xts)




#Calculating daily portfolio returns
# Multiplying each fund's return by its respective weight and sum row-wise

# Matrix multiplication: returns × weights

# Converting to a numeric vector for analysis
# Convert to a numeric vector for analysis
weighted_returns <- returns_xts_clean %*% as.matrix(portfolio_weights)

portfolio_returns <- as.numeric(weighted_returns)

#Portfolio Performance Metrics
# 1. Annualized Return (compounding over 252 trading days)
annual_return <- prod(1 + portfolio_returns)^(252 / length(portfolio_returns)) - 1

# 2. Annualized Standard Deviation (volatility)
annual_sd <- sd(portfolio_returns) * sqrt(252)

# 3. Sharpe Ratio (assuming daily risk-free rate = 0.0002830895)
risk_free_rate <- 0.0002830895
sharpe_ratio <- (annual_return - (risk_free_rate * 252)) / annual_sd

# Print key portfolio metrics
cat("Annual Return (%):", round(annual_return * 100, 2), "\n")
cat("Annual Volatility (%):", round(annual_sd * 100, 2), "\n")
cat("Sharpe Ratio:", round(sharpe_ratio, 2), "\n")

#Visualisation of Portfolio Allocation using Pie-Chart
# Portfolio allocation in percentage

# Portfolio allocation in percentage
portfolio_weights <- c(
  Hybrid = 0.40,
  Midcap = 0.30,
  ICICI = 0.15,
  ELSS = 0.10,
  Debt = 0.05
)
# Create a data frame for plotting
portfolio_df <- data.frame(
  Fund = names(portfolio_weights),
  Allocation = portfolio_weights * 100  # convert to percentage
)

# Calculate label positions and formatted labels
portfolio_df <- portfolio_df %>%
  arrange(desc(Fund)) %>%
  mutate(
    ypos = cumsum(Allocation) - 0.5 * Allocation,
    label = paste0(Fund, ": ", round(Allocation), "%")
  )

# Pie chart with labels
ggplot(portfolio_df, aes(x = "", y = Allocation, fill = Fund)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = label), color = "black", size = 4) +  # add labels
  labs(title = "Portfolio Allocation (₹1,00,000)", x = NULL, y = NULL) +
  theme_void() +

  scale_fill_brewer(palette = "Set3")

# Interpretation of Portfolio Allocation Chart

# The pie chart visualizes the allocation of a ₹1,00,000 investment across five mutual fund categories.

# Hybrid funds receive the largest share (40%), reflecting a preference for balanced exposure between equity and debt.
# Midcap funds are allocated 30%, indicating an appetite for moderate-to-high growth potential with associated risks.
# ICICI (large-cap) is given a 15% weight, offering stability and steady returns from well-established companies.
# ELSS (Tax Saver) receives a 10% share, balancing tax benefits with long-term equity exposure.
# Debt funds are assigned the smallest portion (5%), mainly for capital preservation and reduced volatility.

# This allocation reflects a moderately aggressive investor profile—aiming for capital appreciation while maintaining diversification across asset types.
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Correlation Matrix of Mutual Fund Returns

install.packages("corrplot")  # Run this only once
library(corrplot)

#Computing correlation matrix of mutual fund returns
# Using the cleaned returns_xts_clean (xts object with numeric return columns)
cor_matrix <- cor(returns_xts_clean)

# Plotting the correlation matrix using corrplot
corrplot(
  cor_matrix,                # correlation matrix as input
  method = "color",          # color-coded tiles
  type = "upper",            # show upper triangle only
  addCoef.col = "black",     # show correlation values in black
  tl.col = "black",          # label color for fund names
  tl.srt = 45,               # angle of fund labels
  col = colorRampPalette(c("red", "white", "blue"))(200),  # color gradient
  title = "Correlation Matrix of Mutual Fund Returns",
  mar = c(0, 0, 2, 0)        # margin to give space for title
)

# View the correlation values in the console
print(round(cor_matrix, 2))

# ------------------------ Interpretation: Correlation Matrix ------------------------
# The correlation matrix shows the pairwise correlation between daily returns 
# of the five mutual funds: ICICI, Midcap, Debt, Hybrid, and ELSS.

# Highly Positively Correlated Funds (r > 0.80)
# - ICICI & Hybrid: 0.90
# - ICICI & ELSS: 0.88
# - Midcap & Hybrid: 0.85
# - Midcap & ELSS: 0.83
# - ICICI & Midcap: 0.82
# These funds tend to move in similar directions, implying high co-movement.
# While this can enhance return potential, it limits the diversification benefit.

# Moderate Correlation:
# - Hybrid & ELSS: 0.79 (Slightly below the high threshold)

#  Low Correlation with Debt Fund (r ≈ 0.10):
# - Debt & ICICI/Midcap/Hybrid/ELSS all have weak correlation (~0.10)
# This means debt funds are largely uncorrelated with equity-oriented funds
# and act as effective diversifiers during market volatility.

# 
# - The portfolio consists of highly correlated equity-heavy funds.
# - Inclusion of a low-correlation debt fund helps mitigate risk and smoothen returns.
# -------------------------------------------------------------------------------------

#Value-at-Risk (VaR) Calculation
library(PerformanceAnalytics)
VaR(returns_xts_clean, p = 0.95, method = "historical")
# - For ICICI, there is a 95% chance that daily loss will not exceed 1.33%.
# - Midcap and ELSS are the riskiest funds, with possible daily losses > 1.6%.
# - Debt has the lowest risk profile with a minimal loss risk of 0.17%.
# - Hybrid lies in between, with moderate risk exposure.
# - VaR helps assess downside risk for each fund individually.
# - It confirms that equity-oriented funds (midcap, ELSS, ICICI) carry higher risk.
# - Debt offers stability and helps cushion the portfolio during market dips.
# --------------------------------------------------------------------------------------

#Risk-return Scatter Plot
#Calculating annualized return and risk for each fund
# Multiplying by 252 to annualize daily return and standard deviation

risk_return_df <- data.frame(
  Fund = colnames(returns_xts_clean),
  Annual_Return = apply(returns_xts_clean, 2, mean) * 252,
  Annual_Risk = apply(returns_xts_clean, 2, sd) * sqrt(252) 
)
library(ggplot2)

ggplot(risk_return_df, aes(x = Annual_Risk, y = Annual_Return, label = Fund)) +
  geom_point(color = "orange", size = 4) +  # plot the dots
  geom_text(vjust = -1, size = 4) +            # add fund labels above points
  labs(title = "Risk-Return Profile of Mutual Funds",
       x = "Annualized Risk (Standard Deviation)",
       y = "Annualized Return") +
  theme_minimal()

# ---------------------------- INTERPRETATION: Risk‑Return Scatter ----------------------------
# •  X‑axis  – Annualized Risk (Std.Dev.);  Y‑axis  – Annualized Return. 
#
# •  Midcap  (top‑right) – highest return (~28%) **and** highest risk (~16 %).  
#    ▸ Suitable for aggressive investors seeking growth and willing to accept volatility.
#
# •  Hybrid  – strong return (~23 %) with lower risk (~11 %) than other equity funds.  
#    ▸ Sits closest to an “efficient” zone of high return per unit of risk.
#
# •  ICICI (Large‑Cap) – good return (~21 %) at ~13 % risk; offers blue‑chip stability.  
#
# •  ELSS  – moderate return (~16 %) at ~15 % risk; tax benefit can still justify inclusion.  
#
# •  Debt  (bottom‑left) – lowest risk (~2 %) and lowest return (~8 %).  
#    ▸ Acts as a capital‑preservation anchor and diversifier in the overall portfolio.
#
# Take‑away: combining low‑correlation, lower‑risk assets (Debt, Hybrid) with  
# higher‑return equity funds (Midcap, ICICI, ELSS) helps move the portfolio toward an  
# attractive risk‑adjusted position (higher Sharpe ratio).
# --------------------------------------------------------------------------------------------

#Summary Table
# Calculating annualized return and risk
annual_returns <- apply(returns_xts_clean, 2, mean) * 252
annual_sd_values <- apply(returns_xts_clean, 2, sd) * sqrt(252)

# Assuming constant risk-free rate for Sharpe Ratio (same as before)
risk_free_rate <- 0.0002830895  # daily rate
sharpe_ratios <- (annual_returns - (risk_free_rate * 252)) / annual_sd_values

# Creating a function to calculate beta (vs a benchmark, say "midcap")

# 3. Create a function to calculate beta (vs a benchmark, say "midcap")
calculate_beta <- function(asset_return, benchmark_return) {
  cov(asset_return, benchmark_return) / var(benchmark_return)
}

# Create summary table
summary_table <- data.frame(
  Fund = colnames(returns_xts_clean),
  Annual_Return = round(annual_returns * 100, 2),   # In %
  Annual_Risk = round(annual_sd_values * 100, 2),   # In %
  Sharpe_Ratio = round(sharpe_ratios, 2)            # Risk-adjusted return
)
print(summary_table)

write.csv(summary_table, "mutual_fund_summary.csv", row.names = FALSE)
# Summary Interpretation of Mutual Fund Performance

# The above table summarizes the key performance metrics of the five mutual funds in the portfolio.
# 
#  **Midcap Fund** delivered the **highest annual return (28.88%)**, but also had the **highest volatility (15.80%)**, indicating a higher-risk, higher-return profile. It also had the **best Sharpe Ratio (1.38)**, suggesting strong risk-adjusted performance.
# 
#  **Hybrid Fund** achieved a solid annual return of **22.55%** with **lower volatility (11.36%)**, resulting in a **Sharpe Ratio of 1.36**, making it an efficient fund for risk-averse investors.
# 
#  **ICICI Fund** showed a decent return of **21.11%** with moderate volatility, leading to a **Sharpe Ratio of 1.03**.
# 
# **ELSS Fund** had a relatively lower return of **16.12%**, but its high volatility reduced its **Sharpe Ratio to 0.61**, indicating less efficient risk-adjusted performance.
# 
# **Debt Fund**, as expected, was the least volatile (**Annual Risk = 2.21%**), but also delivered the lowest return (**6.59%**) and had a **negative Sharpe Ratio (-0.25)**, suggesting that it underperformed the risk-free rate in this sample period.

# Overall, the portfolio benefits from diversification, with high-return equity funds balanced by lower-risk debt components.
#-------------------------------------------------------------------------------------

#Mean-Variance Optimization

library(dplyr)
library(ggplot2)

mean_returns <- colMeans(returns_xts_clean)
cov_matrix <- cov(returns_xts_clean)

# We set up simulation parameters
n_portfolios <- 50000
n_assets <- ncol(returns_xts_clean)

# Initializing storage
results <- matrix(nrow = n_portfolios, ncol = n_assets + 3)
colnames(results) <- c(colnames(returns_xts_clean), "Return", "Risk", "Sharpe")

# Risk-free rate (daily)
rf <- 0.0002830895

# Simulating portfolios
set.seed(123)  # For reproducibility
for (i in 1:n_portfolios) {
  # Generating random weights
  weights <- runif(n_assets)
  weights <- weights / sum(weights)
  # Calculating portfolio return
  port_return <- sum(weights * mean_returns) * 252
  
  # Calculating portfolio risk
  port_risk <- sqrt(t(weights) %*% cov_matrix %*% weights) * sqrt(252)
  
  # Calculating Sharpe Ratio
  sharpe <- (port_return - (rf * 252)) / port_risk
  
  # Storing results
  results[i, 1:n_assets] <- weights
  results[i, "Return"] <- port_return
  results[i, "Risk"] <- port_risk
  results[i, "Sharpe"] <- sharpe
}

# Converting to data frame
results_df <- as.data.frame(results)

# Finding optimal portfolio (maximum Sharpe ratio)
max_sharpe_port <- results_df[which.max(results_df$Sharpe), ]

# Printing optimal portfolio
print(round(max_sharpe_port, 4))

# Interpretation:
# The Mean-Variance Optimization simulation evaluated 50,000 randomly weighted mutual fund portfolios.
# The optimal portfolio — the one with the highest Sharpe Ratio of **1.41** — was identified.
# This portfolio maximizes return per unit of risk taken, considering the given risk-free rate.

# Optimal Portfolio Weights:
# - ICICI Fund        : 3.09%
# - Midcap Fund       : 39.14%
# - Debt Fund         : 0.15%
# - Hybrid Fund       : 57.52%
# - ELSS Fund         : 0.11%

#  Portfolio Performance Metrics:
# - Expected Annual Return  : 24.95%
# - Expected Annual Risk    : 12.63%
# - Sharpe Ratio            : 1.41

#  Insight:
# The optimal portfolio suggests a **higher allocation to Hybrid and Midcap funds**, as they offer a favorable balance of return and risk.
# Debt and ELSS funds receive minimal allocation due to their lower return contribution or suboptimal risk-adjusted performance.

# This portfolio is well-suited for moderately aggressive investors seeking strong returns while maintaining risk under control.
#Plotting the Efficient Frontier
ggplot(results_df, aes(x = Risk, y = Return)) +
  geom_point(alpha = 0.4, color = "blue") +  # All portfolios
  geom_point(data = max_sharpe_port, aes(x = Risk, y = Return), 
             color = "red", size = 4, shape = 17) +  # Optimal (Max Sharpe) portfolio
  labs(title = "Efficient Frontier of Mutual Fund Portfolios",
       x = "Annualized Risk (Standard Deviation)",
       y = "Annualized Return") +
  theme_minimal() +
  annotate("text", x = max_sharpe_port$Risk, y = max_sharpe_port$Return + 0.01,
           label = paste0("Max Sharpe\n(", round(max_sharpe_port$Sharpe, 2), ")"),
           color = "red", size = 4, fontface = "bold")
# Efficient Frontier Interpretation:
# This plot visualizes the Efficient Frontier based on 50,000 randomly generated mutual fund portfolios.
# Each blue dot represents a portfolio with a unique combination of weights across the five funds.
# The X-axis shows annualized risk (standard deviation), and the Y-axis shows annualized return.
# The red triangle marks the portfolio with the highest Sharpe Ratio, indicating the most efficient
# portfolio in terms of risk-adjusted return.
# This optimal portfolio achieves a Sharpe Ratio of ~1.41, balancing return and volatility better
# than all other portfolios in the sample.
#---------------------------------------------------------------------------
#DASHBOARD
# Load necessary libraries


library(shiny)
library(ggplot2)
library(dplyr)
library(purrr)
library(xts)
library(reshape2)
library(corrplot)
library(PerformanceAnalytics)

# UI
ui <- fluidPage(
  tags$style(HTML("
    body {
      background: linear-gradient(to right, #e0f7fa, #ffffff);
    }
    .tab-content {
      background-color: white;
      padding: 20px;
      border-radius: 10px;
    }
  ")),
  
  titlePanel("Mutual Fund Dashboard"),
  
  tabsetPanel(
    tabPanel("Cumulative Returns",
             plotOutput("cumulativeReturnsPlot")
    ),
    tabPanel("Efficient Frontier",
             plotOutput("efficientFrontierPlot")
    ),
    tabPanel("Beta Chart",
             plotOutput("betaBarChart")
    ),
    tabPanel("Standard Deviation",
             plotOutput("sdBarChart")
    ),
    tabPanel("Sharpe Ratio",
             plotOutput("sharpeLollipop")
    ),
    tabPanel("Risk-Return Profile",
             plotOutput("riskReturnPlot")
    ),
    tabPanel("Portfolio Allocation",
             plotOutput("portfolioPieChart")
    ),
    tabPanel("Correlation Matrix",
             plotOutput("correlationMatrixPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Load Data
  returns_combined <- reduce(
    list(icici_ret, midcap_ret, debt_ret, hybrid_ret, elss_ret),
    full_join, by = "date"
  ) %>% arrange(date)
  
  returns_xts <- xts(
    data.frame(lapply(returns_combined[, -1], as.numeric)),
    order.by = returns_combined$date
  )
  
  returns_xts_clean <- na.omit(returns_xts)
  
  # Cumulative Returns
  output$cumulativeReturnsPlot <- renderPlot({
    cumulative_returns <- cumprod(1 + returns_xts_clean)
    cumulative_df <- data.frame(date = index(cumulative_returns), coredata(cumulative_returns))
    cumulative_long <- melt(cumulative_df, id.vars = "date", variable.name = "Fund", value.name = "Cumulative_Return")
    
    ggplot(cumulative_long, aes(x = date, y = Cumulative_Return, color = Fund)) +
      geom_line(linewidth = 1.1) +
      labs(title = "Cumulative Returns of Mutual Funds Over Time", x = "Date", y = "Cumulative Return (Growth of ₹1)") +
      theme_minimal()
  })
  
  # Efficient Frontier
  output$efficientFrontierPlot <- renderPlot({
    plot(results_df$Risk, results_df$Return,
         col = "gray", pch = 16, cex = 0.7,
         xlab = "Portfolio Risk (SD)", ylab = "Portfolio Return",
         main = "Efficient Frontier")
    points(max_sharpe_port$Risk, max_sharpe_port$Return,
           col = "red", pch = 19, cex = 1.5)
    text(max_sharpe_port$Risk, max_sharpe_port$Return,
         labels = "Max Sharpe", pos = 4, col = "red")
  })
  
  
  # Beta Bar Chart
  # Beta Bar Chart
  output$betaBarChart <- renderPlot({
    ggplot(risk_summary, aes(x = reorder(Fund, Beta), y = Beta, fill = Fund)) +
      geom_bar(stat = "identity") +
      labs(title = "Beta by Fund", x = "Fund", y = "Beta (Market Sensitivity)") +
      theme_minimal()
  })
  
  
  # Standard Deviation Bar Chart
  output$sdBarChart <- renderPlot({
    ggplot(summary_table, aes(x = Fund, y = Annual_Risk, fill = Fund)) +
      geom_bar(stat = "identity", width = 0.6) +
      labs(title = "Standard Deviation of Mutual Funds", x = "Fund", y = "Annualized SD") +
      theme_minimal()
  })
  
  # Sharpe Ratio Lollipop Chart
  output$sharpeLollipop <- renderPlot({
    ggplot(summary_table, aes(x = Fund, y = Sharpe_Ratio)) +
      geom_segment(aes(x = Fund, xend = Fund, y = 0, yend = Sharpe_Ratio), color = "skyblue") +
      geom_point(size = 5, color = "pink") +
      labs(title = "Sharpe Ratio of Mutual Funds", x = "Fund", y = "Sharpe Ratio") +
      theme_minimal()
  })
  
  # Risk-Return Profile
  output$riskReturnPlot <- renderPlot({
    ggplot(risk_return_df, aes(x = Annual_Risk, y = Annual_Return, label = Fund)) +
      geom_point(color = "orange", size = 4) +
      geom_text(vjust = -1, size = 4) +
      labs(title = "Risk-Return Profile of Mutual Funds",
           x = "Annualized Risk (Standard Deviation)",
           y = "Annualized Return") +
      theme_minimal()
  })
  
  # Portfolio Allocation
  output$portfolioPieChart <- renderPlot({
    portfolio_df <- portfolio_df %>%
      arrange(desc(Fund)) %>%
      mutate(
        ypos = cumsum(Allocation) - 0.5 * Allocation,
        label = paste0(Fund, ": ", round(Allocation), "%")
      )
    
    ggplot(portfolio_df, aes(x = "", y = Allocation, fill = Fund)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(y = ypos, label = label), color = "black", size = 4) +
      labs(title = "Portfolio Allocation (₹1,00,000)", x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Set3")
  })
  
  # Correlation Matrix
  output$correlationMatrixPlot <- renderPlot({
    cor_matrix <- cor(returns_xts_clean)
    corrplot(
      cor_matrix,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      col = colorRampPalette(c("red", "white", "blue"))(200),
      title = "Correlation Matrix of Mutual Fund Returns",
      mar = c(0, 0, 2, 0)
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)




