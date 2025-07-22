# 📊 Mutual Fund Analysis

This project presents a comprehensive analysis of selected mutual funds with the objective of constructing a well-diversified investment portfolio. The analysis simulates the role of a **financial data analyst** who evaluates mutual fund performance and advises on fund allocation based on return, risk, and diversification insights.

---

## 🔍 Objective

- Analyze the performance of various mutual funds using historical NAV data.
- Measure risk and return using standard financial metrics.
- Construct a ₹1,00,000 hypothetical portfolio with optimal weights.
- Visualize the findings using an interactive Shiny dashboard.

---

## 🗃️ Data Source

- **Association of Mutual Funds in India (AMFI)**  
  Website: [https://www.amfiindia.com](https://www.amfiindia.com)  
  Data collected: Net Asset Values (NAVs) of selected mutual funds over a defined time period.

---

## 🛠️ Tools & Technologies Used

- **R** (v4.4.1)
- **RStudio IDE**
- **Shiny** (for interactive dashboard)
- **Packages**:
  - `tidyverse`, `lubridate` – Data wrangling and transformation
  - `xts`, `PerformanceAnalytics` – Time series and risk-return analysis
  - `ggplot2` – Data visualization
  - `reshape2`, `corrplot` – Correlation and plotting
  - `shiny`, `shinythemes` – Dashboard development

---
## 📁 Repository Structure

| File/Folder                         | Description                                                  |
|------------------------------------|--------------------------------------------------------------|
| mutual_fund_analysis_updated.R     | Main R source code for data analysis and dashboard           |
| /data/                              | Folder containing raw CSV files (mutual fund NAVs)           |
| /visualisations/                           | PNG files for bar charts, lollipop charts, heatmaps, etc.    |
| /dashboardpreview/                              | PDF file compiling all dashboard screenshots                 |
| README.md                           | Project documentation file                                   |


---

## 📈 Visualizations & Dashboard Components

The dashboard is designed with multiple tabs and a light gradient theme. It includes:

1. **Cumulative Returns Line Chart**
2. **Efficient Frontier (Risk vs Return curve)**
3. **Beta Bar Chart**
4. **Standard Deviation Bar Chart**
5. **Sharpe Ratio Lollipop Chart**
6. **Risk-Return Scatter Plot**
7. **Portfolio Allocation (Pie Chart)**
8. **Correlation Matrix Heatmap**

> 📌 All visuals are saved under the `/visualisations` folder and also compiled in the `/dashboardpreview.pdf` file.
### 📊 Standard Deviation Bar Chart
![Standard Deviation Bar Chart](visualistions/Bar%Chart&of%Standard%Deviation.png)

### 🍭 Sharpe Ratio Lollipop Chart

![Sharpe Ratio Lollipop Chart](visualistions/Sharpe%20Ratio%20Lollipop%20Chart.png)


---

## 📊 Portfolio Construction

- A hypothetical investment of **₹1,00,000** was allocated across selected funds based on Sharpe ratios and correlation analysis.
- Key portfolio metrics calculated:
  - **Expected Return**
  - **Portfolio Standard Deviation**
  - **Portfolio Sharpe Ratio**

---

## 📌 Insights & Takeaways

- Funds with high Sharpe ratios and lower correlation with others were prioritized.
- Beta analysis helped assess volatility relative to the market.
- The efficient frontier visualized trade-offs between return and risk, guiding better fund selection.

---

## 🚀 How to Run

- Open `mutual_fund_analysis_updated.R` in RStudio.
- Ensure all required libraries are installed (`install.packages(...)` if not).
- Run the script and launch the Shiny dashboard using:

 -  ```R
 - shinyApp(ui = ui, server = server)

---

## 📬 Author
**Ashmita Chatterjee**  
- *MSc Economics (Data Analytics), Symbiosis School of Economics*  
- 📧 [ashmita6404@gmail.com](mailto:ashmita6404@gmail.com)

---

## ⭐ Acknowledgements

- [AMFI India](https://www.amfiindia.com/) for providing reliable mutual fund data.
- The open-source R community for the extensive tools and packages that made this analysis and dashboard possible.

