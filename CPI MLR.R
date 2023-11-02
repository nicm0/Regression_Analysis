# MLR for CPI regressed on commodities prices, loans, money supply and unemployment rate
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dgof")

library(ggplot2)
library(tidyverse)
library(stats)
library(dgof)

# Plots of each of the variables
plot(Inflation_dataset$`CPI`)
plot(Inflation_dataset$`Commodities index`)
plot(Inflation_dataset$`Commercial and industrial loans`)
plot(Inflation_dataset$`M2 money supply`)
plot(Inflation_dataset$`Unemployment rate`)

# Histograms of the change in each of the variables - shows the changes are approx. normal
hist(Inflation_dataset_changes$`CPI_change`)
hist(Inflation_dataset_changes$`Commodities_change`)
hist(Inflation_dataset_changes$`Loans_change`)
hist(Inflation_dataset_changes$`Money_supply_change`)
hist(Inflation_dataset_changes$`Unemployment_rate_change`)

# Scatter plots of each of the variables and CPI
Inflation_dataframe <- data.frame(Inflation_dataset_changes)

# CPI and commodities scatter plot
Inflation_commodities_df <- Inflation_dataframe %>% select(CPI_change, Commodities_change)
CPI_commodities_scatter_plot <- ggplot(data = Inflation_commodities_df, aes(x = Commodities_change, y = CPI_change)) + geom_point()
plot(CPI_commodities_scatter_plot)

# CPI and loans scatter plot
Inflation_loans_df <- Inflation_dataframe %>% select(CPI_change, Loans_change)
CPI_loans_scatter_plot <- ggplot(data = Inflation_loans_df, aes(x = Loans_change, y = CPI_change)) + geom_point()
plot(CPI_loans_scatter_plot)

# CPI and money supply plot
Inflation_MS_df <- Inflation_dataframe %>% select(CPI_change, Money_supply_change)
CPI_MS_scatter_plot <- ggplot(data = Inflation_MS_df, aes(x = Money_supply_change, y = CPI_change)) + geom_point()
plot(CPI_MS_scatter_plot)

# CPI and unemployment rate plot
Inflation_Unemployment_df <- Inflation_dataframe %>% select(CPI_change, Unemployment_rate_change)
CPI_Unemployment_scatter_plot <- ggplot(data = Inflation_Unemployment_df, aes(x = Unemployment_rate_change, y = CPI_change)) + geom_point()
plot(CPI_Unemployment_scatter_plot)

# MLR model and plot
CPI_MLR <- lm(CPI_change~Commodities_change + Commodities_change + Loans_change + Money_supply_change + Unemployment_rate_change, data = Inflation_dataframe)
summary(CPI_MLR)
plot(CPI_MLR)

# Regression equation
Inflation_coeff <- coefficients(CPI_MLR)
print(Inflation_coeff)
Inflation_eq <- paste0("y = ", Inflation_coeff[2], "x1 + ", Inflation_coeff[3], "x2 +", Inflation_coeff[4], "x3 + ", Inflation_coeff[5], "x4 +", Inflation_coeff[1])
print(Inflation_eq)
