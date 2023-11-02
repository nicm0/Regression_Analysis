# MLR for GDP growth regressed on 10 year interest rate, yield curve, unemployment rate, oil price and loan growth
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dgof")

library(ggplot2)
library(tidyverse)
library(stats)
library(dgof)

# Plots of each of the variables
plot(GDP_dataset$`Real GDP`)
plot(GDP_dataset$`10 year real interest rate`)
plot(GDP_dataset$`2y10y`)
plot(GDP_dataset$`Unemployment rate`)
plot(GDP_dataset$`Oil price`)
plot(GDP_dataset$`Commercial and industrial loans`)

# Histogram of changes in each of the variables - shows they are approx. normal
hist(GDP_growth_dataset$GDP_growth)
hist(GDP_growth_dataset$Interest_rate_change)
hist(GDP_growth_dataset$YC_change)
hist(GDP_growth_dataset$Unemployment_rate_change)
hist(GDP_growth_dataset$Oil_price_change)
hist(GDP_growth_dataset$Loans_change)

# Scatter plot of each of the variables and GDP growth
GDP_growth_df <- data.frame(GDP_growth_dataset)

# GDP growth and interest rate change scatter plot
GDP_growth_IR_df <- GDP_growth_df %>% select(GDP_growth, Interest_rate_change)
GDP_growth_IR_scatter_plot <- ggplot(data = GDP_growth_IR_df, aes(x = Interest_rate_change, y = GDP_growth)) + geom_point()
plot(GDP_growth_IR_scatter_plot)

# GDP growth and YC change scatter plot
GDP_growth_YC_df <- GDP_growth_df %>% select(GDP_growth, YC_change)
GDP_growth_YC_scatter_plot <- ggplot(data = GDP_growth_YC_df, aes(x = YC_change, y = GDP_growth)) + geom_point()
plot(GDP_growth_YC_scatter_plot)

# GDP growth and Unemployment rate change scatter plot
GDP_growth_Unemployment_df <- GDP_growth_df %>% select(GDP_growth, Unemployment_rate_change)
GDP_growth_Unemployment_scatter_plot <- ggplot(data = GDP_growth_Unemployment_df, aes(x = Unemployment_rate_change, y = GDP_growth)) + geom_point()
plot(GDP_growth_Unemployment_scatter_plot)

# GDP growth and Oil price change
GDP_growth_Oil_df <- GDP_growth_df %>% select(GDP_growth, Oil_price_change)
GDP_growth_Oil_scatter_plot <- ggplot(data = GDP_growth_Oil_df, aes(x = Oil_price_change, y = GDP_growth)) + geom_point()
plot(GDP_growth_Oil_scatter_plot)

# GDP growth and loan growth
GDP_growth_Loans_df <- GDP_growth_df %>% select(GDP_growth, Loans_change)
GDP_growth_Loans_scatter_plot <- ggplot(data = GDP_growth_Loans_df, aes(x = Loans_change, y = GDP_growth)) + geom_point()
plot(GDP_growth_Loans_scatter_plot)

# MLR model
GDP_growth_MLR <- lm(GDP_growth~Interest_rate_change + YC_change + Unemployment_rate_change + Oil_price_change + Loans_change, data = GDP_growth_df)
summary(GDP_growth_MLR)
plot(GDP_growth_MLR)

# MLR equation
GDP_coeff <- coefficients(GDP_growth_MLR)
print(GDP_coeff)
GDP_growth_eq <- paste0("y = ", GDP_coeff[2], "x1 + ", GDP_coeff[3], "x2 +", GDP_coeff[4], "x3 + ", GDP_coeff[5], "x4 +", GDP_coeff[1])
print(GDP_growth_eq)
