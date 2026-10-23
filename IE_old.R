#Load Libraries for Quantile on Quantile 
library(quantreg)
library(dplyr)
library (haven)
library(tidyverse)
library(stats)
library(plm)
library(pastecs) # used descriptives
library(readxl)

IE_data <- read_excel("~/Downloads/IE_data.xlsx", 
                      sheet = "DevEco")
df <- read_excel("~/Downloads/IE_data.xlsx", 
                 sheet = "DevEco")
df.pd <- pdata.frame(df, index = c("country", "year"), drop.index = FALSE)
df1.pd <- df.pd %>% dplyr::select(nrdf, egi, nrd, lnatcf, eilope, aoi)
View(df1.pd) 

#Declare data as panel data
df <- read_excel("Downloads/IE_Codes_data/IE_data.xlsx")
df.pd <- pdata.frame(df, index = c("country", "year"), drop.index = FALSE)
df1.pd <- df.pd %>% dplyr::select(nrdf, egi, nrd, lnatcf, eilope, aoi)
View(df1.pd) 
#Descriptive Statistics
stat.desc(df1.pd, norm = TRUE, p = 0.95 )
IQR (df1.pd$rene, na.rm = TRUE)
IQR (df1.pd$serv, na.rm = TRUE)
#Scatter Plot
library(car)
# Load the required library
library(car)
# Assuming df is your dataframe with columns nrd and egi
scatterplot(nrdf ~ egi, data = df1.pd,ellipse = FALSE, grid = FALSE, reg = TRUE,regLine = c(col = 1),
            smooth = FALSE,  xlab = "E-Government(egi)",ylab = "NRD)  ",main = "Scatter Plot of ACFT vs. egi")



library(PerformanceAnalytics)
df1.pd <- df1.pd %>% dplyr:: select (nrdf, egi)
chart.Correlation(df1.pd, histogram = TRUE, pch = 19, title = "Correlation Matrix")


#Split the variables into 5 Quantiles
df.quant <- df1.pd %>% mutate(quintile = ntile (nrdf, 5))

#split data into 5 quantile subset
dfq1 <- df.quant %>% filter (quintile == 1)  %>% dplyr::select(-quintile)
dfq2 <- df.quant %>% filter (quintile == 2)  %>% dplyr::select(-quintile)
dfq3 <- df.quant %>% filter (quintile == 3)  %>% dplyr::select(-quintile)
dfq4 <- df.quant %>% filter (quintile == 4)  %>% dplyr::select(-quintile)
dfq5 <- df.quant %>% filter (quintile == 5)  %>% dplyr::select(-quintile)

#Correlation for each
cor1 <- cor (dfq1)
cor2 <- cor (dfq2)
cor3 <- cor (dfq3)
cor4 <- cor (dfq4)
cor5 <- cor (dfq5)

# Combine correlation values and quantile numbers into a data frame
cor_data <- data.frame (
  Quantile = 1:5,
  Correlation = c(cor1[1, 2], cor2[1, 2], cor3[1, 2], cor4[1, 2], cor5[1, 2])
)

# Create a scatter plot (This shows there is a need of Quantile on Quantile regresison, Data is not normal and Correlaiton is not homogeneous)
ggplot (cor_data, aes (x = Quantile, y = Correlation)) +
  geom_text (aes (label = round (Correlation, 2)), just = -0.5) +
  labs (x = "Quantile Number"
        , y = "Correlation Value",
        title = "Correlation vs Quantile Number Scatter Plot")

#Quantile regression
Tors<-df1.pd %>% select(nrd)
un<-df1.pd %>% select(egi)
Tors <- df1.pd %>% dplyr::select(nrd)
un <- df1.pd %>% dplyr::select(egi)

Tors <- df1.pd$nrd

# Apply QR and get the coefficents
taus <- c(seq(from = 0.10, to = 0.95, by = 0.10))


#Run Quantile regression
QR1.coef <- coef (rq(df1.pd$nrd~df1.pd$egi, tau=taus))
QR1.coef
summary (QR1.coef)
QR1 <- rq(df1.pd$nrd~df1.pd$egi, tau=taus)
summary (QR1)

QR1.coef <- matrix(NA, nrow = length(taus), ncol = 2) # Initialize matrix for coefficients and p-values

# Cornrdt code Initialize QR1.coef matrix
n <- length(taus)
p <- 2  # Assuming there are two coefficients (intercept and slope)
QR1.coef <- matrix(NA, nrow = n, ncol = p + 1)



dim(QR1.coef)
colnames(QR1.coef) <- paste0("Quantile_", 1:3)
# Check the number of columns in QR1.coef
ncol_QR1.coef <- ncol(QR1.coef)

library(quantreg)

taus <- seq(0.10, 0.95, by = 0.10)

QR1 <- rq(nrd ~ egi, tau = taus, data = df1.pd)
QR1.coef <- coef(QR1)
summary (QR1)



# Add column names
colnames(QR1.coef) <- c("Coefficient", "P-value", "Some_other_column_name")

for (i in 1:length(taus)) {
  tau <- taus[i]
  QR1 <- rq(df.pd$nrdf ~ df.pd$egi, tau = tau)
  QR1.coef[i, ] <- c(QR1$coefficients[2], summary(QR1)$coefficients[2, "Pr(>|t|)"], NA) # Store coefficient and p-value
}
colnames(QR1.coef) <- c("Coefficient", "P-value", "Some_other_column_name")


#Save coefficient estimates and p-values
write.table(QR1.coef, file = "QR_with_pvalues.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#Load the data with coefficient estimates and p-values
QR_data <- read.csv("QR_with_pvalues.csv")


# Load necessary libraries
library(quantreg)

# Define custom function Iprq
Iprq <- function(x, y, h, m = 9, tau = 0.1) {
  xx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = m)
  fv <- numeric(length(xx))
  dv <- numeric(length(xx))
  pv <- numeric(length(xx))
  
  for (i in 1:length(xx)) {
    z <- x - xx[i]
    WX <- abs(z/h)
    r <- rq(y ~ z, weights = WX, tau = tau, ci = FALSE)
    fv[i] <- r$coef[1]
    dv[i] <- r$coef[2]
    pv[i] <- summary(r)$coefficients[2, "Pr(>|t|)"]
  }
  
  data.frame(dv = dv, pval = pv)
}

# Assuming taus are defined and df.pd and df are your data frames
taus <- seq(0.1, 0.9, by = 0.1)  # Define taus for quantiles

# Initialize QQR.coef matrix (9 quantiles, 2 columns for dv and pval for each quantile)
QQR.coef <- as.data.frame(matrix(0, ncol = 18, nrow = 9))  # 18 columns: 2 per quantile (dv and pval)

# Run QOR in a loop and save estimates in the matrix "QQR.coef"
for (i in 1:9) {
  # Assuming df.pd$nrd and df$egi are your data columns
  x <- Iprq(df.pd$nrd, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_pvalues_nrd_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)





# Load necessary libraries
library(quantreg)

# Define custom function Iprq
Iprq <- function(x, y, h, m = 9, tau = 0.1) {
  xx <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = m)
  fv <- numeric(length(xx))
  dv <- numeric(length(xx))
  pv <- numeric(length(xx))
  
  for (i in 1:length(xx)) {
    z <- x - xx[i]
    WX <- abs(z/h)
    r <- rq(y ~ z, weights = WX, tau = tau, ci = FALSE)
    fv[i] <- r$coef[1]
    dv[i] <- r$coef[2]
    pv[i] <- summary(r)$coefficients[2, "Pr(>|t|)"]
  }
  
  data.frame(dv = dv, pval = pv)
}

# Assuming taus are defined and df.pd and df are your data frames
taus <- seq(0.1, 0.9, by = 0.1)  # Define taus for quantiles

# Initialize QQR.coef matrix (9 quantiles, 2 columns for dv and pval for each quantile)
QQR.coef <- as.data.frame(matrix(0, ncol = 18, nrow = 9))  # 18 columns: 2 per quantile (dv and pval)

# Run QOR in a loop and save estimates in the matrix "QQR.coef"
for (i in 1:9) {
  # Assuming df.pd$nrd and df$egi are your data columns
  x <- Iprq(df.pd$eilope, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_dev_pvalues_eilope_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)






# Load necessary packages
library(ggplot2)
library(gridExtra)

# Scatter plot for Natural Resource Depletion (nrdf) vs. E-Government Index (egi)
plot1 <- ggplot(IE_updated_22aug, aes(x = egi, y = nrdf)) +
  geom_point(color = "blue", shape = 1) +  # Shape 1 represents a hollow circle
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "E-Government Index (EGI)",
       y = "Natural Resource Depletion (NRD)",
       title = "NRD vs. EGI") +
  xlim(0, 0.9) +
  ylim(0, 25)  # Set y-axis limits from 0 to 25

# Scatter plot for Access to clean fuel technologies (lnatcf) vs. E-Government Index (egi)
plot2 <- ggplot(IE_updated_22aug, aes(x = egi, y = lnatcf)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "E-Government Index (EGI)",
       y = "Access to Clean Fuels and Technologies (ACFT)",
       title = "ACFT vs. EGI") +
  xlim(0, 0.9) +
  ylim(0, 2)

# Scatter plot for Energy Intensity (eilope) vs. E-Government Index (egi)
plot3 <- ggplot(IE_updated_22aug, aes(x = egi, y = eilope)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "E-Government Index (EGI)",
       y = "Energy Intensity (EI)",
       title = "EI vs. EGI") +
  xlim(0, 0.9) +
  ylim(0, 11)

# Scatter plot for Agricultural Orientation Index (aoi) vs. E-Government Index (egi)
plot4 <- ggplot(IE_updated_22aug, aes(x = egi, y = aoi)) +
  geom_point(color = "blue", shape = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "E-Government Index (EGI)",
       y = "Agricultural Orientation Index (AOI)",
       title = "AOI vs. EGI") +
  xlim(0, 0.9) +
  ylim(0, 2)

# Combine plots into a single image
final_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
final_plot

sum(is.na(df1.pd$nrd))


# Load necessary packages
library(KRLS)

# Fit the KRLS model
krls_model <- krls(X = df1.pd$egi, y = df1.pd$nrdf)

# Summary of the model
summary(krls_model)

# Predict using the KRLS model
predictions <- predict(krls_model, newdata = df1.pd$egi)

# Plot the actual vs predicted values
plot(df1.pd$egi, df1.pd$nrd,
     main = "Actual vs Predicted Values",
     xlab = "E-Government Index (EGI)",
     ylab = "Natural Resource Depletion (NRD)",
     pch = 19, col = "blue")

points(df1.pd$egi, predictions$fit, pch = 19, col = "red")

legend("topright", legend = c("Actual", "Predicted"),
       col = c("blue", "red"), pch = 19)


# -----------------------
# Pointwise marginal effect plot
# -----------------------

# Load the necessary packages
library(margins)
library(ggplot2)

# Fit a linear model (EGI → AOI, as in your original example)
model <- lm(nrdf ~ egi, data = IE_updated_22aug)

# Calculate the marginal effects
marginal_effects <- margins(model)

# Extract marginal effects for egi
marginal_effects_df <- as.data.frame(marginal_effects)
marginal_effects_df$egi <- IE_updated_22aug$egi

# Plot the pointwise marginal effects
ggplot(marginal_effects_df, aes(x = egi, y = dydx_egi)) +
  geom_point(color = "blue") +  
  geom_smooth(method = "loess", color = "red", se = FALSE) +  
  labs(x = "E-Government Index (EGI)", 
       y = "Pointwise Marginal Effect of EGI on AOI", 
       title = "Pointwise Marginal Effects of EGI on AOI") +
  theme_minimal()


# -----------------------
# Lowess smoother plot (NRDF vs. EGI)
# -----------------------

ggplot(IE_updated_22aug, aes(x = egi, y = nrd)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(x = "E-Government Index (EGI)", 
       y = "Natural Resource Depletion (NRDF)", 
       title = "Lowess Smoother for NRDF vs. EGI") +
  ylim(0, 40) +
  theme_minimal()


df1.pd_clean <- na.omit(df1.pd)


library(ggplot2)
library(gridExtra)

# Increase font size for all plots
title_size <- 14

# Create the scatter plots
plot1 <- ggplot(df1.pd, aes(x = NRD, y = PMENRD)) +
  geom_point(color = "blue") +  # Scatter plot points
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.8) +  # Lowess smoother with bandwidth
  labs(x = "Natural Resource Depletion", 
       y = "Pointwise marginal effect of EGI", 
       title = "Lowess smoother with Bandwidth =.8") +
  xlim(0, 44) +  # Limit x-axis from 0 to 25
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(plot.title = element_text(size = title_size))
print(plot1)


plot2 <- ggplot(df, aes(x = EI, y = PMEEI)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.8) +
  labs(x = "Energy Intensity", 
       y = "Pointwise marginal effect of EGI", 
       title = "Lowess smoother with Bandwidth =.8") +
  xlim(0, 25) +
  ylim(-8, -1) +
  theme_minimal() +
  theme(plot.title = element_text(size = title_size))

plot3 <- ggplot(df, aes(x = AOI, y = PMEAOI)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.8) +
  labs(x = "Agriculture Orientation", 
       y = "Pointwise marginal effect of EGI", 
       title = "Lowess smoother with Bandwidth =.8") +
  xlim(0, 3) +
  ylim(-1, 1.9) +
  theme_minimal() +
  theme(plot.title = element_text(size = title_size))

plot4 <- ggplot(df, aes(x = ATCF, y = PMEATCF)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.8) +
  labs(x = "Access to clean fuel technology for cooking", 
       y = "Pointwise marginal effect of EGI", 
       title = "Lowess smoother with Bandwidth =.8") +
  xlim(0, 2) +
  theme_minimal() +
  theme(plot.title = element_text(size = title_size))

# Arrange the plots into a single image
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

