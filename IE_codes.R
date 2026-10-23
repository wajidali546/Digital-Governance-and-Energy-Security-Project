#Load Libraries for Quantile on Quantile 
library(quantreg)
library(dplyr)
library (haven)
library(tidyverse)
library(stats)
library(plm)
library(pastecs) # used descriptives
library(readxl)


#EGI and NRD Overall QQR


#Declare data as panel data
df <-  read_excel("~/Downloads/IE_Codes_data/IE_updated_22aug.xlsx")
df.pd <- pdata.frame(df, index = c("country", "year"), drop.index = FALSE)
df1.pd <- df.pd %>% dplyr::select(nrdf, egi, nrd, lnatcf, eilope, aoi)
View(df1.pd) 


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
            file = "~/Downloads/QQR_with_pvalues_nrd_20dec2025.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)







# EGI and ATCF (Access to clean fuels and technologies for cooking (% of population) [EG.CFT.ACCS.ZS])

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
  x <- Iprq(df.pd$lnatcf, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_pvalues_lnatcf_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)




# Energy intensity level of primary energy (MJ/$2017 PPP GDP) [EG.EGY.PRIM.PP.KD] (EILOPE) and EGI 

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
            file = "~/Downloads/QQR_with_pvalues_eilope_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)




# Agriculture orientation index and EGI


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
  x <- Iprq(df.pd$aoi, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_pvalues_aoi_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)





# Developing Economies QQR

IE_data <- read_excel("~/Downloads/IE_data.xlsx", 
                      sheet = "DevEco")
df <- read_excel("~/Downloads/IE_data.xlsx", 
                 sheet = "DevEco")
df.pd <- pdata.frame(df, index = c("country", "year"), drop.index = FALSE)
df1.pd <- df.pd %>% dplyr::select(nrdf, egi, nrd, lnatcf, eilope, aoi)
View(df1.pd) 


#Natural resource depletion and EGI

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
  x <- Iprq(df.pd$nrdf, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_dev_pvalues_nrdf_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)




#ACTF and EGI

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
  x <- Iprq(df.pd$lnatcf, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_dev_pvalues_lnatcf_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)



#EILOPE and EGI


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








# Agriculture orientation index


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
  x <- Iprq(df.pd$aoi, df$egi, 0.1, tau = taus[i])
  
  # Store coefficients and p-values
  QQR.coef[, 2 * i - 1] <- x$dv
  QQR.coef[, 2 * i] <- x$pval
}

# Write QQR.coef to a CSV file in Mac Downloads
write.table(QQR.coef, 
            file = "~/Downloads/QQR_with_dev_pvalues_aoi_22aug.csv", 
            sep = ",", 
            col.names = NA, 
            quote = FALSE)



#Overall Data 

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



# Set up a 2x3 grid layout
par(mfrow = c(2, 3))

# Define darker olive green color
dark_olive_green <- "darkolivegreen"

# Create QQ plots for each variable against a theoretical normal distribution
qqnorm(df$nrdf, main = "QQ Plot for NRD", col = dark_olive_green)
qqline(df$nrdf, col = "red", lwd = 2)  # Set line width to 2

qqnorm(df$lnatcf, main = "QQ Plot for ACFT", col = dark_olive_green)
qqline(df$lnatcf, col = "red", lwd = 2)  # Set line width to 2

qqnorm(df$aoi, main = "QQ Plot for AOI", col = dark_olive_green)
qqline(df$aoi, col = "red", lwd = 2)  # Set line width to 2

qqnorm(df$eilope, main = "QQ Plot for EI", col = dark_olive_green)
qqline(df$eilope, col = "red", lwd = 2)  # Set line width to 2

qqnorm(df$egi, main = "QQ Plot for EGI", col = dark_olive_green)
qqline(df$egi, col = "red", lwd = 2)  # Set line wid






# Load necessary packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Convert pdata.frame to regular data.frame
df_plot <- as.data.frame(df1.pd)

# Ensure numeric
vars <- c("nrdf", "lnatcf", "eilope", "aoi", "egi")
df_plot[vars] <- lapply(df_plot[vars], function(x) as.numeric(x))

# Remove rows with NA in any of these columns
df_plot <- df_plot %>% drop_na(nrdf, lnatcf, eilope, aoi, egi)

# Reshape to long format and recode variable names
df_long <- df_plot %>%
  pivot_longer(cols = c("nrdf", "lnatcf", "eilope", "aoi"),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           nrdf = "Natural Resource Depletion",
                           lnatcf = "Access to Clean Fuel",
                           eilope = "Energy Intensity",
                           aoi = "Agriculture Orientation Index")) %>%
  # Apply y-axis limits only for specific variables
  mutate(Value = case_when(
    Variable == "Agriculture Orientation Index" ~ pmin(pmax(Value, 0), 2),
    Variable == "Natural Resource Depletion" ~ pmin(pmax(Value, 0), 25),
    TRUE ~ Value
  ))

# Create 4-panel Lowess plot
ggplot(df_long, aes(x = egi, y = Value)) +
  geom_point(color = "blue", alpha = 0.6, size = 2) +  # All points blue
  geom_smooth(method = "loess", color = "red", se = FALSE, span = 0.8) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(x = "Digital Governance (EGI)",
       y = NULL,
       title = "Relationship of EGI with NRDF, Lnatcf, Eilope, and AOI",
       subtitle = "Lowess Smoother Trends") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

