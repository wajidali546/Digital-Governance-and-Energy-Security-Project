# Digital Governance and Energy Security: A Quantile-on-Quantile Regression Approach

## Overview
This project uses **data analytics and Quantile-on-Quantile Regression (QQR)** to examine the relationship between digital governance and energy-related outcomes across countries. The analysis explores how these relationships vary across different parts of the data distribution, rather than relying only on average effects.

## Analytics & Methodology
- **Data Processing:** Panel data preparation using country and year identifiers.
- **Exploratory Data Analysis:** Descriptive statistics, scatter plots, Q-Q plots, and LOWESS trend analysis.
- **Quantile-on-Quantile Regression:** Estimation of heterogeneous relationships across quantiles.
- **Statistical Inference:** Estimation of coefficients and p-values.
- **Visual Analytics:** Multi-panel plots to explore patterns between digital governance and energy-related indicators.
- **Subgroup Analysis:** Separate analyses for the overall sample and developing economies.

## Key Variables
- **Digital Governance:** E-Government Index (EGI).
- **Natural Resource Depletion:** NRDF.
- **Access to Clean Fuels and Technologies:** Lnatcf.
- **Energy Intensity:** EILOPE.
- **Agricultural Orientation:** AOI.

## Tools & Technologies
**R | Quantitative Analytics | Panel Data | Quantile Regression | Data Visualisation**

Libraries: `quantreg`, `plm`, `dplyr`, `tidyverse`, `readxl`, `ggplot2`, `gridExtra`, and `pastecs`.

## Project Objectives
- Examine how digital governance relates to energy and resource-related outcomes.
- Identify heterogeneous relationships across different quantiles.
- Compare patterns in the overall sample and developing economies.
- Translate statistical results into insights for digital governance and energy policy.

## Outputs
- QQR coefficient and p-value tables exported to CSV.
- Scatter plots and LOWESS trend visualisations.
- Q-Q plots for distributional assessment.
- Comparative visualisations of digital governance and energy-related indicators.

## How to Run
1. Install R and the required packages.
2. Load the relevant country-year panel dataset.
3. Update the input and output file paths in the R script.
4. Run the analysis and examine the generated CSV files and plots.

**Note:** The dataset is not included in this repository. Results depend on the input data and model specifications.

## Research Contribution
This project demonstrates how distribution-sensitive econometric analytics can reveal relationships that average-based models may overlook, supporting a more nuanced understanding of digital governance, energy access, resource use, and sustainability.
