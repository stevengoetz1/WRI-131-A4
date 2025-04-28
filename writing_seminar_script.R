# Load necessary libraries
library(tidyverse)
library(DescTools)

# Load data
my_data <- read.csv("/Users/stevengoetz/Desktop/Princeton Academics/Freshman Spring/WRI 132/A4/industry_merged_data.csv")

df1 <- my_data %>%
  filter(usfirm == 1) %>%
  filter(actual_value >= -10, actual_value <= 10) %>%
  filter(!is.na(standardized_unexpected_earnings), !is.na(announcement_date)) 

df2 <- my_data %>%
  filter(usfirm == 1) 
table(my_data$usfirm)
table(df2$usfirm)

summary(df2$standardized_unexpected_earnings)
str(df2)
df3 <- df2 %>%
  filter(actual_value >= -10, actual_value <= 10) 
str(df3)

df4 <- df3 %>%
  filter(!is.na(standardized_unexpected_earnings), !is.na(announcement_date))
str(df4)

str(df1)

# Winsorize the data to reduce outliers
df1$standardized_unexpected_earnings <- Winsorize(df1$standardized_unexpected_earnings)

# Ensure industry is a factor
df1$ff12_industry <- as.factor(df1$ff12_industry)

## Now for the merge + cleaning (again)

# Renaming the columns to sync up
library(dplyr)

df4 <- df4 %>%
  rename(year = `year.1`)

# How to merge
og1 <- df4
str(og1)

og2 = read.csv("/Users/stevengoetz/Desktop/Princeton Academics/Freshman Spring/WRI 132/A4/total_asset_good.csv",
               stringsAsFactors = FALSE)
str(og2)

# Doing the merge
please_work <- merge(og1, og2, by=c("historical_permo", "year"), all=TRUE)
str(please_work)

# Filtering
please_work <- please_work %>%
  filter(!is.na(disaggcomp), !is.na(at))

# Winsorizing
please_work$at <- Winsorize(please_work$at)
please_work$standardized_unexpected_earnings <- Winsorize(please_work$standardized_unexpected_earnings)

# When disaggcomp == 1
summary( subset(please_work, disaggcomp == 1)$at)

# When disaggcomp == 0
summary( subset(please_work, disaggcomp == 0)$at)

# Set industry variable to be a factor/categorical variable
please_work$ff12_industry <- factor(please_work$ff12_industry)

# Visualize with a boxplot for all firms
table(df1$disaggcomp)
summary(df1$standardized_unexpected_earnings)
tapply(df1$standardized_unexpected_earnings, df1$disaggcomp, summary)
boxplot(standardized_unexpected_earnings ~ disaggcomp, data=df1)

## Boxplot and violin plot

# Abbreviated ff12_industry codes to full names
industry_full_names <- c(
  "BusEq" = "Business Equipment",
  "Chems" = "Chemicals",
  "Durbl" = "Consumer Durables",
  "Enrgy" = "Energy",
  "Hlth"  = "Healthcare",
  "Manuf" = "Manufacturing",
  "Money" = "Finance",
  "NoDur" = "Consumer Non-Durables",
  "Other" = "Other",
  "Shops" = "Retail (Shops)",
  "Telcm" = "Telecommunications",
  "Utils" = "Utilities"
)

# Plot w/ revised labels
ggplot(please_work, aes(x = as.factor(disaggcomp), y = standardized_unexpected_earnings)) +
  geom_boxplot() +
  facet_wrap(~ ff12_industry, scales = "free_y", labeller = labeller(ff12_industry = industry_full_names)) +
  theme_minimal() +
  labs(
    x = "Disaggregated Compensation",
    y = "Standardized Unexpected Earnings",
    title = "Standardized Unexpected Earnings by Disaggregated Compensation Across Industries"
  ) +
  theme(
    strip.text = element_text(size = 8, face = "bold"),  # facet label formatting
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  )

# Doing the same for the violin plot
ggplot(please_work, 
       aes(x = as.factor(disaggcomp), 
           y = standardized_unexpected_earnings)) +
  geom_violin(trim = FALSE, fill = "lightblue") +
  geom_boxplot(width = 0.1, outlier.shape = NA) +  # suppress outliers from boxplot to not double-count
  facet_wrap(~ ff12_industry, scales = "free_y", labeller = labeller(ff12_industry = industry_full_names)) +
  theme_minimal() +
  labs(
    x = "Disaggregated Compensation",
    y = "Standardized Unexpected Earnings",
    title = "Distribution of Standardized Unexpected Earnings by Disaggregated Compensation Across Industries"
  ) +
 
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  )

## Conducting OLS Estimations

# Linear regression - all companies (not industry-specific)
model_all_companies <- lm(standardized_unexpected_earnings  ~ disaggcomp, data = please_work)

summary(model_all_companies)

# Linear regression - industries (industry-specific)
model_industry_interaction <- lm(standardized_unexpected_earnings  ~ disaggcomp * ff12_industry, data = please_work)

summary(model_industry_interaction )

# The rest of the code used the Stargazer package in the console to export all of the regression results 
