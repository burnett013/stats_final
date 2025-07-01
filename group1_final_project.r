

##  Kansas City Housing – full analysis ##
# ==============================================

# ---- 1. Packages ----
packages <- c(
  "readxl",   # read Excel
  "dplyr", "tidyr",          # data wrangling
  "ggplot2", "ggpubr",       # plotting
  "stringr",                 # string helpers
  "broom",                   # tidy model outputs
  "car",                     # VIF
  "sandwich", "lmtest",      # robust SEs
  "multcomp", "multcompView" # Tukey HSD tools
)

# This line loads each package in packages, installing it first if it's missing, and hides the output
invisible(lapply(packages, function(pkg){
  if(!require(pkg, character.only = TRUE)) install.packages(pkg, quiet = TRUE)
  library(pkg, character.only = TRUE)
}))


# ---- 2. Load & clean raw data ----
file_path <- "Kansas City Housing Raw Data.xlsx"
df        <- read_excel(file_path)

# replace explicit zeros with NA
df[df == 0] <- NA

# Removes the listed columns from df and drops any rows with missing values, saving as df_clean.
drop_cols <- c("id", "date", "waterfront", "view", "yr_renovated")
df_clean  <- dplyr::select(df, -all_of(drop_cols)) |> drop_na()

# Converts the condition, grade, and zipcode columns in df_clean into categorical variables (factors).
df_clean <- df_clean |>
  mutate(across(c(condition, grade, zipcode), as.factor))

# ---- 3. Primary analysis set ----
# This code sets a random seed for reproducibility, filters rows where yr_built > 1950 and berooms is between 2 and 4, then randomly samples 500 rows from df_clean into df_primary.
set.seed(50685)
df_primary <- df_clean |>
  filter(yr_built > 1950,
         between(bedrooms %in% c(2, 3, 4)) |>
  slice_sample(n = 500)

# These lines display the structure of df_primary, the minimum year built, and the unique bathroom counts in the filtered dataset.
str(df_primary)
min(df_primary$yr_built)
unique(df_primary$bathrooms)

# ---- 4. Confidence interval & t-test ----
# This code calculates the 95% confidence interval for price and prints it as a formatted dollar range.
ci95 <- t.test(df_primary$price, conf.level = 0.95)$conf.int
cat(sprintf("95%% CI for price: $%0.0f – $%0.0f\n", ci95[1], ci95[2]))

# This code performs a one-tailed t-test to check if the mean price is greater than $650,000 and prints the t-statistic and p-value.
tt  <- t.test(df_primary$price, mu = 650000, alternative = "greater")
cat(sprintf("t = %.2f, one-tailed p = %.4f\n", tt$statistic, tt$p.value))

# ---- 5. Full multiple-regression model ----
# This code fits a multiple linear regression model predicting price using the listed variables from df_primary and displays a summary of the results.
full_mod <- lm(
  price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
    condition + grade + sqft_above + sqft_basement + yr_built +
    zipcode + lat + long,
  data = df_primary
)
# This line displays detailed results of the linear regression model full_mod, including coefficients, p-values, R-squared, and residual statistics.
summary(full_mod)

# ---- 6. Reduced model via backward elimination ----
# This code performs backward stepwise regression on full_mod to remove non-significant variables and displays the summary of the simplified model.
reduced_mod <- step(full_mod, direction = "backward", trace = 0)
summary(reduced_mod)

# ---- 7. Robust (HC1) SEs ----
# This code calculates and prints robust standard errors for the reduced_mod regression model using heteroskedasticity-consistent (HC1) covariance.
robust_se  <- coeftest(reduced_mod, vcov. = vcovHC(reduced_mod, type = "HC1"))
print(robust_se)

# ---- 8. Assumption checks ----
## This code creates a residuals vs. fitted values plot to visually check linearity and homoscedasticity in the regression model.
ggplot(reduced_mod, aes(.fitted, .resid)) +
  geom_point(alpha = .6) +
  geom_smooth(se = FALSE, colour = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()

## This code creates a Q-Q plot of the residuals from reduced_mod to assess whether they are normally distributed.
ggqqplot(residuals(reduced_mod), title = "Q-Q Plot")

# ---- 9. VIF (multicollinearity) ----
# This code calculates the Variance Inflation Factor (VIF) for each predictor in the reduced_mod to assess multicollinearity.
vif_vals <- vif(reduced_mod)

# This code creates a data frame listing each predictor (feature) and its corresponding VIF value to evaluate multicollinearity.
vif_df <- data.frame(
  feature = rownames(as.data.frame(vif_vals)),
  VIF = as.numeric(vif_vals)
)
# Prints the results from above In descending order of VIF
print(vif_df |> arrange(desc(VIF)))

df_clean <- df_clean |>
  mutate(
    condition = factor(condition),
    grade = factor(grade, levels = sort(unique(df$grade))),  # use full set of grades
    zipcode = factor(zipcode, levels = sort(unique(df$zipcode)))  # full zip levels
  )


# ---- 10. Prediction for Abhinav’s property ----
# This code creates a one-row tibble representing a new house with specified characteristics, matching factor levels from df_primary, for use in prediction.
new_house <- tibble(
  bedrooms       = 4,
  bathrooms      = 3,
  sqft_living    = 3600,
  sqft_lot       = 250000,
  floors         = 2,
  condition      = factor(4, levels(df_primary$condition)),
  grade          = factor(5, levels(df_primary$grade)),
  sqft_above     = 2600,
  sqft_basement  = 1000,
  yr_built       = 1986,
  zipcode        = factor(98133, levels(df_primary$zipcode)),
  lat            = 47.3754,
  long           = -122.353
)

# This code predicts the house price and 95% prediction interval using reduced_mod for new_house, then prints the results in a formatted sentence.
pred_price <- predict(reduced_mod, newdata = new_house, interval = "prediction")
cat(sprintf("Predicted price: $%0.2f (95%% PI: %0.2f – %0.2f)\n",
            pred_price[1], pred_price[2], pred_price[3]))

# ---- 11. One-way ANOVAs ----
# This code performs ANOVA on price by bedrooms, tests for equal variances, shows the ANOVA summary, and plots Tukey's HSD for group comparisons
anova_bed <- aov(price ~ factor(bedrooms), data = df_primary)
leveneTest(price ~ factor(bedrooms), data = df_primary)
summary(anova_bed)
plot(TukeyHSD(anova_bed), las = 1)

# This code performs ANOVA on price by floors, tests for equal variances,
# displays the ANOVA summary, and plots Tukey’s HSD results.
anova_floor <- aov(price ~ factor(floors), data = df_primary)
leveneTest(price ~ factor(floors), data = df_primary)
summary(anova_floor)
plot(TukeyHSD(anova_floor), las = 1)

# This code performs ANOVA on price by condition, tests for equal variances, shows the summary, and plots Tukey’s HSD to compare condition levels.
anova_cond <- aov(price ~ condition, data = df_primary)
leveneTest(price ~ condition, data = df_primary)
summary(anova_cond)
plot(TukeyHSD(anova_cond), las = 1)

# ======================
## End of analysis ##