# == Final Project Submission for Group Three // Kansas City Housing Full Analysis == 

# ---- Packages ----
packages <- c(
  "readxl",                   # read Excel
  "dplyr", "tidyr",           # data wrangling
  "ggplot2", "ggpubr",        # plotting
  "stringr",                  # string helpers
  "broom",                    # tidy model outputs
  "car",                      # VIF
  "sandwich", "lmtest",       # robust SEs
  "multcomp", "multcompView", # Tukey HSD tools
  "here"                      # for file paths
)

# === Install & load packages === 
invisible(lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}))

# === Load & clean raw data ===
file_path <- here("Kansas City Housing Raw Data.xlsx")
# file_path <- "Kansas City Housing Raw Data.xlsx"

# Safety check
if (!file.exists(file_path)) {
  stop("ERROR: 'Kansas City Housing Raw Data.xlsx' not found.
       Make sure it is in the SAME folder as this R script or the RStudio project.")
}

df <- read_excel(file_path)

# ---------------------------------
# Count of zero values in each column
zero_counts <- colSums(df == 0, na.rm = TRUE)
print(zero_counts)

# ---------------------------------
# Total number of missing values
total_nas <- sum(is.na(df))
cat("Total missing values:", total_nas, "\n")

# Per column missing values
missing_by_col <- colSums(is.na(df))
print(missing_by_col)
# ---------------------------------

# Replace explicit zeros with NA
df[df == 0] <- NA

# Recheck: Total NA values after replacing zeros
total_nas_after <- sum(is.na(df))
cat("\nTotal missing values AFTER replacing zeros:", total_nas_after, "\n")

# Per column missing values AFTER replacement
missing_by_col_after <- colSums(is.na(df))
cat("\nMissing values per column AFTER replacing zeros:\n")
print(missing_by_col_after)
# ---------------------------------

# Remove listed columns
drop_cols <- c("id", "date", "waterfront", "view", "yr_renovated")

# Step 1: Check missing values before dropping rows
df_temp <- dplyr::select(df, -all_of(drop_cols))
cat("\nMissing values per column AFTER dropping selected columns (before drop_na):\n")

print(colSums(is.na(df_temp)))
cat("Total missing values AFTER dropping selected columns:", sum(is.na(df_temp)), "\n")
# ---------------------------------

# Step 2: Drop rows with any NA values
df_clean <- df_temp |> drop_na()

# Step 3: Verify that df_clean has no missing values
cat("\nAfter drop_na(), total missing values in df_clean:", sum(is.na(df_clean)), "\n")

# Count the number of rows in df_clean
num_rows_clean <- nrow(df_clean)
cat("Number of rows in df_clean:", num_rows_clean, "\n")

# ---------------------------------

# Converts the condition, grade, and zipcode columns in df_clean into categorical variables (factors).
df_clean <- df_clean |>
  mutate(across(c(condition, grade, zipcode), as.factor))

# ---- Primary analysis set ----
# This code sets a random seed for reproducibility, filters rows where yr_built > 1950 and berooms is between 2 and 4, then randomly samples 500 rows from df_clean into df_primary. # "": line_length_linter.
set.seed(50685)

df_primary <- df_clean |>
    filter(yr_built > 1950, bedrooms %in% c(2, 3, 4), grade %in% c(5, 6, 7, 8, 9, 10)) |> 
  # ------- Convert condition, grade, and zipcode to factors -------
  mutate(
    condition = factor(condition, levels = sort(unique(condition))),
    grade     = factor(grade,     levels = sort(unique(grade))),
    zipcode   = factor(zipcode,   levels = sort(unique(zipcode)))
  )

# Display the structure of df_primary, the minimum year built, and the unique bathroom counts in the filtered dataset. 
str(df_primary)
min(df_primary$yr_built)
unique(df_primary$bathrooms)

# Count rows in df_clean that meet the filtering conditions
rows_meeting_conditions <- df_clean |>
  filter(
    yr_built > 1950,            # Year built after 1950
    bedrooms %in% c(2, 3, 4),   # Bedrooms: 2 to 4 (incl.)
    grade %in% 6:10             # Grades between 6 and 10 (incl.)
  ) |> 
  nrow()

cat("Number of rows in df_clean that meet the conditions:", rows_meeting_conditions, "\n")

# === Confidence interval & t-test ===
# This code calculates the 95% confidence interval for price and prints it as a formatted dollar range. 
ci95 <- t.test(df_primary$price, conf.level = 0.95)$conf.int
cat(sprintf("95%% CI for price: $%0.0f – $%0.0f\n", ci95[1], ci95[2]))

# This code performs a one-tailed t-test to check if the mean price is greater than $650,000 and prints the t-statistic and p-value. 
tt  <- t.test(df_primary$price, mu = 650000, alternative = "greater")
cat(sprintf("t = %.2f, one-tailed p = %.4f\n", tt$statistic, tt$p.value))

# === Regression ===

# --- Full multiple-regression model ---
# Fits a multiple linear regression model predicting price using the listed variables from df_primary and displays a summary of the results 
full_mod <- lm(
  price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors +
    condition + grade + sqft_above + sqft_basement + yr_built +
    zipcode + lat + long,
  data = df_primary
)
# Displays detailed results of the linear regression model full_mod, including coefficients, p-values, R-squared, and residual statistics 
summary(full_mod)

# --- Reduced model via backward elimination ---
# Performs backward stepwise regression on full_mod to remove non-significant variables and displays the summary of the simplified model 
reduced_mod <- step(full_mod, direction = "backward", trace = 0)
summary(reduced_mod)

# --- Robust (HC1) SEs ---
# Calculates and prints robust standard errors for the reduced_mod regression model using heteroskedasticity-consistent (HC1) covariance 
robust_se  <- coeftest(reduced_mod, vcov. = vcovHC(reduced_mod, type = "HC1"))
print(robust_se)

# --- Assumption checks for LINE ---
## Creates a residuals vs. fitted values plot to visually check linearity and homoscedasticity in the regression model 
ggplot(reduced_mod, aes(.fitted, .resid)) +
  geom_point(alpha = .6, colour="azure4") +
  geom_smooth(se = FALSE, colour = "red") +
  labs(title = "V. Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()

## Creates a Q-Q plot of the residuals from reduced_mod to assess whether they are normally distributed 
ggqqplot(residuals(reduced_mod), title = "Q-Q Plot")

# === Multicollinearity ===
# calculates the Variance Inflation Factor (VIF) for each predictor in the reduced_mod to assess multicollinearity 
vif_vals <- vif(reduced_mod)

# Creates a data frame listing each predictor (feature) and its corresponding VIF value to evaluate multicollinearity 
vif_df <- data.frame(
  feature = rownames(as.data.frame(vif_vals)),
  VIF = as.numeric(vif_vals)
)
# Prints the results from above In descending order of VIF
print(vif_df |> arrange(desc(VIF)))

df_clean <- df_clean |>
  mutate(
    condition = factor(condition, levels = sort(unique(condition))),
    grade = factor(grade, levels = sort(unique(grade))),       # ensure all grade levels are preserved 
    zipcode = factor(zipcode, levels = sort(unique(zipcode)))    # ensure all zipcodes are preserved 
  )



# === Prediction for Abhinav’s property ===
# This code creates a one-row tibble representing a new house with specified characteristics, matching factor levels from df_primary, for use in prediction. 
# First, check what grades actually exist in the data
table(df_primary$grade)

# Then use one of the existing grades, e.g., grade 6:
new_house <- tibble(
  bedrooms       = 4,
  bathrooms      = 3,
  sqft_living    = 3600,
  sqft_lot       = 250000,
  floors         = 2,
  condition      = factor(4, levels(df_primary$condition)),
  grade          = factor(6, levels(df_primary$grade)),  # Changed from 5 to 6
  sqft_above     = 2600,
  sqft_basement  = 1000,
  yr_built       = 1986,
  zipcode        = factor(98133, levels(df_primary$zipcode)),
  lat            = 47.3754,
  long           = -122.353
)

# Predicts the house price and 95% prediction interval using reduced_mod for new_house, then prints the results in a formatted sentence 
pred_price <- predict(reduced_mod, newdata = new_house, interval = "prediction")
cat(sprintf("Predicted price: $%0.2f (95%% PI: %0.2f – %0.2f)\n",
            pred_price[1], pred_price[2], pred_price[3]))

# === One-way ANOVAs ===
# This code performs ANOVA on price by BEDROOMS, tests for equal variances, shows the ANOVA summary, and plots Tukey's HSD for group comparisons 
anova_bed <- aov(price ~ factor(bedrooms), data = df_primary)
leveneTest(price ~ factor(bedrooms), data = df_primary)
summary(anova_bed)
plot(TukeyHSD(anova_bed), las = 1)

# This code performs ANOVA on price by FLOORS, tests for equal variances,
# Displays the ANOVA summary, and plots Tukey’s HSD results.
anova_floor <- aov(price ~ factor(floors), data = df_primary)
leveneTest(price ~ factor(floors), data = df_primary)
summary(anova_floor)
plot(TukeyHSD(anova_floor), las = 1)

# This code performs ANOVA on price by CONDITION, tests for equal variances, shows the summary, and plots Tukey’s HSD to compare condition levels. 
anova_cond <- aov(price ~ condition, data = df_primary)
leveneTest(price ~ condition, data = df_primary)
summary(anova_cond)
plot(TukeyHSD(anova_cond), las = 1)

# ||==== End of analysis ====||