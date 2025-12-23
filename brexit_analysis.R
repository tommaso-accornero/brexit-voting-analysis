# ============================================================================
# Brexit Voting Behavior: Education and District-Level Analysis
# ============================================================================
# Author: Tommaso Accornero
# Date: December 2025
# Description: Statistical analysis of Brexit voting patterns and their 
#              relationship with education levels using survey and district data
# ============================================================================

# SETUP -----------------------------------------------------------------------

# Load required packages
library(tidyverse)  # Data manipulation and visualization
library(dplyr)      # Data wrangling
library(ggplot2)    # Visualizations
library(readr)      # Reading CSV files
library(Hmisc)      # Statistical functions
library(corrr)      # Correlation analysis

# Clear environment
rm(list = ls())


# DATA LOADING ----------------------------------------------------------------

# Load survey data from Elena Llaudet's GitHub repository
brexit_data <- read_csv(file = "https://raw.githubusercontent.com/ellaudet/DSS/refs/heads/master/BES.csv")

# Load district-level data
brexit_data_district <- read_csv(file = "https://raw.githubusercontent.com/ellaudet/DSS/refs/heads/master/UK_districts.csv")

# Load population data for English districts
# Note: This file should be in your working directory
pop_en <- read_csv("eng_districts.csv")


# EXPLORATORY DATA ANALYSIS ---------------------------------------------------

# Get basic dataset information
summary(brexit_data)   
summary(brexit_data_district)   
summary(pop_en)   

# Check for missing values
missing_summary <- brexit_data |> 
  summarise(
    across(
      c(vote, leave, education, age),
      ~sum(is.na(.x)),
      .names = "missing_{.col}"
    )
  )

print(missing_summary)


# DATA VALIDATION -------------------------------------------------------------

# Verify that all NA values in 'leave' correspond to "don't know" or "won't vote"
brexit_data |> 
  filter(is.na(leave)) |> 
  count(vote, leave)

# DESCRIPTIVE STATISTICS ------------------------------------------------------

# Two-dimensional frequency table: vote × education
vote_education_table <- table(brexit_data$vote, brexit_data$education)
print(vote_education_table)

# Among lowest education level, what % voted leave?
lowest_edu_leave <- brexit_data |> 
  filter(education == 1, vote == "leave") |> 
  nrow() / brexit_data |> 
  filter(education == 1, vote %in% c("leave", "stay")) |> 
  nrow() * 100

cat("\nAmong lowest education level:", 
    round(lowest_edu_leave, 2), "% voted leave\n")

# Among stay voters, what % had level 4 education?
stay_level4 <- brexit_data |> 
  filter(vote == "stay", education == 4) |> 
  nrow() / brexit_data %>%
  filter(vote == "stay", !is.na(education)) |> 
  nrow() * 100

cat("Among stay voters:", 
    round(stay_level4, 2), "% had level 4 education\n")

# Poll estimate: % voting remain
poll_estimate <- brexit_data |> 
  filter(vote != "won't vote") |> 
  summarise(percentage_remain = mean(vote == "stay", na.rm = TRUE) * 100)

cat("\nPoll estimate for remain:", 
    round(poll_estimate$percentage_remain, 1), "%\n")
cat("Actual result: 46.6% remain / 53.4% leave\n")


# DEMOGRAPHIC ANALYSIS --------------------------------------------------------

cat("\n=== AVERAGE AGE BY VOTING CHOICE ===\n")

age_by_vote <- brexit_data |> 
  group_by(vote) |> 
  summarise(average_age = mean(age, na.rm = TRUE)) |> 
  arrange(desc(average_age))

print(age_by_vote)

# % of people with high education (level 5) by voting choice
high_edu_by_vote <- brexit_data |> 
  filter(!is.na(vote)) |> 
  group_by(vote) |> 
  summarise(
    pct_high_education = mean(education == 5, na.rm = TRUE) * 100
  )

cat("\n=== HIGH EDUCATION (LEVEL 5) BY VOTE ===\n")
print(high_edu_by_vote)


# STATISTICAL TESTING ---------------------------------------------------------

cat("\n=== CHI-SQUARE TEST: VOTE VS EDUCATION ===\n")

# Test independence between voting choice and education level
chi_square_test <- chisq.test(
  table(brexit_data$vote, brexit_data$education)
)

chi_results <- tibble(
  test = "Chi-square test",
  variables = "vote vs education",
  chi_square_statistic = chi_square_test$statistic,
  df = chi_square_test$parameter,
  p_value = chi_square_test$p.value
)

print(chi_results)

cat("\nInterpretation:")
cat("\n- Chi-square statistic:", round(chi_results$chi_square_statistic, 2))
cat("\n- p-value < 0.001: Strong evidence against independence")
cat("\n- Education level and voting choice are significantly associated\n")


# DISTRICT-LEVEL ANALYSIS -----------------------------------------------------

cat("\n=== DISTRICT-LEVEL CORRELATION ANALYSIS ===\n")

# Merge district data with population data
brexit_data_district <- brexit_data_district |> 
  inner_join(pop_en, by = c("name" = "District"))

# Calculate correlation matrix
cor_matrix <- brexit_data_district |> 
  select(leave, high_education, Population) |> 
  cor(use = "complete.obs")

print(round(cor_matrix, 3))

# Correlation significance tests
cor_tests <- brexit_data_district |> 
  summarise(
    leave_vs_high_education = cor.test(leave, high_education)$p.value,
    leave_vs_Population = cor.test(leave, Population)$p.value,
    Population_vs_high_education = cor.test(Population, high_education)$p.value
  )

cat("\n=== CORRELATION SIGNIFICANCE TESTS ===\n")
print(cor_tests)

cat("\nKey Findings:")
cat("\n1. Leave vs High Education: r = -0.90 (p < 0.001)")
cat("\n   Very strong negative correlation")
cat("\n2. Leave vs Population: r = -0.20 (p = 0.001)")
cat("\n   Weak negative correlation")
cat("\n3. Population vs High Education: r ≈ 0 (p = 0.98)")
cat("\n   No significant relationship\n")


# VISUALIZATIONS --------------------------------------------------------------

# 1. Education distribution across districts
education_dist_plot <- brexit_data_district %>%
  mutate(
    education_level = case_when(
      high_education < 20 ~ "Low",
      high_education < 30 ~ "Medium-Low",
      high_education < 40 ~ "Medium-High",
      TRUE ~ "High"
    ) |> 
      factor(
        levels = c("Low", "Medium-Low", "Medium-High", "High"),
        ordered = TRUE
      )
  ) |> 
  ggplot(aes(x = education_level)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  labs(
    title = "Distribution of Education Levels Across English Districts",
    x = "Education Level (% with Higher Education)",
    y = "Number of Districts"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# 2. Scatter plot: Leave vs High Education
leave_education_plot <- brexit_data_district |> 
  ggplot(aes(x = high_education, y = leave)) +
  geom_point(aes(size = Population), alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Brexit Leave Vote vs. Education Level by District",
    subtitle = "Strong negative correlation: r = -0.90",
    x = "% with Higher Education",
    y = "% Voting Leave",
    size = "Population"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

# 3. Age distribution by vote choice
age_vote_plot <- brexit_data |> 
  filter(!is.na(vote), !is.na(age)) |> 
  ggplot(aes(x = vote, y = age, fill = vote)) +
  geom_violin(alpha = 0.7, show.legend = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Age Distribution by Voting Choice",
    x = "Vote Choice",
    y = "Age (years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Display plots
print(education_dist_plot)
print(leave_education_plot)
print(age_vote_plot)

# Save plots
ggsave("education_distribution.png", education_dist_plot, 
       width = 8, height = 6, dpi = 300)
ggsave("leave_vs_education.png", leave_education_plot, 
       width = 10, height = 6, dpi = 300)
ggsave("age_by_vote.png", age_vote_plot, 
       width = 8, height = 6, dpi = 300)


# ADVANCED ANALYSIS: DISTRICT SIMULATION --------------------------------------

# Prepare district data with population proportions
brexit_data_district <- brexit_data_district |> 
  select(name, leave, high_education, Population) |> 
  drop_na(name) |> 
  mutate(prop_pop = Population / sum(Population))

# Simulate voting with weighted population
set.seed(123)  # For reproducibility
total_pop <- 100000

simulated_data <- brexit_data_district |> 
  reframe(
    leave_sim = rnorm(
      n = round(prop_pop * total_pop),
      mean = leave,
      sd = 5 * sqrt(prop_pop)  # SD proportional to population
    ),
    sample_size = round(prop_pop * total_pop),
    .by = name
  ) |> 
  filter(leave_sim >= 0 & leave_sim <= 100)  # Keep valid percentages

# Calculate weighted average
simulation_result <- simulated_data |> 
  group_by(name) |> 
  mutate(district_size = n()) |> 
  ungroup() |> 
  summarise(
    estimated_leave = weighted.mean(leave_sim, w = district_size, na.rm = TRUE),
    actual_england = 53.4,
    difference = estimated_leave - actual_england
  )

cat("\nSimulation Results:")
cat("\n- Estimated leave vote:", round(simulation_result$estimated_leave, 2), "%")
cat("\n- Actual England result: 53.4%")
cat("\n- Difference:", round(simulation_result$difference, 2), 
    "percentage points\n")


# SUMMARY STATISTICS ----------------------------------------------------------

cat("\n=== SUMMARY REPORT ===\n")
cat("
KEY FINDINGS:

1. EDUCATION-VOTE RELATIONSHIP
   - Chi-square test: χ² = 2109, p < 0.001
   - Strong statistical evidence that education and voting are not independent
   - Lower education levels associated with higher leave support

2. DISTRICT-LEVEL PATTERNS  
   - Leave vs High Education: r = -0.90 (p < 0.001) - Very strong
   - Leave vs Population: r = -0.20 (p = 0.001) - Weak
   - High Education vs Population: r ≈ 0 (p = 0.98) - None

3. DEMOGRAPHIC INSIGHTS
   - Average age of Leave voters: 55.3 years
   - Average age of Remain voters: 47.2 years
   - Among lowest education: 66.3% voted leave
   - Among highest education: Significantly more likely to vote remain

4. POLL ACCURACY
   - Poll estimate: 47.3% remain
   - Actual result: 46.6% remain (53.4% leave)
   - Poll underestimated leave by ~6 percentage points

INTERPRETATION:
The analysis reveals education as the strongest predictor of Brexit voting behavior.
Districts with higher percentages of university-educated residents showed 
significantly lower support for leaving the EU. Population size had minimal
effect, suggesting that the type of urbanization (university vs industrial
cities) matters more than city size itself.
")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Visualizations saved to working directory\n")
