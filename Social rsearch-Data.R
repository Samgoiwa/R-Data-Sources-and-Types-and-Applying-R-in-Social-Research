#--------------------------------------------------------
# STEP 1: Load Required Libraries
#--------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(ggpubr)
library(caret)
library(broom)
library(flextable)
library(officer)
library(corrplot)
library(ggcorrplot)

#--------------------------------------------------------
# STEP 2: Load and Clean the Dataset
#--------------------------------------------------------

# Load the dataset (replace path as needed)
survey_data <- read_csv("C:/Users/user/Desktop/Submission/PAU Evaluation/SICSS-2025/social_survey_data.csv") %>%
  clean_names()

#--------------------------------------------------------
# STEP 3: Explore and Summarize Data
#--------------------------------------------------------

glimpse(survey_data)
summary(survey_data)

# Filter for youth (age 18–35) and unemployed
youth_unemployed <- survey_data %>%
  filter(age >= 18, age <= 35, employment_status == "Unemployed")

# Create income categories
survey_data <- survey_data %>%
  mutate(income_level = case_when(
    income < 15000 ~ "Low",
    income >= 15000 & income < 45000 ~ "Middle",
    income >= 45000 ~ "High"
  ))

# Grouped summary of average trust by education
trust_summary <- survey_data %>%
  group_by(education) %>%
  summarise(avg_trust = mean(trust_in_government, na.rm = TRUE))

print(trust_summary)

#--------------------------------------------------------
# STEP 4: Visualization
#--------------------------------------------------------

# 4A. Trust in Government by Education
ggplot(trust_summary, aes(x = reorder(education, avg_trust), y = avg_trust, fill = education)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Average Trust in Government by Education Level",
    x = "Education Level", y = "Average Trust (1-5)"
  ) +
  theme_minimal()

# 4B. Civic Participation by Gender
ggplot(survey_data, aes(x = gender, fill = civic_participation)) +
  geom_bar(position = "fill") +
  labs(title = "Civic Participation by Gender", y = "Proportion", x = "Gender") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()

# 4C. Income by Employment Status
ggplot(survey_data, aes(x = employment_status, y = income, fill = employment_status)) +
  geom_boxplot() +
  labs(
    title = "Income Distribution by Employment Status",
    x = "Employment Status", y = "Monthly Income"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

# 4D. Trust by Region and Civic Participation
ggplot(survey_data, aes(x = region, y = trust_in_government, fill = civic_participation)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(
    title = "Trust in Government by Region and Civic Participation",
    x = "Region", y = "Average Trust Score"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "#66c2a5", "No" = "#fc8d62")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#--------------------------------------------------------
# STEP 5: Modeling - Predict Trust in Government
#--------------------------------------------------------

# Convert relevant variables to factors
survey_data <- survey_data %>%
  mutate(across(c(gender, education, employment_status, region, civic_participation), as.factor))

# Train/test split
set.seed(123)
train_index <- createDataPartition(survey_data$trust_in_government, p = 0.8, list = FALSE)
train_data <- survey_data[train_index, ]
test_data <- survey_data[-train_index, ]

# Linear regression model
model <- lm(trust_in_government ~ age + income + gender + education + employment_status + civic_participation,
            data = train_data)

# Model summary
summary(model)

# Tidy model output
tidy(model) %>% print()

#--------------------------------------------------------
# STEP 6: Summary Tables
#--------------------------------------------------------

# Regional summary
table_summary <- survey_data %>%
  group_by(region) %>%
  summarise(
    Avg_Age = round(mean(age), 1),
    Avg_Income = round(mean(income), 1),
    Trust_Score = round(mean(trust_in_government), 2)
  )

# Education-level summary
summary_table <- survey_data %>%
  group_by(education) %>%
  summarise(
    avg_income = round(mean(income, na.rm = TRUE), 1),
    avg_trust = round(mean(trust_in_government, na.rm = TRUE), 2),
    respondent_count = n()
  )

print(summary_table)

#--------------------------------------------------------
# STEP 7: Correlation Analysis and Visualization
#--------------------------------------------------------

# Select numeric columns
numeric_data <- survey_data %>%
  select(age, income, trust_in_government)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)

# 7A. corrplot visualization
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix: Social Survey (Numerical)", mar = c(0,0,1,0))

# 7B. ggcorrplot visualization
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           method = "square", 
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Matrix of Key Variables",
           ggtheme = theme_minimal())

#--------------------------------------------------------
# STEP 8: Export Summary Table to Word
#--------------------------------------------------------

doc <- read_docx() %>%
  body_add_par("Regional Summary of Respondents", style = "heading 1") %>%
  body_add_flextable(flextable(table_summary))

print(doc, target = "Regional_Summary_Report.docx")
