library(tidyverse)
library(naniar)
library(skimr)
library(knitr)

# Potential Questions
# How does overall health (as measured by BMI, BP, weight, height) 
# and ultimately length of stay in the hospital vary across different 
# demographic groups (marital status, ethnicity, insurance)?
# What is the distribution of BP, weight, height, and body mass index (BMI) in the dataset?

# Read in admissions
admissions <- read_csv("data/admissions.csv")
admissions

# Read in omr
omr <- read_csv("data/omr.csv")
omr

# Missing variable summary for admissions
# Death time has 98.2 percent missing. Likely because not all admissions are 
# pronounced dead. Speaks to overall outcome
# Equal missingness (40.5%) for ED registration/exit times because not every patient
# is admitted to the ER. Same value is a positive sign that there probably aren't 
# observations that have missing ed_reg time but have an ed_exit time and vice versa
# Discharge location has 24.2% missing
# Marital status has 12.6% missing
# Admission location has 11.5% missing
miss_var_summary(admissions)

admissions |> 
  distinct(subject_id)
# subject_id is not a unique variable for each observation
# running this gives 257,356 rows. When admissions is 524,520 rows
# assuming this is from readmissions

# Missing variable summary for omr
# None missing
miss_var_summary(omr)

# Expand result_name to get the different vitals as different variables
# Use separate wider delim to get BP systolic and diastolic measurements as 2 
# different variables
omr_wider <- omr |> 
  pivot_wider(
    names_from = result_name,
    values_from = result_value
  ) |> 
  separate_wider_delim(
    `Blood Pressure`,
    delim = "/",
    names = c("Systolic Blood Pressure", "Diastolic Blood Pressure"))
omr_wider

# Run missing variable summary
miss_var_summary(omr_wider)

# Notice that there is very high percentage of missingness for variables Height,
# eGFR, Weight, and Blood Pressures with different modalities
# Very few patients were assigned values for these variables and potentially, 
# for weight and height, there was a data entry issue since the more detailed 
# description of those 2 variables is also in the dataset and has relatively 
# less percentage missing
# Therefore, the decision is made to remove the Height, eGFR, Weight, and BMI
# variables

# Many different columns for blood pressure since the position of the patient
# matters in certain conditions to get a more accurate reading or in order to have
# more specific understanding of the measurement relative to a baseline which is
# different for standing vs sitting vs lying and is also dependent on time
# For the purposes of the project we will remove all the extra BP measurements
# and only use the BP systolic and BP diastolic

# Also will remove sequence number. Even though it has valuable information
# An analysis on changes in vital signs for each patients is too complex and would
# require much more time and more thorough analysis
# In short, every patient has a different number of observations all with different sequences and
# many missing variables as well which complicates this even further.
# Great for future direction though 

omr_wider <- omr_wider |>
  select(subject_id, chartdate, `Diastolic Blood Pressure`, 
         `Systolic Blood Pressure`, `Weight (Lbs)`, `Height (Inches)`, `BMI (kg/m2)`)
omr_wider

# Goal is to have one value for each of the separate variables for each patient,
# in order to achieve this the different observations for each subject needs to
# be averaged out

# First step is to change the vital sign measurements from characters to numerical values
# Be advised there is some limitation to using mean. The mean may not be entirely
# representative of the actual medical episodes the patient experiences. For example a
# a patient could be stable and have normal vitals (BP, weight, height, BMI) for 
# an extended period of time but then they could undergo a time period where they 
# become unstable and have abnormal vitals. In this test case, if the mean of the
# vitals is used as the form of measurement, this variable may not be representative
# of the overall experiences that the patient faces
# Also the mean may not be representative beacause the number of data points for
# each patient varies and the number of total observations may not be enough to
# create a value which is resistant to extreme outliers or the opposite where if the data 
# points for each patient is to great and the value may not be representative of
# the patients experiences/overall health especially in cases where there is instantaneous
# trauma or short episodes of instability.

# However, that being said, the usage of mean is advantageous in the context of
# chronic diseases or overall health state since patients who are chronically ill
# or chronically sick typcially have lower vitals.

omr_wider

# Removed all NA values because the dataset became too large. Difficulty loading
# first mutate the vitals to be numerical variables and then group by patient
# then use summarise to get the mean value for each patient
omr_patient <- omr_wider |> 
  na.omit() |> 
  mutate(systolic_bp = as.numeric(`Systolic Blood Pressure`),
         diastolic_bp = as.numeric(`Diastolic Blood Pressure`),
         weight_lbs = as.numeric(`Weight (Lbs)`),
         height_inches = as.numeric(`Height (Inches)`),
         bmi_kgm2 = as.numeric(`BMI (kg/m2)`)
  ) |> 
  group_by(subject_id) |> 
  summarize(systolic_bp = mean(systolic_bp),
            diastolic_bp = mean(diastolic_bp),
            weight_lbs = mean(weight_lbs),
            height_inches = mean(height_inches),
            bmi_kgm2 = mean(bmi_kgm2)
  )
omr_patient

# check to make sure that subject ID is an adequate primary key
omr_patient |> 
  count(subject_id) |> 
  filter(n > 1)

# multiple subject ID used for observations
admissions |> 
  count(subject_id) |> 
  filter(n > 1)

# decision to do right join
# use relationship = "many-to-many" so that the multiple observations for each
# patient all have the same average values for the vitals
# select relevant variables
admissions_omr <- admissions |> 
  right_join(omr_patient, join_by(subject_id), relationship = "many-to-many") |> 
  select(subject_id, admittime, dischtime, deathtime, admission_type, insurance,
         language, marital_status, ethnicity, systolic_bp, diastolic_bp, weight_lbs,
         height_inches, bmi_kgm2)
admissions_omr

miss_var_summary(admissions_omr)

# Univariate analysis of systolic blood pressure
# unimodal, no skew, center around 120, general range between 50-200
ua_systolic_plot <- admissions_omr |>  ggplot(mapping = aes(x = systolic_bp)) +
  geom_histogram(color = "white", fill = "royalblue4") +
  labs(title = "The Distribution of Systolic Blood Pressure",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Systolic Blood Pressure")
ggsave(filename = "ua-systolic-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ua_systolic_plot)

# unimodal, no skew, center around 80, general range between 30-130
ua_diastolic_plot <- admissions_omr |>  ggplot(mapping = aes(x = diastolic_bp)) +
  geom_histogram(color = "white", fill = "royalblue4") +
  labs(title = "The Distribution of Diastolic Blood Pressure",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Diastolic Blood Pressure")
ggsave(filename = "ua-diastolic-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ua_diastolic_plot)

# abnormal observations for weight possibly due to data entry errors
admissions_omr |> 
  select(subject_id, weight_lbs) |> 
  arrange(desc(weight_lbs))

# take out weight that is greater 500
admissions_omr <- admissions_omr |> 
  filter(weight_lbs < 500)

# unimodal, slight right skew, center around 180, general range between 60-400
ua_weight_plot <- admissions_omr |>  ggplot(mapping = aes(x = weight_lbs)) +
  geom_histogram(color = "white", fill = "royalblue4") +
  labs(title = "The Distribution of Weight",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Weight (lbs)")
ggsave(filename = "ua-weight-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ua_weight_plot)

# unimodal, no skew, center around 65, general range between 50-80
ua_height_plot <- admissions_omr |>  ggplot(mapping = aes(x = height_inches)) +
  geom_histogram(color = "white", fill = "royalblue4") +
  labs(title = "The Distribution of Height",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Height (in)")
ggsave(filename = "ua-height-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ua_height_plot)

# abnormal observations for bmi possibly due to data entry errors
admissions_omr |> 
  select(subject_id, bmi_kgm2) |> 
  arrange(desc(bmi_kgm2))

# take out bmi that is greater 400
admissions_omr <- admissions_omr |> 
  filter(bmi_kgm2 < 400)

# unimodal, slight right skew, center around 30, general range between 10-75
ua_bmi_plot <- admissions_omr |>  ggplot(mapping = aes(x = bmi_kgm2)) +
  geom_histogram(color = "white", fill = "royalblue4") +
  labs(title = "The Distribution of Body Mass Index",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Body Mass Index (kg/m^2)")
ggsave(filename = "ua-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ua_bmi_plot)

# Correlation table with numeric values to see if any variables have strong correlation
# Overall pretty low correlation values except what obvious relations. 
# No significant findings.
# Diastolic blood pressure and systolic blood pressure correlation value of 0.432
# Weight and bmi have correlation value of 0.877. expected since weight is used to
# calculate bmi and it has a heavy weight there
# Weight and Height have correlation value of 0.441 

admissions_omr |> 
  select(where(is.numeric)) |> 
  cor() |> 
  round(digits = 3)

# bivariate graph for systolic_bp and diastolic_bp
ba_bp_plot <- admissions_omr |> ggplot(mapping = aes(x = systolic_bp, y = diastolic_bp)) +
  geom_point(alpha = 0.1, color = "royalblue4") +
  geom_smooth(se = FALSE, method = "lm", color = "white") +
  labs(title = "The Relationship Between Systolic Blood Pressure and Diastolic Blood Pressure",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Systolic Blood Pressure",
       y = "Diastolic Blood Pressure")
ggsave(filename = "ba-bp-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_bp_plot)

# bivariate graph for weight and height
ba_weight_height_plot <- admissions_omr |> ggplot(mapping = aes(x = weight_lbs, y = height_inches)) +
  geom_point(alpha = 0.1, color = "royalblue4") +
  geom_smooth(se = FALSE, method = "lm", color = "white") +
  labs(title = "The Relationship Between Weight and Height",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Weight (lbs)",
       y = "Height (in)")
ggsave(filename = "ba-weight-height-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_weight_height_plot)

# bivariate graph for weight and bmi
ba_weight_bmi_plot <- admissions_omr |> ggplot(mapping = aes(x = weight_lbs, y = bmi_kgm2)) +
  geom_point(alpha = 0.1, color = "royalblue4") +
  geom_smooth(se = FALSE, method = "lm", color = "white") +
  labs(title = "The Relationship Between Weight and Body Mass Index",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Weight (lbs)",
       y = "Body Mass Index (kg/m^2)")
ggsave(filename = "ba-weight-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_weight_bmi_plot)

# use group_by and skim without charts to get distribution metrics for
# cateogrical and numerical bivarate

# insurance and bmi 
admissions_omr |> 
  group_by(insurance) |> 
  skim_without_charts(bmi_kgm2)

# bivariate graph for insurance and bmi
ba_insurance_bmi_plot <- admissions_omr |> ggplot(mapping = aes(x = bmi_kgm2, y = insurance)) +
  geom_boxplot(color = "royalblue4", fill = "white", varwidth = TRUE) +
  labs(title = "Distributions of Body Mass Index for Patients with different Insurance",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Body Mass Index (kg/m^2)",
       y = "Insurance Type")
ggsave(filename = "ba-insurance-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_insurance_bmi_plot)

# language and bmi 
admissions_omr |> 
  group_by(language) |> 
  skim_without_charts(bmi_kgm2)

# bivariate graph for language and bmi
ba_language_bmi_plot <- admissions_omr |> ggplot(mapping = aes(x = bmi_kgm2, y = language)) +
  geom_boxplot(color = "royalblue4", fill = "white", varwidth = TRUE) +
  labs(title = "Distributions of Body Mass Index for Patients with different Language",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Body Mass Index (kg/m^2)",
       y = "Language")
ggsave(filename = "ba-language-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_language_bmi_plot)


# marital status and bmi 
admissions_omr |> 
  group_by(marital_status) |> 
  skim_without_charts(bmi_kgm2)

# bivariate graph for marital status and bmi
ba_marriage_bmi_plot <- admissions_omr |> ggplot(mapping = aes(x = bmi_kgm2, y = marital_status)) +
  geom_boxplot(color = "royalblue4", fill = "white", varwidth = TRUE) +
  labs(title = "Distributions of Body Mass Index for Patients with different Marital Status",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Body Mass Index (kg/m^2)",
       y = "Marital Status")
ggsave(filename = "ba-marriage-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_marriage_bmi_plot)


# ethnicity and bmi 
admissions_omr |> 
  group_by(ethnicity) |> 
  skim_without_charts(bmi_kgm2)

# bivariate graph for ethnicity and bmi 
ba_ethnicity_bmi_plot <- admissions_omr |> ggplot(mapping = aes(x = bmi_kgm2, y = ethnicity)) +
  geom_boxplot(color = "royalblue4", fill = "white", varwidth = TRUE) +
  labs(title = "Distributions of Body Mass Index for Patients with different Ethnicity",
       subtitle = "For patients at the Beth Isreal Medical Center",
       x = "Body Mass Index (kg/m^2)",
       y = "Ethnicity")
ggsave(filename = "ba-ethnicity-bmi-plot.png", 
       path = "/Users/dhruvpatel/Desktop/Stat301/final-project/Final-Project-DhruvP004/Plots",
       plot = ba_ethnicity_bmi_plot)
