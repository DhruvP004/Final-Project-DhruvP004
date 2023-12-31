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