library(tidyverse)
library(naniar)

# Potential Questions
# How does overall health (as measured by BMI, BP, weight, height) 
# and ultimately length of stay in the hospital vary across different 
# demographic groups (marital status, ethnicity, insurance)?
# What is the distribution of BP, weight, height, and body mass index (BMI) in the dataset?

# Read in admissions
admissions <- read_csv("data/admissions.csv")
admissions

# Read in icd
icd <- read_csv("data/d_icd_diagnoses.csv")
icd

# Read in diagnoses
diagnoses <- read_csv("data/diagnoses_icd.csv")
diagnoses

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

# Missing variable summary for icd
# None missing
miss_var_summary(icd)

# Missing variable summary for diagnoses
# None missing
miss_var_summary(diagnoses)

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

# First step is need to change the character variables to numerical variables so
# I can do mathematical operations like mean()



omr_patient <- omr_wider |> 
  group_by(subject_id) |> 
  mutate("Diastolic Blood Pressure" = as.numeric("Diastolic Blood Pressure"),
         "Systolic Blood Pressure")
omr_patient

print(omr_patient)

admissions


