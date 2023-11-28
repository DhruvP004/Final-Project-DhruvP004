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
