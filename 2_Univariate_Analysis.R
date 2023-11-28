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
