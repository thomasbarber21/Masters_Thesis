# Description: Gives the formatting of analysis output in LaTex code for reporting

# Section 1: Synthetic Control Output Tables ----
# Air Quality Table
print(xtable(Air_Quality_synthetic_control_table))

# This table shows ATTs fluctuating around 0 in pre treatment, indicating good fit.
# Post treatment shows sharp declines, indicating that policy had a positive effect in decreasing PM2.5 pollution

# EZ Pass Table
print(xtable(EZPass_Speeds_ATT_agg))

# This table also indicates a good pre treatment fit
# We also see sharp declines in post treatment, indicating decreasing speeds due to policy, with April 2025 showing massive increase in speed


# Section 2: Regression Output for Crime Volume ----
modelsummary(list("(2)" = did_model_simple, "(3)" = did_model_control_variables, "(4)" = did_model_distance_ring_simple, "(5)" = did_model_distance_ring_demographic_variables), output = "latex",
             title = "Crime Volume Regression Results",
             gof_omit = "IC|Log.Lik", stars = TRUE)


# Section 3: Regression Output for Distance Analysis ----
modelsummary(list("(6)" = did_model_distance_analysis), output = "latex",
             title = "Distance Analysis Regression Results",
             gof_omit = "IC|Log.Lik", stars = TRUE)


# Section 4: ATT Output ----
# Air Quality
before <- AQ_synthetic_control_data[AQ_synthetic_control_data$month_num < "202501" & AQ_synthetic_control_data$SiteID %in% c("36061NY10130", "36061NY09929", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454"),]
after <- AQ_synthetic_control_data[AQ_synthetic_control_data$month_num >= "202501" & AQ_synthetic_control_data$SiteID %in% c("36061NY10130", "36061NY09929", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454"),]

before_mean_AQ <- mean(before$Value)
after_mean_AQ <- mean(after$Value)

percent_change_AQ <- ((after_mean_AQ - before_mean_AQ)/before_mean_AQ) * 100

# EZ Pass
ez_cbd <- ez_pass[["ezpass_speeds_CBD"]]
ez_cbd$week <- as.Date(ez_cbd$week)
ez_cbd$week_num <- as.numeric(format(ez_cbd$week, "%Y%m%d"))

before <- ez_cbd[ez_cbd$week_num < "20250101", ]
before_mean_ez <- mean(before$median_speed_fps)

after <- ez_cbd[ez_cbd$week_num >= "20250101", ]
after_mean_ez <- mean(after$median_speed_fps)

percent_change_ezpass <- ((after_mean_ez - before_mean_ez)/before_mean_ez) * 100
# The average ATT post treatment of 0.0689 represents 0.483% of average pre treatment observations

# Felony
before <- felony_volume[felony_volume$month_year_num < "20250101" & felony_volume$within_cbd == 1, ]
before_mean_felony <- mean(before$felony)
after <- felony_volume[felony_volume$month_year_num >= "20250101" & felony_volume$within_cbd == 1, ]
after_mean_felony <- mean(after$felony)

percent_change_felony <- ((after_mean_felony - before_mean_felony)/before_mean_felony) * 100
# The average ATT post treatment of 0.3138 represents 0.3145% of average pre treatment observations

# Misdemeanor
before <- misdemeanor_volume[misdemeanor_volume$month_year_num < "20250101" & misdemeanor_volume$within_cbd == 1, ]
before_mean_misdemeanor <- mean(before$misdemeanor)
after <- misdemeanor_volume[misdemeanor_volume$month_year_num >= "20250101" & misdemeanor_volume$within_cbd == 1, ]
after_mean_misdemeanor <- mean(after$misdemeanor)

percent_change_misdemeanor <- ((after_mean_misdemeanor - before_mean_misdemeanor)/before_mean_misdemeanor) * 100
# The average ATT post treatment of 4.8388 represents 3.714% of average pre treatment observations

# Violations
before <- violations_volume[violations_volume$month_year_num < "20250101" & violations_volume$within_cbd == 1, ]
before_mean_violations <- mean(before$violations)
after <- violations_volume[violations_volume$month_year_num >= "20250101" & violations_volume$within_cbd == 1, ]
after_mean_violations <- mean(after$violations)

percent_change_violations <- ((after_mean_violations - before_mean_violations)/before_mean_violations) * 100
# The average ATT post treatment of 0.5519 represents 32.414% of average pre treatment observations

# Format into table
ATT_Table <- data.frame(
  Variable = c('PM2.5', 'EZ Pass Speeds', 'Felony Volume', 'Misdemeanor Volume', 'Violation Volume'),
  Avg_Pre_Treat_Value = c(before_mean_AQ, before_mean_ez, before_mean_felony, before_mean_misdemeanor, before_mean_violations),
  Avg_Post_Treat_Value = c(after_mean_AQ, after_mean_ez, after_mean_felony, after_mean_misdemeanor, after_mean_violations),
  Percent_Change = c(percent_change_AQ, percent_change_ezpass, percent_change_felony, percent_change_misdemeanor, percent_change_violations)
)

# Format in Latex Code
print(xtable(ATT_Table))

# All ATTs
print(xtable(Felonies_synthetic_control_table))
print(xtable(Misdemeanor_synthetic_control_table))
print(xtable(Violations_synthetic_control_table))


# Section 5: PDS LASSO Output ----
print(xtable(pds_lasso_table))

