# Description: Regression analysis to analytically answer the research question

# Section 1: Initial Parallel Pre Trends Test ----

# Run a model regressing arrests with an interaction term between Date and Boroughs
NYC_arrests[["Aggregated"]]$week <- as.factor(NYC_arrests[["Aggregated"]]$week)
parallel_pretrends_test <- lm(Arrests ~ week * Manhattan, data = NYC_arrests[["Aggregated"]][NYC_arrests[["Aggregated"]]$pre_treatment_period == 1, ])

# Summarize the model output
sink("../output/old_visuals/Parallel Pretrends Test.txt")   # Asked ChatGPT: is there a function to save a regression in txt format
print(summary(parallel_pretrends_test))
sink()

# F-stat of 0.2446 with a p-value of 1 implies no differential trends in arrests
# between Manhattan and the other boroughs, parallel pre-trends holds


# Section 2: Test for Parallel Pre-trends ----
NYC_arrests_cbd$ARREST_DATE <- as.Date(NYC_arrests_cbd$ARREST_DATE, format = "%Y-%m-%d")
NYC_arrests_non_cbd$ARREST_DATE <- as.Date(NYC_arrests_non_cbd$ARREST_DATE, format = "%Y-%m-%d")

NYC_arrests_cbd$week <- cut(NYC_arrests_cbd$ARREST_DATE, breaks = "week")
NYC_arrests_non_cbd$week <- cut(NYC_arrests_non_cbd$ARREST_DATE, breaks = "week")

NYC_arrests_cbd_aggregated <- aggregate(Arrests ~ week + ARREST_BORO, data = NYC_arrests_cbd, FUN = sum)
NYC_arrests_non_cbd_aggregated <- aggregate(Arrests ~ week + ARREST_BORO, data = NYC_arrests_non_cbd, FUN = sum)

NYC_arrests_cbd_aggregated$week <- as.Date(NYC_arrests_cbd_aggregated$week, format = "%Y-%m-%d")
NYC_arrests_non_cbd_aggregated$week <- as.Date(NYC_arrests_non_cbd_aggregated$week, format = "%Y-%m-%d")

NYC_arrests_cbd_aggregated$Manhattan <- ifelse(NYC_arrests_cbd_aggregated$ARREST_BORO == "Manhattan", 1, 0)
NYC_arrests_non_cbd_aggregated$Manhattan <- ifelse(NYC_arrests_non_cbd_aggregated$ARREST_BORO == "Manhattan", 1, 0)

NYC_arrests_cbd_aggregated$pre_treatment_period <- ifelse(NYC_arrests_cbd_aggregated$week <= as.Date("2025-01-03"), 1, 0)
NYC_arrests_non_cbd_aggregated$pre_treatment_period <- ifelse(NYC_arrests_non_cbd_aggregated$week <= as.Date("2025-01-03"), 1, 0)

NYC_arrests_cbd_aggregated$within_cbd <- 1
NYC_arrests_non_cbd_aggregated$within_cbd <- 0

pre_trends_test <- rbind(NYC_arrests_cbd_aggregated, NYC_arrests_non_cbd_aggregated)

# Run a model regressing arrests with an interaction term between Date and Boroughs
pre_trends_test$week <- as.factor(pre_trends_test$week)
parallel_pretrends_test_cbd <- lm(Arrests ~ week * within_cbd, data = pre_trends_test[pre_trends_test$pre_treatment_period == 1, ])

# Summarize the model output
sink("../output/Parallel_Pretrends_Test_CBD.txt")
print(summary(parallel_pretrends_test_cbd))
sink()

# F-stat of 0.2319 with a p-value of 1 implies no differential trends in arrests
# between Manhattan and the other boroughs, parallel pre-trends holds

# Plot Parallel Pre-trends
# Data for Pre Treatment Plot
cbd_pre_trends <- subset(pre_trends_test, within_cbd == 1 & pre_treatment_period == 1)
non_cbd_pre_trends <- subset(pre_trends_test, within_cbd == 0 & pre_treatment_period == 1)

non_cbd_pre_trends <- aggregate(Arrests ~ week, data = non_cbd_pre_trends, FUN = mean)

cbd_pre_trends$week <- as.Date(cbd_pre_trends$week)
non_cbd_pre_trends$week <- as.Date(non_cbd_pre_trends$week)

# Create a png file and send output to correct folder
png(filename = "../output/Parallel_pretrends_visualization_cbd.png", width = 2100, height = 1400, res = 300)

# Plot the Data
plot(cbd_pre_trends$week[-c(1)], cbd_pre_trends$Arrests[-c(1)], type = "l", col = "black", ylim = c(50, 1000), xlab = "Date", ylab = "Arrests")
lines(non_cbd_pre_trends$week[-c(1)], non_cbd_pre_trends$Arrests[-c(1)], type = "l", col = "black", lty = 2)

# Vertical line for implementation date and legend
abline(v = implementation_date, col = "red", lty = 2)
legend("bottomleft", legend = c("CBD Arrests", "Non-CBD Arrests"), lty = c(1, 2), col = c("black", "black"))

dev.off()


# Section 3: Simple DiD Regressions ----
Regressions <- list()
Regressions$Crime_Volume_regression <- NYC_arrests[["Cleaned_Arrests_Data"]]

Regressions[["Crime_Volume_regression"]]$month <- format(Regressions[["Crime_Volume_regression"]]$ARREST_DATE, format = "%Y-%m")

# Aggregate data by month, borough, and treatment status to get total crime volume
Regressions[["Crime_Volume_regression"]] <- aggregate(ARREST_KEY ~ month + ARREST_BORO + Post + Manhattan, data = Regressions[["Crime_Volume_regression"]], FUN = length)
Regressions[["Crime_Volume_regression"]]$Interaction_term <- Regressions[["Crime_Volume_regression"]]$Post * Regressions[["Crime_Volume_regression"]]$Manhattan

Simple_DiD_Model <- lm(ARREST_KEY ~ Post + Manhattan + Interaction_term, data = Regressions[["Crime_Volume_regression"]])
summary(Simple_DiD_Model)

# With Time and Borough Fixed Effects
Simple_DiD_Model_FE <- lm(ARREST_KEY ~ Post + Manhattan + Interaction_term + as.factor(month) + as.factor(ARREST_BORO), data = Regressions[["Crime_Volume_regression"]])
summary(Simple_DiD_Model_FE)

# No evidence of a statistically significant change in arrest volume in Manhattan due to the congestion charge


# Section 4: Implement DiD Regression for CBD vs non-CBD Arrest Data ----
DiD_Arrest_Data <- rbind(NYC_arrests_cbd, NYC_arrests_non_cbd)

DiD_Arrest_Data_simple <- aggregate(Arrests ~ week + within_cbd + Post, data = DiD_Arrest_Data, FUN = sum)

# Simple DiD model using fixest
did_model_simple <- feols(Arrests ~ within_cbd + Post + within_cbd * Post | week, data = DiD_Arrest_Data_simple, cluster = ~week)
summary(did_model_simple)

# Try with control variables for demographic
DiD_Arrest_Data_control_variables <- aggregate(Arrests ~ week + within_cbd + Post + AGE_GROUP + PERP_SEX + PERP_RACE, data = DiD_Arrest_Data, FUN = sum)

did_model_control_variables <- feols(Arrests ~ within_cbd + Post + within_cbd * Post + AGE_GROUP + PERP_SEX + PERP_RACE | week, data = DiD_Arrest_Data_control_variables, cluster = ~week)
summary(did_model_control_variables)
# DiD estimate shows within_cbd:Post arrests within the CBD per subgroup per week dropped by 9.4, relative to outside the CBD, statistically significant at 99% confidence, clustered by week


# Section 5: Implement DiD Regression for CBD vs non-CBD Arrest Data with Distance to Public Transportation ----
# Extract Data with Distance Rings
NYC_arrests_cbd_distance_rings <- st_within(NYC_arrests[["arrests_proj"]]$geometry, manhattan_cbd, sparse = FALSE)

# Extract True/False Matrix and index on original data to obtain CBD and non-CBD data with distance rings
within_cbd_distance_rings <- NYC_arrests_cbd_distance_rings[, 1]
NYC_arrests_cbd_distance_rings <- NYC_arrests[["arrests_proj"]][within_cbd_distance_rings, ]

NYC_arrests_non_cbd_distance_rings <- NYC_arrests[["arrests_proj"]][!within_cbd_distance_rings, ]

NYC_arrests_cbd_distance_rings <- st_drop_geometry(NYC_arrests_cbd_distance_rings)
NYC_arrests_non_cbd_distance_rings <- st_drop_geometry(NYC_arrests_non_cbd_distance_rings)

# Format data
names(NYC_arrests_cbd_distance_rings)[which(names(NYC_arrests_cbd_distance_rings) == "Manhattan")] <- "within_cbd"
names(NYC_arrests_non_cbd_distance_rings)[which(names(NYC_arrests_non_cbd_distance_rings) == "Manhattan")] <- "within_cbd"
NYC_arrests_non_cbd_distance_rings$within_cbd <- 0

NYC_arrests_cbd_distance_rings$week <- as.Date(NYC_arrests_cbd_distance_rings$ARREST_DATE, format = "%Y-%m-%d")
NYC_arrests_non_cbd_distance_rings$week <- as.Date(NYC_arrests_non_cbd_distance_rings$ARREST_DATE, format = "%Y-%m-%d")

DiD_Arrest_Data_with_Distances <- rbind(NYC_arrests_cbd_distance_rings, NYC_arrests_non_cbd_distance_rings)
DiD_Arrest_Data_with_Distances$Arrests <- 1

# Implement Regressions without demographic controls
DiD_Arrest_Data_with_Distances_simple <- aggregate(Arrests ~ week + within_cbd + Post + distance_ring, data = DiD_Arrest_Data_with_Distances, FUN = sum)

did_model_distance_ring_simple <- feols(Arrests ~ within_cbd + Post + within_cbd * Post + distance_ring, data = DiD_Arrest_Data_with_Distances_simple, cluster = ~week)
summary(did_model_distance_ring_simple)
# When I included fixed effects for week in this model Post was dropped due to collinearity

# Implement Regressions with demographic controls
DiD_Arrest_Data_with_Distances_demographic_variables <- aggregate(Arrests ~ week + within_cbd + Post + distance_ring + AGE_GROUP + PERP_SEX + PERP_RACE, data = DiD_Arrest_Data_with_Distances, FUN = sum)

did_model_distance_ring_demographic_variables <- feols(Arrests ~ within_cbd + Post + within_cbd * Post + distance_ring + AGE_GROUP + PERP_SEX + PERP_RACE, data = DiD_Arrest_Data_with_Distances_demographic_variables, cluster = ~week)
summary(did_model_distance_ring_demographic_variables)

# Regression to Analyze Effect of Treatment on Arrests distance to Subway or Bus Stops
did_model_distance_analysis <- feols(Arrests ~ within_cbd + Post + distance_ring + within_cbd * Post + within_cbd * distance_ring + Post * distance_ring + within_cbd * Post * distance_ring, data = DiD_Arrest_Data_with_Distances_simple, cluster = ~week)
summary(did_model_distance_analysis)


# Section 6: Plot the Post Treatment Data ----

# DiD Plot
treat_group <- NYC_arrests_cbd_aggregated
cont_group <- aggregate(Arrests ~ week, data = NYC_arrests_non_cbd_aggregated, FUN = mean)

png(filename = "../output/CBD_DiD_Graph.png", width = 2100, height = 1400, res = 300)
plot(treat_group$week[-c(1:2, nrow(treat_group))], treat_group$Arrests[-c(1:2, nrow(treat_group))], type = "l", ylim = c(450, 1050), ylab = "Arrests", xlab = "Time (weeks)")
lines(cont_group$week[-c(1:2, nrow(cont_group))], cont_group$Arrests[-c(1:2, nrow(cont_group))], type = "l", lty = 2, col = "blue")
abline(v = implementation_date, lty = 2, col = "red")
legend("topleft", legend = c("CBD Arrests", "Non-CBD Arrests"), lty = c(1, 2), col = c("black", "blue"))
dev.off()

# Plot the Treatment Effects for Distance Rings
baseline <- coef(did_model_distance_analysis)["within_cbd:Post"]
distance_25ft <- coef(did_model_distance_analysis)["within_cbd:Post:distance_ring25ft"]
distance_50ft <- coef(did_model_distance_analysis)["within_cbd:Post:distance_ring50ft"]
distance_100ft <- coef(did_model_distance_analysis)["within_cbd:Post:distance_ring100ft"]

treat_effects <- c("25ft" = baseline + distance_25ft, "50ft" = baseline + distance_50ft, "100ft" = baseline + distance_100ft, ">100ft" = baseline)

distances <- c("25ft", "50ft", "100ft", ">100ft")

png(filename = "../output/Distance_Rings_Treatment_Effect.png", width = 2100, height = 1400, res = 300)
plot(treat_effects, xaxt = "n", pch = 16, type = "p", main = "Treatment Effect Based on Distance", ylab = "Arrests", xlab = "Distance from Subway or Bus Stop")
axis(1, at = 1:4, labels = distances)
abline(h = 0, lty = 2, col = "gray")
dev.off()

# From treatment effect we see that while arrests declined everywhere in the CBD within 100ft of public transit,
# arrests are highest near public transportation due to a lack of reduction from the policy.
# So relative to farther away locations the policy led to a higher number of arrests near transit


# Section 7: PDS LASSO Implementation ----
pds_lasso_data <- DiD_Arrest_Data_with_Distances

X <- model.matrix(~ Post + within_cbd + PD_DESC + OFNS_DESC + LAW_CAT_CD + ARREST_BORO + AGE_GROUP + PERP_SEX + PERP_RACE + distance_ring + week, data = pds_lasso_data)[,-1]

Y <- pds_lasso_data$Arrests

D_interaction <- pds_lasso_data$Post * pds_lasso_data$within_cbd

pds_lasso_model <- rlassoEffect(x = X, y = Y, d = D_interaction, method = "double selection")
pds_coef <- summary(pds_lasso_model)

pds_lasso_table <- data.table(Interaction = "Post x Within CBD", Estimate = summary(pds_lasso_model)$coef[1], `Std. Error` = summary(pds_lasso_model)$coef[2], `t value` = summary(pds_lasso_model)$coef[3], `p value` = summary(pds_lasso_model)$coef[4])

