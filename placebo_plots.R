# Description: Placebo Tests and Plots for Synthetic Controls

# Section 1: Changing Air Quality Treatment Boroughs ----

# The Bronx
air_quality_placebo <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025, air_quality$may2025, air_quality$june2025)
air_quality_placebo$Within_CBD <- ifelse(air_quality_placebo$SiteID %in% c("36005NY11534", "36005NY11790", "36005NY12387", "36061NY12380"), 1, 0)

air_quality_placebo$Year <- ifelse(air_quality_placebo$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)


air_quality_placebo$month_num <- air_quality_placebo$month
air_quality_placebo$month_num[air_quality_placebo$month == "may"] <- 202405
air_quality_placebo$month_num[air_quality_placebo$month == "june"] <- 202406
air_quality_placebo$month_num[air_quality_placebo$month == "july"] <- 202407
air_quality_placebo$month_num[air_quality_placebo$month == "aug"] <- 202408
air_quality_placebo$month_num[air_quality_placebo$month == "sept"] <- 202409
air_quality_placebo$month_num[air_quality_placebo$month == "oct"] <- 202410
air_quality_placebo$month_num[air_quality_placebo$month == "nov"] <- 202411
air_quality_placebo$month_num[air_quality_placebo$month == "dec"] <- 202412
air_quality_placebo$month_num[air_quality_placebo$month == "jan"] <- 202501
air_quality_placebo$month_num[air_quality_placebo$month == "feb"] <- 202502
air_quality_placebo$month_num[air_quality_placebo$month == "march"] <- 202503
air_quality_placebo$month_num[air_quality_placebo$month == "april"] <- 202504
air_quality_placebo$month_num[air_quality_placebo$month == "may25"] <- 202505
air_quality_placebo$month_num[air_quality_placebo$month == "june25"] <- 202506
air_quality_placebo$month_num <- as.numeric(air_quality_placebo$month_num)

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 1, ]
control_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 0, ]

treatment_group <- treatment_group_AQ
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]
treatment_group <- aggregate(Value ~ month_num + SiteID, data = treatment_group, FUN = mean, na.rm = TRUE)
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]

control_group <- control_group_AQ[, c("SiteID", "month_num", "Value")]
control_group <- aggregate(Value ~ month_num + SiteID, data = control_group, FUN = mean, na.rm = TRUE)
control_group <- control_group[, c("SiteID", "month_num", "Value")]

# Combine data, assign unique IDs to units of control and treated observations
AQ_synthetic_control_data <- rbind(control_group, treatment_group)
AQ_synthetic_control_data$unit_ID <- as.numeric(factor(AQ_synthetic_control_data$SiteID))

AQ_control_IDs <- unique(AQ_synthetic_control_data$unit_ID[AQ_synthetic_control_data$SiteID %in% control_group$SiteID])


AQ_treated_sites <- c("36005NY11534", "36005NY11790", "36005NY12387", "36061NY12380")
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ_bronx <- gsynth(Value ~ D, 
                    data = AQ_synthetic_control_data, 
                    index = c("unit_ID", "month_num"), 
                    force = "two-way",    # Unit and Time TWFE
                    inference = "nonparametric",    # recommended when there are more treated units
                    estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                    #se = TRUE,
                    #nboots = 1000,
                    min.T0 = 7
)

plot(gsynth_AQ_bronx$att, type = "l", main = "Air Quality Synthetic Control", xlab = "Time (months)", ylab = "PM2.5 Value")
abline(v = 8, lty = 2, col = "red")

# Queens
air_quality_placebo <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025, air_quality$may2025, air_quality$june2025)
air_quality_placebo$Within_CBD <- ifelse(air_quality_placebo$SiteID %in% c("36081NY08198", "36081NY07615", "36081NY09285"), 1, 0)

air_quality_placebo$Year <- ifelse(air_quality_placebo$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)


air_quality_placebo$month_num <- air_quality_placebo$month
air_quality_placebo$month_num[air_quality_placebo$month == "may"] <- 202405
air_quality_placebo$month_num[air_quality_placebo$month == "june"] <- 202406
air_quality_placebo$month_num[air_quality_placebo$month == "july"] <- 202407
air_quality_placebo$month_num[air_quality_placebo$month == "aug"] <- 202408
air_quality_placebo$month_num[air_quality_placebo$month == "sept"] <- 202409
air_quality_placebo$month_num[air_quality_placebo$month == "oct"] <- 202410
air_quality_placebo$month_num[air_quality_placebo$month == "nov"] <- 202411
air_quality_placebo$month_num[air_quality_placebo$month == "dec"] <- 202412
air_quality_placebo$month_num[air_quality_placebo$month == "jan"] <- 202501
air_quality_placebo$month_num[air_quality_placebo$month == "feb"] <- 202502
air_quality_placebo$month_num[air_quality_placebo$month == "march"] <- 202503
air_quality_placebo$month_num[air_quality_placebo$month == "april"] <- 202504
air_quality_placebo$month_num[air_quality_placebo$month == "may25"] <- 202505
air_quality_placebo$month_num[air_quality_placebo$month == "june25"] <- 202506
air_quality_placebo$month_num <- as.numeric(air_quality_placebo$month_num)

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 1, ]
control_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 0, ]

treatment_group <- treatment_group_AQ
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]
treatment_group <- aggregate(Value ~ month_num + SiteID, data = treatment_group, FUN = mean, na.rm = TRUE)
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]

control_group <- control_group_AQ[, c("SiteID", "month_num", "Value")]
control_group <- aggregate(Value ~ month_num + SiteID, data = control_group, FUN = mean, na.rm = TRUE)
control_group <- control_group[, c("SiteID", "month_num", "Value")]

# Combine data, assign unique IDs to units of control and treated observations
AQ_synthetic_control_data <- rbind(control_group, treatment_group)
AQ_synthetic_control_data$unit_ID <- as.numeric(factor(AQ_synthetic_control_data$SiteID))

AQ_control_IDs <- unique(AQ_synthetic_control_data$unit_ID[AQ_synthetic_control_data$SiteID %in% control_group$SiteID])


AQ_treated_sites <- c("36081NY08198", "36081NY07615", "36081NY09285")
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ_queens <- gsynth(Value ~ D, 
                          data = AQ_synthetic_control_data, 
                          index = c("unit_ID", "month_num"), 
                          force = "two-way",    # Unit and Time TWFE
                          inference = "nonparametric",    # recommended when there are more treated units
                          estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                          #se = TRUE,
                          #nboots = 1000,
                          min.T0 = 7
)

plot(gsynth_AQ_queens$att, type = "l", main = "Air Quality Synthetic Control", xlab = "Time (months)", ylab = "PM2.5 Value")
abline(v = 8, lty = 2, col = "red")

# Brooklyn
air_quality_placebo <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025, air_quality$may2025, air_quality$june2025)
air_quality_placebo$Within_CBD <- ifelse(air_quality_placebo$SiteID %in% c("36047NY07974"), 1, 0)

air_quality_placebo$Year <- ifelse(air_quality_placebo$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)


air_quality_placebo$month_num <- air_quality_placebo$month
air_quality_placebo$month_num[air_quality_placebo$month == "may"] <- 202405
air_quality_placebo$month_num[air_quality_placebo$month == "june"] <- 202406
air_quality_placebo$month_num[air_quality_placebo$month == "july"] <- 202407
air_quality_placebo$month_num[air_quality_placebo$month == "aug"] <- 202408
air_quality_placebo$month_num[air_quality_placebo$month == "sept"] <- 202409
air_quality_placebo$month_num[air_quality_placebo$month == "oct"] <- 202410
air_quality_placebo$month_num[air_quality_placebo$month == "nov"] <- 202411
air_quality_placebo$month_num[air_quality_placebo$month == "dec"] <- 202412
air_quality_placebo$month_num[air_quality_placebo$month == "jan"] <- 202501
air_quality_placebo$month_num[air_quality_placebo$month == "feb"] <- 202502
air_quality_placebo$month_num[air_quality_placebo$month == "march"] <- 202503
air_quality_placebo$month_num[air_quality_placebo$month == "april"] <- 202504
air_quality_placebo$month_num[air_quality_placebo$month == "may25"] <- 202505
air_quality_placebo$month_num[air_quality_placebo$month == "june25"] <- 202506
air_quality_placebo$month_num <- as.numeric(air_quality_placebo$month_num)

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 1, ]
control_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 0, ]

treatment_group <- treatment_group_AQ
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]
treatment_group <- aggregate(Value ~ month_num + SiteID, data = treatment_group, FUN = mean, na.rm = TRUE)
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]

control_group <- control_group_AQ[, c("SiteID", "month_num", "Value")]
control_group <- aggregate(Value ~ month_num + SiteID, data = control_group, FUN = mean, na.rm = TRUE)
control_group <- control_group[, c("SiteID", "month_num", "Value")]

# Combine data, assign unique IDs to units of control and treated observations
AQ_synthetic_control_data <- rbind(control_group, treatment_group)
AQ_synthetic_control_data$unit_ID <- as.numeric(factor(AQ_synthetic_control_data$SiteID))

AQ_control_IDs <- unique(AQ_synthetic_control_data$unit_ID[AQ_synthetic_control_data$SiteID %in% control_group$SiteID])


AQ_treated_sites <- c("36047NY07974")
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ_brooklyn <- gsynth(Value ~ D, 
                           data = AQ_synthetic_control_data, 
                           index = c("unit_ID", "month_num"), 
                           force = "two-way",    # Unit and Time TWFE
                           inference = "nonparametric",    # recommended when there are more treated units
                           estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                           #se = TRUE,
                           #nboots = 1000,
                           min.T0 = 6
)

plot(gsynth_AQ_brooklyn$att, type = "l", main = "Air Quality Synthetic Control", xlab = "Time (months)", ylab = "PM2.5 Value")
abline(v = 8, lty = 2, col = "red")

# Staten Island
air_quality_placebo <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025, air_quality$may2025, air_quality$june2025)
air_quality_placebo$Within_CBD <- ifelse(air_quality_placebo$SiteID %in% c("36085NY03820"), 1, 0)

air_quality_placebo$Year <- ifelse(air_quality_placebo$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)


air_quality_placebo$month_num <- air_quality_placebo$month
air_quality_placebo$month_num[air_quality_placebo$month == "may"] <- 202405
air_quality_placebo$month_num[air_quality_placebo$month == "june"] <- 202406
air_quality_placebo$month_num[air_quality_placebo$month == "july"] <- 202407
air_quality_placebo$month_num[air_quality_placebo$month == "aug"] <- 202408
air_quality_placebo$month_num[air_quality_placebo$month == "sept"] <- 202409
air_quality_placebo$month_num[air_quality_placebo$month == "oct"] <- 202410
air_quality_placebo$month_num[air_quality_placebo$month == "nov"] <- 202411
air_quality_placebo$month_num[air_quality_placebo$month == "dec"] <- 202412
air_quality_placebo$month_num[air_quality_placebo$month == "jan"] <- 202501
air_quality_placebo$month_num[air_quality_placebo$month == "feb"] <- 202502
air_quality_placebo$month_num[air_quality_placebo$month == "march"] <- 202503
air_quality_placebo$month_num[air_quality_placebo$month == "april"] <- 202504
air_quality_placebo$month_num[air_quality_placebo$month == "may25"] <- 202505
air_quality_placebo$month_num[air_quality_placebo$month == "june25"] <- 202506
air_quality_placebo$month_num <- as.numeric(air_quality_placebo$month_num)

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 1, ]
control_group_AQ <- air_quality_placebo[air_quality_placebo$Within_CBD == 0, ]

treatment_group <- treatment_group_AQ
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]
treatment_group <- aggregate(Value ~ month_num + SiteID, data = treatment_group, FUN = mean, na.rm = TRUE)
treatment_group <- treatment_group[, c("SiteID", "month_num", "Value")]

control_group <- control_group_AQ[, c("SiteID", "month_num", "Value")]
control_group <- aggregate(Value ~ month_num + SiteID, data = control_group, FUN = mean, na.rm = TRUE)
control_group <- control_group[, c("SiteID", "month_num", "Value")]

# Combine data, assign unique IDs to units of control and treated observations
AQ_synthetic_control_data <- rbind(control_group, treatment_group)
AQ_synthetic_control_data$unit_ID <- as.numeric(factor(AQ_synthetic_control_data$SiteID))

AQ_control_IDs <- unique(AQ_synthetic_control_data$unit_ID[AQ_synthetic_control_data$SiteID %in% control_group$SiteID])


AQ_treated_sites <- c("36085NY03820")
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ_staten <- gsynth(Value ~ D, 
                             data = AQ_synthetic_control_data, 
                             index = c("unit_ID", "month_num"), 
                             force = "two-way",    # Unit and Time TWFE
                             inference = "nonparametric",    # recommended when there are more treated units
                             estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                             #se = TRUE,
                             #nboots = 1000,
                             min.T0 = 5
)

plot(gsynth_AQ_staten$att, type = "l", main = "Air Quality Synthetic Control", xlab = "Time (months)", ylab = "PM2.5 Value")
abline(v = 8, lty = 2, col = "red")

gsynth_AQ_bronx$time <- paste0(gsynth_AQ_bronx$time, "01")
gsynth_AQ_brooklyn$time <- paste0(gsynth_AQ_brooklyn$time, "01")
gsynth_AQ_queens$time <- paste0(gsynth_AQ_queens$time, "01")
gsynth_AQ_staten$time <- paste0(gsynth_AQ_staten$time, "01")

gsynth_AQ_brooklyn$time <- as.Date(as.character(gsynth_AQ_brooklyn$time), format = "%Y%m%d")
gsynth_AQ_queens$time <- as.Date(as.character(gsynth_AQ_queens$time), format = "%Y%m%d")
gsynth_AQ_staten$time <- as.Date(as.character(gsynth_AQ_staten$time), format = "%Y%m%d")
gsynth_AQ_bronx$time <- as.Date(as.character(gsynth_AQ_bronx$time), format = "%Y%m%d")

# Plot all the ATTs of the Placebo Tests with original treatment
png(filename = "../output/Air_Quality_Placebo_Plot.png", width = 2100, height = 1400, res = 300)
plot(gsynth_AQ$time, gsynth_AQ$att, type = "l", xlab = "Time (months)", ylab = "Gap in PM2.5 Value", col = "black", ylim = c(-5, 4), lwd = 2)
lines(gsynth_AQ_bronx$time, gsynth_AQ_bronx$att, type = "l", col = "grey")
lines(gsynth_AQ_brooklyn$time, gsynth_AQ_brooklyn$att, type = "l", col = "grey")
lines(gsynth_AQ_queens$time, gsynth_AQ_queens$att, type = "l", col = "grey")
lines(gsynth_AQ_staten$time, gsynth_AQ_staten$att, type = "l", col = "grey")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
legend("bottomleft", legend = c("CBD", "Control CBD"), fill = c("black","grey"))
dev.off()


# Section 2: Changing EZ Pass Treatment Variable ----

# Brooklyn
ezpass_placebo <-  rbind(ez_pass_aggregated$sept2024, ez_pass_aggregated$oct2024, ez_pass_aggregated$nov2024, ez_pass_aggregated$dec2024, ez_pass_aggregated$jan2025, ez_pass_aggregated$feb2025, ez_pass_aggregated$march2025, ez_pass_aggregated$april2025, ez_pass_aggregated$may2025, ez_pass_aggregated$july2025, ez_pass_aggregated$aug2025)
ezpass_placebo$Within_CBD <- ifelse(ezpass_placebo$borough == "Brooklyn" , 1, 0) # Change for each borough

lat <- numeric(length(ezpass_placebo$polyline))
lon <- numeric(length(ezpass_placebo$polyline))

for (i in 1:length(ezpass_placebo$polyline)) {
  ezpass_placebo$decoded <- decode(ezpass_placebo$polyline[i])
  
  lat[i] <- ezpass_placebo[["decoded"]][[1]]$lat[1]
  lon[i] <- ezpass_placebo[["decoded"]][[1]]$lon[1]
}
ezpass_placebo$lat <- lat
ezpass_placebo$lon <- lon

# Utilize shapefile of Manhattan's CBD to extract values within the CBD
ezpass_placebo_sf <- st_as_sf(ezpass_placebo, coords = c("lon", "lat"), crs = 4326)
manhattan_cbd <- st_transform(manhattan_cbd, crs = st_crs(ezpass_placebo_sf))

ezpass_placebo$inside_cbd <- ifelse(st_within(ezpass_placebo_sf, manhattan_cbd, sparse = FALSE)[, 1], 0, 1)

ezpass_placebo$week <- as.Date(ezpass_placebo$week)

# Split data into treatment and control groups
ezpass_treatment_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 1 & ezpass_placebo$borough == "Brooklyn", ] # Insert borough here
ezpass_control_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 0 | ezpass_placebo$borough %in% c("Manhattan", "Bronx", "Queens", "Staten Island"), ] # Insert remaining boroughs

# Pull, organize, and aggregate necessary data
ezpass_treatment_group <- ezpass_treatment_group[, c("polyline", "week", "median_speed_fps")]
ezpass_treatment_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_treatment_group, na.rm = TRUE)

ezpass_control_group <- ezpass_control_group[, c("polyline", "week", "median_speed_fps")]
ezpass_control_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_control_group, na.rm = TRUE)

# Combine the groups, assigning unique IDs for treated and control observations
ezpass_speeds_synthetic_control_data <- rbind(ezpass_control_group, ezpass_treatment_group)
ezpass_speeds_synthetic_control_data$unit_ID <- as.numeric(factor(ezpass_speeds_synthetic_control_data$polyline))

ezpass_control_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_control_group$polyline])

ezpass_treated_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_treatment_group$polyline])

ezpass_speeds_synthetic_control_data$week_num <- as.numeric(format(ezpass_speeds_synthetic_control_data$week, "%Y%m%d"))

ezpass_speeds_synthetic_control_data$D <- ifelse(
  ezpass_speeds_synthetic_control_data$unit_ID %in% ezpass_treated_IDs & ezpass_speeds_synthetic_control_data$week_num >= 20250101,
  1,
  0
)

# Implement Synthetic Control
gsynth_ezpass_brooklyn <- gsynth(median_speed_fps ~ D, 
                        data = ezpass_speeds_synthetic_control_data, 
                        index = c("unit_ID", "week_num"), 
                        force = "two-way",    # Unit and Time TWFE
                        inference = "nonparametric",    # recommended when there are more treated units
                        estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                        #se = TRUE,
                        #nboots = 1000,
                        min.T0 = 7
)

plot(gsynth_ezpass_brooklyn$att, type = "l", main = "EZ Pass Synthetic Control", xlab = "Time (weeks)", ylab = "Speed (FPS)")
abline(v = 19, lty = 2, col = "red")

# Queens
ezpass_placebo <-  rbind(ez_pass_aggregated$sept2024, ez_pass_aggregated$oct2024, ez_pass_aggregated$nov2024, ez_pass_aggregated$dec2024, ez_pass_aggregated$jan2025, ez_pass_aggregated$feb2025, ez_pass_aggregated$march2025, ez_pass_aggregated$april2025, ez_pass_aggregated$may2025, ez_pass_aggregated$july2025, ez_pass_aggregated$aug2025)
ezpass_placebo$Within_CBD <- ifelse(ezpass_placebo$borough == "Queens" , 1, 0) # Change for each borough

lat <- numeric(length(ezpass_placebo$polyline))
lon <- numeric(length(ezpass_placebo$polyline))

for (i in 1:length(ezpass_placebo$polyline)) {
  ezpass_placebo$decoded <- decode(ezpass_placebo$polyline[i])
  
  lat[i] <- ezpass_placebo[["decoded"]][[1]]$lat[1]
  lon[i] <- ezpass_placebo[["decoded"]][[1]]$lon[1]
}
ezpass_placebo$lat <- lat
ezpass_placebo$lon <- lon

# Utilize shapefile of Manhattan's CBD to extract values within the CBD
ezpass_placebo_sf <- st_as_sf(ezpass_placebo, coords = c("lon", "lat"), crs = 4326)
manhattan_cbd <- st_transform(manhattan_cbd, crs = st_crs(ezpass_placebo_sf))

ezpass_placebo$inside_cbd <- ifelse(st_within(ezpass_placebo_sf, manhattan_cbd, sparse = FALSE)[, 1], 0, 1)

ezpass_placebo$week <- as.Date(ezpass_placebo$week)

# Split data into treatment and control groups
ezpass_treatment_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 1 & ezpass_placebo$borough == "Queens", ] # Insert borough here
ezpass_control_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 0 | ezpass_placebo$borough %in% c("Manhattan", "Bronx", "Brooklyn", "Staten Island"), ] # Insert remaining boroughs

# Pull, organize, and aggregate necessary data
ezpass_treatment_group <- ezpass_treatment_group[, c("polyline", "week", "median_speed_fps")]
ezpass_treatment_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_treatment_group, na.rm = TRUE)

ezpass_control_group <- ezpass_control_group[, c("polyline", "week", "median_speed_fps")]
ezpass_control_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_control_group, na.rm = TRUE)

# Combine the groups, assigning unique IDs for treated and control observations
ezpass_speeds_synthetic_control_data <- rbind(ezpass_control_group, ezpass_treatment_group)
ezpass_speeds_synthetic_control_data$unit_ID <- as.numeric(factor(ezpass_speeds_synthetic_control_data$polyline))

ezpass_control_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_control_group$polyline])

ezpass_treated_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_treatment_group$polyline])

ezpass_speeds_synthetic_control_data$week_num <- as.numeric(format(ezpass_speeds_synthetic_control_data$week, "%Y%m%d"))

ezpass_speeds_synthetic_control_data$D <- ifelse(
  ezpass_speeds_synthetic_control_data$unit_ID %in% ezpass_treated_IDs & ezpass_speeds_synthetic_control_data$week_num >= 20250101,
  1,
  0
)

# Implement Synthetic Control
gsynth_ezpass_queens <- gsynth(median_speed_fps ~ D, 
                                 data = ezpass_speeds_synthetic_control_data, 
                                 index = c("unit_ID", "week_num"), 
                                 force = "two-way",    # Unit and Time TWFE
                                 inference = "nonparametric",    # recommended when there are more treated units
                                 estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                 #se = TRUE,
                                 #nboots = 1000,
                                 min.T0 = 7
)

plot(gsynth_ezpass_queens$att, type = "l", main = "EZ Pass Synthetic Control", xlab = "Time (weeks)", ylab = "Speed (FPS)")
abline(v = 19, lty = 2, col = "red")

# Staten Island
ezpass_placebo <-  rbind(ez_pass_aggregated$sept2024, ez_pass_aggregated$oct2024, ez_pass_aggregated$nov2024, ez_pass_aggregated$dec2024, ez_pass_aggregated$jan2025, ez_pass_aggregated$feb2025, ez_pass_aggregated$march2025, ez_pass_aggregated$april2025, ez_pass_aggregated$may2025, ez_pass_aggregated$july2025, ez_pass_aggregated$aug2025)
ezpass_placebo$Within_CBD <- ifelse(ezpass_placebo$borough == "Staten Island" , 1, 0) # Change for each borough

lat <- numeric(length(ezpass_placebo$polyline))
lon <- numeric(length(ezpass_placebo$polyline))

for (i in 1:length(ezpass_placebo$polyline)) {
  ezpass_placebo$decoded <- decode(ezpass_placebo$polyline[i])
  
  lat[i] <- ezpass_placebo[["decoded"]][[1]]$lat[1]
  lon[i] <- ezpass_placebo[["decoded"]][[1]]$lon[1]
}
ezpass_placebo$lat <- lat
ezpass_placebo$lon <- lon

# Utilize shapefile of Manhattan's CBD to extract values within the CBD
ezpass_placebo_sf <- st_as_sf(ezpass_placebo, coords = c("lon", "lat"), crs = 4326)
manhattan_cbd <- st_transform(manhattan_cbd, crs = st_crs(ezpass_placebo_sf))

ezpass_placebo$inside_cbd <- ifelse(st_within(ezpass_placebo_sf, manhattan_cbd, sparse = FALSE)[, 1], 0, 1)

ezpass_placebo$week <- as.Date(ezpass_placebo$week)

# Split data into treatment and control groups
ezpass_treatment_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 1 & ezpass_placebo$borough == "Staten Island", ] # Insert borough here
ezpass_control_group <- ezpass_placebo[ezpass_placebo$inside_cbd == 0 | ezpass_placebo$borough %in% c("Manhattan", "Bronx", "Queens", "Brooklyn"), ] # Insert remaining boroughs

# Pull, organize, and aggregate necessary data
ezpass_treatment_group <- ezpass_treatment_group[, c("polyline", "week", "median_speed_fps")]
ezpass_treatment_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_treatment_group, na.rm = TRUE)

ezpass_control_group <- ezpass_control_group[, c("polyline", "week", "median_speed_fps")]
ezpass_control_group <- aggregate(median_speed_fps ~ week + polyline, FUN = mean, data = ezpass_control_group, na.rm = TRUE)

# Combine the groups, assigning unique IDs for treated and control observations
ezpass_speeds_synthetic_control_data <- rbind(ezpass_control_group, ezpass_treatment_group)
ezpass_speeds_synthetic_control_data$unit_ID <- as.numeric(factor(ezpass_speeds_synthetic_control_data$polyline))

ezpass_control_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_control_group$polyline])

ezpass_treated_IDs <- unique(ezpass_speeds_synthetic_control_data$unit_ID[ezpass_speeds_synthetic_control_data$polyline %in% ezpass_treatment_group$polyline])

ezpass_speeds_synthetic_control_data$week_num <- as.numeric(format(ezpass_speeds_synthetic_control_data$week, "%Y%m%d"))

ezpass_speeds_synthetic_control_data$D <- ifelse(
  ezpass_speeds_synthetic_control_data$unit_ID %in% ezpass_treated_IDs & ezpass_speeds_synthetic_control_data$week_num >= 20250101,
  1,
  0
)

# Implement Synthetic Control
gsynth_ezpass_statenisland <- gsynth(median_speed_fps ~ D, 
                                 data = ezpass_speeds_synthetic_control_data, 
                                 index = c("unit_ID", "week_num"), 
                                 force = "two-way",    # Unit and Time TWFE
                                 inference = "nonparametric",    # recommended when there are more treated units
                                 estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                 #se = TRUE,
                                 #nboots = 1000,
                                 min.T0 = 7
)

plot(gsynth_ezpass_statenisland$att, type = "l", main = "EZ Pass Synthetic Control", xlab = "Time (weeks)", ylab = "Speed (FPS)")
abline(v = 19, lty = 2, col = "red")

# Plot all of the ATTs from the Placebo Tests
gsynth_ezpass_brooklyn$time <- as.Date(as.character(gsynth_ezpass_brooklyn$time), format = "%Y%m%d")
gsynth_ezpass_queens$time <- as.Date(as.character(gsynth_ezpass_queens$time), format = "%Y%m%d")
gsynth_ezpass_statenisland$time <- as.Date(as.character(gsynth_ezpass_statenisland$time), format = "%Y%m%d")

ezpass_brooklyn <- data.frame(gsynth_ezpass_brooklyn$time, gsynth_ezpass_brooklyn$att)
ezpass_brooklyn$subset <- 1:nrow(ezpass_brooklyn)
ezpass_queens <- data.frame(gsynth_ezpass_queens$time, gsynth_ezpass_queens$att)
ezpass_queens$subset <- 1:nrow(ezpass_queens)
ezpass_statenisland <- data.frame(gsynth_ezpass_statenisland$time, gsynth_ezpass_statenisland$att)
ezpass_statenisland$subset <- 1:nrow(ezpass_statenisland)

png(filename = "../output/EZ_Pass_Placebo_Plot.png", width = 2100, height = 1400, res = 300)
plot(ezpass$gsynth_ezpass.time[1:38], ezpass$gsynth_ezpass.att[1:38], , type = "l", xlab = "Time (weeks)", ylab = "Gap in Speed (FPS)", ylim = c(-20, 10), xlim = c(min(ezpass$gsynth_ezpass.time), max(ezpass$gsynth_ezpass.time)), col = "black", lwd = 2)
lines(ezpass_brooklyn$gsynth_ezpass_brooklyn.time[1:38], ezpass_brooklyn$gsynth_ezpass_brooklyn.att[1:38], type = "l", col = "grey")
lines(ezpass_queens$gsynth_ezpass_queens.time[1:38], ezpass_queens$gsynth_ezpass_queens.att[1:38], type = "l", col = "grey")
lines(ezpass_statenisland$gsynth_ezpass_statenisland.time[1:38], ezpass_statenisland$gsynth_ezpass_statenisland.att[1:38], type = "l", col = "grey")
lines(ezpass$gsynth_ezpass.time[39:47], ezpass$gsynth_ezpass.att[39:47], type = "l", col = "black", lwd = 2)
lines(ezpass_brooklyn$gsynth_ezpass_brooklyn.time[39:47], ezpass_brooklyn$gsynth_ezpass_brooklyn.att[39:47], type = "l", col = "grey")
lines(ezpass_queens$gsynth_ezpass_queens.time[39:47], ezpass_queens$gsynth_ezpass_queens.att[39:47], type = "l", col = "grey")
lines(ezpass_statenisland$gsynth_ezpass_statenisland.time[39:47], ezpass_statenisland$gsynth_ezpass_statenisland.att[39:47], type = "l", col = "grey")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
legend("bottomleft", legend = c("CBD", "Control CBD"), fill = c("black","grey"))
dev.off()

# Note: There is no data for the Bronx and so Placebo tests could not be conducted or for May 16th - June 30th
# The EZ Pass tolls near the Bronx are on bridges and are counted as Queens


# Section 3: Changing Crime Type Treatment Variable for Felonies ----
crime_type_placebo <- rbind(NYC_arrests_cbd, NYC_arrests_non_cbd)

crime_type_placebo$felony <- ifelse(crime_type_placebo$LAW_CAT_CD == "F", 1, 0)
crime_type_placebo$misdemeanor <- ifelse(crime_type_placebo$LAW_CAT_CD == "M", 1, 0)
crime_type_placebo$violations <- ifelse(crime_type_placebo$LAW_CAT_CD == "V", 1, 0)
crime_type_placebo$month_year <- format(as.Date(crime_type_placebo$ARREST_DATE), "%m%Y")

# The Bronx
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Bronx", 1, 0)

# Felonies Aggregation
felony_volume <- aggregate(felony ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
felony_volume$month_year <- as.Date(paste0("01", felony_volume$month_year), "%d%m%Y")
felony_volume$D <- ifelse(felony_volume$within_cbd == 1 & felony_volume$month_year >= as.Date("2025-01-03"), 1, 0)
felony_volume$month_year_num <- as.numeric(format(felony_volume$month_year, "%Y%m%d"))
felony_volume$unit_id <- paste("Precinct", felony_volume$ARREST_PRECINCT, "CBD", felony_volume$within_cbd)

# Implement Synthetic Control
gsynth_felony_bronx <- gsynth(felony ~ D, 
                        data = felony_volume, 
                        index = c("unit_id", "month_year_num"), 
                        force = "two-way",    # Unit and Time TWFE
                        inference = "nonparametric",    # recommended when there are more treated units
                        estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                        #se = TRUE,
                        #nboots = 1000,
                        min.T0 = 24
)

plot(gsynth_felony_bronx$att, type = "l", main = "Bronx Felony Synthetic Control", xlab = "Time (months)", ylab = "Count Felonies")
abline(v = 25, lty = 2, col = "red")

# Brooklyn
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Brooklyn", 1, 0)

# Felonies Aggregation
felony_volume <- aggregate(felony ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
felony_volume$month_year <- as.Date(paste0("01", felony_volume$month_year), "%d%m%Y")
felony_volume$D <- ifelse(felony_volume$within_cbd == 1 & felony_volume$month_year >= as.Date("2025-01-03"), 1, 0)
felony_volume$month_year_num <- as.numeric(format(felony_volume$month_year, "%Y%m%d"))
felony_volume$unit_id <- paste("Precinct", felony_volume$ARREST_PRECINCT, "CBD", felony_volume$within_cbd)

# Implement Synthetic Control
gsynth_felony_brooklyn <- gsynth(felony ~ D, 
                              data = felony_volume, 
                              index = c("unit_id", "month_year_num"), 
                              force = "two-way",    # Unit and Time TWFE
                              inference = "nonparametric",    # recommended when there are more treated units
                              estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                              #se = TRUE,
                              #nboots = 1000,
                              min.T0 = 24
)

plot(gsynth_felony_brooklyn$att, type = "l", main = "Brooklyn Felony Synthetic Control", xlab = "Time (months)", ylab = "Count Felonies")
abline(v = 25, lty = 2, col = "red")

# Queens
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Queens", 1, 0)

# Felonies Aggregation
felony_volume <- aggregate(felony ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
felony_volume$month_year <- as.Date(paste0("01", felony_volume$month_year), "%d%m%Y")
felony_volume$D <- ifelse(felony_volume$within_cbd == 1 & felony_volume$month_year >= as.Date("2025-01-03"), 1, 0)
felony_volume$month_year_num <- as.numeric(format(felony_volume$month_year, "%Y%m%d"))
felony_volume$unit_id <- paste("Precinct", felony_volume$ARREST_PRECINCT, "CBD", felony_volume$within_cbd)

# Implement Synthetic Control
gsynth_felony_queens <- gsynth(felony ~ D, 
                              data = felony_volume, 
                              index = c("unit_id", "month_year_num"), 
                              force = "two-way",    # Unit and Time TWFE
                              inference = "nonparametric",    # recommended when there are more treated units
                              estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                              #se = TRUE,
                              #nboots = 1000,
                              min.T0 = 24
)

plot(gsynth_felony_queens$att, type = "l", main = "Queens Felony Synthetic Control", xlab = "Time (months)", ylab = "Count Felonies")
abline(v = 25, lty = 2, col = "red")

# Staten Island
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Staten_Island", 1, 0)

# Felonies Aggregation
felony_volume <- aggregate(felony ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
felony_volume$month_year <- as.Date(paste0("01", felony_volume$month_year), "%d%m%Y")
felony_volume$D <- ifelse(felony_volume$within_cbd == 1 & felony_volume$month_year >= as.Date("2025-01-03"), 1, 0)
felony_volume$month_year_num <- as.numeric(format(felony_volume$month_year, "%Y%m%d"))
felony_volume$unit_id <- paste("Precinct", felony_volume$ARREST_PRECINCT, "CBD", felony_volume$within_cbd)

# Implement Synthetic Control
gsynth_felony_statenisland <- gsynth(felony ~ D, 
                              data = felony_volume, 
                              index = c("unit_id", "month_year_num"), 
                              force = "two-way",    # Unit and Time TWFE
                              inference = "nonparametric",    # recommended when there are more treated units
                              estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                              #se = TRUE,
                              #nboots = 1000,
                              min.T0 = 24
)

plot(gsynth_felony_statenisland$att, type = "l", main = "Staten Island Felony Synthetic Control", xlab = "Time (months)", ylab = "Count Felonies")
abline(v = 25, lty = 2, col = "red")

gsynth_felony_brooklyn$time <- as.Date(as.character(gsynth_felony_brooklyn$time), format = "%Y%m%d")
gsynth_felony_queens$time <- as.Date(as.character(gsynth_felony_queens$time), format = "%Y%m%d")
gsynth_felony_statenisland$time <- as.Date(as.character(gsynth_felony_statenisland$time), format = "%Y%m%d")
gsynth_felony_bronx$time <- as.Date(as.character(gsynth_felony_bronx$time), format = "%Y%m%d")

# Plot all of the ATTs from the Placebo Tests
png(filename = "../output/Felony_Placebo_Plot.png", width = 2100, height = 1400, res = 300)
plot(gsynth_felony$time, gsynth_felony$att, type = "l", xlab = "Time (months)", ylab = "Gap in Count Felonies", ylim = c(-20, 20), col = "black", lwd = 2)
lines(gsynth_felony_bronx$time, gsynth_felony_bronx$att, type = "l", col = "grey")
lines(gsynth_felony_brooklyn$time, gsynth_felony_brooklyn$att, type = "l", col = "grey")
lines(gsynth_felony_queens$time, gsynth_felony_queens$att, type = "l", col = "grey")
lines(gsynth_felony_statenisland$time, gsynth_felony_statenisland$att, type = "l", col = "grey")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
legend("bottomleft", legend = c("CBD", "Control CBD"), fill = c("black","grey"))
dev.off()


# Section 4: Changing Crime Type Treatment Variables for Misdemeanors ----
# The Bronx
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Bronx", 1, 0)

# Misdemeanor Aggregation
misdemeanor_volume <- aggregate(misdemeanor ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
misdemeanor_volume$month_year <- as.Date(paste0("01", misdemeanor_volume$month_year), "%d%m%Y")
misdemeanor_volume$D <- ifelse(misdemeanor_volume$within_cbd == 1 & misdemeanor_volume$month_year >= as.Date("2025-01-03"), 1, 0)
misdemeanor_volume$month_year_num <- as.numeric(format(misdemeanor_volume$month_year, "%Y%m%d"))
misdemeanor_volume$unit_id <- paste("Precinct", misdemeanor_volume$ARREST_PRECINCT, "CBD", misdemeanor_volume$within_cbd)

# Implement Synthetic Control
gsynth_misdemeanor_bronx <- gsynth(misdemeanor ~ D, 
                                   data = misdemeanor_volume, 
                                   index = c("unit_id", "month_year_num"), 
                                   force = "two-way",    # Unit and Time TWFE
                                   inference = "nonparametric",    # recommended when there are more treated units
                                   estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                   #se = TRUE,
                                   #nboots = 1000,
                                   min.T0 = 24
)

plot(gsynth_misdemeanor_bronx$att, type = "l", main = "Bronx misdemeanor Synthetic Control", xlab = "Time (months)", ylab = "Count Misdemeanor")
abline(v = 25, lty = 2, col = "red")

# Brooklyn
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Brooklyn", 1, 0)

# Misdemeanor Aggregation
misdemeanor_volume <- aggregate(misdemeanor ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
misdemeanor_volume$month_year <- as.Date(paste0("01", misdemeanor_volume$month_year), "%d%m%Y")
misdemeanor_volume$D <- ifelse(misdemeanor_volume$within_cbd == 1 & misdemeanor_volume$month_year >= as.Date("2025-01-03"), 1, 0)
misdemeanor_volume$month_year_num <- as.numeric(format(misdemeanor_volume$month_year, "%Y%m%d"))
misdemeanor_volume$unit_id <- paste("Precinct", misdemeanor_volume$ARREST_PRECINCT, "CBD", misdemeanor_volume$within_cbd)

# Implement Synthetic Control
gsynth_misdemeanor_brooklyn <- gsynth(misdemeanor ~ D, 
                                      data = misdemeanor_volume, 
                                      index = c("unit_id", "month_year_num"), 
                                      force = "two-way",    # Unit and Time TWFE
                                      inference = "nonparametric",    # recommended when there are more treated units
                                      estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                      #se = TRUE,
                                      #nboots = 1000,
                                      min.T0 = 24
)

plot(gsynth_misdemeanor_brooklyn$att, type = "l", main = "Brooklyn misdemeanor Synthetic Control", xlab = "Time (months)", ylab = "Count Misdemeanor")
abline(v = 25, lty = 2, col = "red")

# Queens
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Queens", 1, 0)

# Misdemeanor Aggregation
misdemeanor_volume <- aggregate(misdemeanor ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
misdemeanor_volume$month_year <- as.Date(paste0("01", misdemeanor_volume$month_year), "%d%m%Y")
misdemeanor_volume$D <- ifelse(misdemeanor_volume$within_cbd == 1 & misdemeanor_volume$month_year >= as.Date("2025-01-03"), 1, 0)
misdemeanor_volume$month_year_num <- as.numeric(format(misdemeanor_volume$month_year, "%Y%m%d"))
misdemeanor_volume$unit_id <- paste("Precinct", misdemeanor_volume$ARREST_PRECINCT, "CBD", misdemeanor_volume$within_cbd)

# Implement Synthetic Control
gsynth_misdemeanor_queens <- gsynth(misdemeanor ~ D, 
                                    data = misdemeanor_volume, 
                                    index = c("unit_id", "month_year_num"), 
                                    force = "two-way",    # Unit and Time TWFE
                                    inference = "nonparametric",    # recommended when there are more treated units
                                    estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                    #se = TRUE,
                                    #nboots = 1000,
                                    min.T0 = 24
)

plot(gsynth_misdemeanor_queens$att, type = "l", main = "Queens misdemeanor Synthetic Control", xlab = "Time (months)", ylab = "Count Misdemeanor")
abline(v = 25, lty = 2, col = "red")

# Staten Island
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Staten_Island", 1, 0)

# Misdemeanor Aggregation
misdemeanor_volume <- aggregate(misdemeanor ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
misdemeanor_volume$month_year <- as.Date(paste0("01", misdemeanor_volume$month_year), "%d%m%Y")
misdemeanor_volume$D <- ifelse(misdemeanor_volume$within_cbd == 1 & misdemeanor_volume$month_year >= as.Date("2025-01-03"), 1, 0)
misdemeanor_volume$month_year_num <- as.numeric(format(misdemeanor_volume$month_year, "%Y%m%d"))
misdemeanor_volume$unit_id <- paste("Precinct", misdemeanor_volume$ARREST_PRECINCT, "CBD", misdemeanor_volume$within_cbd)

# Implement Synthetic Control
gsynth_misdemeanor_statenisland <- gsynth(misdemeanor ~ D, 
                                          data = misdemeanor_volume, 
                                          index = c("unit_id", "month_year_num"), 
                                          force = "two-way",    # Unit and Time TWFE
                                          inference = "nonparametric",    # recommended when there are more treated units
                                          estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                          #se = TRUE,
                                          #nboots = 1000,
                                          min.T0 = 24
)

plot(gsynth_misdemeanor_statenisland$att, type = "l", main = "Staten Island misdemeanor Synthetic Control", xlab = "Time (months)", ylab = "Count Misdemeanor")
abline(v = 25, lty = 2, col = "red")

gsynth_misdemeanor_brooklyn$time <- as.Date(as.character(gsynth_misdemeanor_brooklyn$time), format = "%Y%m%d")
gsynth_misdemeanor_queens$time <- as.Date(as.character(gsynth_misdemeanor_queens$time), format = "%Y%m%d")
gsynth_misdemeanor_statenisland$time <- as.Date(as.character(gsynth_misdemeanor_statenisland$time), format = "%Y%m%d")
gsynth_misdemeanor_bronx$time <- as.Date(as.character(gsynth_misdemeanor_bronx$time), format = "%Y%m%d")

# Plot all of the ATTs from the Placebo Tests
png(filename = "../output/Misdemeanor_Placebo_Plot.png", width = 2100, height = 1400, res = 300)
plot(gsynth_misdemeanor$time, gsynth_misdemeanor$att, type = "l", xlab = "Time (months)", ylab = "Gap in Count Misdemeanors", ylim = c(-25, 25), col = "black", lwd = 2)
lines(gsynth_misdemeanor_bronx$time, gsynth_misdemeanor_bronx$att, type = "l", col = "grey")
lines(gsynth_misdemeanor_brooklyn$time, gsynth_misdemeanor_brooklyn$att, type = "l", col = "grey")
lines(gsynth_misdemeanor_queens$time, gsynth_misdemeanor_queens$att, type = "l", col = "grey")
lines(gsynth_misdemeanor_statenisland$time, gsynth_misdemeanor_statenisland$att, type = "l", col = "grey")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
legend("bottomleft", legend = c("CBD", "Control CBD"), fill = c("black","grey"))
dev.off()


# Section 5: Changing Crime Type Treatment Variables for Violations ----
# The Bronx
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Bronx", 1, 0)

# Violations Aggregation
violations_volume <- aggregate(violations ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
violations_volume$month_year <- as.Date(paste0("01", violations_volume$month_year), "%d%m%Y")
violations_volume$D <- ifelse(violations_volume$within_cbd == 1 & violations_volume$month_year >= as.Date("2025-01-03"), 1, 0)
violations_volume$month_year_num <- as.numeric(format(violations_volume$month_year, "%Y%m%d"))
violations_volume$unit_id <- paste("Precinct", violations_volume$ARREST_PRECINCT, "CBD", violations_volume$within_cbd)

# Implement Synthetic Control
gsynth_violations_bronx <- gsynth(violations ~ D, 
                                  data = violations_volume, 
                                  index = c("unit_id", "month_year_num"), 
                                  force = "two-way",    # Unit and Time TWFE
                                  inference = "nonparametric",    # recommended when there are more treated units
                                  estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                  #se = TRUE,
                                  #nboots = 1000,
                                  min.T0 = 24
)

plot(gsynth_violations_bronx$att, type = "l", main = "Bronx violations Synthetic Control", xlab = "Time (months)", ylab = "Count Violations")
abline(v = 25, lty = 2, col = "red")

# Brooklyn
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Brooklyn", 1, 0)

# Violations Aggregation
violations_volume <- aggregate(violations ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
violations_volume$month_year <- as.Date(paste0("01", violations_volume$month_year), "%d%m%Y")
violations_volume$D <- ifelse(violations_volume$within_cbd == 1 & violations_volume$month_year >= as.Date("2025-01-03"), 1, 0)
violations_volume$month_year_num <- as.numeric(format(violations_volume$month_year, "%Y%m%d"))
violations_volume$unit_id <- paste("Precinct", violations_volume$ARREST_PRECINCT, "CBD", violations_volume$within_cbd)

# Implement Synthetic Control
gsynth_violations_brooklyn <- gsynth(violations ~ D, 
                                     data = violations_volume, 
                                     index = c("unit_id", "month_year_num"), 
                                     force = "two-way",    # Unit and Time TWFE
                                     inference = "nonparametric",    # recommended when there are more treated units
                                     estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                     #se = TRUE,
                                     #nboots = 1000,
                                     min.T0 = 24
)

plot(gsynth_violations_brooklyn$att, type = "l", main = "Brooklyn violations Synthetic Control", xlab = "Time (months)", ylab = "Count Violations")
abline(v = 25, lty = 2, col = "red")

# Queens
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Queens", 1, 0)

# Violations Aggregation
violations_volume <- aggregate(violations ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
violations_volume$month_year <- as.Date(paste0("01", violations_volume$month_year), "%d%m%Y")
violations_volume$D <- ifelse(violations_volume$within_cbd == 1 & violations_volume$month_year >= as.Date("2025-01-03"), 1, 0)
violations_volume$month_year_num <- as.numeric(format(violations_volume$month_year, "%Y%m%d"))
violations_volume$unit_id <- paste("Precinct", violations_volume$ARREST_PRECINCT, "CBD", violations_volume$within_cbd)

# Implement Synthetic Control
gsynth_violations_queens <- gsynth(violations ~ D, 
                                   data = violations_volume, 
                                   index = c("unit_id", "month_year_num"), 
                                   force = "two-way",    # Unit and Time TWFE
                                   inference = "nonparametric",    # recommended when there are more treated units
                                   estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                   #se = TRUE,
                                   #nboots = 1000,
                                   min.T0 = 24
)

plot(gsynth_violations_queens$att, type = "l", main = "Queens violations Synthetic Control", xlab = "Time (months)", ylab = "Count Violations")
abline(v = 25, lty = 2, col = "red")

# Staten Island
crime_type_placebo$within_cbd <- ifelse(crime_type_placebo$ARREST_BORO == "Staten_Island", 1, 0)

# Violations Aggregation
violations_volume <- aggregate(violations ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_placebo)
violations_volume$month_year <- as.Date(paste0("01", violations_volume$month_year), "%d%m%Y")
violations_volume$D <- ifelse(violations_volume$within_cbd == 1 & violations_volume$month_year >= as.Date("2025-01-03"), 1, 0)
violations_volume$month_year_num <- as.numeric(format(violations_volume$month_year, "%Y%m%d"))
violations_volume$unit_id <- paste("Precinct", violations_volume$ARREST_PRECINCT, "CBD", violations_volume$within_cbd)

# Implement Synthetic Control
gsynth_violations_statenisland <- gsynth(violations ~ D, 
                                         data = violations_volume, 
                                         index = c("unit_id", "month_year_num"), 
                                         force = "two-way",    # Unit and Time TWFE
                                         inference = "nonparametric",    # recommended when there are more treated units
                                         estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                                         #se = TRUE,
                                         #nboots = 1000,
                                         min.T0 = 24
)

plot(gsynth_violations_statenisland$att, type = "l", main = "Staten Island violations Synthetic Control", xlab = "Time (months)", ylab = "Count Violations")
abline(v = 25, lty = 2, col = "red")

gsynth_violations_brooklyn$time <- as.Date(as.character(gsynth_violations_brooklyn$time), format = "%Y%m%d")
gsynth_violations_queens$time <- as.Date(as.character(gsynth_violations_queens$time), format = "%Y%m%d")
gsynth_violations_statenisland$time <- as.Date(as.character(gsynth_violations_statenisland$time), format = "%Y%m%d")
gsynth_violations_bronx$time <- as.Date(as.character(gsynth_violations_bronx$time), format = "%Y%m%d")

# Plot all of the ATTs from the Placebo Tests
png(filename = "../output/Violations_Placebo_Plot.png", width = 2100, height = 1400, res = 300)
plot(gsynth_violations$time, gsynth_violations$att, type = "l", xlab = "Time (months)", ylab = "Gap in Count Violations", ylim = c(-6, 6), col = "black", lwd = 2)
lines(gsynth_violations_bronx$time, gsynth_violations_bronx$att, type = "l", col = "grey")
lines(gsynth_violations_brooklyn$time, gsynth_violations_brooklyn$att, type = "l", col = "grey")
lines(gsynth_violations_queens$time, gsynth_violations_queens$att, type = "l", col = "grey")
lines(gsynth_violations_statenisland$time, gsynth_violations_statenisland$att, type = "l", col = "grey")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
legend("bottomleft", legend = c("CBD", "Control CBD"), fill = c("black","grey"))
dev.off()

