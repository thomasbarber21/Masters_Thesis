# Description: Subset data to the Central Business District of Manhattan for Air Quality
# and EZ Pass Data. Utilize synthetic control to create a counterfactual for Manhattan 
# in the post congestion charge era. Also implements synthetic control for crime types

# Section 1: Separate Air Quality Collection Sites within Manhattan CBD and Other Boroughs ----
air_quality$airquality_synthetic_control <- rbind(air_quality$may2024, air_quality$june2024, air_quality$july2024, air_quality$aug2024, air_quality$sept2024, air_quality$oct2024, air_quality$nov2024, air_quality$dec2024, air_quality$jan2025, air_quality$feb2025, air_quality$march2025, air_quality$april2025, air_quality$may2025, air_quality$june2025)

air_quality[["airquality_synthetic_control"]]$Within_CBD <- ifelse(air_quality[["airquality_synthetic_control"]]$SiteID %in% c("36061NY10130", "36061NY09929", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454"), 1, 0)

# Utilizing site location maps from NYC Open Portal, separate into CBD and non CBD
air_quality$airquality_CBD <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == 1,]
air_quality$airquality_nonCBD <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == 0,]


# Section 2: Separate EZ Pass Data into Manhattan CBD and non CBD ----
ez_pass$ezpass_speeds_synthetic_control <-  rbind(ez_pass_aggregated$sept2024, ez_pass_aggregated$oct2024, ez_pass_aggregated$nov2024, ez_pass_aggregated$dec2024, ez_pass_aggregated$jan2025, ez_pass_aggregated$feb2025, ez_pass_aggregated$march2025, ez_pass_aggregated$april2025, ez_pass_aggregated$may2025, ez_pass_aggregated$july2025, ez_pass_aggregated$aug2025)

# First break up data into Manhattan and non Manhattan Data
ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD <- ifelse(ez_pass[["ezpass_speeds_synthetic_control"]]$borough == "Manhattan" , 1, 0)

ez_pass$ezpass_speeds_Manhattan <- ez_pass[["ezpass_speeds_synthetic_control"]][ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD == 1,]
ez_pass$ezpass_speeds_nonManhattan <- ez_pass[["ezpass_speeds_synthetic_control"]][ez_pass[["ezpass_speeds_synthetic_control"]]$Within_CBD == 0,]

# Utilize Polyline data and GooglePolyline package to break polyline into latitudes and longitudes
lat <- numeric(length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline))
lon <- numeric(length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline))

for (i in 1:length(ez_pass[["ezpass_speeds_Manhattan"]]$polyline)) {
  ez_pass$decoded <- decode(ez_pass[["ezpass_speeds_Manhattan"]]$polyline[i])
  
  lat[i] <- ez_pass[["decoded"]][[1]]$lat[1]
  lon[i] <- ez_pass[["decoded"]][[1]]$lon[1]
}
ez_pass[["ezpass_speeds_Manhattan"]]$lat <- lat
ez_pass[["ezpass_speeds_Manhattan"]]$lon <- lon

lat <- numeric(length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline))
lon <- numeric(length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline))

for (i in 1:length(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline)) {
  ez_pass$decoded <- decode(ez_pass[["ezpass_speeds_nonManhattan"]]$polyline[i])
  
  lat[i] <- ez_pass[["decoded"]][[1]]$lat[1]
  lon[i] <- ez_pass[["decoded"]][[1]]$lon[1]
}

ez_pass[["ezpass_speeds_nonManhattan"]]$lat <- lat
ez_pass[["ezpass_speeds_nonManhattan"]]$lon <- lon

# Utilize shapefile of Manhattan's CBD to extract values within the CBD
ez_pass$ez_pass_sf <- st_as_sf(ez_pass[["ezpass_speeds_Manhattan"]], coords = c("lon", "lat"), crs = 4326)
ez_pass$inside_cbd <- st_within(ez_pass[["ez_pass_sf"]], manhattan_cbd, sparse = FALSE)

ez_pass$ezpass_speeds_CBD <- ez_pass[["ez_pass_sf"]][ez_pass[["inside_cbd"]], ]
ez_pass$ezpass_speeds_nonCBD_Manhattan <- ez_pass[["ez_pass_sf"]][!(ez_pass[["inside_cbd"]]), ]

# Drop geometries and bind data into CBD and non CBD dataframes
ez_pass[["ezpass_speeds_CBD"]] <- st_drop_geometry(ez_pass[["ezpass_speeds_CBD"]])
ez_pass[["ezpass_speeds_nonCBD_Manhattan"]] <- st_drop_geometry(ez_pass[["ezpass_speeds_nonCBD_Manhattan"]])
ez_pass[["ezpass_speeds_nonManhattan"]] <- ez_pass[["ezpass_speeds_nonManhattan"]][, -c(6,7)]

ez_pass[["ezpass_speeds_nonCBD"]] <- rbind(ez_pass[["ezpass_speeds_nonCBD_Manhattan"]], ez_pass[["ezpass_speeds_nonManhattan"]])

ez_pass[["ezpass_speeds_nonCBD"]]$week <- as.Date(ez_pass[["ezpass_speeds_nonCBD"]]$week)


# Section 3: Implement synthetic control for Air Quality Data ----
# Combine CBD and non-CBD data with an indicator
air_quality[["airquality_CBD"]]$Within_CBD <- "Yes"
air_quality[["airquality_nonCBD"]]$Within_CBD <- "No"
air_quality$airquality_synthetic_control <- rbind(air_quality[["airquality_CBD"]], air_quality[["airquality_nonCBD"]])

# IMPORTANT: If you include more data later on with more months you will need to edit this to prevent months from different years being included in pretreatment period
air_quality[["airquality_synthetic_control"]]$Year <- ifelse(air_quality[["airquality_synthetic_control"]]$month %in% c("may", "june", "july", "aug", "sept", "oct", "nov", "dec"), 2024, 2025)

air_quality[["airquality_synthetic_control"]]$month_num <- air_quality[["airquality_synthetic_control"]]$month
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "may"] <- 202405
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "june"] <- 202406
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "july"] <- 202407
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "aug"] <- 202408
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "sept"] <- 202409
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "oct"] <- 202410
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "nov"] <- 202411
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "dec"] <- 202412
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "jan"] <- 202501
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "feb"] <- 202502
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "march"] <- 202503
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "april"] <- 202504
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "may25"] <- 202505
air_quality[["airquality_synthetic_control"]]$month_num[air_quality[["airquality_synthetic_control"]]$month == "june25"] <- 202506
air_quality[["airquality_synthetic_control"]]$month_num <- as.numeric(air_quality[["airquality_synthetic_control"]]$month_num)

# Had issues with balanced panels when using synth package, so utilize gsynth

# Split data into treatment and control groups, organize and aggregate data
treatment_group_AQ <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == "Yes", ]
control_group_AQ <- air_quality[["airquality_synthetic_control"]][air_quality[["airquality_synthetic_control"]]$Within_CBD == "No", ]

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

AQ_treated_sites <- c("36061NY10130", "36061NY09734", "36061NY08653", "36061NY08552", "36061NY08454") # had to remove one site due to lack of data
AQ_synthetic_control_data$D <- ifelse(
  AQ_synthetic_control_data$SiteID %in% AQ_treated_sites & AQ_synthetic_control_data$month_num >= 202501,
  1,
  0
)

# Utilize gsynth to implement synthetic control
gsynth_AQ <- gsynth(Value ~ D, 
                     data = AQ_synthetic_control_data, 
                     index = c("unit_ID", "month_num"), 
                     force = "two-way",    # Unit and Time TWFE
                     inference = "nonparametric",    # recommended when there are more treated units
                     estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                     #se = TRUE,
                     #nboots = 1000,
                     min.T0 = 7
)

time_date <- rownames(gsynth_AQ$Y.tr)
time_date <- as.yearmon(time_date, "%Y%m")
time_date <- as.Date(time_date)

observed_outcome <- rowMeans(gsynth_AQ$Y.tr, na.rm = TRUE)
synthetic_outcome <- rowMeans(gsynth_AQ$Y.ct, na.rm = TRUE)

png(filename = "../output/Air_Quality_Synthetic_Control.png", width = 2100, height = 1400, res = 300)
plot(time_date, observed_outcome, type = "l", xlab = "Time (months)", ylab = "PM2.5 Value")
lines(time_date, synthetic_outcome, col = "red", type = "l", lty = 2)
abline(v = time_date[9])
legend("topleft", legend = c("CBD", "Synthetic CBD"), fill = c("black", "red"))
dev.off()

# Store ATTs in Data Table
time_values_AQ <- unique(AQ_synthetic_control_data$month_num)
post_AQ <- time_values_AQ[gsynth_AQ$post]
Air_Quality_synthetic_control_table <- data.frame(
  Time = unique(AQ_synthetic_control_data$month_num),
  ATT = gsynth_AQ$att
)
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202405"] <- "May 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202406"] <- "June 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202407"] <- "July 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202408"] <- "Aug 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202409"] <- "Sept 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202410"] <- "Oct 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202411"] <- "Nov 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202412"] <- "Dec 2024"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202501"] <- "Jan 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202502"] <- "Feb 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202503"] <- "March 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202504"] <- "April 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202505"] <- "May 2025"
Air_Quality_synthetic_control_table$Time[Air_Quality_synthetic_control_table$Time == "202506"] <- "June 2025"


# Section 4: Implement Synthetic Control for EZ Pass Data ----

# Split data into treatment and control groups
ezpass_treatment_group <- ez_pass[["ezpass_speeds_CBD"]]
ezpass_control_group <- ez_pass[["ezpass_speeds_nonCBD"]]

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

# Implement gsynth
gsynth_ezpass <- gsynth(median_speed_fps ~ D, 
                     data = ezpass_speeds_synthetic_control_data, 
                     index = c("unit_ID", "week_num"), 
                     force = "two-way",    # Unit and Time TWFE
                     inference = "nonparametric",    # recommended when there are more treated units
                     estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                     #se = TRUE,
                     #nboots = 1000,
                     min.T0 = 7
)

time_label <- rownames(gsynth_ezpass$Y.tr)
time_label <- as.Date(time_label, "%Y%m%d")
time_label <- data.frame(time_label)
time_label$subset <- 1:nrow(time_label)

observed_outcome <- rowMeans(gsynth_ezpass$Y.tr, na.rm = TRUE)
observed_outcome <- data.frame(observed_outcome)
observed_outcome$subset <- 1:nrow(observed_outcome)
synthetic_outcome <- rowMeans(gsynth_ezpass$Y.ct, na.rm = TRUE)
synthetic_outcome <- data.frame(synthetic_outcome)
synthetic_outcome$subset <- 1:nrow(synthetic_outcome)

png(filename = "../output/EZPass_Speeds_Synthetic_Control.png", width = 2100, height = 1400, res = 300)
plot(time_label$time_label[1:38], observed_outcome$observed_outcome[1:38], type = "l", xlab = "Time (weeks)", ylab = "Speed (fps)", xlim = c(min(time_label$time_label), max(time_label$time_label)), ylim = c(min(synthetic_outcome$synthetic_outcome), max(synthetic_outcome$synthetic_outcome)))
lines(time_label$time_label[1:38], synthetic_outcome$synthetic_outcome[1:38], col = "red", type = "l", lty = 2)
lines(time_label$time_label[39:47], observed_outcome$observed_outcome[39:47], col = "black", type = "l")
lines(time_label$time_label[39:47], synthetic_outcome$synthetic_outcome[39:47], col = "red", type = "l", lty = 2)
abline(v = as.Date("2025-01-06"))
legend("bottomleft", legend = c("CBD", "Synthetic CBD"), fill = c("black", "red"))
dev.off()

# Store ATTs in Data Table
ezpass_time_values <- unique(ezpass_speeds_synthetic_control_data$week_num)
ezpass_post <- ezpass_time_values[gsynth_ezpass$post]
EZPass_Speeds_synthetic_control_table <- data.frame(
  Week_Number = unique(ezpass_speeds_synthetic_control_data$week_num),
  ATT = gsynth_ezpass$att
)

EZPass_Speeds_synthetic_control_table$Week_Number <- as.character(EZPass_Speeds_synthetic_control_table$Week_Number)
EZPass_Speeds_synthetic_control_table$Week_Number <- as.Date(EZPass_Speeds_synthetic_control_table$Week_Number, "%Y%m%d")

EZPass_Speeds_synthetic_control_table$Time <- format(EZPass_Speeds_synthetic_control_table$Week_Number, "%b %Y")
EZPass_Speeds_ATT_agg <- aggregate(ATT ~ Time, FUN = mean, data = EZPass_Speeds_synthetic_control_table)

EZPass_Speeds_ATT_agg$Date <- as.Date(paste("01", EZPass_Speeds_ATT_agg$Time), format = "%d %b %Y")
EZPass_Speeds_ATT_agg <- EZPass_Speeds_ATT_agg[order(EZPass_Speeds_ATT_agg$Date), ]
EZPass_Speeds_ATT_agg <- EZPass_Speeds_ATT_agg[1:2]


# Section 5: Implement Synthetic Control for Crime Type Data - Felonies ----
# Break data into CBD vs Non-CBD
# Utilize shapefile of Manhattan's CBD to extract values within the CBD
manhattan_cbd <- st_transform(manhattan_cbd, st_crs(NYC_arrests[["arrests_sf"]]))
NYC_arrests_cbd <- st_within(NYC_arrests[["arrests_sf"]]$geometry, manhattan_cbd, sparse = FALSE)

# Extract True/False Matrix and index on original data to obtain CBD and non-CBD data
within_cbd <- NYC_arrests_cbd[, 1]
NYC_arrests_cbd <- NYC_arrests[["arrests_sf"]][within_cbd, ]

NYC_arrests_non_cbd <- NYC_arrests[["arrests_sf"]][!within_cbd, ]

NYC_arrests_cbd <- st_drop_geometry(NYC_arrests_cbd)
NYC_arrests_non_cbd <- st_drop_geometry(NYC_arrests_non_cbd)

# Create Arrests and CBD binary column
NYC_arrests_cbd$Arrests <- 1
NYC_arrests_non_cbd$Arrests <- 1

names(NYC_arrests_cbd)[which(names(NYC_arrests_cbd) == "Manhattan")] <- "within_cbd"
names(NYC_arrests_non_cbd)[which(names(NYC_arrests_non_cbd) == "Manhattan")] <- "within_cbd"
NYC_arrests_non_cbd$within_cbd <- 0

crime_type_dataset <- rbind(NYC_arrests_cbd, NYC_arrests_non_cbd)

crime_type_dataset$felony <- ifelse(crime_type_dataset$LAW_CAT_CD == "F", 1, 0)
crime_type_dataset$misdemeanor <- ifelse(crime_type_dataset$LAW_CAT_CD == "M", 1, 0)
crime_type_dataset$violations <- ifelse(crime_type_dataset$LAW_CAT_CD == "V", 1, 0)
crime_type_dataset$month_year <- format(as.Date(crime_type_dataset$ARREST_DATE), "%m%Y")

# Aggregate by Crime Type
felony_volume <- aggregate(felony ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_dataset)
felony_volume$month_year <- as.Date(paste0("01", felony_volume$month_year), "%d%m%Y")
felony_volume$D <- ifelse(felony_volume$within_cbd == 1 & felony_volume$month_year >= as.Date("2025-01-03"), 1, 0)
felony_volume$month_year_num <- as.numeric(format(felony_volume$month_year, "%Y%m%d"))
felony_volume$unit_id <- paste("Precinct", felony_volume$ARREST_PRECINCT, "CBD", felony_volume$within_cbd)

# Implement gsythn for felonies
gsynth_felony <- gsynth(felony ~ D, 
                        data = felony_volume, 
                        index = c("unit_id", "month_year_num"), 
                        force = "two-way",    # Unit and Time TWFE
                        inference = "nonparametric",    # recommended when there are more treated units
                        estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                        #se = TRUE,
                        #nboots = 1000,
                        min.T0 = 24
)

time_label <- rownames(gsynth_felony$Y.tr)
time_label <- as.Date(time_label, "%Y%m%d")

observed_outcome <- rowMeans(gsynth_felony$Y.tr, na.rm = TRUE)
synthetic_outcome <- rowMeans(gsynth_felony$Y.ct, na.rm = TRUE)

png(filename = "../output/Felony_Synthetic_Control.png", width = 2100, height = 1400, res = 300)
plot(time_label, observed_outcome, type = "l", xlab = "Time (weeks)", ylab = "Count of Felonies")
lines(time_label, synthetic_outcome, col = "red", type = "l", lty = 2)
abline(v = time_label[time_label == as.Date("2025-01-01")])
legend("topleft", legend = c("CBD", "Synthetic CBD"), fill = c("black", "red"))
dev.off()

# Store in table
felony_time_values <- unique(felony_volume$month_year_num)
felony_post <- felony_time_values[gsynth_felony$post]
Felonies_synthetic_control_table <- data.frame(
  Month_Number = unique(felony_volume$month_year_num),
  ATT = gsynth_felony$att
)

Felonies_synthetic_control_table$Month_Number <- as.character(Felonies_synthetic_control_table$Month_Number)
Felonies_synthetic_control_table$Month_Number <- as.Date(Felonies_synthetic_control_table$Month_Number, "%Y%m%d")

Felonies_synthetic_control_table$Time <- format(Felonies_synthetic_control_table$Month_Number, "%b %Y")
Felonies_synthetic_control_table <- Felonies_synthetic_control_table[order(Felonies_synthetic_control_table$Month_Number),]
Felonies_synthetic_control_table <- Felonies_synthetic_control_table[, c("Time", "ATT")]


# Section 6: Implement Synthetic Control for Crime Type Data - Misdemeanors ----
# Aggregate by Crime Type
misdemeanor_volume <- aggregate(misdemeanor ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_dataset)
misdemeanor_volume$month_year <- as.Date(paste0("01", misdemeanor_volume$month_year), "%d%m%Y")
misdemeanor_volume$D <- ifelse(misdemeanor_volume$within_cbd == 1 & misdemeanor_volume$month_year >= as.Date("2025-01-03"), 1, 0)
misdemeanor_volume$month_year_num <- as.numeric(format(misdemeanor_volume$month_year, "%Y%m%d"))
misdemeanor_volume$unit_id <- paste("Precinct", misdemeanor_volume$ARREST_PRECINCT, "CBD", misdemeanor_volume$within_cbd)

# Implement gsythn for misdemeanors
gsynth_misdemeanor <- gsynth(misdemeanor ~ D, 
                        data = misdemeanor_volume, 
                        index = c("unit_id", "month_year_num"), 
                        force = "two-way",    # Unit and Time TWFE
                        inference = "nonparametric",    # recommended when there are more treated units
                        estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                        #se = TRUE,
                        #nboots = 1000,
                        min.T0 = 24
)

time_label <- rownames(gsynth_misdemeanor$Y.tr)
time_label <- as.Date(time_label, "%Y%m%d")

observed_outcome <- rowMeans(gsynth_misdemeanor$Y.tr, na.rm = TRUE)
synthetic_outcome <- rowMeans(gsynth_misdemeanor$Y.ct, na.rm = TRUE)

png(filename = "../output/Misdemeanor_Synthetic_Control.png", width = 2100, height = 1400, res = 300)
plot(time_label, observed_outcome, type = "l", xlab = "Time (weeks)", ylab = "Count of Misdemeanors")
lines(time_label, synthetic_outcome, col = "red", type = "l", lty = 2)
abline(v = time_label[time_label == as.Date("2025-01-01")])
legend("topleft", legend = c("CBD", "Synthetic CBD"), fill = c("black", "red"))
dev.off()

# Store in table
misdemeanor_time_values <- unique(misdemeanor_volume$month_year_num)
misdemeanor_post <- misdemeanor_time_values[gsynth_misdemeanor$post]
Misdemeanor_synthetic_control_table <- data.frame(
  Month_Number = unique(misdemeanor_volume$month_year_num),
  ATT = gsynth_misdemeanor$att
)

Misdemeanor_synthetic_control_table$Month_Number <- as.character(Misdemeanor_synthetic_control_table$Month_Number)
Misdemeanor_synthetic_control_table$Month_Number <- as.Date(Misdemeanor_synthetic_control_table$Month_Number, "%Y%m%d")

Misdemeanor_synthetic_control_table$Time <- format(Misdemeanor_synthetic_control_table$Month_Number, "%b %Y")
Misdemeanor_synthetic_control_table <- Misdemeanor_synthetic_control_table[order(Misdemeanor_synthetic_control_table$Month_Number),]
Misdemeanor_synthetic_control_table <- Misdemeanor_synthetic_control_table[, c("Time", "ATT")]


# Section 7: Implement Synthetic Control for Crime Type Data - Violations ----
# Aggregate by Crime Type
violations_volume <- aggregate(violations ~ month_year + within_cbd + ARREST_PRECINCT, FUN = sum, data = crime_type_dataset)
violations_volume$month_year <- as.Date(paste0("01", violations_volume$month_year), "%d%m%Y")
violations_volume$D <- ifelse(violations_volume$within_cbd == 1 & violations_volume$month_year >= as.Date("2025-01-03"), 1, 0)
violations_volume$month_year_num <- as.numeric(format(violations_volume$month_year, "%Y%m%d"))
violations_volume$unit_id <- paste("Precinct", violations_volume$ARREST_PRECINCT, "CBD", violations_volume$within_cbd)

# Implement gsythn for violations
gsynth_violations <- gsynth(violations ~ D, 
                            data = violations_volume, 
                            index = c("unit_id", "month_year_num"), 
                            force = "two-way",    # Unit and Time TWFE
                            inference = "nonparametric",    # recommended when there are more treated units
                            estimator = "mc",    # Matrix completion model fits better for fewer/noisy data points
                            #se = TRUE,
                            #nboots = 1000,
                            min.T0 = 24
)

time_label <- rownames(gsynth_violations$Y.tr)
time_label <- as.Date(time_label, "%Y%m%d")

observed_outcome <- rowMeans(gsynth_violations$Y.tr, na.rm = TRUE)
synthetic_outcome <- rowMeans(gsynth_violations$Y.ct, na.rm = TRUE)

png(filename = "../output/Violations_Synthetic_Control.png", width = 2100, height = 1400, res = 300)
plot(time_label, observed_outcome, type = "l", xlab = "Time (weeks)", ylab = "Count of Violations")
lines(time_label, synthetic_outcome, col = "red", type = "l", lty = 2)
abline(v = time_label[time_label == as.Date("2025-01-01")])
legend("topleft", legend = c("CBD", "Synthetic CBD"), fill = c("black", "red"))
dev.off()

# Store in table
violation_time_values <- unique(violations_volume$month_year_num)
violation_post <- violation_time_values[gsynth_violations$post]
Violations_synthetic_control_table <- data.frame(
  Month_Number = unique(violations_volume$month_year_num),
  ATT = gsynth_violations$att
)

Violations_synthetic_control_table$Month_Number <- as.character(Violations_synthetic_control_table$Month_Number)
Violations_synthetic_control_table$Month_Number <- as.Date(Violations_synthetic_control_table$Month_Number, "%Y%m%d")

Violations_synthetic_control_table$Time <- format(Violations_synthetic_control_table$Month_Number, "%b %Y")
Violations_synthetic_control_table <- Violations_synthetic_control_table[order(Violations_synthetic_control_table$Month_Number),]
Violations_synthetic_control_table <- Violations_synthetic_control_table[, c("Time", "ATT")]


# Section 8: Plot ATTs of Gsynth implementation ----

# Air Quality
gsynth_AQ$time <- paste0(gsynth_AQ$time, "01")
gsynth_AQ$time <- as.Date(as.character(gsynth_AQ$time), format = "%Y%m%d")
png(filename = "../output/Air_Quality_ATT.png", width = 2100, height = 1400, res = 300)
plot(gsynth_AQ$time, gsynth_AQ$att, type = "l", lwd = 2, xlab = "Month", ylab = "ATT")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
dev.off()

# EZ Pass
gsynth_ezpass$time <- as.Date(as.character(gsynth_ezpass$time), format = "%Y%m%d")
ezpass <- data.frame(gsynth_ezpass$time, gsynth_ezpass$att)
ezpass$subset <- 1:nrow(ezpass)

png(filename = "../output/EZ_Pass_ATT.png", width = 2100, height = 1400, res = 300)
plot(gsynth_ezpass$time[1:38], gsynth_ezpass$att[1:38], type = "l", lwd = 2, xlab = "Week", xlim = c(min(ezpass$gsynth_ezpass.time), max(ezpass$gsynth_ezpass.time)), ylab = "ATT")
lines(gsynth_ezpass$time[39:47], gsynth_ezpass$att[39:47], type = "l", lwd = 2)
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
dev.off()


# Felonies
gsynth_felony$time <- as.Date(as.character(gsynth_felony$time), format = "%Y%m%d")
png(filename = "../output/Felony_ATT.png", width = 2100, height = 1400, res = 300)
plot(gsynth_felony$time, gsynth_felony$att, type = "l", lwd = 2, xlab = "Month", ylab = "ATT")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
dev.off()


# Misdemeanors
gsynth_misdemeanor$time <- as.Date(as.character(gsynth_misdemeanor$time), format = "%Y%m%d")
png(filename = "../output/Misdemeanor_ATT.png", width = 2100, height = 1400, res = 300)
plot(gsynth_misdemeanor$time, gsynth_misdemeanor$att, type = "l", lwd = 2, xlab = "Month", ylab = "ATT")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
dev.off()


# Violations
gsynth_violations$time <- as.Date(as.character(gsynth_violations$time), format = "%Y%m%d")
png(filename = "../output/Violations_ATT.png", width = 2100, height = 1400, res = 300)
plot(gsynth_violations$time, gsynth_violations$att, type = "l", lwd = 2, xlab = "Month", ylab = "ATT")
abline(v = as.Date("2025-01-01"), lty = 2, col = "red")
abline(h = 0, lty = 2)
dev.off()

