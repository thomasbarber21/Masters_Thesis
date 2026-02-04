# Description: Use the uploaded data that has been cleaned to plot necessary visualizations

# Plot 1: Outline of Congestion Charge ----

# Plot the data
manhattan_cbd_crs <- st_transform(manhattan_cbd, st_crs(boroughs))
png(filename = "../output/Borough_Outline.png", width = 1900, height = 1400, res = 300)
plot(stgeo, col = 'lightgray')
plot(st_geometry(manhattan_cbd_crs), add = TRUE, col = 'red', border = 'black')
legend("topleft", legend = "Manhattan CBD", col = "red", fill = 'red', title = "Congestion Charge Implemented")
dev.off()


# Plot 2: Initial Plots of PM2.5 in NYC ----

# Create a png displaying PM 2.5 data in NYC from Oct 2024 - Feb 2025
png(filename = "../output/old_visuals/PM2.5_NYC.png", width = 800, height = 600)

plot(air_quality[["air_quality_aggregated"]]$week, air_quality[["air_quality_aggregated"]]$Value, type = "l", col = "black", xlab = "Date (Week)", ylab = "PM2.5 Value", cex.lab = 1.5, cex.axis = 1.5, main = "PM 2.5 in NYC", cex.main = 2)
air_quality$air_quality_pre <- subset(air_quality[["air_quality_aggregated"]], week < as.Date("2025-01-03"))
air_quality$air_quality_post <- subset(air_quality[["air_quality_aggregated"]], week >= as.Date("2025-01-03"))

# Create models to plot trend lines
air_quality$model_pre <- lm(Value ~ week, data = air_quality[["air_quality_pre"]])
air_quality$pred_pre_AQ <- predict(air_quality$model_pre)
air_quality$model_post <- lm(Value ~ week, data = air_quality[["air_quality_post"]])
air_quality$pred_post_AQ <- predict(air_quality$model_post)
lines(air_quality$air_quality_pre$week, air_quality$pred_pre_AQ, col = "red", lwd = 2)
lines(air_quality$air_quality_post$week, air_quality$pred_post_AQ, col = "red", lwd = 2)
abline(v = implementation_date, lty = 2, col = "red")

dev.off()


# Plot 3: Speed captured by EZ Pass Tolls

first_data <- subset(ez_pass_aggregated[["ezpass_weekly_averages"]], week_number <= 46)
second_data <- subset(ez_pass_aggregated[["ezpass_weekly_averages"]], week_number > 46)

# Create a png file displaying average speed in frames per second captured by NYC EZ Pass Tolls
png(filename = "../output/old_visuals/EZ_Pass_Speeds.png", width = 800, height = 600)

plot(first_data$week, first_data$median_speed_fps, type = "l", col = "black", xlab = "Date (Week)", ylab = "Average Speed (FPS)", xlim = c(min(ez_pass_aggregated[["ezpass_weekly_averages"]]$week), max(ez_pass_aggregated[["ezpass_weekly_averages"]]$week)), cex.lab = 1.5, cex.axis = 1.5, main = "Average Speed Captured by EZ Pass Tolls", cex.main = 2)
lines(second_data$week, second_data$median_speed_fps)
ez_pass_aggregated$speed_pre <- subset(ez_pass_aggregated[["ezpass_weekly_averages"]], week < as.Date("2025-01-03"))
ez_pass_aggregated$speed_post <- subset(ez_pass_aggregated[["ezpass_weekly_averages"]], week >= as.Date("2025-01-03"))

# Create models to plot trend lines
ez_pass_aggregated$model1_pre <- lm(median_speed_fps ~ week, data = ez_pass_aggregated[["speed_pre"]])
ez_pass_aggregated$pred_pre_speed <- predict(ez_pass_aggregated$model1_pre)
ez_pass_aggregated$model1_post <- lm(median_speed_fps ~ week, data = ez_pass_aggregated[["speed_post"]])
ez_pass_aggregated$pred_post_speed <- predict(ez_pass_aggregated$model1_post)
lines(ez_pass_aggregated$speed_pre$week, ez_pass_aggregated$pred_pre_speed, col = "red", lwd = 2)
lines(ez_pass_aggregated$speed_post$week, ez_pass_aggregated$pred_post_speed, col = "red", lwd = 2)
abline(v = implementation_date, lty = 2, col = "red")

dev.off()


# Plot 4: Initial Demand Plots Post Congestion Charge ----

# Create a png file, specify parameters and multi-graph layout
png(filename = "../output/old_visuals/Daily_Ridership.png", width = 1000, height = 800)
layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 7, 7), nrow = 3, byrow = TRUE), heights = c(1, 1, 1, 0.5), widths = c(1, 1, 1, 1))
par(oma = c(2, 2, 4, 2))
par(mar = c(4, 6, 4, 6))

type_of_transport <- c("Subways", "Busses", "Metro_North", "Access_A_Ride", "Bridges_Tunnels", "Staten_Island_railroad")
colors <- c("black", "pink", "blue", "green", "orange", "purple")
ylabs <- c("Mean Ridership", "Mean Ridership", "Mean Ridership", "Mean Ridership", "Mean Traffic", "Mean Ridership")

# Plots each mode of transportation and a vertical line for implementation date of congestion charge
for (i in 1:length(type_of_transport)) {
  plot(MTA_ridership_data[["weekly_averages"]]$week, MTA_ridership_data[["weekly_averages"]][[type_of_transport[i]]], type = "l", col = colors[i], xlab = "Date (Week)", ylab = ylabs[i], cex.lab = 1.5, cex.axis = 1.5)
  abline(v = implementation_date, lty = 2, col = "red")
}

# Define parameters and create an empty plot, which is filled by the legend
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", horiz = TRUE, legend = c("Subway", "Bus", "Metro North", "Access a Ride", "Bridge/Tunnel Traffic", "Staten Island Railway"), fill = c("black", "pink", "blue", "green", "orange", "purple"), cex = 1.5, title = "Mode of Transportation")

# main title of plot
mtext("Public Transportation Ridership in NYC", side = 3, line = 0.5, font = 1, cex = 2, outer = TRUE)

dev.off()


# Plot 5: Weekly Arrest Frequency per Borough ----

# Create a png file and set parameters
png(filename = "../output/old_visuals/Arrests_by_Borough.png", width = 1000, height = 800)
layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2), heights = c(1, 1, 1, 0.5), widths = c(1, 1, 1, 1))
layout.show()
par(oma = c(2, 2, 4, 2))
par(mar = c(4, 6, 4, 6))

# Plot the Data
NYC_arrests$borough_crime_list <- list(Manhattan = NYC_arrests$Manhattan_crime, Bronx = NYC_arrests$Bronx_crime, Brooklyn = NYC_arrests$Brooklyn_crime, Queens = NYC_arrests$Queens_crime, StatenIsland = NYC_arrests$StatenIsland_crime)
colors_borough <- c("black", "pink", "blue", "green", "purple")
for (i in 1:length(NYC_arrests[["borough_crime_list"]])) {
  NYC_arrests$crime_data <- NYC_arrests[["borough_crime_list"]][[i]]
  week_subset <- NYC_arrests[["crime_data"]]$week[-c(1, length(NYC_arrests[["crime_data"]]$week))]
  arrest_subset <- NYC_arrests[["crime_data"]]$Arrests[-c(1, length(NYC_arrests[["crime_data"]]$Arrests))]
  plot(week_subset, arrest_subset, type = "l", col = colors_borough[i], xlab = "Date (Week)", ylab = "Arrests", cex.lab = 1.5, cex.axis = 1.5)
  abline(v = implementation_date, lty = 2, col = "red")
  
  NYC_arrests$pre_data <- subset(NYC_arrests[["crime_data"]], week < as.Date("2025-01-03"))
  NYC_arrests$post_data <- subset(NYC_arrests[["crime_data"]], week >= as.Date("2025-01-03"))
  
  # Create models to plot trend lines
  NYC_arrests$model2_pre <- lm(Arrests ~ week, data = NYC_arrests[["pre_data"]])
  NYC_arrests$pred_pre <- predict(NYC_arrests[["model2_pre"]])
  
  NYC_arrests$model2_post <- lm(Arrests ~ week, data = NYC_arrests[["post_data"]])
  NYC_arrests$pred_post <- predict(NYC_arrests[["model2_post"]])
  
  lines(NYC_arrests[["pre_data"]]$week, NYC_arrests[["pred_pre"]], col = "red", lwd = 2)
  lines(NYC_arrests[["post_data"]]$week, NYC_arrests[["pred_post"]], col = "red", lwd = 2)
}

# Define parameters and create an empty plot, which is filled by the legend
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", horiz = TRUE, legend = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island"), fill = c("black", "pink", "blue", "green", "purple"), cex = 1.25, title = "Borough")

# main title of plot
mtext("Arrests by Borough", side = 3, line = 0.5, font = 1, cex = 2, outer = TRUE)

dev.off()


# Plot 6: Model 1 Regression Results ----

# Create a png file and send output to correct folder
png(filename = "../output/old_visuals/Parallel_pretrends_visualization.png", width = 800, height = 600)

# Plot the Data
plot(NYC_arrests[["manhattan_pre_trends"]]$week[-c(1)], NYC_arrests[["manhattan_pre_trends"]]$Arrests[-c(1)], type = "l", col = "black", ylim = c(550, 1500), xlab = "Date", ylab = "Arrests", main = "Visualization of Parallel Pre Trends")
lines(NYC_arrests[["non_manhattan_pretrends"]]$week[-c(1)], NYC_arrests[["non_manhattan_pretrends"]]$Arrests[-c(1)], type = "l", col = "black", lty = 2)

# Vertical line for implementation date and legend
abline(v = implementation_date, col = "red", lty = 2)
legend("bottomleft", legend = c("Manhattan Arrests", "Other Boroughs Arrests"), lty = c(1, 2), col = c("black", "black"))

dev.off()


# Plot 7: Subway and Bus Stop Locations ----
png(filename = "../output/Bus_and_Subway_Locations.png", width = 1400, height = 1200, res = 300)

plot(st_geometry(Public_transport_locations$boroughs_proj), col = "lightgrey", border = "darkgray")
plot(st_geometry(Public_transport_locations$subway_proj), col = adjustcolor("blue", alpha.f = 0.05), pch = 19, cex = 0.8, add = TRUE)
plot(st_geometry(Public_transport_locations$bus_proj), col = adjustcolor("blue", alpha.f = 0.05), pch = 19, cex = 0.8, add = TRUE)

dev.off()


# Plot 8: Subway and Bus Stops Layered with Arrests ----
# Define colors for the distance rings including NA
Public_transport_locations$ring_colors <- c("10ft" = "darkred", "25ft" = "red", "50ft" = "orange", "100ft" = adjustcolor("yellow", alpha.f = 0.1), ">100ft" = adjustcolor("tan", alpha.f = 0.05))

# Plot arrests colored by distance ring
# Extract colors for each arrest based on distance_ring factor
ring_values <- as.character(NYC_arrests$arrests_proj$distance_ring)
unique(ring_values)
Public_transport_locations$point_colors <- Public_transport_locations$ring_colors[ring_values]

png(filename = "../output/Arrest_and_Public_Transportation_Mapping.png", width = 1400, height = 1200, res = 300)
plot(st_geometry(Public_transport_locations$boroughs_proj), col = "lightgrey", border = "darkgray")

# Plot points
plot(st_geometry(NYC_arrests$arrests_proj), col = Public_transport_locations$point_colors, pch = 19, cex = 0.6, add = TRUE)

# Add legend
legend("topleft", legend = c("Within 10ft", "Within 25ft", "Within 50ft", "Within 100ft", "Beyond 100ft"),
       col = c("darkred", "red", "orange", "yellow", "tan"), pch = 19, cex = 0.8)

dev.off()


# Plot 9: Air Quality Station Locations ----
png(filename = "../output/Air_Quality_Station_Location.png", width = 1400, height = 1200, res = 300)
plot(stgeo, col = "lightgrey", border = "darkgray")

station_coord <- st_as_sf(air_quality_stations, coords = c("Longitude", "Latitude"), crs = 2263)
plot(st_geometry(station_coord), col = "red", pch = 19, cex = 0.6, add = TRUE)

legend("topleft", legend = "Air Quality Station", col = "red", pch = 19, cex = 0.8)

dev.off()

