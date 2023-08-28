# Load required library
library(ggplot2)

# Create the plot
p <- ggplot(data = precipitation_data, aes(x = Year)) +
  
  # Light blue zone for the spatial averaged SD of the ERA5 precipitation data
  geom_ribbon(aes(ymin = CHELSA - sd_CHELSA, ymax = CHELSA + sd_CHELSA, fill = "CHELSA SD"), 
              alpha = 0.2, color = NA) +
  
  # Dark blue zone for the spatial averaged SD of the observed precipitation data
  geom_ribbon(aes(ymin = Observed - sd_Observed, ymax = Observed + sd_Observed, fill = "Observed SD"), 
              alpha = 0.2, color = NA) +
  
  # Light blue line for ERA5 precipitation data
  geom_line(aes(y = CHELSA, color = "CHELSA"), size = 1) +
  
  # Dark blue line for observed precipitation data
  geom_line(aes(y = Observed, color = "Observed"), size = 1) +
  
  # Setting the colors for lines and fill
  scale_color_manual(values = c("CHELSA" = "lightblue", "Observed" = "darkblue"), 
                     name = "Data Source") +
  scale_fill_manual(values = c("CHELSA SD" = "lightblue", "Observed SD" = "darkblue"),
                    name = "Standard Deviation") +
  
  # Labels and title
  labs(
    title = "Trends in Annual Precipitation in the USA Mainland (2003-2016)",
    x = "Year",
    y = "Annual Precipitation"
  ) +
  
  # Theme settings for a cleaner look
  theme_minimal()

# Display the plot
print(p)

install.packages("ggExtra")
install.packages("dplyr")
install.packages("Metrics")
# Load necessary packages
install.packages(c("ggplot2","ggExtra","dplyr","Metrics"))
library(ggplot2)
library(ggExtra)
library(dplyr)
library(Metrics)

# Load your datasets (replace with your actual file paths)
observed_data <- read.csv("path/to/your/observed_data.csv")
chelsea_data <- read.csv("path/to/your/chelsea_data.csv")

# Assuming the datasets have similar structure, you might want to merge them.
# Let's assume 'year' and 'location' are common identifiers.
combined_data <- merge(observed_data, chelsea_data, by = c("Year","Location"))

# Compute the performance metrics
cc <- cor(combined_data$Prep.x, combined_data$Prep.y)
rmse <- rmse(combined_data$Prep.x, combined_data$Prep.y)
bias <- mean(combined_data$Prep.x - combined_data$Prep.y)

# Create the scatterplot
p <- ggplot(combined_data, aes(x = Prep.x, y = Prep.y)) +
  geom_point(size = 0.25) + 
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of OBS and CHELSA Precipitation Data",
    subtitle = paste0("CC: ", round(cc, 2), 
                      ", RMSE: ",round(rmse, 2),
                      ", Bias: ",round(bias, 2)),
    x = "Observed Precipitation",
    y = "CHELSA Precipitation")

# Show the plot
print(p)



#1.Trend graph

df <- read.csv("/Users/libinjie/Downloads/毕业论文/data1.csv")


extract_data_for_season <- function(season){
  if (season == "Annual") {
    return(df[,c("Year", "OBS", "CHELSA", "SD_OBS", "SD_CHELSA")])
  } else {
    return(data.frame(Year = df$Year,
                      OBS = df[[paste0("OBS_", season)]],
                      CHELSA = df[[paste0("CHELSA_", season)]],
                      SD_OBS = df[[paste0("SD_OBS_", season)]],
                      SD_CHELSA = df[[paste0("SD_CHELSA_", season)]]
    ))
  }
}

install.packages(c("ggplot2", "gridExtra"))
library(ggplot2)
library(gridExtra)

draw_plot <- function(data, title) {
  lm_obs <- lm(OBS ~ Year, data = data)
  lm_chelsa <- lm(CHELSA ~ Year, data = data)
  slope_obs <- summary(lm_obs)$coefficients[2, 1] * 10
  slope_chelsa <- summary(lm_chelsa)$coefficients[2, 1] * 10
  
  ggplot(data, aes(x = Year)) +
    geom_ribbon(aes(ymin = OBS - SD_OBS, ymax = OBS + SD_OBS, fill = "SD_OBS"), alpha = 0.3) +
    geom_ribbon(aes(ymin = CHELSA - SD_CHELSA, ymax = CHELSA + SD_CHELSA, fill = "SD_CHELSA"), alpha = 0.3) +
    geom_line(aes(y = OBS, color = "P_OBS"), size = 0.8) +
    geom_line(aes(y = CHELSA, color = "P_CHELSA"), size = 0.8) +
    labs(y = "Precipitation (mm)", fill = "", color = "") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    annotate("text", x = 2015, y = max(data$CHELSA + data$SD_CHELSA) * 0.95, 
             label = paste("Slope OBS =", round(slope_obs, 2), "mm/decade\n",
                           "Slope CHELSA =", round(slope_chelsa, 2), "mm/decade"), 
             hjust = 1) +
    scale_color_manual(values = c("P_OBS" = "darkblue", "P_CHELSA" = "lightblue"),
                       breaks = c("P_OBS", "P_CHELSA"), 
                       labels = c("P_OBS", "P_CHELSA")) +
    scale_fill_manual(values = c("SD_OBS" = "darkblue", "SD_CHELSA" = "lightblue"),
                      breaks = c("SD_OBS", "SD_CHELSA"), 
                      labels = c("SD_OBS", "SD_CHELSA"))
}


seasons <- c("Annual", "Spring", "Summer", "Autumn", "Winter")
plot_list <- list()

for (season in seasons){
  seasonal_data <- extract_data_for_season(season)
  p <- draw_plot(seasonal_data, paste0("Graph of trends in seasonal precipitation in USA mainland during 2003–2016 (", season, ")"))
  plot_list[[season]] <- p
}

final_plot <- grid.arrange(grobs = plot_list, ncol = 1)


ggsave("/Users/libinjie/Downloads/毕业论文/seasonal_trends.png", plot = final_plot, width = 10, height = 14)



df <- read.csv("/Users/libinjie/Downloads/毕业论文/data1.csv")


extract_data_for_season <- function(season){
  if (season == "Annual") {
    return(df[,c("Year", "OBS", "CHELSA", "SD_OBS", "SD_CHELSA")])
  } else {
    return(data.frame(Year = df$Year,
                      OBS = df[[paste0("OBS_", season)]],
                      CHELSA = df[[paste0("CHELSA_", season)]],
                      SD_OBS = df[[paste0("SD_OBS_", season)]],
                      SD_CHELSA = df[[paste0("SD_CHELSA_", season)]]
    ))
  }
}


install.packages(c("ggplot2", "gridExtra"))
library(ggplot2)
library(gridExtra)


draw_plot <- function(data, title) {
  lm_obs <- lm(OBS ~ Year, data = data)
  lm_chelsa <- lm(CHELSA ~ Year, data = data)
  slope_obs <- summary(lm_obs)$coefficients[2, 1] * 10
  slope_chelsa <- summary(lm_chelsa)$coefficients[2, 1] * 10
  
  ggplot(data, aes(x = Year)) +
    geom_ribbon(aes(ymin = OBS - SD_OBS, ymax = OBS + SD_OBS, fill = "SD_OBS"), alpha = 0.3) +
    geom_ribbon(aes(ymin = CHELSA - SD_CHELSA, ymax = CHELSA + SD_CHELSA, fill = "SD_CHELSA"), alpha = 0.3) +
    geom_line(aes(y = OBS, color = "P_OBS"), size = 0.8) +
    geom_line(aes(y = CHELSA, color = "P_CHELSA"), size = 0.8) +
    geom_point(aes(y = OBS, color = "P_OBS"), size = 2) +
    geom_point(aes(y = CHELSA, color = "P_CHELSA"), size = 2) +
    labs(y = "Precipitation (mm)", fill = "", color = "") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.ticks = element_line(color = "black"),
      legend.position = "bottom"
    ) +
    annotate("text", x = 2015, y = max(data$CHELSA + data$SD_CHELSA) * 0.95, 
             label = paste("Slope OBS =", round(slope_obs, 2), "mm/decade\n",
                           "Slope CHELSA =", round(slope_chelsa, 2), "mm/decade"), 
             hjust = 1) +
    scale_x_continuous(breaks = 2003:2016) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) + 
    scale_color_manual(values = c("P_OBS" = "darkblue", "P_CHELSA" = "#008B8B"),
                       breaks = c("P_OBS", "P_CHELSA"), 
                       labels = c("P_OBS", "P_CHELSA")) +
    scale_fill_manual(values = c("SD_OBS" = "darkblue", "SD_CHELSA" = "#008B8B"),
                      breaks = c("SD_OBS", "SD_CHELSA"), 
                      labels = c("SD_OBS", "SD_CHELSA"))
}


seasons <- c("Annual", "Spring", "Summer", "Autumn", "Winter")
plot_list <- list()

for (season in seasons){
  seasonal_data <- extract_data_for_season(season)
  p <- draw_plot(seasonal_data, paste0("Graph of trends in seasonal precipitation in USA mainland during 2003–2016 (", season, ")"))
  plot_list[[season]] <- p
}

final_plot <- grid.arrange(grobs = plot_list, ncol = 1)

# ---- 6. 保存图片 ----
ggsave("/Users/libinjie/Downloads/毕业论文/seasonal_trends.png", plot = final_plot, width = 10, height = 14)



#update
df <- read.csv("/Users/libinjie/Downloads/毕业论文/data1.csv")

extract_data_for_season <- function(season){
  if (season == "Annual") {
    return(df[,c("Year", "OBS", "CHELSA", "SD_OBS", "SD_CHELSA")])
  } else {
    return(data.frame(Year = df$Year,
                      OBS = df[[paste0("OBS_", season)]],
                      CHELSA = df[[paste0("CHELSA_", season)]],
                      SD_OBS = df[[paste0("SD_OBS_", season)]],
                      SD_CHELSA = df[[paste0("SD_CHELSA_", season)]]
    ))
  }
}

library(ggplot2)
library(gridExtra)

calculate_slope <- function(data, var_name) {
  model <- lm(data[[var_name]] ~ data$Year)
  return(coef(model)[2]) 
}

plot_precipitation <- function(data, add_legend=FALSE){
  slope_obs_text <- sprintf("Slope OBS = %.2f mm/decade", calculate_slope(data, "OBS"))
  slope_chelsa_text <- sprintf("Slope CHELSA = %.2f mm/decade", calculate_slope(data, "CHELSA"))
  
  plot <- ggplot(data, aes(x=Year)) +
    geom_ribbon(aes(ymin=OBS-SD_OBS, ymax=OBS+SD_OBS), fill="darkblue", alpha=0.4) +
    geom_ribbon(aes(ymin=CHELSA-SD_CHELSA, ymax=CHELSA+SD_CHELSA), fill="lightblue", alpha=0.4) +
    geom_line(aes(y=OBS, color="Observed"), size=0.8) +
    geom_line(aes(y=CHELSA, color="CHELSA"), size=0.8) +
    labs(y="Precipitation (mm)", color="Data Source") +
    scale_color_manual(values=c("Observed"="darkblue", "CHELSA"="lightblue")) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_text(size=10),
          axis.text.x=element_text(size=8),
          axis.text.y=element_text(size=8)) +
    annotate("text", x=max(data$Year)*0.95, y=max(data$OBS, na.rm=TRUE)*0.90, label=paste(slope_obs_text, slope_chelsa_text, sep="\n"), hjust=1, size=3)
  
  if (add_legend) {
    plot <- plot + theme(legend.position="bottom")
  }
  return(plot)
}

seasons <- c("Annual", "Spring", "Summer", "Autumn", "Winter")
plots <- lapply(seasons, function(season) {
  data_for_season <- extract_data_for_season(season)
  if (season == "Winter") {
    return(plot_precipitation(data_for_season, add_legend=TRUE))
  } else {
    return(plot_precipitation(data_for_season))
  }
})

do.call(grid.arrange, c(plots, ncol=1))




#2.Box plot making
# Convert continuous elevation into categorical
library(dplyr)
library(tidyr)

# Convert continuous elevation into categorical
df$Elevation_cat <- cut(df$Elevation, 
                        breaks=c(-Inf, 1000, 2000, Inf), 
                        labels=c("<1000", "1000-2000", ">2000"))

# Compute CC and Bias for each site for each year
compute_metrics <- function(chelsa, observed) {
  cc <- cor(chelsa, observed, method="pearson")
  bias <- mean(chelsa - observed)
  
  return(data.frame(CC = cc, Bias = bias))
}

df <- df %>%
  rowwise() %>%
  do(compute_metrics(.$CHELSA_Precip, .$Observed_Precip)) %>%
  bind_cols(df, .)


# Convert data to long format for easier plotting
library(ggplot2)
library(tidyr)

df_long <- df %>%
  select(Elevation_cat, CC, Bias) %>%
  pivot_longer(cols = c(CC, Bias), names_to = "Metric", values_to = "Value")

ggplot(df_long, aes(x = Elevation_cat, y = Value, fill = Metric)) + 
  geom_boxplot() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(y = "Value", x = "Elevation ") +
  theme_minimal()

#box plot of CC
library(dplyr)
library(ggplot2)
df<-read.csv("/Users/libinjie/Downloads/毕业论文/data2.csv")
library(dplyr)

df$CC <- mapply(cor, df$CHELSA_Precip, df$Observed_Precip)

df$Elevation_cat <- cut(df$Elevation, 
                        breaks=c(-Inf, 1000, 2000, Inf), 
                        labels=c("<1000", "1000-2000", ">2000"))


ggplot(df, aes(x = Elevation_cat, y = CC)) + 
  geom_boxplot() +
  labs(title = "Correlation Coefficient between CHELSA and Observed Precipitation",
       y = "Correlation Coefficient", x = "Elevation Category") +
  theme_minimal()

#map
install.packages("sf")
install.packages("dplyr")
library(sf)
shape_data <- st_read("/Users/libinjie/Downloads/毕业论文/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
plot(shape_data)
install.packages("rgdal")
library(rgdal)
library(ggplot2)
shape_data <- readOGR(dsn = "/Users/libinjie/Downloads/毕业论文/cb_2018_us_state_500k/cb_2018_us_state_500k.shp", layer = "your_shapefile_name_without_extension")
ggplot(data = shape_data) +
  geom_sf() +
  theme_minimal()
plot(shape_data)

##tif 
install.packages("raster")
library(raster)
tif_file <- raster("/Users/libinjie/Downloads/CHELSA_pr_01_2003_V.2.1.tif/")
plot(tif_file)

install.packages(c("raster", "sf"))
library(raster)
library(sf)


## Input tif file (Single)
install.packages(c("raster", "sf"))
library(raster)
library(sf)
# Load tif file
precip_raster2007 <- raster("/Users/libinjie/Downloads/CHELSAcruts_prec_9_2016_V.1.0.tif.crdownload")
precip_raster2011 <- raster("/Users/libinjie/Downloads/CHELSAcruts_prec_10_2011_V.1.0.tif")
print(precip_raster)
# Assume there is a csv file with station location "longitude"and"latitude"
stations <- read.csv("path_to_your_stations_file.csv")
#Convert your observation site data into spatial point data boxes
stations_sf <- st_as_sf(stations, coords = c("longitude", "latitude"), crs = crs(precip_raster))
stations$precipitation <- extract(precip_raster, stations_sf)
write.csv(stations, "output_precipitation_data2011.csv", row.names = FALSE)
head(stations)

## Input tif file (All)
install.packages(c("raster", "sf"))
install.packages("stringr")
library(stringr)
library(raster)
library(sf)
library(dplyr)

stations <- read.csv("/Users/libinjie/Downloads/毕业论文/allstation.csv")
stations <- select(stations, -c(X, X.1))
stations_sf <- st_as_sf(stations, coords = c("LONGITUDE", "LATITUDE"), crs = 4326) # 假设你的数据是在WGS84坐标系中


file_list <- list.files("/Users/libinjie/Downloads/毕业论文/new data", pattern="\\.tif$", full.names=TRUE)

for (i in seq_along(file_list)) {
  precip_raster <- raster(file_list[i])
  
  
  if (st_crs(stations_sf) != crs(precip_raster)) {
    stations_sf <- st_transform(stations_sf, crs(precip_raster))
  }
  

  month_precipitation <- extract(precip_raster, stations_sf)
  

  file_basename <- basename(file_list[i])
  year <- sub(".*_prec_.*_([0-9]{4})_.*", "\\1", file_basename)
  month <- sub(".*_prec_([0-9]+)_.*", "\\1", file_basename)
  
 
  colname <- paste(year, str_pad(month, 2, pad = "0"), sep="_")
  
  stations[[colname]] <- month_precipitation
}

write.csv(stations, "output_precipitation_data.csv", row.names = FALSE)

#calculate annual mean
stations <- read.csv("output_precipitation_data.csv")


yearly_precipitation <- data.frame(year = 2003:2016)

for (year in 2003:2016) {
  columns_for_year <- paste("X", year, str_pad(1:12, 2, pad = "0"), sep = "_")
  
  missing_columns <- columns_for_year[!(columns_for_year %in% names(stations))]
  if (length(missing_columns) > 0) {
    cat("Missing columns for year", year, ":", missing_columns, "\n")
  }
}

yearly_precipitation <- data.frame(year = 2003:2016)

for (year in 2003:2016) {
  columns_for_year <- paste0("X", year, "_", str_pad(1:12, 2, pad = "0"))
  
  
  if (all(columns_for_year %in% names(stations))) {
    yearly_precipitation[yearly_precipitation$year == year, "avg_precipitation"] <- mean(rowSums(stations[, columns_for_year], na.rm = TRUE))
  } else {
    print(paste("Incomplete data for year:", year))
  }
}

write.csv(yearly_precipitation, "yearly_avg_precipitation.csv", row.names = FALSE)

#calculate SD


yearly_stddev <- data.frame(year = 2003:2016)

for (year in 2003:2016) {
  columns_for_year <- paste0(year, "_", str_pad(1:12, 2, pad = "0"))
  

  if (all(columns_for_year %in% names(stations))) {
    yearly_sums <- rowSums(stations[, columns_for_year], na.rm = TRUE)
    yearly_stddev[yearly_stddev$year == year, "stddev_precipitation"] <- sd(yearly_sums, na.rm = TRUE)
  } else {
    print(paste("Incomplete data for year:", year))
  }
}

write.csv(yearly_stddev, "yearly_stddev_precipitation.csv", row.names = FALSE)



yearly_stddev <- data.frame(year = 2003:2016)

for (year in 2003:2016) {
  columns_for_year <- paste0(year, "_", str_pad(1:12, 2, pad = "0"))
  
  missing_columns <- columns_for_year[!(columns_for_year %in% names(stations))]
  
  
  if (length(missing_columns) == 0) {
    yearly_sums <- rowSums(stations[, columns_for_year], na.rm = TRUE)
    yearly_stddev[yearly_stddev$year == year, "stddev_precipitation"] <- sd(yearly_sums, na.rm = TRUE)
  } else {
    print(paste("Missing columns for year", year, ":", paste(missing_columns, collapse = " ")))
  }
}

write.csv(yearly_stddev, "yearly_stddev_precipitation.csv", row.names = FALSE)

head(stations)

yearly_stddev <- data.frame(year = 2003:2016)

for (year in 2003:2016) {
  columns_for_year <- paste0("X", year, "_", str_pad(1:12, 2, pad = "0"))
  
  available_columns <- columns_for_year[columns_for_year %in% names(stations)]
  
  if (length(available_columns) == 12) {
    yearly_sums <- rowSums(stations[, available_columns], na.rm = TRUE)
    yearly_stddev[yearly_stddev$year == year, "stddev_precipitation"] <- sd(yearly_sums, na.rm = TRUE)
  } else {
    missing_columns <- setdiff(columns_for_year, available_columns)
    print(paste("Missing columns for year", year, ":", paste(missing_columns, collapse = " ")))
  }
}

write.csv(yearly_stddev, "yearly_stddev_precipitation.csv", row.names = FALSE)

##calculate seasonal mean and std
library(readr)
library(dplyr)
library(stringr)


stations <- read.csv("output_precipitation_data.csv")


seasons <- list(Spring = c("03", "04", "05"),
                Summer = c("06", "07", "08"),
                Autumn = c("09", "10", "11"),
                Winter = c("12", "01", "02"))


results <- data.frame(Station = stations$STATION, 
                      `2003-2016 mean` = NA, 
                      Spring_mean = NA, 
                      Summer_mean = NA, 
                      Autumn_mean = NA, 
                      Winter_mean = NA)


yearly_cols <- sapply(2003:2016, function(year) grep(paste0("X", year, "_[0-9]{2}"), colnames(stations)))
yearly_sums <- apply(yearly_cols, 2, function(cols) rowSums(stations[, cols], na.rm = TRUE))
results$`2003-2016 mean` <- rowMeans(yearly_sums, na.rm = TRUE)


for (season in names(seasons)) {
  seasonal_cols <- lapply(2003:2016, function(year) {
    grep(paste0("X", year, "_", paste0(seasons[[season]], collapse = "|")), colnames(stations))
  })
  seasonal_sums_per_year <- sapply(seasonal_cols, function(cols) {
    if(length(cols) > 0) {
      return(rowSums(stations[, cols], na.rm = TRUE))
    } else {
      return(rep(NA, nrow(stations)))
    }
  })
  results[paste0(season, "_mean")] <- rowMeans(seasonal_sums_per_year, na.rm = TRUE)
}


write.csv(results, "seasonal_averages.csv", row.names = FALSE)
          
##Table
install.packages("dplyr")
install.packages("hydroGOF")

library(dplyr)
library(hydroGOF)


data <- read.csv("/Users/libinjie/Downloads/毕业论文/final data.csv")


observed <- data$observed
chelsa <- data$chelsa
calculate_stats <- function(observed, chelsa) {

  diff <- observed - chelsa
  
  
  bias <- mean(diff)
  
  
  rmse <- sqrt(mean(diff^2))
  
  list(
    quantile_5 = quantile(diff, probs = 0.05),
    quantile_50 = quantile(diff, probs = 0.5),
    quantile_95 = quantile(diff, probs = 0.95),
    bias = bias,
    rmse = rmse
  )
}

results <- calculate_stats(observed, chelsa)
print(results)

results_df <- as.data.frame(t(as.data.frame(results)))


write.csv(results_df, file = "results.csv", row.names = FALSE)

#input shapefile
install.packages("sf")
install.packages("ggplot2")
library(sf)
library(ggplot2)
shape_data_sf <- st_read("/Users/libinjie/Downloads/毕业论文/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
ggplot(data = shape_data_sf) +
  geom_sf() +
  theme_minimal()

#Mann kendall test
install.packages("Kendall")
install.packages("dplyr")
library(Kendall)
library(dplyr)

data <- read.csv("/Users/libinjie/Downloads/毕业论文/final data1.csv")
perform_mk_test <- function(chelsa_column, observed_column) {
  test_result <- MannKendall(observed_column - chelsa_column)
  return(list(tau = test_result$tau, p.value = test_result$p.value))
}
filtered_data <- data %>%
  group_by(Station) %>%
  filter(n() >= 3)  
perform_mk_test <- function(chelsa_column, observed_column) {
  if(length(chelsa_column) < 3 || length(observed_column) < 3) {
    return(list(tau = NA, p.value = NA))
  }
  test_result <- MannKendall(observed_column - chelsa_column)
  return(list(tau = test_result$tau, p.value = test_result$p.value))
}

results <- data %>%
  group_by(Station) %>%
  summarise(
    tau_Annual = perform_mk_test(Chelsa_Annual, Observed_Annual)$tau,
    p_value_Annual = perform_mk_test(Chelsa_Annual, Observed_Annual)$p.value,
    tau_Spring = perform_mk_test(Chelsa_Spring, Observed_Spring)$tau,
    p_value_Spring = perform_mk_test(Chelsa_Spring, Observed_Spring)$p.value,
    tau_Summer = perform_mk_test(Chelsa_Summer, Observed_Summer)$tau,
    p_value_Summer = perform_mk_test(Chelsa_Summer, Observed_Summer)$p.value,
    tau_Autumn = perform_mk_test(Chelsa_Autumn, Observed_Autumn)$tau,
    p_value_Autumn = perform_mk_test(Chelsa_Autumn, Observed_Autumn)$p.value,
    tau_Winter = perform_mk_test(Chelsa_Winter, Observed_Winter)$tau,
    p_value_Winter = perform_mk_test(Chelsa_Winter, Observed_Winter)$p.value,
    )
write.csv(results, "MK_test_difference_results.csv", row.names = FALSE)


install.packages("tidyverse")
library(tidyverse)


data <- read.csv("data3.csv")


long_data <- data %>%
  gather(key = "Date", value = "Precipitation", -STATION) %>%
  arrange(STATION)


write.csv(long_data, "your_output_file.csv", row.names = FALSE)


data <- read.csv("/Users/libinjie/Downloads/毕业论文/data4.csv")


if('Observed' %in% colnames(data)){
  data$Observed <- data$Observed * 25.4
} else {
  print("'Observed' column not found.")
}


write.csv(data, "modified_file_path.csv", row.names = FALSE)

##MK test
install.packages("trend")
install.packages("readr")
install.packages("dplyr")
install.packages("Kendall")
install.packages("tidyr")
library(Kendall)
library(tidyr) 
library(trend)
library(dplyr)
library(readr)


data <- read_csv("/Users/libinjie/Downloads/毕业论文/modified_file_path.csv")

library(tidyverse)
library(Kendall)


    
long_data <- data %>%
  gather(key = "Date", value = "Value", -Station) %>%
  mutate(Year = as.integer(substr(Date, 1, 4)),
         Month = as.integer(substr(Date, 6, 7)))
long_data <- long_data %>%
  mutate(Date = gsub("^X", "", Date))

     
overall_results <- long_data %>%
  group_by(Station) %>%
  summarise(
    tau_overall = mk.test(Value)$tau,
    p_value_overall = mk.test(Value)$p.value
  )


long_data <- long_data %>%
  mutate(Season = case_when(
    Month %in% 1:3 ~ "Winter",
    Month %in% 4:6 ~ "Spring",
    Month %in% 7:9 ~ "Summer",
    Month %in% 10:12 ~ "Autumn",
    TRUE ~ NA_character_
  ))


seasonal_results <- long_data %>%
  group_by(Station, Year, Season) %>%
  summarise(Seasonal_Value = mean(Value, na.rm = TRUE)) %>%
  group_by(Station, Season) %>%
  summarise(
    tau_season = mk.test(Seasonal_Value)$tau,
    p_value_season = mk.test(Seasonal_Value)$p.value
  )


final_results <- overall_results %>%
  full_join(seasonal_results, by = "Station")


write_csv(final_results, "MK_test_results.csv")

##update
library(tidyr)
library(dplyr)
library(trend)


data <- read_csv("/Users/libinjie/Downloads/毕业论文/modified_file_path.csv")
library(dplyr)
library(tidyr)
library(trend)


data <- read.csv("/Users/libinjie/Downloads/毕业论文/modified_file_path.csv", stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(trend)




names(data) <- gsub("^DateX", "Date", names(data))


long_data <- data %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "Date",
    values_to = "Value"
  )


results <- long_data %>%
  group_by(Station, Date) %>%
  summarize(Difference = mean(Chelsa - Observed, na.rm = TRUE), .groups = 'drop') %>%
  rowwise() %>%
  mutate(mk_test = list(mk.test(Difference)))


final_results <- results %>%
  mutate(
    tau = map_dbl(mk_test, ~ .x$tau),
    p.value = map_dbl(mk_test, ~ .x$p.value)
  ) %>%
  select(station, Date, tau, p.value)


write.csv(final_results, "MK_test_results.csv")

library(dplyr)
library(tidyr)
library(purrr)
library(trend)


data <- read.csv("/Users/libinjie/Downloads/毕业论文/modified_file_path.csv", stringsAsFactors = FALSE)


names(data) <- gsub("^DateX", "Date", names(data))


long_data <- data %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "Date",
    values_to = "Value"
  )


diff_data <- long_data %>%
  group_by(Station, Date) %>%
  summarize(Difference = mean(Chelsa - Observed, na.rm = TRUE), .groups = 'drop')


results <- diff_data %>%
  group_by(Station) %>%
  filter(n() >= 3) %>%
  rowwise() %>%
  mutate(mk_test = list(mk.test(Difference)))


final_results <- results %>%
  mutate(
    tau = map_dbl(mk_test, ~ .x$tau),
    p.value = map_dbl(mk_test, ~ .x$p.value)
  ) %>%
  select(station, Date, tau, p.value)


write.csv(final_results, "MK_test_results.csv")


names(data) <- gsub("^DateX", "Date", names(data))


long_data <- data %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "Date",
    values_to = "Value"
  )


diff_data <- long_data %>%
  group_by(Station, Date) %>%
  summarize(Difference = mean(Chelsa - Observed, na.rm = TRUE), .groups = 'drop')


safe_mk_test <- function(data){
  if(length(data) >= 3) {
    res <- mk.test(data)
    return(list(tau = res$tau, p.value = res$p.value))
  } else {
    return(list(tau = NA_real_, p.value = NA_real_))
  }
}


results <- diff_data %>%
  group_by(Station) %>%
  summarise(
    tau = safe_mk_test(Difference)$tau,
    p.value = safe_mk_test(Difference)$p.value
  )

write.csv(results, "MK_test_results.csv")

##
library(dplyr)
library(readr)
library(Kendall)

# Function to perform the Mann-Kendall test safely
perform_mk_test <- function(difference) {
  if(length(difference) < 3) {
    return(list(tau = NA, p.value = NA))
  }
  
  result <- tryCatch(
    mk.test(difference),
    error = function(e) list(tau = NA, p.value = NA)
  )
  return(result)
}

data <- read_csv("/Users/libinjie/Downloads/毕业论文/modified_file_path.csv")


data <- data %>%
  mutate(Difference = Observed - Chelsa)


results <- data %>%
  group_by(Station) %>%
  nest(data = c(Date, Observed, Chelsa, Difference)) %>%
  rowwise() %>%
  mutate(
    mk_result = list(perform_mk_test(data$Difference))
  ) %>%
  mutate(
    Tau = ifelse(!is.null(mk_result$tau), mk_result$tau, NA),
    p.value = ifelse(!is.null(mk_result$p.value), mk_result$p.value, NA)
  ) %>%
  ungroup() %>%
  select(Station, Tau, p.value)


write_csv(mk_results, "mk_test_results4.csv")

install.packages("sf")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("raster")




