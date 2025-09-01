setwd("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/")

source("~/public/Evaluation/Projects/KP0023_legumes/Scripts/lift_data_processing/functions/functions_lift.R")

time_zone <- "MET"


#### check for gps file 0613 ## fix me

require(data.table)
require(ggplot2)


tol3qualitative=c("#4477AA", "#DDCC77", "#CC6677")
tol4qualitative=c("#4477AA", "#117733", "#DDCC77", "#CC6677")
tol5qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677")
tol6qualitative=c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol7qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#DDCC77", "#CC6677","#AA4499")
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499")
tol10qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol11qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#882255", "#AA4499")
tol12qualitative=c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")

tol14rainbow=c("#882E72", "#B178A6", "#D6C1DE", "#1965B0", "#5289C7", "#7BAFDE", "#4EB265", "#90C987", "#CAE0AB", "#F7EE55", "#F6C141", "#F1932D", "#E8601C", "#DC050C")
tol15rainbow=c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA")
tol18rainbow=c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
tol21rainbow= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")




# gps_file <- "Lot1_pea_2024-05-10_gps.csv"
csv_files <- list.files(pattern = "*.csv")
csv_files <- csv_files[!grepl("copy",csv_files)]

###
# ChlF_files <- "Lot1_pea_2024-05-10_data.csv"
ChlF_files <- csv_files[grepl("_data.csv",csv_files)]
ChlF_files <- ChlF_files[!grepl("~~",ChlF_files)]
# ChlF_files <- ChlF_files[2:3]
# ChlF_files <- ChlF_files[17:18]


#######
Transient <- lapply(ChlF_files,get_ChlF_Transient)
Transient <- rbindlist(Transient)
nrow(Transient)/400
##
TransientData <- get_ChlF_parameters(Transient)
TransientData <- TransientData[!duplicated(TransientData$TimeMSec),]
nrow(Transient)/nrow(TransientData)

max <- TransientData[,max(Datetime), by=Filename]
min <- TransientData[,min(Datetime), by=Filename]
min
# max$V1[grepl("06-13",max$V1)]
# 
# min$V1[grepl("06-18",min$V1)]

p <- TransientData#[grepl("06-13",TransientData$Datetime),]
p <- p[grepl("0618",p$Filename)]
p <- p[order(p$TimeMSec),]
p$shift_time <- shift(p$Datetime,n=1)
p$time_dif <- as.numeric(p$Datetime-p$shift_time, units = "secs")
# p$time_dif <- as.integer(p$Datetime)-as.integer(p$shift_time)
shift_time_not_actualized <- max(p$time_dif , na.rm = TRUE)
shift_time_not_actualized

# Convert your string boundaries to POSIXct
start_t <- as.POSIXct("2025-06-13 15:31:33", tz = time_zone)
end_t   <- as.POSIXct("2025-06-14 15:31:33", tz = time_zone)

TransientData$TimeMSec[TransientData$Datetime>start_t&TransientData$Datetime<end_t] <- TransientData$TimeMSec[TransientData$Datetime>start_t&TransientData$Datetime<end_t]+shift_time_not_actualized-1

# Subtract seconds directly (POSIXct is numeric under the hood)
TransientData$Datetime[
  TransientData$Datetime > start_t &
    TransientData$Datetime < end_t
] <- TransientData$Datetime[
  TransientData$Datetime > start_t &
    TransientData$Datetime < end_t
] + shift_time_not_actualized -1

start_t <- as.POSIXct("2025-06-12 18:08:01", tz = time_zone)
end_t   <- as.POSIXct("2025-06-12 23:08:01", tz = time_zone)

p <- TransientData#[TransientData$Datetime>"2025-06-12 07:08:01"&TransientData$Datetime<"2025-06-13 23:08:01",] 
p <- p[grepl("0613",p$Filename)]
p <- p[order(p$TimeMSec),]
p$shift_time <- shift(p$Datetime,n=1)
p$time_dif <- as.numeric(p$Datetime-p$shift_time, units = "secs")
# p$time_dif <- as.integer(p$Datetime)-as.integer(p$shift_time)
shift_time_not_actualized <- max(p$time_dif , na.rm = TRUE)
shift_time_not_actualized

TransientData$TimeMSec[TransientData$Datetime>start_t&TransientData$Datetime<end_t] <- TransientData$TimeMSec[TransientData$Datetime>start_t&TransientData$Datetime<end_t]+shift_time_not_actualized-5

# Subtract seconds directly (POSIXct is numeric under the hood)
TransientData$Datetime[
  TransientData$Datetime > start_t &
    TransientData$Datetime < end_t
] <- TransientData$Datetime[
  TransientData$Datetime > start_t &
    TransientData$Datetime < end_t
] + shift_time_not_actualized -5


# Convert TimeMSec to POSIXct
# gps_out$Datetime <- as.POSIXct(gps_out$TimeMSec, origin = "1970-01-01", tz = time_zone)
# 
# start_gps <- as.POSIXct("2025-06-13 10:08:01", tz = time_zone)
# end_gps   <- as.POSIXct("2025-06-13 23:08:01", tz = time_zone)
# 
# # Subset rows within the time window
# subset_gps <- gps_out[gps_out$Datetime >= start_gps & gps_out$Datetime <= end_gps, ]
# 
# subset_gps
#######

## do not run
p <- subset(Transient, S.N_RAT>100&DataPt%in%1:310)
length(unique(p$Transient))
p$Hour <- format(p$Datetime, format = "%H")
p$Date <- format(p$Datetime, format = "%m-%d-%Y")

p <- p[,list(mean=mean(Relative),SD=sd(Relative)),by=.(DataPt,Date,Hour)]



require(ggplot2)
ggplot(p, aes(x=DataPt,y=mean, color=Hour))+
  theme_bw()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),color="grey")+
  geom_point()+
  scale_color_manual(values = tol12qualitative)+
  facet_grid(.~Date)



#####
library(data.table)

# start from Transient (data.frame or data.table)
setDT(Transient)

# filter and pick up to 20 unique TimeMSec
p <- Transient[S.N_RAT > 100]
meas_select <- unique(p$TimeMSec)

set.seed(1)  # optional for reproducibility
n_sel  <- min(20L, length(meas_select))
select <- sample(meas_select, n_sel)

# keep only the selected TimeMSec
p <- p[TimeMSec %in% select]

# build a small lookup: first 10 -> RowPlot "1", next 10 -> "2"
map <- data.table(
  TimeMSec = select,
  RowPlot  = rep(c("1","2"), each = 10L)[seq_len(n_sel)],
  N_plot   = rep(1:10, length.out = n_sel)
)

# join labels back to p
p <- map[p, on = "TimeMSec"]

# (optional) keep a tidy column order
setcolorder(p, c("TimeMSec", "RowPlot", "N_plot"))
p$N_plot <- as.factor(p$N_plot)

require(ggplot2)
ggplot(p, aes(x=DataPt,y=Relative,color=N_plot))+
  theme_bw()+
  # geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),color="grey")+
  geom_point()+
  scale_color_manual(values = tol12qualitative)+
  facet_grid(RowPlot~N_plot)


p <- p[,list(mean=mean(Relative),SD=sd(Relative)),by=.(Time.us)]
p$Time.us <- as.numeric(p$Time.us)/1000
require(ggplot2)
ggplot(p, aes(x=Time.us,y=mean))+xlab("Time (ms)")+ylab("Fluorescence (a.u.)")+
  theme(text = element_text(size=24),text.title = element_text(size=24))+
  theme_bw()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),color="grey")+
  geom_point(color="darkblue")+
  scale_color_manual(values = tol12qualitative)

## ggsave("ChlF_transient.png",  width = 80, height = 50, units = "mm")



####
########

ext_gps_files <- list.files(path = "./GPS/Processed",pattern = "*.csv")
ext_gps_files <- ext_gps_files[grepl("COM",ext_gps_files)]
ext_gps_files



# Load data
read_gps <- function(ext_gps_file){
  dt <- fread(paste0("./GPS/Processed/", ext_gps_file),na.strings="")
  # dt <- fread(paste0("./GPS/",ext_gps_files[9]),na.strings="")
  names(dt)[names(dt)=="Alt (MSL)"] <- "Alt"
  
  required_cols <- c("UTC", "Lon", "Lat")
  
  if (!all(required_cols %in% names(dt))) {
    # Skip this file
    return(NULL)
  }
  
  if (!"Alt" %in% names(dt)) {
    dt[, Alt := NA]
  }
  
  # Fix the function to handle one string at a time
  convert_datetime_to_us <- function(datetime_str) {
    if (is.na(datetime_str) || grepl("\\?\\?", datetime_str)) return(NA_real_)
    
    parts <- strsplit(datetime_str, " ")[[1]]
    if (length(parts) != 2) return(NA_real_)
    
    time_part <- parts[1]
    date_part <- parts[2]
    
    time_split <- strsplit(time_part, "\\.")[[1]]
    main_time <- time_split[1]
    ms_part <- ifelse(length(time_split) == 2, as.numeric(time_split[2]), 0)
    
    full_datetime <- paste(date_part, main_time)
    
    dt <- as.POSIXct(full_datetime, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
    if (is.na(dt)) return(NA_real_)
    
    total_us <- as.numeric(dt) * 1e7 + ms_part * 1000  # microseconds
    return(total_us)  # numeric (not integer!)
  }
  
  dt <- dt[!is.na(dt$UTC)]
  
  # Apply to each row using sapply
  dt$timestamp <- sapply(dt$UTC, convert_datetime_to_us)
  
  
  # Add placeholders
  dt[, `:=`(
    timestamp=timestamp/10,
    longitude = Lon,
    latitude = Lat,        # Replace with actual if available
    altitude = Alt,      # Convert to mm if needed
    heading = NA_real_          # Placeholder
  )]
  
  # Keep only needed columns
  dt_out <- dt[, .(timestamp, longitude, latitude, altitude, heading)]
  write.csv(dt_out,paste0(dt$timestamp[!is.na(dt$timestamp)][1],"_gps.csv"),row.names = F)
}
lapply(ext_gps_files, read_gps)

###
gps_files <- list.files(pattern = "*.csv")
gps_files <- gps_files[grepl("_gps",gps_files)]
# gps_files <- gps_files[1] # exclude gps tests
# gps_data <- fread(gps_files)
# round(gps_data$timestamp / 1000000.0,digits=1)

gps_data_mean <- lapply(gps_files,get_gps_data)
gps_data_mean <- rbindlist(gps_data_mean,fill=T)
# gps_data_mean <- subset(gps_data_mean, latitude>47.4&latitude<47.5&longitude>8.6815&longitude<9)

p <- gps_data_mean
p$datetime <- as.POSIXct(p$TimeMSec, origin = "1970-01-01", tz = "MET")
p$Date <- as.Date(p$datetime )
p$Hour <- as.numeric(format(p$datetime, "%H"))

p[, time_diff_sec := as.numeric(datetime - shift(datetime, type = "lag"), units = "secs")]
# p <- subset(p, Date=="2025-05-30")
p

require(ggplot2)
ggplot(p, aes(x=longitude,y=latitude,color=timestamp))+
  theme_bw()+
  geom_point()+
  facet_grid(.~Date)
# scale_color_man

# hist(p$time_diff_sec[p$time_diff_sec>2&p$time_diff_sec<100])

p <- subset(p, time_diff_sec>2&time_diff_sec<1000)

require(ggplot2)
ggplot(p, aes(x=longitude,y=latitude,color=time_diff_sec))+
  theme_bw()+
  geom_point()+
  facet_grid(.~Date)
# scale_color_man
################
##### design 2025

library(sf)
# library(dplyr)

# gps_data_mean
polygons <- st_read("FPSE-lotC-unlabeled-wgs84.geojson") ##is soybean
# Create a sequence of unique IDs
ggplot(polygons) +
  geom_sf()

# Assuming 'polygons' is your sf object containing multipolygons
polygons_splitC <- polygons %>% 
  st_cast("POLYGON", group_or_split = TRUE, warn = FALSE)

polygons <- st_read("FPSE-lotD-unlabeled-wgs84.geojson") ##is soybean
# Create a sequence of unique IDs
ggplot(polygons) +
  geom_sf()

# Assuming 'polygons' is your sf object containing multipolygons
polygons_splitD <- polygons %>% 
  st_cast("POLYGON", group_or_split = TRUE, warn = FALSE)

polygons_split <- rbind(polygons_splitC,polygons_splitD)
#####

Design <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2025/Design/20250811_Design_Measurements_ESSB019.csv")
# Design <- Design[1:72,]
Design <- subset(Design, range!=0&range!=6)
Design$Lot_nr <- 1
Design$Lot_nr[Design$row%in%1:14] <- 2

Design <- Design[order(Design$Lot_nr,Design$range,Design$row),]

Design <- Design[1:140,]

Design$rows_polygon <-NA
Design$rows_polygon <- c(1:28,28:1,1:28,28:1,1:28)#24:13,25:36,48:37,49:60#seq_len(nrow(polygons_split))
# Design <- Design[order(Design$range,Design$rows_polygon),]

# Design$ID <- 1:nrow(Design)
polygons_split$name <-  rev(Design$plot_UID)#Design$plot_UID[c(15:28,)] #

library(ggplot2)

ggplot(polygons_split) +
  geom_sf() +
  geom_sf_label(aes(label = name)) + # Add labels with the unique_id column
  scale_fill_gradient(low = "lightblue", high = "darkred") + # Example fill color gradient
  theme_minimal()

combined_polygons <- st_combine(polygons_split)
combined_polygons$name

polygons <- st_transform(polygons_split, 4326)

ggplot(polygons) +
  geom_sf()+geom_sf_label(aes(label = name),size=2)

# sf::st_write(polygons, dsn = "2025v1-soybean_labeled-plots-lotC_D.geojson", layer = "polygons")

# Filter out polygons where 'name' contains "Border"
polygons_filtered <- polygons_split[!grepl("Border", polygons_split$name), ]

# Transform the coordinates to EPSG:4326 (WGS84, lat/lon) or your desired CRS
polygons_transformed <- st_transform(polygons_filtered, 4326)

# Extract the transformed coordinates
polygon_coords <- st_coordinates(polygons_transformed)

# Convert to a data frame
polygon_df <- data.frame(X = polygon_coords[, "X"],
                         Y = polygon_coords[, "Y"],
                         id = as.factor(polygon_coords[, "L1"]),
                         part = polygon_coords[, "L2"])


# Plot the filtered polygons using geom_path
ggplot(polygon_df, aes(x = X, y = Y, group = interaction(id, part))) +
  geom_path(color = "black") +
  theme_minimal() +
  labs(title = "Filtered Polygon Boundaries", x = "Longitude", y = "Latitude")
#####




#### design 2024
# library(sf)
# # library(dplyr)
# 
# gps_data_mean
# polygons <- st_read("2024-beans-plots-lot6.geojson") ##is soybean
# # Create a sequence of unique IDs
# ggplot(polygons) +
#   geom_sf()
# 
# # Assuming 'polygons' is your sf object containing multipolygons
# polygons_split <- polygons %>% 
#   st_cast("POLYGON", group_or_split = TRUE, warn = FALSE)
# 
# 
# #####
# 
# Design <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2024/Design/20240423_Design_Measurements_FPSB018.csv")
# Design <- Design[1:72,]
# Design <- Design[order(Design$range,Design$row),]
# 
# Design$rows_polygon <- c(rep(c(1:12,12:1),times=3))#24:13,25:36,48:37,49:60#seq_len(nrow(polygons_split))
# # Design <- Design[order(Design$range,Design$rows_polygon),]
# 
# polygons_split$name <- rev(Design$plot_UID)
# 
# library(ggplot2)
# 
# ggplot(polygons_split) +
#   geom_sf() +
#   geom_sf_label(aes(label = name)) + # Add labels with the unique_id column
#   scale_fill_gradient(low = "lightblue", high = "darkred") + # Example fill color gradient
#   theme_minimal()
# 
# combined_polygons <- st_combine(polygons_split)
# combined_polygons$name
# 
# polygons <- st_transform(polygons_split, 4326)
# 
# ggplot(polygons) +
#   geom_sf()
# 
# # sf::st_write(polygons, dsn = "2024-soybean_labeled-plots-lot6.geojson", layer = "polygons")
# 
# # Filter out polygons where 'name' contains "Border"
# polygons_filtered <- polygons_split[!grepl("Border", polygons_split$name), ]
# 
# # Transform the coordinates to EPSG:4326 (WGS84, lat/lon) or your desired CRS
# polygons_transformed <- st_transform(polygons_filtered, 4326)
# 
# # Extract the transformed coordinates
# polygon_coords <- st_coordinates(polygons_transformed)
# 
# # Convert to a data frame
# polygon_df <- data.frame(X = polygon_coords[, "X"],
#                          Y = polygon_coords[, "Y"],
#                          id = as.factor(polygon_coords[, "L1"]),
#                          part = polygon_coords[, "L2"])
# 
# 
# # Plot the filtered polygons using geom_path
# ggplot(polygon_df, aes(x = X, y = Y, group = interaction(id, part))) +
#   geom_path(color = "black") +
#   theme_minimal() +
#   labs(title = "Filtered Polygon Boundaries", x = "Longitude", y = "Latitude")
#####


gps_data_filtered <- gps_data_mean#get_gps_data_filtered(gps_data_mean,polygons)



p <- gps_data_filtered

require(ggplot2)
ggplot(p, aes(x=longitude,y=latitude))+
  theme_bw()+
  geom_point()

####
library(data.table)
library(sf)
library(geosphere)

gps_df = gps_data_mean
setDT(gps_df)
setorder(gps_df, TimeMSec)

# Minimal heading calculation
gps_df[, `:=`(
  lon = longitude,
  lat = latitude,
  lon_prev = shift(longitude),
  lat_prev = shift(latitude),
  lon_next = shift(longitude, type = "lead"),
  lat_next = shift(latitude, type = "lead")
)]

bear_norm <- function(x) (x + 360) %% 360
circ_mean2 <- function(a, b) {
  ar <- a*pi/180; br <- b*pi/180
  mr <- atan2(sin(ar) + sin(br), cos(ar) + cos(br))
  (mr * 180/pi + 360) %% 360
}

gps_df[, b_prev := bear_norm(bearing(cbind(lon_prev, lat_prev), cbind(lon, lat)))]
gps_df[, b_next := bear_norm(bearing(cbind(lon, lat), cbind(lon_next, lat_next)))]

gps_df[, heading_filled := fifelse(is.na(heading), circ_mean2(b_prev, b_next), heading)]
gps_df[, heading_filled := (heading_filled + 360) %% 360]

# --- build gps_out ---
gps_out <- gps_df[, .(TimeMSec,
                      lon_orig = lon,
                      lat_orig = lat,
                      heading_filled)]

# --- apply sensor offset ---
baseline_forward_m <- 1.25  # sensor is 1.5 m in front of GPS
baseline_right_m   <- 0.0  # lateral offset

coords0 <- cbind(gps_out$lon_orig, gps_out$lat_orig)
pt_forward <- geosphere::destPoint(p = coords0,
                                   b = gps_out$heading_filled,
                                   d = baseline_forward_m)

bearing_right <- (gps_out$heading_filled + 90) %% 360
pt_sensor <- geosphere::destPoint(p = pt_forward,
                                  b = bearing_right,
                                  d = baseline_right_m)

gps_out[, `:=`(
  lon_sensor = pt_sensor[,1],
  lat_sensor = pt_sensor[,2]
)]

# Now gps_out has original GPS coords and sensor coords

# Start from ORIGINAL GPS coords (lon_orig/lat_orig) for sensor placement
coords0 <- cbind(gps_out$lon_orig, gps_out$lat_orig)

# Step 1: shift forward by the baseline (along heading)
pt_forward <- geosphere::destPoint(
  p = coords0,
  b = gps_out$heading_filled,
  d = baseline_forward_m
)

# Step 2 (optional): apply lateral offset (bearing +90 = to the right)
bearing_right <- (gps_out$heading_filled + 90) %% 360
pt_sensor <- geosphere::destPoint(
  p = pt_forward,
  b = bearing_right,
  d = baseline_right_m
)

# Store sensor coordinates where the S.N_RAT was measured
gps_out[, `:=`(
  lon_sensor = pt_sensor[,1],
  lat_sensor = pt_sensor[,2]
)]

library(data.table)
library(sf)

# 0) Ensure polygons are WGS84
if (is.na(st_crs(polygons))) stop("polygons has no CRS.")
if (st_crs(polygons) != st_crs(4326)) polygons <- st_transform(polygons, 4326)

# 1) Choose coordinates to use for plot assignment:
#    prefer sensor coords, else fall back to original GPS
gps_out[, `:=`(
  lon_use = fifelse(is.finite(lon_sensor) & is.finite(lat_sensor), lon_sensor, lon_orig),
  lat_use = fifelse(is.finite(lon_sensor) & is.finite(lat_sensor), lat_sensor, lat_orig)
)]

# 2) Keep only rows with valid coordinates for the sf conversion
ok <- is.finite(gps_out$lon_use) & is.finite(gps_out$lat_use)

# 3) Build sf points and join to polygons (strictly within)
sensor_sf <- st_as_sf(gps_out[ok], coords = c("lon_use", "lat_use"), crs = 4326, remove = FALSE)

# If your points can lie exactly on plot borders and you want them included, use st_intersects.
# For strictly inside, keep st_within:
joined <- st_join(sensor_sf, polygons["name"], join = st_within, left = TRUE)

# 4) Copy plot_UID back to the full table in original order
gps_out[, plot_UID := NA_character_]
gps_out[ok, plot_UID := st_drop_geometry(joined)$name]

# 5) (Optional) Drop rows not in any polygon
# gps_out <- gps_out[!is.na(plot_UID)]

# 6) (Optional) Quick check
# table(is.na(gps_out$plot_UID))



# Merge S.N_RAT onto gps_out so it carries lon_sensor/lat_sensor
ChlF_gps_test <- merge(
  gps_out[, .(TimeMSec, lon_orig, lat_orig, lon_sensor, lat_sensor, heading_filled,plot_UID)],
  TransientData[, .(TimeMSec, S.N_RAT)],
  by = "TimeMSec", all.x = FALSE
)

# Plot: grey = original GPS (rear), colored = S.N_RAT at sensor (front)
library(ggplot2)
ggplot() +
  theme_bw() +
  geom_point(data = gps_out,
             aes(x = lon_orig, y = lat_orig),
             alpha = 0.25, size = 0.6) +
  geom_point(data = subset(ChlF_gps_test, S.N_RAT < 10),
             aes(x = lon_sensor, y = lat_sensor, color = S.N_RAT),
             size = 0.9) +
  geom_path(data=polygon_df,aes(x = X, y = Y, group = interaction(id, part)),color = "black",linewidth=0.35) +
  scale_color_gradientn(colours = tol3qualitative) +
  coord_quickmap()
# Visual arrows from GPS (rear) -> sensor (front)

library(ggplot2)
ggplot() +
  theme_bw() + theme(legend.position = "none")+
  geom_point(data = gps_out,
             aes(x = lon_orig, y = lat_orig),
             alpha = 0.25, size = 0.6) +
  geom_point(data = subset(ChlF_gps_test),
             aes(x = lon_sensor, y = lat_sensor, color = plot_UID),
             size = 0.9) +
  geom_path(data=polygon_df,aes(x = X, y = Y, group = interaction(id, part)),color = "black",linewidth=0.35) +
  # scale_color_gradientn(colours = tol3qualitative) +
  coord_quickmap()



library(grid)
arrows_df <- ChlF_gps_test[sample(.N, min(.N, 800))]
ggplot(arrows_df) +
  theme_bw() +
  geom_segment(aes(x = lon_orig, y = lat_orig,
                   xend = lon_sensor, yend = lat_sensor, color = S.N_RAT),
               arrow = arrow(length = unit(0.12, "cm")), alpha = 0.7) +
  coord_quickmap()

# Bearings of GPS->sensor vectors (should match rover heading distribution)
off_bear <- geosphere::bearing(
  cbind(ChlF_gps_test$lon_orig, ChlF_gps_test$lat_orig),
  cbind(ChlF_gps_test$lon_sensor, ChlF_gps_test$lat_sensor)
)
# hist((off_bear + 360) %% 360, breaks = 36, main = "Sensor offset bearings")

gps_data_with_polygons <- gps_out

### add design
Design1 <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2024/Design/20240423_Design_Measurements_FPSB018.csv")
Design2 <- fread("~/public/Evaluation/Projects/KP0023_legumes/Soybean/2025/Design/20250811_Design_Measurements_ESSB019.csv")
# Design3 <- fread("~/public/Evaluation/Projects/KP0023_legumes/Pea/2024/design/FPSE007_fieldbook.csv")
Design <- rbind(Design1,Design2,fill=T)
# Design$rep <- Design$row_block
Design$Genotype <- Design$Genotype_name
Design$row_lot <- Design$row

Design <- unique(Design[,c("lot","range","row_lot","rep","plot_UID","Genotype","exp_UID")])
Design <- Design[!is.na(Design$plot_UID),]
Design <- Design[!duplicated(Design$plot_UID)]

gps_data_design <- merge(gps_data_with_polygons, Design, by="plot_UID",all.x = T)
#######



###




## add PPFR data
weather_files_2024 <- list.files(path="~/public/Evaluation/Projects/KP0023_legumes/Agrometeo/",pattern="_2024.csv",full.names = T)
weather_files_2025 <- list.files(path="~/public/Evaluation/Projects/KP0023_legumes/Agrometeo/",pattern="_2025.csv",full.names = T)
weather_files <- c(weather_files_2024,weather_files_2025)

PPFR_data_all <- lapply(weather_files,fread)
PPFR_data_all <- rbindlist(PPFR_data_all)
PPFR_data <- subset(PPFR_data_all, Outlier=="No")
PPFR_data <- PPFR_data[!duplicated(paste(PPFR_data$TIMESTAMP,PPFR_data$variable)),] #fix me

PPFR_data <- dcast(PPFR_data, TIMESTAMP~variable, value.var="value")
names(PPFR_data)[grepl("Air_temp",names(PPFR_data))] <- "Temperature"
names(PPFR_data)[grepl("PAR",names(PPFR_data))] <- "PPFR"
PPFR_data <- PPFR_data[,c("TIMESTAMP","Temperature","PPFR")]

PPFR_data$TIMESTAMP <- as.POSIXct(PPFR_data$TIMESTAMP , origin="1970-01-01", tz=time_zone, format= '%Y-%m-%d %H:%M:%S')
PPFR_data$TimeSecR <- as.integer(PPFR_data$TIMESTAMP )
PPFR_data$TIMESTAMP <- NULL
PPFR_data <- PPFR_data[!duplicated(PPFR_data$TimeSecR),]
# hist(PPFR_data$PPFR)
PPFR_data

PPFR_highRes_2024 <- list.files(path="PPFR/",pattern="lift-par",full.names = T)
PPFR_data_highRes <- lapply(PPFR_highRes_2024,fread)
PPFR_data_highRes <- rbindlist(PPFR_data_highRes)
names(PPFR_data_highRes)[3] <- "PPFR"
PPFR_data_highRes
hist(PPFR_data_highRes$PPFR)
PPFR_data_highRes <- subset(PPFR_data_highRes, PPFR>50)
###
library(data.table)

# --- Inputs in memory:
# PPFR_data (TimeSecR, PPFR, Temperature ...)
# PPFR_data_highRes (`Datum Zeit, GMT+02:00`, PPFR)

# Choose timezone (use your 'time_zone' if defined, else Europe/Zurich)
tz_used <- if (exists("time_zone")) time_zone else "Europe/Zurich"

# 1) Parse times
PPFR_data <- copy(PPFR_data)
PPFR_data[, Time := as.POSIXct(TimeSecR, origin = "1970-01-01", tz = tz_used)]
PPFR_data <- PPFR_data[!is.na(Time)]
setkey(PPFR_data, Time)

PPFR_data_highRes <- copy(PPFR_data_highRes)
# Your high-res timestamp is already one combined column: "Datum Zeit, GMT+02:00"
PPFR_data_highRes[, DateTime := as.POSIXct(`Datum Zeit, GMT+02:00`,
                                           format = "%m.%d.%y %I:%M:%S %p",
                                           tz = tz_used)]
PPFR_data_highRes <- PPFR_data_highRes[!is.na(DateTime)]
setkey(PPFR_data_highRes, DateTime)

# 2) Shift-evaluate correlation (nearest join; raw cor)
cor_at_shift <- function(shift_sec) {
  hr <- PPFR_data_highRes[, .(DateTime = DateTime + shift_sec, PPFR_highRes = PPFR)]
  merged <- PPFR_data[hr, on = .(Time = DateTime), roll = "nearest"]
  suppressWarnings(cor(merged$PPFR, merged$PPFR_highRes, use = "complete.obs"))
}

# 3) Search best constant shift (you set the range)
shifts <- seq(0, 5400, by = 1)  # 0..90 minutes in 1-second steps
cors   <- sapply(shifts, cor_at_shift)
plot(shifts, cors, type = "l", xlab = "Shift (sec)", ylab = "Correlation")
best_shift <- shifts[which.max(cors)]
best_corr  <- max(cors, na.rm = TRUE)

# 4) Build two joins:
#    (a) aligned_all: keep ALL low-res rows (for final fused output)
#    (b) aligned_calib: only high-PAR high-res rows for fitting
hr_best_all <- PPFR_data_highRes[, .(DateTime = DateTime + best_shift, PPFR_highRes = PPFR)]
aligned_all <- PPFR_data[hr_best_all, on = .(Time = DateTime), roll = "nearest"]  # keeps all low-res rows

hr_calib <- PPFR_data_highRes[PPFR > 1000, .(DateTime = DateTime + best_shift, PPFR_highRes = PPFR)]
aligned_calib <- PPFR_data[hr_calib, on = .(Time = DateTime), roll = "nearest"]

# 5) Fit calibration (low ~ a + b*high) using only high-PAR pairs
calib_df <- aligned_calib[is.finite(PPFR) & is.finite(PPFR_highRes),
                          .(PPFR_lowRes = PPFR, PPFR_highRes)]

  fit <- lm(PPFR_lowRes ~ PPFR_highRes, data = calib_df)
  a <- unname(coef(fit)[1]); b <- unname(coef(fit)[2])
  # Predict ON aligned_all to create PPFR_highRes_cal there
PPFR_data_highRes$PPFR_highRes <- PPFR_data_highRes$PPFR
PPFR_data_highRes[, PPFR_highRes_cal := as.numeric(predict(fit, newdata = .SD)),
              .SDcols = "PPFR_highRes"]


aligned_all <-   PPFR_data_highRes
# 6) Fuse: use calibrated high-res when available, else original low-res
aligned_all[, PPFR_fused := fifelse(is.finite(PPFR_highRes_cal), PPFR_highRes_cal, PPFR)]

# (Optional) Quick correlations on overlapping rows only
corr_before <- suppressWarnings(cor(aligned_all$PPFR, aligned_all$PPFR_highRes, use = "complete.obs"))
corr_after  <- suppressWarnings(cor(aligned_all$PPFR, aligned_all$PPFR_highRes_cal, use = "complete.obs"))

cat("Best shift (sec): ", best_shift, "\n", sep = "")
cat("Best raw correlation at best shift: ", round(best_corr, 4), "\n", sep = "")
cat("Calibration (low ~ a + b*high): a = ", round(a, 4), ", b = ", round(b, 4), "\n", sep = "")
cat("Overlap corr (raw): ", round(corr_before, 4), " | (calibrated): ", round(corr_after, 4), "\n", sep = "")

# PPFR_data
# PPFR_data_highRes

library(data.table)

# Example: assuming PPFR_data_highRes is already a data.table
# If it's a data.frame, convert: setDT(PPFR_data_highRes)

PPFR_data_formatted <- PPFR_data_highRes[, .(
  Temperature = NA_real_,                                  # No temp in highRes
  PPFR = PPFR_fused,                                        # Use fused values
  TimeSecR = as.integer(as.POSIXct(DateTime, tz = "UTC")),  # UNIX timestamp
  Time = as.POSIXct(DateTime, tz = "UTC")                   # POSIX datetime
)]




PPFR_final <- rbind(PPFR_data,PPFR_data_formatted)
PPFR_final <- PPFR_final[order(is.na(Temperature))]  # non-NA first
PPFR_final <- PPFR_final[!duplicated(TimeSecR)]      # keep first occurrence
# Check if any duplicates remain
PPFR_final[duplicated(TimeSecR)]

PPFR_final$TimeSec <- PPFR_final$TimeSecR
PPFR_final$TimeSecR <- NULL

### add spectro data
References <-fread("~/public/Evaluation/Projects/KP0023_legumes/Pea/2024/LIFT/Data/grey_references_2017may12_L007_edit.csv") 

Spectro_all <- lapply(ChlF_files, function(x) get_spectro_data(x) )
Spectro_all <- rbindlist(Spectro_all)
names(Spectro_all)



Spectro_data <- lapply(ChlF_files, function(x) get_spectro_parameter(x,References, PPFR_final))
Spectro_data <- rbindlist(Spectro_data)

nrow(Spectro_data)/nrow(TransientData)

##########
gps_data_design$TimeMSecR <- gps_data_design$TimeMSec*10
gps_data_design$TimeMSecR <- round(gps_data_design$TimeMSecR/5)*5 /10

TransientData$TimeMSecR <- TransientData$TimeMSec*10
TransientData$TimeMSecR <- round(TransientData$TimeMSecR/5)*5 /10

gps_data_design <- unique(gps_data_design)

gps_data_design_R <- gps_data_design[!duplicated(gps_data_design$TimeMSecR),]
names(gps_data_design_R)[names(gps_data_design_R)=="TimeMSec"] <- "TimeMSec_GPS"

# ChlF_gps_all <- merge(TransientData, gps_data_design_R, by="TimeMSecR",all.x = F, all.y = F)

library(data.table)

merge_nearest_time <- function(transient, gps, key = "TimeMSecR", window ) {
  td  <- as.data.table(transient)
  gps <- as.data.table(gps)
  
  # Drop NA times
  td  <- td[!is.na(get(key))]
  gps <- gps[!is.na(get(key))]
  
  # Save original transient time to compute diff after join
  td[, transient_time := get(key)]
  
  # Set keys
  setkeyv(td,  key)
  setkeyv(gps, key)
  
  # Join: keep all transient rows, match nearest GPS row
  merged <- gps[td, roll = "nearest", on = key]
  
  # Compute difference (gps - transient)
  merged[, time_diff_ms := TimeMSec_GPS - transient_time]
  
  # Blank GPS data if beyond ±window ms
  gps_cols <- setdiff(names(gps), key)
  # if (length(gps_cols)) {
  #   merged[abs(time_diff_ms) > window, (gps_cols) := NA]
  # }
  merged <- merged[abs(time_diff_ms) < window,]
  merged[]
}



# Example usage
ChlF_gps_all <- merge_nearest_time(
  TransientData,
  gps_data_design_R,
  key = "TimeMSecR",
  window = 3000
)
# p <- ChlF_gps_all[!is.na(ChlF_gps_all$latitude),]
###

p <- subset(ChlF_gps_all,S.N_RAT<750&S.N_RAT>25)
p$Date <- as.factor(as.Date(p$Datetime))
p[,N:=nrow(.SD),by=Date]

ggplot(p, aes(x=Date,y=S.N_RAT))+
  theme_bw()+theme(legend.position="none")+
  geom_boxplot()+
  geom_text(label=p$N,y=-2, check_overlap = T,color="grey")


######
design_sub <- gps_data_design_R
design_sub <- subset(design_sub, Genotype%in%unique(Design$Genotype)[1:23])
design_sub <- design_sub[,c("TimeMSecR","Genotype")]
Spectro_mean <- Spectro_all
# Spectro_mean$TimeMSecR <- Spectro_mean$TimeMSec*10
Spectro_mean$TimeMSecR <- round(Spectro_mean$TimeMSec,digits = 1) 

p0 <- Spectro_mean$TimeMSecR [1]
p1 <- design_sub$TimeMSecR [1]

Spectro_mean <- merge(Spectro_mean,design_sub, by="TimeMSecR")
Spectro_mean$Week <- format(Spectro_mean$Datetime, "%U")
Spectro_mean$Hour <- format(Spectro_mean$Datetime, "%H")
Spectro_mean$Date <- format(Spectro_mean$Datetime, "%Y-%m-%d")

Spectro_mean <- Spectro_mean[,list(mean=mean(Signal,na.rm = T), sd=sd(Signal,na.rm = T),se=sd(Signal,na.rm = T)/sqrt(nrow(.SD)), N=nrow(.SD)),by=.(Wavelength,Date,Week,Genotype)]
# hist(Spectro_mean$N)

df <- subset(Spectro_mean, Date%in%"2025-06-25"&Wavelength>425)
df$Week <- paste("Week",df$Week)
df$Hour <- paste0(df$Date,", ",df$Hour,":00h")

# df$Genotype <- paste0("Soy",df$Genotype)
df$Genotype <- gsub("-","",df$Genotype)


ggSpectra <- ggplot(df, aes(x = Wavelength, y = mean, color = Genotype)) +
  theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=12))+
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2,color="grey") +
  # labs(x = "Wavelength (nm)",   y = "Reflectance (raw digits)") +
  geom_point() +
  # scale_x_continuous(limits = c(min(df$Wavelength), max(df$Wavelength))) +
  guides(color = guide_legend(override.aes = list(size=1),nrow=2))+
  scale_color_manual(values = tol12qualitative)
  # facet_grid(Hour~.,switch = "y")

legendGeno <- cowplot::get_legend(ggSpectra)

ggSpectra <- ggplot(df, aes(x = Wavelength, y = mean, color = Genotype)) +
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=12))+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2,color="grey") +
  labs(x = "Wavelength (nm)",   y = "Reflectance (raw digits)") +
  geom_point(size=0.75) +
  scale_x_continuous(limits = c(min(df$Wavelength), max(df$Wavelength))) +
  # scale_color_discrete(name = "Genotype")+
  scale_color_manual(values = tol12qualitative)
  # facet_grid(Hour~.,switch = "y")

## ggsave("Spectra_week.pdf",  width = 160, height = 150, units = "mm",ggSpectra)
Transient_mean <- subset(Transient, S.N_RAT<750&S.N_RAT>25)
Transient_mean$TimeMSecR <- Transient_mean$TimeMSec*10
Transient_mean$TimeMSecR <- round(Transient_mean$TimeMSecR/5)*5 /10

Transient_mean <- merge(Transient_mean,design_sub, by="TimeMSecR")
Transient_mean$Week <- format(Transient_mean$Datetime, "%U")
Transient_mean$Hour <- format(Transient_mean$Datetime, "%H")
Transient_mean$Date <- format(Transient_mean$Datetime, "%Y-%m-%d")
Transient_mean <- Transient_mean[,list(mean=mean(RelativeFm,na.rm = T), SD=sd(RelativeFm,na.rm = T), N=nrow(.SD)),by=.(Time.us, Date, DataPt, Week,Genotype)]


df <- subset(Transient_mean, Date%in%"2025-06-27"&DataPt%in%0:330)
df$Week <- paste("Week",df$Week)
df$Hour <- paste0(df$Date,", ",df$Hour,"h")

# df$Genotype <- paste0("Soy",df$Genotype)
df$Genotype <- gsub("-","",df$Genotype)

require(ggplot2)
ggplot(df, aes(x=DataPt,y=mean, color=Genotype))+
  theme_bw()+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),color="grey90")+
  geom_point(size=0.5)+
  scale_color_manual(values = tol12qualitative)
  # facet_grid(.~Date)

p <- df
p$Time.us <- as.numeric(p$Time.us)/1000
require(ggplot2)
ggTransient <- ggplot(p, aes(x=Time.us,y=mean, color=Genotype))+xlab("Time (ms)")+ylab("Fluorescence (a.u.)")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=12))+
  geom_errorbar(aes(ymin=mean-SD, ymax=mean+SD),color="grey90")+
  geom_point(size=0.5)+
  scale_color_manual(values = tol12qualitative)


#####

nrow(ChlF_gps_all)
ChlF_gps <- subset(ChlF_gps_all, Fv.Fm<0.8&Fv.Fm>0.05)
ChlF_gps$latitude <- ChlF_gps$lat_use
ChlF_gps$longitude <- ChlF_gps$lon_use

ChlF_gps <- ChlF_gps[!is.na(ChlF_gps$latitude),]
hist(ChlF_gps$S.N_RAT[ChlF_gps$S.N_RAT<500])
ChlF_gps <- subset(ChlF_gps, S.N_RAT>20)
nrow(ChlF_gps)

library(data.table)

# Assuming hlF_gps$heading_filled already exists (0–360° from North, clockwise)
# Example: convert to factor with four main directions
library(data.table)

# Helper: normalize angle difference to 0..180
ang_diff <- function(a, b) {
  d <- abs(a - b) %% 360
  pmin(d, 360 - d)
}

# Assign direction based on smallest angular distance to center angles
# Centers: NW=315°, NE=45°, SE=135°, SW=225°
ChlF_gps[, Heading := {
  centers <- c("NW-ward" = 315, "NE-ward" = 45, "SE-ward" = 135, "SW-ward" = 225)
  v <- heading_filled
  out <- character(length(v))
  for (nm in names(centers)) {
    idx <- ang_diff(v, centers[nm]) <= 60  # ±60° from center
    out[idx] <- nm
  }
  out
}]


# Make it a factor in a nice order
ChlF_gps[, Heading := factor(
  Heading,
  levels = c("NW-ward", "NE-ward", "SE-ward", "SW-ward")
)]

ChlF_gps <- ChlF_gps[order(ChlF_gps$TimeMSec),]
ChlF_gps$row <- ChlF_gps$row_lot


library(data.table)

setDT(ChlF_gps)
setorder(ChlF_gps, Datetime)

min_len <- 120

# 1) contiguous segments over 'row'
ChlF_gps[, seg_id := rleid(row)]

# 2) segment summary (time/order preserved by seg_id)
seg_summary <- ChlF_gps[, .(row = row[1L], seg_len = .N), by = seg_id]

# 3) keep only valid segments (>= min_len) and non-NA rows, add sequential id
seg_summary <- seg_summary[seg_len >= min_len & !is.na(row)]
seg_summary[, id := .I]  # 1..n in time order

# 4) previous occurrence id for the same row (NA if first time)
seg_summary[, prev_id := shift(id), by = row]

# 5) assign run ids:
#    - start run = 1
#    - if a segment's prev_id is within the current run window, start a new run
seg_summary[, run := {
  out <- integer(.N)
  run <- 1L
  current_start <- 1L
  for (i in seq_len(.N)) {
    if (!is.na(prev_id[i]) && prev_id[i] >= current_start) {
      run <- run + 1L
      current_start <- i
    }
    out[i] <- run
  }
  out
}]

# 6) join run back to all rows; label whole segment; invalids stay NA
ChlF_gps <- seg_summary[ChlF_gps, on = "seg_id"]
ChlF_gps[, Run := ifelse(!is.na(run), paste0("Run ", run), NA_character_)]

# Optional: keep helper columns or drop them
# ChlF_gps[, .(Datetime, row, seg_id, seg_len, run, Run)]

# ChlF_gps[, Run := paste0("Run ",Run)]
p <- subset(ChlF_gps, Run%in%c(paste0("Run ",1)))
# p$longitude[p$Heading=="North-to-South" ] <- p$longitude[p$Heading=="North-to-South" ]+0.000005
p$Hour <- format(p$Datetime, format = "%H")

require(ggplot2)
ggplot(p, aes(x=longitude,y=latitude, color=as.factor(row)))+
  theme_bw()+
  geom_point()+
# scale_color_manual(values = tol12qualitative)+
  facet_grid(.~Run)

## ggsave("ChlF_gsp.pdf",  width = 180, height = 150, units = "mm")

ggplot(p, aes(x=longitude,y=latitude, color=as.factor(Heading)))+
  theme_bw()+
  geom_point()+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(.~Run)


oo <- subset(ChlF_gps, Run%in%c(paste0("Run ",2)))
# oo <- subset(ChlF_gps_all, TimeMSecR>min(p$TimeMSecR)&TimeMSecR<max(p$TimeMSecR))

require(ggplot2)
ggplot(oo, aes(x=longitude,y=latitude, color=as.factor(plot_UID)))+
  theme_bw()+
  geom_point()+
  theme_bw()+theme(legend.position = "none")+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(.~Run)


ggplot(subset(p), aes(x=Heading,y=Fv.Fm))+
  theme_bw()+
  geom_boxplot()+
  facet_grid(.~Hour)


####### add spectro
ChlF_gps$TimeMSec[1:10]
Spectro_data$TimeMSec[1:10]


library(data.table)


# Convert to data.table
transient_dt <- as.data.table(ChlF_gps)
spectro_dt <- as.data.table(Spectro_data)

# Add a row ID to transient for joining
transient_dt[, transient_row_id := .I]

# Initialize
used <- rep(FALSE, nrow(transient_dt))
matched_transient_ids <- rep(NA_integer_, nrow(spectro_dt))

# Loop over spectro times and find the first unused transient that is:
# - earlier than or equal to spectro time
# - no more than 5ms later
for (i in seq_along(spectro_dt$TimeMSec)) {
  s_time <- spectro_dt$TimeMSec[i]

  candidates <- which(
    transient_dt$TimeMSec >= s_time &
      transient_dt$TimeMSec <= s_time + 5 &
      !used
  )

  if (length(candidates) > 0) {
    match_id <- candidates[1]
    matched_transient_ids[i] <- match_id
    used[match_id] <- TRUE
  }
}

# Add matched transient row ID to spectro
spectro_dt[, transient_row_id := matched_transient_ids]
names(spectro_dt)[1:2] <- paste0(names(spectro_dt)[1:2], "_spectro")

# Merge: only spectro records with matched transient
ChlF_spectr_design <- merge(
  transient_dt,
  spectro_dt,
  by = c("transient_row_id","Filename"),
  all.x = T,
  all.y = FALSE
)

# Remove helper columns
ChlF_spectr_design[, transient_row_id := NULL]
ChlF_spectr_design$Date <- as.Date(ChlF_spectr_design$Datetime)
ChlF_spectr_design$Sowing_date <- as.Date("2025-05-02")
ChlF_spectr_design$DAS <- ChlF_spectr_design$Date-ChlF_spectr_design$Sowing_date
ChlF_spectr_design$Hour <- format(ChlF_spectr_design$Datetime, format = "%H")

library(data.table)

library(data.table)

library(data.table)

align_PPFR_to_signal <- function(ChlF_spectr_design, PPFR_final, signal_col = "Irradiance",
                                 shift_window_sec = 1 * 600, shift_step_sec = 1,
                                 ppfr_apply_thresh = 750) {
  
  DT <- as.data.table(ChlF_spectr_design)[
    Fv.Fm > 0 & Fv.Fm < 0.9 & 
      PPFR > 100 & S.N_RAT > 20 & S.N_RAT < 600 
  ]
  DT[, t_sec := as.numeric(TimeMSecR)]
  DT <- DT[!is.na(t_sec) & !is.na(get(signal_col))]
  DT$PPFR_spectro <- DT$PPFR
  
  signal <- DT[, .(t_sec, signal = get(signal_col), PPFR_spectro)]
  
  ppfrf <- as.data.table(PPFR_final)[, .(t_sec = TimeSec, PPFR)]
  ppfrf[, PPFR_org := PPFR]
  setkey(signal, t_sec)
  setkey(ppfrf, t_sec)
  
  cor_at_shift <- function(shift_sec) {
    ppfr_shift <- ppfrf[PPFR_org > ppfr_apply_thresh, 
                        .(t_sec = t_sec + shift_sec, PPFR, PPFR_org)]
    merged <- merge(ppfr_shift, signal, by = "t_sec", all = FALSE)
    if (nrow(merged) == 0) return(NA_real_)
    suppressWarnings(cor(merged$signal, merged$PPFR, use = "complete.obs"))
  }
  
  shifts <- seq(-shift_window_sec, shift_window_sec, by = shift_step_sec)
  cors   <- vapply(shifts, cor_at_shift, numeric(1))
  
  # Plot shift vs. correlation
  plot(shifts, cors, type = "l",
       xlab = sprintf("Shift (sec) [PPFR_final shifted relative to %s]", signal_col),
       ylab = "Correlation", main = paste("Shifted Correlation with", signal_col))
  
  
  best_idx   <- which.max(abs(cors))
  best_shift <- shifts[best_idx]
  best_corr  <- cors[best_idx]
  
  cat(sprintf("Best shift: %d sec | Correlation: %.4f\n", best_shift, best_corr))
  
  # Apply best shift and merge back
  ppfr_best <- ppfrf[PPFR > ppfr_apply_thresh, 
                     .(t_sec = t_sec + best_shift, PPFR_shift = PPFR)]
  setkey(ppfr_best, t_sec)
  DT[ppfr_best, PPFR_shift := i.PPFR_shift, on = .(t_sec), roll = "nearest"]
  
  new_colname <- paste0("PPFR_", signal_col)
  setnames(DT, "PPFR_shift", new_colname)
  
  return(DT[, .SD, .SDcols = !c("t_sec")])
}

ChlF_spectr_design_proc <- align_PPFR_to_signal(
  ChlF_spectr_design,
  PPFR_final         = PPFR_final,
  signal_col         = "Irradiance"  # or replace with other column name
)

ChlF_spectr_design_proc <- align_PPFR_to_signal(
ChlF_spectr_design_proc,
  PPFR_final         = PPFR_final,
  signal_col         = "Fv.Fm"  # or replace with other column name
)

ChlF_spectr_design_proc[,nrow(.SD),by=Date]
ChlF_spectr_design_proc$Genotype <- gsub("-","",ChlF_spectr_design_proc$Genotype)

##################
p <- melt.data.table(ChlF_spectr_design_proc, measure.vars = c("Fv.Fm","NDVI"))
# p <- subset(p, Run!="Run NA")
p$variable <- as.factor(p$variable)
levels(p$variable) <- c(expression("F"["q"]*"'"/"F"["m"]*"'"), "NDVI")
p$Value <- p$value
# p <- subset(p, Heading!="Unclear")
p <- p[!grepl("order",p$plot_UID),]
# p <- p[!is.na(p$plot_UID),]

require(ggplot2)
# p <- subset(p, Hour%in%c(11:16)&Heading=="South-to-North")
ggGPS <- ggplot()+xlab("Longitude")+ylab("Latitude")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.key.height=unit(0.75,"line"),legend.key.width = unit(1.5, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle =90, hjust = 1),text = element_text(size=12),axis.title = element_text(size=14))+
  geom_point(data = subset(gps_out, lon_orig<8.6818),
             aes(x = lon_orig, y = lat_orig),
             alpha = 0.25, size = 0.6) +
  geom_point(data=p, aes(x=longitude,y=latitude, color=Value,shape=Heading))+
  geom_path(data=polygon_df,aes(x = X, y = Y, group = interaction(id, part)),color = "black",linewidth=0.35) +
  scale_color_gradientn(colors = tol4qualitative)+
  guides(shape = guide_legend(nrow=2))+
  facet_grid(variable~., labeller = label_parsed, switch = "y")
ggGPS

require(cowplot)
first_row <- plot_grid(ggTransient,  ggSpectra, ncol=2, labels = c("B","C"))
ggPlots1 <- plot_grid(legendGeno, first_row,  rel_heights =   c(0.15,1), nrow = 2, labels = c(""))  #,vjust=0.5+
require(magick)
image_lift_beam <- image_ggplot(image_read("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/LIFT_caterra_image.png"))

ggPlots <- plot_grid(image_lift_beam, ggPlots1,  rel_widths =   c(0.39,1), ncol = 2, labels = c("A",""))  #,vjust=0.5+

# ggsave("LIFT_Soybean_Method.png",  width = 240, height = 100, units = "mm",  ggPlots)
# ggsave("LIFT_Soybean_Method.pdf",  width = 190, height = 100, units = "mm",  ggPlots)

# ggsave("LIFT_GPS.png",  width = 190, height = 150, units = "mm",  ggGPS)
# ggsave("LIFT_Spectra.png",  width = 100, height = 100, units = "mm",  ggSpectra)







p <- subset(ChlF_spectr_design_proc)

# Variables for outlier removal
vars <- c("S.N_RAT","Irradiance","PPFR","Fv.Fm",
          "carQ","Fr2.Fv","X680","NDVI","pNDVI","pNDVI_II","MTCI","pMTCI","pMTCI_K","PRI","Absorbance","Rtot","PPFR_Fv.Fm","PPFR_Irradiance")

# Function to remove rows with outliers for a given variable
remove_outliers <- function(df, col) {
  if (is.numeric(df[[col]])) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  return(df)
}

# Apply the outlier removal to each variable in sequence
for (v in vars) {
  p <- remove_outliers(p, v)
}

# Now p has the outliers removed

names(p)
corr <-  p[, .SD, .SDcols = vars]
corr <- corr[,colSums(is.na(corr))<nrow(corr),with = F]
p_corr <- as.data.frame(cor(corr,use = "complete"))
p_corr <-p_corr[order(abs(p_corr$Fv.Fm),decreasing = T),]
rownames(p_corr)
p_corr


p <- melt.data.table(p, measure.vars = c("NDVI","pMTCI"))

ggBoxplot <- ggplot(p, aes(x=Genotype,y=value, color=Genotype))+
  theme_bw()+theme(legend.position="none")+
  geom_boxplot()+
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(variable~.,scale="free_y")



p <- melt.data.table(ChlF_spectr_design_proc, measure.vars = c("Fv.Fm"))
p$variable <- gsub("trend","Photosynthetic response G:PPFR",p$variable)


require(ggplot2)
ggResponse <- ggplot(p, aes(x=PPFR,y=value, color=Genotype,group=Genotype))+
  theme_bw()+theme(legend.position="none")+
  geom_point()+
  geom_smooth(method='lm',formula=y~sqrt(x))+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(variable~.)
ggResponse

ggplot(p, aes(x=pMTCI,y=value, color=Genotype,group=1))+
  theme_bw()+theme(legend.position="none")+
  geom_point()+
  geom_smooth(method='lm',formula=y~sqrt(x))+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(variable~.)

p <- subset(p,Genotype%in%unique(p$Genotype)[1:25])
ggplot(p, aes(x=PPFR,y=value, color=Genotype,group=1))+
  theme_bw()+theme(legend.position="none")+
  geom_point()+
  geom_smooth(method='lm',formula=y~sqrt(x))+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(variable~Genotype)

###
p1 <- melt(ChlF_spectr_design_proc, measure.vars = c("Fv.Fm"),id.vars = c("Genotype","Datetime","PPFR","DAS","Date"))
p1$dataset <- "FPSB018"

# unique(PPFR_data_all$variable)
# oo <-  subset(PPFR_data_all,Date=="2024-06-27"&variable=="WEA_PAR_Den_Avg")
# o <- oo$value[grepl("PAR",oo$variable)]
# oo <-  subset(ChlF_spectr_design_proc,Date=="2024-06-27"&Hour=="09")



p <- setDT(p1)[, list(value=mean(value, na.rm=T), SD=sd(value, na.rm=T),PPFR=mean(PPFR, na.rm=T) ) , by=.(Datetime,variable,dataset,DAS,Genotype,Date)]

levels(p$variable) <- c(expression("F"["q"]*"'"/"F"["m"]*"'"), "PRI" ) ##,expression(atop("F"["r2"]/"F"["v"],"F"["r2"]*"'"/"F"["q"]*"'")


gg3 <- ggplot(p, aes(Datetime, value, color=PPFR))+ 
  theme_bw()+theme(strip.placement = "outside",axis.title.x = element_blank(), strip.background = element_blank(),legend.key.width = unit(2, "lines"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=14),axis.title = element_blank())+
  geom_errorbar(aes(ymin=value-SD, ymax=value+SD),color="grey",width=0.001)  +
  geom_point(size=1.5, alpha=0.6)+
  # scale_color_manual(values = tol14rainbow)+
  scale_color_gradientn(name=expression("PPFR (μmol photons m"^-2*"s"^-1*")"),colors= c("blue4","#205493","#4773aa","#8ba6ca","#dce4ef"))+ ##
  # scale_color_manual(values = c(tol9qualitative , tol21rainbow))+
  facet_grid(variable~Date,scale="free",switch="both", labeller = label_parsed)
# scale_x_datetime(date_labels="%b", date_breaks = "1 week")
gg3
# ggsave("LIFT_Soybean_fluctuations.png",  width = 210, height = 80, units = "mm",  gg3)



require(cowplot)
require(cowplot)
ggPlots <- plot_grid(ggGPS,  gg3, ncol=1, labels = "AUTO")
# ggsave("LIFT_Soybean_overview_FPCB001.png",  width = 190, height = 290, units = "mm",  ggPlots)


# require(ggplot2)
# ggplot(p, aes(x=Datetime,y=PPFR, color=Genotype))+
#   theme_bw()+theme(legend.position="none")+
#   geom_point()+
#   # scale_color_manual(values = tol12qualitative)+
#   facet_grid(variable~Run)


############
get_Emmeans <-   function(lm) {
  library(emmeans)
  value_AD <- emmeans (lm,  ~ Genotype) #,rg.limit = 15000
  value_AD <- as.data.frame(value_AD)
  value_AD[] <- lapply(value_AD, as.character)
  value_AD <- as.data.frame(value_AD)
  value_AD[] <- lapply(value_AD, as.character)
  value_AD$emmean <- as.numeric(value_AD$emmean)
  value_AD$SE_emmean <- as.numeric(value_AD$SE)
  value_AD$SE <- NULL
  value_AD$lower.CL <- NULL
  value_AD$upper.CL <- NULL
  value_AD$df <- NULL

  return(value_AD)
  gc()
}

adjust_Means <-   function(lm) {
  library(emmeans)
  value_AD <- emmeans (lm,  ~ Genotype) #,rg.limit = 15000
  value_AD <- as.data.frame(value_AD)
  value_AD[] <- lapply(value_AD, as.character)
  value_AD <- as.data.frame(value_AD)
  value_AD[] <- lapply(value_AD, as.character)
  value_AD$emmean <- as.numeric(value_AD$emmean)
  value_AD$SE_emmean <- as.numeric(value_AD$SE)
  value_AD$SE <- NULL
  value_AD$lower.CL <- NULL
  value_AD$upper.CL <- NULL
  value_AD$df <- NULL
  pTrends <- emtrends (lm,  ~ Genotype, var="Predictor") #,rg.limit = 15000
  # pTrends <- emtrends (lm1,  ~ Genotype+Experiment, var="Predictor")
  
  pTrends <- as.data.frame(pTrends)
  # value_AD <- merge(value_AD, pTrends,by=c("Experiment","Genotype","Rep"))
  value_AD$trend <- as.numeric(pTrends$Predictor.trend)
  value_AD$SE_trend <- as.numeric(pTrends$SE)
  
  gc()
  return(value_AD)
  gc()
}

#######
p <- subset(ChlF_spectr_design_proc, PPFR>500&PPFR<2250)

# Apply the outlier removal to each variable in sequence
for (v in vars) {
  p <- remove_outliers(p, v)
}



p <- melt.data.table(p, measure.vars = c("Fv.Fm"))
p$Predictor <- p$Irradiance#(sqrt(p$PPFR))
p <- p[!is.na(p$Predictor),]
p$Date    <- as.factor(p$Date)
p$Heading <- as.factor(p$Heading)
p$Hour    <- as.factor(p$Hour)

# p$value <- asin(sqrt(p$value))
# hist(p$S.N_RAT)
# p <- subset(p, S.N_RAT>50)
# p[,N:=nrow(.SD), by=.(Genotype, plot_UID, rep, variable)]
# hist(p$N[!duplicated(p$plot_UID)])
# p <- subset(p, N>50)
require(lme4)
lm0<- lm(value ~ (Date)+Heading*Hour+Genotype*Predictor+pMTCI+PPFR_Fv.Fm, data=p)
drop1(lm0,test="F")
require(plgraphics)
# plregr(lm0)
par(mfrow = c(2,2))
plot(lm0)
par(mfrow = c(1,1))

lm1<- lmer(value ~ (1|Date)+(1|Heading:Hour)+Genotype*Predictor+pMTCI+PPFR_Fv.Fm, data=p)
require(car)
vif(lm1)
anova(lm1)
# par(mfrow = c(2,2))
# plot(lm1)
# par(mfrow = c(1,1))
library(splines)
lm2 <- lmer(value ~ (1|Date) + (1|Heading:Hour) +
              Genotype*Predictor + pMTCI + PPFR_Fv.Fm  +
              ns(latitude, 4) + ns(longitude, 4),
            data = p)

library(MuMIn)
# Get marginal & conditional R²
r.squaredGLMM(lm1)
r.squaredGLMM(lm2)


library(mgcv)
p$Heading_Hour <- interaction(p$Heading, p$Hour, drop = TRUE)

library(mgcv)
g2d <- gamm(
  value ~ s(latitude, longitude, k = 20) +
    Genotype*Predictor + pMTCI + PPFR_Fv.Fm + Absorbance,
  random = list(Date = ~1, Heading_Hour = ~1),
  data = p
)

r.squaredGLMM(lm1)
r.squaredGLMM(lm2)
r.squaredGLMM(g2d$lme)


adjMeans1 <- adjust_Means(lm2)                     

# Assuming 'adjMeans1' is your data frame and it contains columns 'Genotype', 'trend', and 'SE_trend'


lmNdvi <- lmer(value ~ (1|Date)+(1|Heading:Hour)+Genotype, data=p)

adjMeansNdvi <- get_Emmeans(lmNdvi)
p <- setDT(adjMeansNdvi)
p <- melt.data.table(p, measure.vars = c("emmean"))
p$SE <- p$SE_emmean
p$variable <- "Emmean NDVI"
pNdvi <- p

# Step 1: Sort the data frame by 'trend'
p <- adjMeans1[order(adjMeans1$trend), ]
p$Genotype = with(p, reorder(Genotype, trend))

p <- setDT(p)
p <- melt.data.table(p, measure.vars = c("emmean","trend"))
p$SE <- p$SE_emmean
p$SE[p$variable=="trend"] <- p$SE_trend[p$variable=="trend"]

p$variable <- gsub("trend","Response G:PPFR",p$variable)
p$variable <- gsub("emmean","Emmean",p$variable)
# p <- rbind(p, pNdvi,fill=T)
# Identify which genotypes should be colored
highlight_genos <- unique(df$Genotype)  # genotypes from df
p <- subset(p, Genotype%in%highlight_genos)

# Create color vector for all genotypes in p
len_geno <- length(highlight_genos)
cols <- rep("black", times = len_geno)

# Assign colors only to genotypes in highlight_genos
geno_levels <- levels(factor(df$Genotype))  # ordered factor levels
cols[geno_levels %in% highlight_genos] <- tol12qualitative

# Make sure the factor order matches the color vector
p$Genotype <- factor(p$Genotype, levels = geno_levels)
length(unique(p$Genotype))
# Plot
ggCoef <- ggplot(p, aes(x=Genotype, y=value, colour = Genotype)) +
  ylab(expression("F"["q"]*"'"/"F"["m"]*"'")) + xlab("Breeding line") +
  theme_bw() +
  theme(
    strip.placement = "outside",
    plot.title = element_text(hjust = -0.2),
    strip.background = element_blank(),
    legend.key = element_rect(size = 0.6, color = "white"),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  geom_errorbar(aes(ymin = value - SE, ymax = value + SE),
                color = "grey", width = 0.1) +
  geom_point(size = 3) +
  scale_color_manual(values = cols) +
  facet_grid(variable~., scales = "free", switch = "y")


max_geno <- adjMeans1[order(adjMeans1$trend,decreasing = T),]
max_geno <- max_geno$Genotype[1]
min_geno <- adjMeans1[order(adjMeans1$trend,decreasing = F),]
min_geno <- min_geno$Genotype[1]
xtrem_geno <- c(max_geno,min_geno)

p <- subset(ChlF_spectr_design_proc, PPFR>750&PPFR<2000&Genotype%in%highlight_genos)
p <- melt.data.table(p, measure.vars = c("Fv.Fm"))
# p <- subset(p,Genotype%in%xtrem_geno)

require(ggplot2)
ggResponse <- ggplot(p, aes(x=PPFR,y=value, color=Genotype,group=Genotype))+ylab(expression(atop("F"["q"]*"'"/"F"["m"]*"'"," ")))+xlab(expression("PPFR (μmol photons m"^-2*"s"^-1*")"))+
  theme_bw()+theme(legend.position="none")+
  geom_point()+
  geom_smooth(method='lm',formula=y~sqrt(x))+
  scale_color_manual(values = tol12qualitative,name="Breeding line")
  # facet_grid(variable~.)


require(cowplot)
ggPlots <- plot_grid(ggCoef, ggResponse, ncol=1, labels = "AUTO")
ggPlots
# ggsave("LIFT_soybean_response_FPSB018.png",  width = 100, height = 180, units = "mm",  ggPlots)

##############
# ChlF_spectr_design_proc$longitude_r <- round(ChlF_spectr_design_proc$longitude,digits = 5)
# ChlF_spectr_design_proc[,N_block:=nrow(.SD),by=.(longitude_r,plot_UID)]
# hist(ChlF_spectr_design_proc$N_block)

ChlF_spectr_shifted <- NULL
for (ii in 0:9) {
p <- subset(ChlF_spectr_design_proc, PPFR>500&PPFR<2250)
p$longitude_r <- round(p$longitude+ii/100000,digits = 4)
unique(p$longitude_r )
# p$longitude_r <- round(p$longitude_r/5,digits = 5)*5
# unique(p$longitude_r )

p$long_shift <- ii
ChlF_spectr_shifted <- rbind(ChlF_spectr_shifted,p)
}
unique(ChlF_spectr_shifted$long_shift)

ChlF_spectr_shifted[,mean(longitude_r),by=.(long_shift)]
ChlF_spectr_shifted[,Longitude_mean:=mean(longitude),by=.(longitude_r,long_shift,plot_UID)]
ChlF_spectr_shifted[,Latitude_mean:=mean(latitude),by=.(long_shift,plot_UID)]

ChlF_spectr_shifted[,N_block:=nrow(.SD),by=.(long_shift,longitude_r,plot_UID)]
hist(ChlF_spectr_shifted$N_block)
ChlF_spectr_shifted[,N_block:=nrow(.SD),by=.(long_shift,longitude_r,plot_UID,Longitude_mean,Latitude_mean)]
hist(ChlF_spectr_shifted$N_block)

ChlF_spectr_shifted[,N_plot:=length(unique(plot_UID)),by=.(long_shift,longitude_r)]
hist(ChlF_spectr_shifted$N_plot)


ChlF_spectr_shifted <- subset(ChlF_spectr_shifted, N_block>15)
ChlF_spectr_shifted$Genotype_name <- ChlF_spectr_shifted$Genotype
ChlF_spectr_shifted$Genotype <- ChlF_spectr_shifted$plot_UID

grid <- ChlF_spectr_shifted[,list(Latitude_mean=mean(latitude),Longitude_mean=mean(longitude)),by=.(long_shift,longitude_r,plot_UID)]
# ChlF_spectr_shifted$long_shift_r <- paste0(ChlF_spectr_shifted$longitude_r, ChlF_spectr_shifted$long_shift)

adjust_Means_Model <- function(x) {
  x <- droplevels(x)
  require(lme4)
  lm1<- lm(value ~ Genotype*(Predictor)+pMTCI+PPFR_Fv.Fm, data=x, weights = S.N_RAT)
  adjusted_means <- adjust_Means(lm1)                     
  return(adjusted_means)
}

p <- subset(ChlF_spectr_shifted)
p <- melt.data.table(p, id.vars=c("Genotype","plot_UID","long_shift","longitude_r","Longitude_mean","Latitude_mean","S.N_RAT","N_plot","PPFR","PPFR_Fv.Fm","Irradiance","Absorbance","pMTCI","Hour","Heading","Date"), measure.vars = c("Fv.Fm"))

for (v in vars) {
  p <- remove_outliers(p, v)
}

p$Predictor <- (p$Irradiance)
hist(p$S.N_RAT)
p <- subset(p, S.N_RAT>20)
p[,N_plot:=length(unique(plot_UID)),by=.(long_shift,longitude_r)]
p <- subset(p, N_plot>2)
p <- na.omit(p)
p[,N:=nrow(.SD),by=.(long_shift,variable,longitude_r)]
hist(p$N)
adjust_means_shift <- p[,adjust_Means_Model(.SD),by=.(long_shift,variable,longitude_r)]

p <- adjust_means_shift
p <- melt.data.table(p, measure.vars = c("emmean","trend"))
p$SE <- p$SE_emmean
p$SE[p$variable.1=="trend"] <- p$SE_trend[p$variable.1=="trend"]

p$plot_UID <- p$Genotype
p1 <- merge(p, grid, by=c("plot_UID","longitude_r","long_shift"))
p1$variable.1 <- gsub("trend","Photosynthetic response G:PPFR",p1$variable.1 )
p <- subset(p1, variable.1=="emmean")

ggEmmean <- ggplot(p, aes(x=Longitude_mean, y=Latitude_mean,color=value)) +
  # ylab("Photosynthetic efficiency") +
  theme_bw() +
  theme(
    strip.placement = "outside",
    plot.title = element_text(hjust = -0.2),
    strip.background = element_blank(),
    legend.key = element_rect(size = 0.6, color = "white"),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "right",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  # geom_errorbar(aes(ymin = value - SE, ymax = value + SE), color = "grey", width = 0.00001) +
  geom_point(size = 3)+
  facet_grid(variable.1~.,scales = "free",switch="y")


p <- subset(p1, variable.1!="emmean")

ggTrend <- ggplot(p, aes(x=Longitude_mean, y=Latitude_mean,color=value)) +
  xlab("Longitude")+ylab("Latitude")+
  geom_path(data=polygon_df,aes(x = X, y = Y, group = interaction(id, part)),color = "black",linewidth=0.35) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    plot.title = element_text(hjust = -0.2),
    strip.background = element_blank(),
    legend.key = element_rect(size = 0.6, color = "white"),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "right",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  scale_color_gradientn(colors = tol4qualitative)+
  # geom_errorbar(aes(ymin = Latitude_mean - SE, ymax = Latitude_mean + SE), color = "grey", width = 0.00001) +
  geom_point(size = 1.5)+
  facet_grid(variable.1~.,scales = "free",switch="y")

# ggsave("LIFT_soybean_trend_heterogenity_FPSB018.png",  width = 160, height = 100, units = "mm",  ggTrend)


ggplot(p, aes(x=Genotype,y=value, color=Genotype))+
  theme_bw()+theme(legend.position="none")+
  geom_boxplot()+
  # geom_smooth(method='lm',formula=y~sqrt(x))+
  # scale_color_manual(values = tol12qualitative)+
  facet_grid(variable.1~.,scale="free_y")

ggTrend

require(cowplot)
ggHeterogenity <- plot_grid(ggEmmean,ggTrend, ncol=1, labels = "AUTO")


# ggsave("LIFT_soybean_heterogenity_ESSB019.png",  width = 190, height = 290, units = "mm",  ggHeterogenity)
ggPlots <- plot_grid(ggCoef, ggResponse, ncol=2, labels =  c("B","C"))
ggTrends <- plot_grid(ggTrend, ggPlots, ncol=1, labels = c("A",""))
# ggsave("LIFT_soybean_trends.png",  width = 170, height = 190, units = "mm",  ggTrends)

##########################################
design_22 <- fread("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2022/Design/20220513_Design_Measurements_FPSB016.csv")
names(design_22)[3] <- "plot_fip"
design_22$Sowing_date <- as.Date("2022-04-21")
design_22$treatment <- "Control"
names(design_22)

design_23 <- fread("/home/kellebea/public/Evaluation/Projects/KP0023_legumes/Soybean/2023/Design/20240209_Design_Measurements_FPSB017.csv")
design_23$Sowing_date <- as.Date("2022-04-27")
# design_23$plot_UID <- design_23$UID
# design_23$exp_UID  <- design_23$year_site_UID
# design_23$rep  <- design_23$replication

names(design_23)

design <- rbind(design_22,design_23,fill=T)
design <- unique(design)


yield_23 <- fread("/home/kellebea/public/Evaluation/FIP/RW/2023/SB017/Reftraits/db/yield/FPSB017_yield_data.csv")
protein_23 <- fread("/home/kellebea/public/Evaluation/FIP/RW/2023/SB017/Reftraits/db/protein/FPSB017_protein_data.csv")
protein_23$trait <- "protein"
yield_22 <- fread("/home/kellebea/public/Evaluation/FIP/RW/2022/SB016/Reftraits/db/yield/FPSB016_yield_data.csv")
protein_22 <- fread("/home/kellebea/public/Evaluation/FIP/RW/2022/SB016/Reftraits/db/protein/FPSB016_protein_data.csv")

yield_data <- rbind(yield_23, yield_22, protein_23, protein_22,fill=T)
yield_data$variable <- yield_data$trait

yield_data <- merge(yield_data, design, by="plot_UID")
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x}
yield_data$variable <- firstup(yield_data$variable )
yield_data$variable <- gsub("Protein","Protein.content",yield_data$variable )
yield_data$Location <- "Eschikon"

yield_data$genotype.id <- yield_data$Genotype_name
yield_data$Row <- yield_data$row
yield_data$Range <- yield_data$range
yield_data$year_site.UID <- yield_data$exp_UID
yield_data$Date <- yield_data$timestamp
yield_data$Rep <- yield_data$rep
yield_data <- subset(yield_data, treatment!="9_rows_80seeds_per_m2")
###



getSpats <- function(dat){   
  # dat <-subset(p1, variable=="Yield")
  
  require(SpATS)
  require(tidyr)
  require(plyr)
  
  print("Start with")
  print(paste(dat$variable[1], dat$year_site.UID[1]  ))
  
  
  dat<-dat  %>%  arrange(year_site.UID, Rep, Range, Row) 
  
  
  dat<- dat[!is.na(dat$value),]
  dat<-droplevels(dat)
  
  #Make a Row and Range block with unique numers for each Lot
  dat<-data.table(dat)
  # str(dat)
  
  
  ### set fields from different year_site.UIDs apart
  # levels(dat$year_site.UID)
  maxRowRange <- setDT(dat)[, list(maxRow=max(as.numeric(as.character(Row))),maxRange=max(as.numeric(as.character(Range))),minRow=min(as.numeric(as.character(Row))),minRange=min(as.numeric(as.character(Range))) ), by=year_site.UID]
  
  maxRowRange$RowAbove <- cumsum(maxRowRange$maxRow)
  maxRowRange$RangeAbove <- cumsum(maxRowRange$maxRange)
  maxRowRange$Distance <- 20
  maxRowRange$Distance <- cumsum(maxRowRange$Distance)
  
  maxRowRange$DistanceRow <- 20
  maxRowRange$DistanceRow <- cumsum(maxRowRange$DistanceRow)
  
  maxRowRange$RowAbove <- c(0,   maxRowRange$RowAbove[-length( maxRowRange$RowAbove)])
  maxRowRange$RangeAbove <- c(0,   maxRowRange$RangeAbove[-length( maxRowRange$RangeAbove)])
  
  dat <- merge(dat, maxRowRange, by="year_site.UID")
  # # make the factors
  dat$RowBL <- dat$Row+dat$RowAbove+dat$DistanceRow
  dat$RangeBL <- dat$Range+dat$RangeAbove+dat$Distance
  
  
  #It is important to have the right sorting range, row
  dat<-dat  %>%  arrange(year_site.UID, RangeBL,  RowBL)
  # dat$RangeBL
  dat$row_f = as.factor(dat$Row) 
  dat$col_f = as.factor(dat$Range)
  # dat$genotype.id = as.factor(dat$Genotype)
  
  
  dat$value.cleaned <- dat$value
  NAbefore <- length(dat$value.cleaned[is.na(dat$value.cleaned)])
  
  testSp<-dat
  head(testSp)
  testSp$year_site.UID <- as.factor(testSp$year_site.UI)
  ##### return cleaned data
  w <- 1                                         # Starter 
  k <- 5    
  
  # Number of standard deviations to consider extreme outliers 
  while (w>=1) {
    if(nlevels(testSp$year_site.UI)==1){
      fit.SpATS <- SpATS(response = "value.cleaned", random= ~ row_f+col_f,
                         spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(25,25), nest.div=c(5,5)),
                         genotype = "genotype.id", genotype.as.random = F, data = testSp)
    }else{
      fit.SpATS <- SpATS(response = "value.cleaned", random= ~ row_f+col_f+year_site.UID,
                         spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(25,25), nest.div=c(5,5)),
                         genotype = "genotype.id", genotype.as.random = F, data = testSp
      )   }
    
    
    Obs <- fit.SpATS$nobs
    Eff.dim <- sum(c(fit.SpATS$eff.dim))
    Var_resi <- sum( na.omit(residuals(fit.SpATS))^2 ) / (Obs - Eff.dim) # Sum(Errores^ 2)/ED_e
    vect_res <- residuals(fit.SpATS)
    
    # Number of extreme residuals (above k standard deviations) in this iteration
    w <- length( which( abs(vect_res) > abs(k * sqrt(Var_resi)) ) )
    print(w)
    # What is the most extreme residual ?
    p <- which( abs(vect_res) > abs(k * sqrt(Var_resi)) )[which.max( abs( vect_res[which(abs(vect_res) > abs(k * sqrt(Var_resi)))] ) )]
    
    
    testSp$value.cleaned[p] <- NA
    
    if(nlevels(testSp$year_site.UI)==1){
      fit.SpATS_H2 <- SpATS(response = "value.cleaned", random= ~ row_f+col_f, 
                            spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(25,25), nest.div=c(5,5)),
                            genotype = "genotype.id", genotype.as.random = T, data = testSp)
    }else{
      fit.SpATS <- SpATS(response = "value.cleaned", random= ~ row_f+col_f+year_site.UID,
                         spatial = ~PSANOVA(RangeBL, RowBL, nseg = c(25,25), nest.div=c(5,5)),
                         genotype = "genotype.id", genotype.as.random = F, data = testSp
      )   }
    
    h2_SpATS <- getHeritability(fit.SpATS_H2)  
    gc()
    cat("\n year_site.UID:",  levels(testSp$year_site.UID) , "\tN_xtreme_residuals:" , w, "\tHeritability in this iteration:", getHeritability(fit.SpATS_H2) )
    
  }
  
  NAafter <- length(testSp$value.cleaned[is.na(testSp$value.cleaned)])
  N_Datapt_removed=NAafter-NAbefore
  
  
  if(nlevels(testSp$year_site.UI)==1){
    png(paste0("FigSpATS_",paste(dat$variable[1], dat$year_site.UID[1]),round(h2_SpATS[1],2),".png"),  width = 168, height = 170, units = "mm", res = 100)
    plot(fit.SpATS, main=paste(paste(dat$variable[1]),"h2=",round(h2_SpATS[1],2)) )
    dev.off()
  }
  summary(fit.SpATS)
  
  
  #This part was taken from the plot.SpATS function
  ##############################################
  
  x<-fit.SpATS
  main=0
  xlab <-x$terms$spatial$terms.formula$x.coord
  ylab <-x$terms$spatial$terms.formula$y.coord
  
  x.coord <- x$data[, xlab]
  y.coord <- x$data[, ylab]
  
  response <- x$data[, x$model$response]
  #Unique ID consists of Plot_ID and Date
  Plot_ID<-x$data$plot
  year_site.UID<-x$data$year_site.UID
  
  # Date<-x$data$Date
  # value<-x$data$value # same as response
  genotype.id <- x$data$genotype
  Rep <- x$data$Rep
  #Model output to estimate the envrionment and Genotyp+Noise components
  intercept<-x$coeff['Intercept']
  fitted <- x$fitted
  residuals <- x$residuals
  ####################################
  #These functions from the SpATS Package are needed for running the code below
  ####################################
  
  construct.genotype.prediction.matrix <-
    function(object, newdata) {
      Z_geno = make_design_matrix(newdata[,object$model$geno$genotype], object$terms$geno$geno_names)
      if(object$model$geno$as.random)
        Z_geno <- Z_geno[,object$terms$geno$ndx]
      else
        Z_geno <- Z_geno[, object$terms$geno$ndx[2:length(object$terms$geno$ndx)]]
      Z_geno	
    }
  
  make_design_matrix <-
    function(geno, names) {
      Nrow = length(geno)
      Ncol = length(names)
      col = match(geno, names)
      frame = data.frame(i = c(1:Nrow), j = col, v = rep(1,Nrow))
      frame = subset(frame, is.na(col) == FALSE)
      L = as.list(frame)
      X = spam::spam(L, nrow = Nrow, ncol = Ncol)
      return(X)
    }
  geno.model.matrix <- construct.genotype.prediction.matrix(x,x$data)
  geno.coeff <- x$coeff[1:ncol(geno.model.matrix)]
  geno.pred <- as.vector(geno.model.matrix %*% geno.coeff)
  
  #from the plotting function, not sure why necessary
  residuals[x$data$weights == 0] <- NA
  fitted[x$data$weights == 0] <- NA
  geno.pred[x$data$weights == 0] <- NA
  
  #The SpATs SpatioTemporal data 
  Trait.SST<-fitted-geno.pred-intercept
  
  Trait.SGE<-intercept+geno.pred+residuals
  
  Fitted.SGE<-fitted
  #Fitted values plus residuals get the response data.                 
  plot(Trait.SGE, response) 
  plot(Trait.SST, response) 
  plot(residuals, response) 
  
  variable <- testSp$variable[1]
  Date <- testSp$Date[1]
  
  df <- data.table(Date, year_site.UID, Plot_ID, genotype.id, Rep, response,Trait.SST, Trait.SGE, Fitted.SGE,  residuals, h2_SpATS, N_Datapt_removed,variable)#, x.coord, y.coord)
  # names(df)[3:5]<-c(paste0(x$model$response,".SST"), paste0(x$model$response, ".SGE"), paste0(x$model$response, ".SRE"))
  df[] <- lapply(df,as.character) 
  
  BLUPs <- predict(object = fit.SpATS, which = 'genotype.id')
  BLUPs$year_site.UID <- year_site.UID[1]
  BLUPs$variable <- variable
  BLUPs$Date <- Date
  
  gc()
  
  return(list(df,BLUPs))
  # save dataframes
  # write.csv(dat, file = paste0("SpATScorr-20190130_",Z,"_",Y,".csv"))
  # save(VarPlt, file = paste0(Z,"_",Y,".rda"))
}


######
traits <- c("Yield","Protein.content" ) #,"TKW","Days_to_Max_Leaf.area.index","Max_fit_90_Leaf.area.index"

dat <- yield_data[,c("value","variable","Location","year_site.UID","Row","Range","Rep","plot_UID","genotype.id","Date")]
# subset(dat, year_site.UID=="FPSB008"&variable=="Yield")

p <- subset(dat, variable%in%c(traits)&value!=0)
p <- setDT(p)
p <- droplevels(p)
p <- p[,value_dup:=duplicated(value), by=.(variable,year_site.UID,Date,genotype.id)]
p <- subset(p, value_dup==FALSE)
Only_1_Rep <- setDT(p)[, length(Rep[!duplicated(Rep)]), by=.(variable, year_site.UID, Date)]
Only_1_Rep <- Only_1_Rep[Only_1_Rep$V1==1,]
# p <- subset(dat, variable%in%c(traits)&value!=0&!(variable%in%c("Protein.content")&year_site.UID=="FPSB012"))
Protein_traits <- subset(p, paste(p$year_site.UID, p$variable, p$Date)%in%paste(Only_1_Rep$year_site.UID, Only_1_Rep$variable, Only_1_Rep$Date))
Protein_traits <- Protein_traits[!duplicated(paste(Protein_traits$year_site.UID, Protein_traits$genotype.id, Protein_traits$value)),]
p <- subset(p, !paste(p$year_site.UID, p$variable, p$Date)%in%paste(Only_1_Rep$year_site.UID, Only_1_Rep$variable, Only_1_Rep$Date))

p$variable2 <- p$variable

p$value[p$value==Inf] <- NA

# p <- na.omit(p)
p <- p[!is.na(p$value),]
p <- droplevels(p)
p <- setDT(p)[, NperTrial:=nrow(na.omit(.SD)), by=.(variable2,year_site.UID,Date)]
p <- subset(p, NperTrial>35)
hist(p$NperTrial)

p$year_site.UID2 <- p$year_site.UID
p$Date2 <- p$Date

setDT(p)[, nrow(na.omit(.SD)), by=.(variable2, year_site.UID2, Date2)]
o <- setDT(p)[, nlevels(as.factor(Row)), by=.(variable2, year_site.UID2, Date2)]
min(o$V1)
o <- setDT(p)[, nlevels(as.factor(Range)), by=.(variable2, year_site.UID2, Date2)]
min(o$V1)

ggplot(p,aes(x=value, fill=year_site.UID, color=year_site.UID))+ ylab("Value")+
  theme_bw()+theme(plot.title=element_text(hjust=-0.2),strip.placement = "outside", panel.spacing.x = unit(-0.2, "lines"), strip.background = element_blank(),legend.title = element_blank(),legend.key.height=unit(0.5,"line"),legend.key.size = unit(1, "lines"), legend.position="top",panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.minor = element_blank(),panel.grid.major = element_blank(),axis.text.x = element_text(angle = 0, hjust = 0.5),text = element_text(size=10),axis.title = element_blank())+
  geom_density(alpha=0.5)+
  scale_color_manual(values = tol18rainbow)+
  scale_fill_manual(values = tol18rainbow)+
  facet_wrap(variable~., scales = "free", switch="both", labeller = label_parsed)

# p <- subset(p, variable%in%c("DF","Yield"))
# spats <- setDT(p)[, list( getSpats(.SD) ), by=.(variable2, year_site.UID2, Date2)]




######

p1 <- subset(dat, variable%in%c("Yield","Protein.content"))
p1$year_site.UID <- "AcrossAllEnv"
p1 <- droplevels(p1)
p1$variable2 <- p1$variable
setDT(p1)[, nrow(na.omit(.SD)), by=.(variable2)]
# p1 <- subset(p1, NperTrial>35)

# 
SpatsAcrossAllEnv <- setDT(p1)[, list( getSpats(.SD) ), by=.(variable2)] ## fix me: cannot plot figure. 
SpatsAcrossAllEnv$data <- rep(c("data","BLUE"), times=nrow(SpatsAcrossAllEnv)/2)

SpATsBLUE_overall <- rbindlist(SpatsAcrossAllEnv$V1[SpatsAcrossAllEnv$data=="BLUE"])
SpATsBLUE_overall$Genotype <- SpATsBLUE_overall$genotype.id
#####
# SpATsBLUE_overall_melt <- melt.data.table(SpATsBLUE_overall, measure.vars = c("predicted.values","standard.errors"))
SpATsBLUE_overall_cast <- dcast.data.table(SpATsBLUE_overall, Genotype~variable, value.var=c("predicted.values"))

###
yield_coef <- merge(adjMeans1, SpATsBLUE_overall_cast, by="Genotype")
# yield_coef$Protein.yield <- yield_coef$Protein.content*yield_coef$Yield
cor(yield_coef[,c("emmean","trend","Yield","Protein.content")])
plot(yield_coef$Protein.content, yield_coef$trend)
p <- yield_coef[order(yield_coef$trend), ]
p$Genotype = with(p, reorder(Genotype, trend))


p <- setDT(p)
p <- melt.data.table(p, measure.vars = c("emmean","trend","Yield","Protein.content"))
p$SE <- p$SE_emmean
p$SE[p$variable=="trend"] <- p$SE_trend


# Now, create the plot using the reordered Genotype levels
library(ggplot2)

ggCoef <- ggplot(p, aes(x=Genotype, y=value)) +
  ylab("Photosynthetic efficiency") +
  theme_bw() +
  theme(
    strip.placement = "outside",
    plot.title = element_text(hjust = -0.2),
    strip.background = element_blank(),
    legend.key = element_rect(size = 0.6, color = "white"),
    legend.key.size = unit(0.6, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = 11),
    axis.title = element_text(size = 11)
  ) +
  geom_errorbar(aes(ymin = value - SE, ymax = value + SE), color = "grey", width = 0.1) +
  geom_point(size = 3)+
  facet_grid(variable~.,scales = "free",switch="y")
ggCoef


######

image_timestamp <- fread("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/RGB/20250618_triggered/image_timestamps.csv")
image_timestamp


# Ensure Timestamp is POSIXct
image_timestamp[, Datetime := as.POSIXct(Timestamp, tz = "UTC")]
# Rename column for merge compatibility
setnames(image_timestamp, "Filename", "Image_Filename")
image_timestamp$Datetime[100:110]

ChlF_spectr_design_proc_copy <- subset(ChlF_spectr_design_proc, Date=="2025-06-13")
ChlF_spectr_design_proc_copy[, Datetime := as.POSIXct(Datetime, tz = "UTC")]
ChlF_spectr_design_proc_copy$Datetime[1:10]

# Create a merged version (don't modify original ChlF_spectr_design_proc)
ChlF_with_images <- merge(
  ChlF_spectr_design_proc_copy,  # original table (unchanged)
  image_timestamp,          # image timestamp table (renamed)
  by = "Datetime",
  all.x = F              # left join: keep all ChlF rows
)

# Optional: rename the image filename column to avoid confusion

# Check result

ChlF_with_images <- ChlF_with_images[!is.na(ChlF_with_images$Image_Filename)]
ChlF_with_images

### check number of data points

nrow(TransientData)
nrow(ChlF_gps_all)
nrow(ChlF_spectr_design_proc)


########

# assuming gps_out is your data.table
gps_out[, same_pos := (lon_use == shift(lon_use)) & (lat_use == shift(lat_use))]

# create a group ID for consecutive same positions
gps_out[, stand_group := rleid(same_pos)]

# count the number of rows per group and flag if it's a "standing" group with 4+ rows
gps_out[, n_in_group := .N, by = stand_group]
gps_out[, standing := same_pos & n_in_group >= 4]

gps_moving <- subset(gps_out, standing==F)

####

# image_timestamp <- fread("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/RGB/20250627.1/image_timestamps.csv")
image_timestamp <- fread("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/RGB/20250618_triggered/image_timestamps.csv")
names(image_timestamp)[1] <- "Filename_image"
image_timestamp$TimeSec <- as.POSIXct(image_timestamp$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz=time_zone)
image_timestamp$TimeMSec <- as.integer(image_timestamp$TimeSec) -4*3600

p <- image_timestamp
p <- p[grepl("06\\-13",p$Timestamp)]
p <- p[order(p$TimeMSec),]
p$TimeMSec <- as.integer(p$TimeSec)  + 417881.5 
p$Timestamp <- NULL
p$Timestamp <- as.POSIXct(p$TimeMSec, format="%Y-%m-%dT%H:%M:%S", tz=time_zone) ## fix time shift
image_timestamp <- rbind(p,image_timestamp)

pp <- TransientData
pp$Date <- as.Date(pp$Datetime)
pp <- subset(pp,Date=="2025-06-18")


merged_images <- merge(image_timestamp, ChlF_spectr_design_proc, by="TimeMSec")
merged_images <- merged_images[!is.na(merged_images$plot_UID),]
unique(merged_images$Genotype)

select_images_date <- subset(merged_images,Date=="2025-06-18"&TimeMSecR%in%gps_moving$TimeMSec)

select_images <- subset(merged_images,  Genotype=="10794")#90093
select_images <- subset(select_images, plot_UID==select_images$plot_UID[1])
select_images <- setDT(select_images)
select_images
select_images$Filename_image #fc69_save_2025-06-27-123016-0086.jpg
select_images$Timestamp 
# fwrite(as.list(select_images$Filename_image[10:15]), "select_images.csv")


image_timestamp <- fread("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/RGB/20250627.2/image_timestamps.csv")
names(image_timestamp)[1] <- "Filename_image"
image_timestamp$TimeSec <- as.POSIXct(image_timestamp$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz=time_zone)
image_timestamp$TimeMSec <- as.integer(image_timestamp$TimeSec)#-2*3600
image_timestamp
merged_images <- merge(image_timestamp, ChlF_spectr_design_proc, by="TimeMSec")
merged_images <- merged_images[!is.na(merged_images$plot_UID),]
unique(merged_images$Genotype)

p <- subset(ChlF_spectr_design_proc,Date=="2025-06-27"&TimeMSecR%in%gps_moving$TimeMSec)
max(p$Datetime)

select_images_date <- subset(merged_images,Date=="2025-06-27"&TimeMSecR%in%gps_moving$TimeMSec)

select_images <- subset(select_images_date,  Genotype=="10794")
select_images <- subset(select_images, plot_UID==select_images$plot_UID[1])
select_images <- setDT(select_images)
select_images
select_images$Datetime[6]+2*3600
select_images$Filename_image #fc69_save_2025-06-27-123016-0086.jpg
select_images$Timestamp 
# fwrite(as.list(select_images$Filename_image), "select_images_2.csv")






image_timestamp <- fread("~/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/RGB/20250612/image_timestamps.csv")
names(image_timestamp)[1] <- "Filename_image"
image_timestamp$TimeSec <- as.POSIXct(image_timestamp$Timestamp, format="%Y-%m-%dT%H:%M:%S", tz=time_zone)
image_timestamp$TimeMSec <- as.integer(image_timestamp$TimeSec)-4*3600
min(image_timestamp$Timestamp)
merged_images <- merge(image_timestamp, ChlF_spectr_design_proc, by="TimeMSec")
merged_images <- merged_images[!is.na(merged_images$plot_UID),]
unique(merged_images$Genotype)

p <- subset(ChlF_spectr_design_proc,Date=="2025-06-12"&TimeMSecR%in%gps_moving$TimeMSec)
min(p$Datetime)
max(p$Datetime)

select_images_date <- subset(merged_images,Date=="2025-06-12"&TimeMSecR%in%gps_moving$TimeMSec)

select_images <- subset(select_images_date,  Genotype=="10794")
select_images <- subset(select_images, plot_UID==select_images$plot_UID[1])
select_images <- setDT(select_images)
select_images
select_images$Datetime[6]+2*3600
select_images$Filename_image #
select_images$Timestamp 
# fwrite(as.list(select_images$Filename_image), "select_images_2.csv")