get_gps_data <- function(gps_file){
  gps_data <- fread(gps_file)
  gps_data$TimeMSec <- round(gps_data$timestamp / 1000000.0,digits=1)
  # p <- gps_data$TimeMSec[1]
  gps_data_mean <- gps_data[, lapply(.SD, mean), by = TimeMSec]
  return(gps_data_mean)
}

get_gps_data_filtered <- function(gps_data_mean,polygons){
  # Convert GPS data to sf object
  gps_sf <- st_as_sf(gps_data_mean, coords = c("longitude", "latitude"), crs = 4326)
  unique(gps_sf$geometry[1:10])
  ###
  
  
  
  # Check CRS of polygons
  print(st_crs(polygons))
  
  # If necessary, reproject polygons to match the GPS data CRS
  # polygons <- st_transform(polygons, 4326)
  
  coordinates_matrix <- st_coordinates(polygons)
  # Find the min and max for X and Y separately
  # Assuming coordinates_matrix is a matrix where each row represents a coordinate [x, y]
  
  # Extracting the first column (assumed to be X coordinates)
  x_coords <- coordinates_matrix[, 1]
  
  # Finding the indices of the min and max X values
  min_x_index <- which.min(x_coords)
  max_x_index <- which.max(x_coords)
  
  # Extracting the corresponding rows for min and max X
  point1 <- coordinates_matrix[min_x_index, ]
  point2 <- coordinates_matrix[max_x_index, ]
  
  # Similarly, for Y coordinates if needed
  y_coords <- coordinates_matrix[, 2]
  
  # Finding the indices of the min and max Y values
  min_y_index <- which.min(y_coords)
  max_y_index <- which.max(y_coords)
  
  # Extracting the corresponding rows for min and max Y
  point3 <- coordinates_matrix[min_y_index, ]
  point4 <- coordinates_matrix[max_y_index, ]
  
  
  ##
  library(sf)
  # big_square_bbox is already defined
  library(sf)
  
  # Assuming big_square_bbox is already defined
  # Correctly order the coordinates and close the polygon by repeating the first point
  big_square_polygon_coords <- rbind(point1,point3,point2,point4,point1)
  
  big_square_polygon_coords
  
  big_square_polygon_coords <- big_square_polygon_coords[, c("X", "Y")]
  library(sf)
  
  # Assuming big_square_polygon_coords is already defined
  big_square_polygon <- sf::st_polygon(list(big_square_polygon_coords))
  big_square_polygon <- st_zm(big_square_polygon)
  poly <- st_sfc(big_square_polygon)
  sf_poly <- st_as_sf(poly, crs = 4326)
  
  
  # Drop Z and M values
  sf_poly_nozm <- st_zm(sf_poly)
  plot(sf_poly_nozm)
  
  points_inside <- st_within(gps_sf$geometry, sf_poly_nozm)
  points_inside
  # Convert the list to a numeric vector with 0 and 1
  
  points_inside[1]
  points_inside[2]
  
  
  # Assuming points_inside is a list of lists, some of which may be empty
  points_vector <- sapply(points_inside, function(x) {
    if(length(x) > 0 && x[1] == 1) { 
      return(1) 
    } else { 
      return(0) 
    }
  })
  
  
  
  
  gps_data_filtered <- gps_data_mean[points_vector==1, ]
  return(gps_data_filtered)
}


get_ChlF_Transient <- function(ChlF_file){
  ChlF_data <- fread(ChlF_file,fill=TRUE, na.strings = c("==========","----","========="))
  
  # ChlF_data[1:20,]
  
  n_measurements_chlf <- length(na.omit(ChlF_data$V4[ChlF_data$V1=="DateTime:"]))
  print(paste(n_measurements_chlf,"measurements"))
  rows_per_measurment <- nrow(ChlF_data)/n_measurements_chlf
  flashlet_number <- nrow(ChlF_data)/n_measurements_chlf-17
  if(ChlF_data$V1[8]=="S/N Ratio:"){
    flashlet_number <- flashlet_number+3
  }
  ChlF_data$V5 <- NULL
  ChlF_data$V6 <- NULL
  ChlF_data$V7 <- NULL
  ChlF_data$V8 <- NULL
  
  TimeSecs <- na.omit(ChlF_data$V4[ChlF_data$V1=="DateTime:"])
  # Irradiance <- na.omit(ChlF_data$V4[ChlF_data$V1=="Irradiance:"])
  
  # dup_TimeSecs <- TimeSecs[duplicated(TimeSecs)]
  
  ChlF_data$TimeMSec<- rep(na.omit(ChlF_data$V4[ChlF_data$V1=="DateTime:"]),each=rows_per_measurment)
  # ChlF_data$TimeMSec[ChlF_data$TimeMSec==dup_TimeSecs] <- NA #fix me
  # ChlF_data <- ChlF_data[!is.na(ChlF_data$TimeMSec),]
  
  ChlF_data$SN_Rat <- rep(na.omit(ChlF_data$V2[ChlF_data$V1=="SNR_raw:"]),each=rows_per_measurment)
  ChlF_data$Irradiance <- rep(na.omit(ChlF_data$V2[ChlF_data$V1=="Irradiance:"]),each=rows_per_measurment)
  ChlF_data$Irradiance <- as.numeric(ChlF_data$Irradiance)
  ChlF_data$Transient <- rep(c(1:length(TimeSecs)),each=rows_per_measurment)
  ChlF_data$TimeMSec <- as.numeric(ChlF_data$TimeMSec)
  
  if(ChlF_data$V1[8]=="S/N Ratio:"){
    ChlF_data$SN_Rat <- rep(na.omit(ChlF_data$V2[ChlF_data$V1=="S/N Ratio:"]),each=rows_per_measurment)
    ChlF_data$TimeMSec <- as.numeric(ChlF_data$TimeMSec)*1000
    ChlF_data[, N := length(unique(Transient)), by = TimeMSec]
    ChlF_data[, N_per_second := rep(1:length(unique(Transient)),each=rows_per_measurment), by = TimeMSec]
    ChlF_data[, Max_per_second := length(unique(Transient)), by = TimeMSec]
    # hist(((ChlF_data$N_per_second-1)/  (ChlF_data$Max_per_second)  )*1000 )
    ChlF_data$TimeMSec <-     ChlF_data$TimeMSec +((ChlF_data$N_per_second-1)/  (ChlF_data$Max_per_second)  )*1000 # fix me
    ChlF_data$N_per_second <- NULL
    ChlF_data$Max_per_second <- NULL
    
    # ChlF_data[, SN_Rat := replace(SN_Rat, 1:13, NA), by = Transient]
  }
# p <- unique(ChlF_data$TimeMSec -7*10^12)[1:10]
# p
# p <- round(ChlF_data$TimeMSec / 1000.0,digits =1 )
# unique(p)[1:10]
  ChlF_data$TimeMSec <- round(ChlF_data$TimeMSec / 1000.0,digits = 1)
  # p <- ChlF_data$TimeMSec
  # p <- unique(p)[1:10]
  # p
  # p[duplicated(p)]
  # # dup_TimeSecs_2 <- na.omit(ChlF_data$TimeSecs[ChlF_data$V1=="DateTime:"])[duplicated(na.omit(ChlF_data$TimeSecs[ChlF_data$V1=="DateTime:"]))]
  
  # p1 <- ChlF_data$TimeMSec[1]
  
  # Convert to POSIXct datetime object
  ChlF_data$Datetime <- as.POSIXct(ChlF_data$TimeMSec , origin="1970-01-01", tz=time_zone)
  
  # Print the datetime
  # print(ChlF_data$Datetime[1])
  number_of_header_lines <- which(ChlF_data$V1==0)[1] -1
  
  # check names
  names(ChlF_data)[1:4] <- as.character(ChlF_data[(number_of_header_lines-1),1:4])
  names(ChlF_data) <- gsub("\\(us\\)", ".us",names(ChlF_data))
  
  
  Transient<-na.omit(ChlF_data)
  # Transient[1:20,]
  Transient$EX <- NULL
  
  Transient <- Transient[, .SD[number_of_header_lines:.N], by = .(TimeMSec,Transient)]
  # Transient$Time.us <- NULL
  
  nrow(Transient)/(flashlet_number)
  Transient$Transient = rep(1:(nrow(Transient)/(flashlet_number)),each=flashlet_number)
  
  
  Transient$EM<-as.numeric(Transient$EM)
  Transient$DataPt<-as.numeric(Transient$DataPt)
  
  names(Transient) <- gsub("EM","value", names(Transient))
  class(Transient$value)
  
  Transient <- Transient[,.SD[sum(value)!=0,],by = .(TimeMSec,Transient)]
  Transient$Fo<- rep((Transient[DataPt%in%1, mean(value), by = .( Transient,TimeMSec)])$V1, each=flashlet_number)
  Transient$Fm<- rep((Transient[DataPt%in%302, mean(value), by = .( Transient,TimeMSec)])$V1, each=flashlet_number)
  
  Transient$Relative <- (Transient$value  -   Transient$Fo)  /   (Transient$Fm  -   Transient$Fo)
  Transient$RelativeFm <- Transient$value  /   Transient$Fm
  
  Transient$value <- NULL
  
  Transient$Fi.Fm <-rep((Transient[DataPt%in%1:300, mean(Relative), by = .( Transient,TimeMSec)])$V1, each=flashlet_number)
  Transient$S.N_RAT <- 1/rep((Transient[DataPt%in%280:300, sd(Relative), by = .( Transient,TimeMSec)])$V1, each=flashlet_number)
  
  Transient <- as.data.table(Transient)
  Transient <- Transient[!is.na(Relative),]
  Transient <- Transient[!is.na(S.N_RAT),]
  Transient$Filename <- gsub("_data.csv","",ChlF_file)
  
  # Transient <- na.omit(Transient)
  
  # Transient$Saturation <- rep((Transient[DataPt%in%280:300, (.lm.fit(as.matrix(cbind(1,DataPt)), y=Relative)$coefficients[2]), by = .(TimeMSec)])$V1, each=flashlet_number)
  
  return(Transient)
}


get_ChlF_parameters <- function(Transient){
  # Transient$Sig<- rep(c(sum(Transient$Relative[Transient$DataPt%in%1:300])), each=flashlet_number )
  TransientData<-subset(Transient, DataPt>299)
  TransientData$Fv.Fm<-(TransientData$Fm-TransientData$Fo) /TransientData$Fm
  # TransientData<-subset(TransientData, S.N_RAT>300)
  # nrow(TransientData)/(flashlet_number+1-300)
  
  TransientData[, carQ:=1-mean(Relative[DataPt%in%301:302]), by = .(Transient,TimeMSec)]
  
  TransientData$Time.us <- as.numeric(TransientData$Time.us)
  TransientData[,TimeusDif:=Time.us-shift(Time.us,1),by=.(Transient,TimeMSec)]
  
  TransientData$Fr.Fv <-   TransientData$TimeusDif*TransientData$Relative#/(1+TransientData$carQ)
  
  TransientData[, Fr0.Fv:=1-sum(Fr.Fv[DataPt%in%303:304])/ sum(TimeusDif[DataPt%in%303:304]), by = .(Transient,TimeMSec)]
  TransientData[, Fr1.Fv:=1-sum(Fr.Fv[DataPt%in%303:321])/ sum(TimeusDif[DataPt%in%303:321]), by = .(Transient,TimeMSec)]
  TransientData[, Fr2.Fv:=1-sum(Fr.Fv[DataPt%in%303:354])/ sum(TimeusDif[DataPt%in%303:354]), by = .(Transient,TimeMSec)]
  TransientData[, Fr3.Fv:=1-sum(Fr.Fv[DataPt%in%303:400])/ sum(TimeusDif[DataPt%in%303:400]), by = .(Transient,TimeMSec)]
  
  
  TransientData$Time.us[TransientData$DataPt==304][1]-TransientData$Time.us[TransientData$DataPt==302][1]
  TransientData$Time.us[TransientData$DataPt==321][1]-TransientData$Time.us[TransientData$DataPt==302][1]
  TransientData$Time.us[TransientData$DataPt==354][1]-TransientData$Time.us[TransientData$DataPt==302][1]
  TransientData$Time.us[TransientData$DataPt==400][1]-TransientData$Time.us[TransientData$DataPt==302][1]
  
  
  TransientData$Fr.Fv <- NULL
  TransientData$Time.us <- NULL
  TransientData$TimeusDif <- NULL  
  # TransientData$Relative <- NULL
  
  TransientData <- subset(TransientData, DataPt==300)
  return(TransientData)
}

get_spectro_data <- function(ChlF_file) {
  
Spectro_file <- gsub("_data","_spectr",ChlF_file)
Spectro_data_raw <- fread(Spectro_file,fill=TRUE, na.strings = c("==========","----","=========","======"))

n_measurements_spectr <- length(na.omit(Spectro_data_raw$V4[Spectro_data_raw$V1=="DateTime:"]))

# if(n_measurements_chlf-n_measurements_spectr!=0){print("No complete pairs of ChlF and spectro measurements")}
print(paste(n_measurements_spectr,"measurements"))

ff <- nrow(Spectro_data_raw)/(n_measurements_spectr)
ff_floor <- ceiling(ff)
if (all.equal(ff,ff_floor)!=T) {
  print("Incomplete measurement. Cut rows at the end")
  n_measurements_spectr <-   n_measurements_spectr-1
  nrow_new  <- ff_floor*(n_measurements_spectr)
  Spectro_data_raw <- Spectro_data_raw[1:nrow_new,]
  ff <- ff_floor
}


TimeSecs <- na.omit(Spectro_data_raw$V4[Spectro_data_raw$V1=="DateTime:"])
Spectro_data_raw$TimeUSec<- rep(na.omit(Spectro_data_raw$V4[Spectro_data_raw$V1=="DateTime:"]),each=ff)
Spectro_data_raw$Transient <- rep(c(1:length(TimeSecs)),each=ff)


if( nchar(Spectro_data_raw$TimeUSec[1]) ==10){
  rows_per_measurment <- ff
  Spectro_data_raw$TimeUSec <- as.numeric(Spectro_data_raw$TimeUSec)*1000
  Spectro_data_raw[, N_per_second := rep(1:length(unique(Transient)),each=rows_per_measurment), by = TimeUSec]
  Spectro_data_raw[, Max_per_second := length(unique(Transient)), by = TimeUSec]
  
  Spectro_data_raw$TimeUSec <-     Spectro_data_raw$TimeUSec +((Spectro_data_raw$N_per_second-1)*  (1000/Spectro_data_raw$Max_per_second)   ) # fix me
  Spectro_data_raw$N_per_second <- NULL
  Spectro_data_raw$Max_per_second <- NULL
  }




# Spectro_data_raw[1:20,]
number_of_header_lines <- which(Spectro_data_raw$V1=="Pixel")[1]

names(Spectro_data_raw)[1:3] <- as.character(Spectro_data_raw[number_of_header_lines,1:3])
Spectro_data_raw$V4 <- NULL
Spectro_data_raw$V5 <- NULL

Spectro_mean <- Spectro_data_raw[, .SD[(number_of_header_lines+2):.N], by = .(TimeUSec,Transient)]
Spectro_mean$TimeUSec <- as.numeric(Spectro_mean$TimeUSec)
Spectro_mean$TimeMSec <- round(Spectro_mean$TimeUSec / 1000.0,digits = 1)



Spectro_mean$Wavelength<-as.numeric(Spectro_mean$Wavelen)
Spectro_mean$Signal<-as.numeric(Spectro_mean$Signal)

Spectro_mean$Wavelength<-c(round(signif(Spectro_mean$Wavelength, digits = 3)/2)*2)
Spectro_mean = Spectro_mean[,list(Signal = mean(Signal)), by = .(Wavelength,TimeMSec,Transient)]
n_wavelength <- nrow(Spectro_mean)/(n_measurements_spectr)
# Spectro_mean$Transient = rep(1:n_measurements_spectr,each=n_wavelength)


Spectro_mean$Datetime <- as.POSIXct(Spectro_mean$TimeMSec , origin="1970-01-01", tz=time_zone)
Spectro_mean$Datetime <-as.POSIXct(round(Spectro_mean$Datetime,"mins"))
Spectro_mean$TimeSecR <- as.integer(Spectro_mean$Datetime )
return(Spectro_mean)
}

get_spectro_parameter <- function(ChlF_file,References,PPFR_data) {
  Spectro_file <- gsub("_data","_spectr",ChlF_file)
  
  Spectro_mean_all <- get_spectro_data(ChlF_file)
  # Spectro_mean_PPFR <- merge(Spectro_mean, PPFR_data, by="TimeSecR",all.x = T)

  PPFR_data      <- as.data.table(PPFR_data)
  Spectro_mean   <- as.data.table(Spectro_mean_all)

  # Create common time column for join
  Spectro_mean[, t_sec := TimeMSec]
  PPFR_data[,   t_sec := TimeSec]
  
  # Save original time from Spectro_mean before it's lost in join
  Spectro_mean[, t_sec_orig := t_sec]
  
  # Set keys for rolling join
  setkey(PPFR_data, t_sec)
  setkey(Spectro_mean, t_sec)
  
  # Rolling nearest join: for each Spectro row, find nearest PPFR
  Spectro_mean_PPFR <- PPFR_data[Spectro_mean, roll = "nearest"]
  
  # Compute time difference between Spectro_mean's original time and matched PPFR
  Spectro_mean_PPFR[, time_diff := abs(t_sec - t_sec_orig)]
  
  # Null out PPFR values where time mismatch is too big (>120 sec)
  Spectro_mean_PPFR[time_diff > 120, PPFR := NA_real_]
  
  # Clean up and restore time column name
  Spectro_mean_PPFR[, `:=`(TimeSec = t_sec_orig)]
  Spectro_mean_PPFR[, c("t_sec", "t_sec_orig", "time_diff") := NULL]
  
  References <- setDT(References)
  References$SD <- NULL
  References$PPFR <- as.numeric(References$PPFD)
  References$PPFD <- NULL
  Spectro_mean_PPFR <-(References[,.SD,by = .(Wavelength)][Spectro_mean_PPFR,roll="nearest", on=c("Wavelength","PPFR")])
  Spectro_mean_PPFR$Reflectance <- Spectro_mean_PPFR$i.Signal/Spectro_mean_PPFR$Signal
  Spectro_mean_PPFR$Reflectance <- Spectro_mean_PPFR$Reflectance*50 # Grey reference reflecting 50% 
  
  Spectro_mean_PPFR$Signal <- Spectro_mean_PPFR$i.Signal
  Spectro_mean_PPFR$i.Signal <- NULL
  nrow(Spectro_mean_PPFR)/201
  References<- NULL
  
  Spectro_mean_PPFR$Absorbance <-  rep((Spectro_mean_PPFR[Wavelength%in% c(500:700), mean(Reflectance), by = .(TimeMSec,Transient)])$V1, each=(201)) ## 
  Spectro_mean_PPFR$Absorbance <-  1-Spectro_mean_PPFR$Absorbance/100
  Spectro_mean_PPFR$Reflectance_tot <-  rep((Spectro_mean_PPFR[Wavelength%in% c(500:700), sum(Signal), by = .(TimeMSec,Transient)])$V1, each=(201)) ## 
  
  
  Spectro_data<-subset(Spectro_mean_PPFR, Wavelength %in% c(800,680,740,750,710,706,754,710,660,570,530,780,540,410,540,530,550,446,416,436))  ###
  Spectro_data<-as.data.table(Spectro_data)
  Spectro_data$Filename <- gsub("_data.csv","",ChlF_file)
  
  
  
  Spectro_data<-dcast.data.table(Spectro_data,TimeMSec+Transient+Absorbance+Reflectance_tot+Filename+PPFR+Temperature ~Wavelength  , value.var=c("Signal","Reflectance"))
  Spectro_data <- as.data.frame(Spectro_data)
  colnames(Spectro_data)<- gsub("Reflectance_", "R",names(Spectro_data))
  colnames(Spectro_data)<- gsub("Signal_", "X",names(Spectro_data))
  
  nrow(Spectro_data)
  
  
  
  Spectro_data$NDVI <- (Spectro_data$R750-Spectro_data$R706)/(Spectro_data$R750+Spectro_data$R706)
  Spectro_data$NDVI_I <- (Spectro_data$R800-Spectro_data$R680)/(Spectro_data$R800+Spectro_data$R680)
  Spectro_data$NDVI_II <- (Spectro_data$R740-Spectro_data$R680)/(Spectro_data$R740+Spectro_data$R680)
  Spectro_data$NDVI_III <- (Spectro_data$R800-Spectro_data$R660)/(Spectro_data$R800+Spectro_data$R660)
  Spectro_data$MTCI_K <- (Spectro_data$R754-Spectro_data$R710)/(Spectro_data$R710+Spectro_data$R680)
  Spectro_data$MTCI <- (Spectro_data$R754-Spectro_data$R710)/(Spectro_data$R710-Spectro_data$R680)
  Spectro_data$MTCI_I <- Spectro_data$R750/Spectro_data$R710
  
  Spectro_data$PRI <- (Spectro_data$R530-Spectro_data$R570)/(Spectro_data$R530+Spectro_data$R570)
  Spectro_data$GNDVI <- (Spectro_data$R740-Spectro_data$R540)/(Spectro_data$R740+Spectro_data$R540)
  
  Spectro_data$NSDI_1 <- (Spectro_data$R410-Spectro_data$R710)/(Spectro_data$R410+Spectro_data$R710)
  Spectro_data$NSDI_2 <- (Spectro_data$R540-Spectro_data$R410)/(Spectro_data$R540+Spectro_data$R410)
  Spectro_data$NSDI_3 <- (Spectro_data$R710-Spectro_data$R530)/(Spectro_data$R710+Spectro_data$R530)
  Spectro_data$NSDI_4 <- (Spectro_data$R530-Spectro_data$R550)/(Spectro_data$R530+Spectro_data$R550)
  
  Spectro_data$SIPI <- (Spectro_data$R800-Spectro_data$R446)/(Spectro_data$R800+Spectro_data$R680)
  Spectro_data$NPQI <- (Spectro_data$R416-Spectro_data$R436)/(Spectro_data$R416+Spectro_data$R436)
  
  
  Spectro_data$pNDVI <- (Spectro_data$X750-Spectro_data$X706)/(Spectro_data$X750+Spectro_data$X706)
  Spectro_data$pNDVI_I <- (Spectro_data$X800-Spectro_data$X680)/(Spectro_data$X800+Spectro_data$X680)
  Spectro_data$pNDVI_II <- (Spectro_data$X740-Spectro_data$X680)/(Spectro_data$X740+Spectro_data$X680)
  Spectro_data$pNDVI_III <- (Spectro_data$X800-Spectro_data$X660)/(Spectro_data$X800+Spectro_data$X660)
  Spectro_data$pPRI <- (Spectro_data$X530-Spectro_data$X570)/(Spectro_data$X530+Spectro_data$X570)
  Spectro_data$pGNDVI <- (Spectro_data$X740-Spectro_data$X540)/(Spectro_data$X740+Spectro_data$X540)
  Spectro_data$pMTCI <- (Spectro_data$X754-Spectro_data$X710)/(Spectro_data$X710-Spectro_data$X680)
  Spectro_data$pMTCI_K <- (Spectro_data$X754-Spectro_data$X710)/(Spectro_data$X710+Spectro_data$X680)
  
  Spectro_data$pNSDI_1 <- (Spectro_data$X410-Spectro_data$X710)/(Spectro_data$X410+Spectro_data$X710)
  Spectro_data$pNSDI_2 <- (Spectro_data$X540-Spectro_data$X410)/(Spectro_data$X540+Spectro_data$X410)
  Spectro_data$pNSDI_3 <- (Spectro_data$X710-Spectro_data$X530)/(Spectro_data$X710+Spectro_data$X530)
  Spectro_data$pNSDI_4 <- (Spectro_data$X530-Spectro_data$X550)/(Spectro_data$X530+Spectro_data$X550)
  
  Spectro_data$pSIPI <- (Spectro_data$X800-Spectro_data$X446)/(Spectro_data$X800+Spectro_data$X680)
  Spectro_data$pNPQI <- (Spectro_data$X416-Spectro_data$X436)/(Spectro_data$X416+Spectro_data$X436)
  return(Spectro_data)
}
