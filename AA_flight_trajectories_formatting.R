

# Loading packages
library(rgl)
library(scatterplot3d)
library(gdata)
library(mFilter)
library(signal)
library(matlib)



local_path = dirname(rstudioapi::getSourceEditorContext()$path)

############################# importing/formating flight coordinates ############################# 

# trajectory coordinates have already been smoothed using the Kalman filter method
# this was done in Matlab using the code 'kalman_filtering_Camille.m'
mypath = paste0(local_path,'/00_Digitized_Coordinates_only3D/kalmanFiltered_coordinates/All_fps/')

smooth_coords_files = dir(mypath, pattern = glob2rx("*.csv")) # vector containing the names of all the .csv files

total_frames = rep(NA, length(smooth_coords_files))
# empty vector containing as many 'NA' as there is objects in smooth_coords_files

for(i in 1:(length(smooth_coords_files) )){
  scan_loop= c(mypath, smooth_coords_files[i]) # automatically read in data for multiple files in a directory
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=',', dec='.'))
  # create a matrix pasting all data in the .csv files that are in the folder indicated by the path
  
  total_frames[i] = length(((i-1)*length(read.smooth_coords_files[,1])+1):(i*length(read.smooth_coords_files[,1])))
}
# total.frame contains now the number of frame for each sequence (= each files in the folder)

flight = matrix(NA, sum(total_frames), 5)

i=1
scan_loop = c(mypath, smooth_coords_files[i]) # automatically read the ith files of the path
read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))

flight[1:total_frames[i],] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)

for(i in 2:(length(smooth_coords_files))){
  scan_loop = c(mypath, smooth_coords_files[i]) 
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
  flight[(sum(total_frames[1:(i-1)]) +1):((sum(total_frames[1:(i-1)])) + total_frames[i]),] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)
}

flight <- flight[,c(4,1,2,3,5)]
colnames(flight) <-  c('time', 'X', 'Y', 'Z', 'index')


######################## Correcting for the 120 fps sequences ########################

# time steps in sequences shot at 120 fps must be changed 
files_ID <- read.csv(paste0(local_path,'/00_Digitized_Coordinates_only3D/files_ID.csv'), h=T,sep=";")
files_ID$species = as.factor(files_ID$species)
files_ID$ecology = as.factor(files_ID$ecology)
files_ID$flight_ID = as.factor(files_ID$flight_ID)

seq_120fps <- which(files_ID$fps == 120)
for(i in seq_120fps){
  flight[which(flight[,5]==i), 1] = seq(from=0, to=(1/120)*(total_frames[i]-1), by=1/120)
}


############################ Storing data in an array ############################ 

Flight = array(NA, dim= c(max(total_frames), 4, length(smooth_coords_files)))
colnames(Flight) <-  c('time', 'X', 'Y', 'Z')

for(i in 1:length(smooth_coords_files)){
  Flight[1:total_frames[i],,i] = flight[flight[,5] == i, 1:4]
}



############################ plotting trajectory ############################

# the flight trajectory 'ID' is the 3th dimension in the array (n in flight2[,2:4,n])

# plot3d(Flight[,2:4,14], typ="l", col="blue", lwd=3, xlab="X", ylab="Y", zlab="Z", aspect=F)



############################# importing/formating stroke position coordinates ############################# 

# stroke positions have been digitized in parallel to the raw (non-smoothed) flight coordinates.
# starting and ending frame of the digitized trajectory within the raw files must be find in order to
# correctly align the stroke positions with the smoothed flight path.

## importing/formating original coordinates
path_to_raw = paste0(local_path,'/00_Digitized_Coordinates_only3D/raw_3Dcoords_All_fps/')
raw_coords_files = dir(path_to_raw, pattern = glob2rx("*.csv")) # vector containing the names of all the .csv files

total_raw_frames = rep(NA, length(raw_coords_files))

for(i in 1:(length(raw_coords_files) )){
  scan_loop = c(path_to_raw, raw_coords_files[i])
  read.raw_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=',', dec='.'))
  total_raw_frames[i] = length(((i-1)*length(read.raw_coords_files[,1])+1):(i*length(read.raw_coords_files[,1])))
}

raw_flight = matrix(NA, sum(total_raw_frames), 5)

i=1
scan_loop = c(path_to_raw, raw_coords_files[i]) # automatically read the ith files of the path
read.raw_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
raw_flight[1:total_raw_frames[i],] = cbind(read.raw_coords_files, seq(from=0, to=(1/240)*(total_raw_frames[i]-1), by=1/240), i)

for(i in 2:(length(raw_coords_files))){
  scan_loop = c(path_to_raw, raw_coords_files[i]) 
  read.raw_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
  raw_flight[(sum(total_raw_frames[1:(i-1)]) +1):((sum(total_raw_frames[1:(i-1)])) + total_raw_frames[i]),] = cbind(read.raw_coords_files, seq(from=0, to=(1/240)*(total_raw_frames[i]-1), by=1/240), i)
}

raw_flight <- raw_flight[,c(4,1,2,3,5)]
colnames(raw_flight) <-  c('time', 'X', 'Y', 'Z', 'index') # time is not corrected for the 120fps sequences

# correcting for the 120 fps sequences
for(i in seq_120fps){
  raw_flight[which(raw_flight[,5]==i), 1] = seq(from=0, to=(1/120)*(total_raw_frames[i]-1), by=1/120)
}

# storing data in an array
raw_Flight = array(NA, dim= c(max(total_raw_frames), 4, length(raw_coords_files)))
colnames(raw_Flight) <-  c('time', 'X', 'Y', 'Z')
for(i in 1:length(raw_coords_files)){
  raw_Flight[1:total_raw_frames[i],,i] = raw_flight[raw_flight[,5] == i, 1:4]
}

# find first and last digitized frame in raw coordinates
first_digit_frame = matrix(NA, length(raw_coords_files), 1)
last_digit_frame = matrix(NA, length(raw_coords_files), 1)
for (i in 1:length(raw_coords_files)){
  first_digit_frame[i] = which(is.na(raw_Flight[,2,i])==F)[1]
  last_digit_frame[i] = last(which(is.na(raw_Flight[,2,i])==F))
}



############################# aligning stroke positions with flight path #############################  

path_to_strokes = paste0(local_path,'/00_Digitized_Stroke_Positions/')
stroke_files = dir(path_to_strokes, pattern= glob2rx("*.csv"))

total_stroke_frames = rep(NA, length(stroke_files))

for(i in 1:(length(stroke_files))){
  scan_loop= c(path_to_strokes, stroke_files[i])
  read.stroke_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=',', dec='.'))  
  total_stroke_frames[i] = length(((i-1)*length(read.stroke_files[,1])+1):(i*length(read.stroke_files[,1])))
}

strokes = matrix(NA, sum(total_stroke_frames), 3)

i=1
scan_loop = c(path_to_strokes, stroke_files[i])
read.stroke_files= as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
strokes[1:total_stroke_frames[i],] = cbind(read.stroke_files[,2], seq(from=0, to=(1/240)*(total_stroke_frames[i]-1), by=1/240), i)

for(i in 2:(length(stroke_files))){
  scan_loop= c(path_to_strokes, stroke_files[i]) 
  read.stroke_files= as.matrix(read.csv(paste(scan_loop, collapse= ""), header=T, sep=",", dec="."))
  strokes[(sum(total_stroke_frames[1:(i-1)]) +1):((sum(total_stroke_frames[1:(i-1)])) + total_stroke_frames[i]),] = cbind(read.stroke_files[,2], seq(from=0, to=(1/240)*(total_stroke_frames[i]-1), by=1/240), i)
}

# reorder the columns to put time in first
strokes <- strokes[,c(2,1,3)]
colnames(strokes) <-  c('time', 'Y', 'index')

# storing data in an array
Strokes = array(NA, dim= c(max(total_stroke_frames), 2, length(stroke_files)))
colnames(Strokes) <-  c('time','Y')
for(i in 1:length(stroke_files)){
  Strokes[1:total_stroke_frames[i],,i] = strokes[strokes[,3] == i, 1:2]
}

# binding the stroke positions coordinates next to the smoothed coordinates
# we add separated columns for down and upstrokes
Flight_and_strokes = array(NA, dim= c(dim(Flight)[1], 6, length(smooth_coords_files)))
colnames(Flight_and_strokes) <-  c('time', 'X', 'Y', 'Z', 'stroke_value', 'stroke')
Flight_and_strokes[,1:4,] = Flight

# binding stroke values (<240 for downstroke and >240 for upstroke) in the 'stroke_value' column
for (i in 1:dim(Flight)[3]){
  Flight_and_strokes[1:total_frames[i],5,i] = Strokes[first_digit_frame[i]:(last_digit_frame[i]-1),2,i]
}
# indexing 'ds' for downstroke and 'us' for upstroke in the 'stroke' column
for (i in 1:dim(Flight)[3]){
  seq_now_ds = which(as.numeric(Flight_and_strokes[1:total_frames[i],5,i]) < 480/2)
  seq_now_us = which(as.numeric(Flight_and_strokes[1:total_frames[i],5,i]) > 480/2)
  Flight_and_strokes[seq_now_ds,6,i] = 'ds'
  Flight_and_strokes[seq_now_us,6,i] = 'us'
}


# ############################ cutting noisy frames ############################
# number of (first and last) frame to remove
c = 30

# remove first frame
Flight_and_strokes = Flight_and_strokes[c:max(total_frames),,]
# update total_frames
for (i in 1:length(smooth_coords_files)){
  if(is.na(last(Flight_and_strokes[,1,i]))==F){total_frames[i] = which(Flight_and_strokes[,1,i]==last(Flight_and_strokes[,1,i]))}
  else {
    total_frames[i] = which(is.na(Flight_and_strokes[,1,i])==T)[1] - 1
  }
}
# remove last frames
for (i in 1:length(smooth_coords_files)){
  Flight_and_strokes[(total_frames[i] - (c-1)):total_frames[i],,i] = NA
}
# update total_frames (again)
for (i in 1:length(smooth_coords_files)){
  if(is.na(last(Flight_and_strokes[,1,i]))==F){total_frames[i] = which(Flight_and_strokes[,1,i]==last(Flight_and_strokes[,1,i]))}
  else {
    total_frames[i] = which(is.na(Flight_and_strokes[,1,i])==T)[1] - 1
  }
}
Flight_and_strokes = Flight_and_strokes[1:max(total_frames),,]



# isolating 3D coordinates of the downstrokes only
ds_coords = array(NA, dim= c((max(total_frames)), 3, length(smooth_coords_files)))
for (i in  1:length(smooth_coords_files)){
  seq_now_ds = which(as.numeric(Flight_and_strokes[1:total_frames[i],5,i]) < 480/2)
  ds_coords[seq_now_ds,1:3,i] = Flight_and_strokes[seq_now_ds,2:4,i]
}
# isolating 3D coordinates of the upstrokes only
us_coords = array(NA, dim= c((max(total_frames)), 3, length(smooth_coords_files)))
for (i in  1:length(smooth_coords_files)){
  seq_now_us = which(as.numeric(Flight_and_strokes[1:total_frames[i],5,i]) > 480/2)
  us_coords[seq_now_us,1:3,i] = Flight_and_strokes[seq_now_us,2:4,i]
}


#################### plotting simultaneously flight path and stroke positions ####################

# p=185
# plot3d(Flight_and_strokes[,2:4,p], aspect=F, typ="p", col="black", xlab="X", ylab="Y", zlab="Z", lwd=1,  box=F)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)



#################### extracting duration of stroke intervals #################### 

# indexing stroke intervals
stroke_intervals = array(NA, dim = c((max(total_frames)), 3, length(smooth_coords_files)))
stroke_intervals[,1:2,] = Flight_and_strokes[,5:6,]

for (t in 1:length(smooth_coords_files)){
  if (is.na(stroke_intervals[1,2,t])==F){stroke_intervals[1,3,t] = 1; k=1}
  if (is.na(stroke_intervals[1,2,t])==T){stroke_intervals[1,3,t] = 0; k=0}
  for (i in 2:total_frames[t]){
    if (is.na(Flight_and_strokes[i,6,t])==T & k==0){stroke_intervals[i,3,t] = 0}
    if (is.na(Flight_and_strokes[i,6,t])==T & k==1){stroke_intervals[i,3,t] = 1}
    if (is.na(Flight_and_strokes[i,6,t])==T & k>1){stroke_intervals[i,3,t] = k}
    if (is.na(Flight_and_strokes[i,6,t])==F){k=k+1 ; stroke_intervals[i,3,t] = k}
  }
}

# correcting for sequence in which last digitized frame is on a stroke (e.g. n?237)
for (i in 1:length(smooth_coords_files)){
  if(is.na(stroke_intervals[total_frames[i],2,i])==F){
    stroke_intervals[total_frames[i],3,i] = stroke_intervals[total_frames[i]-1,3,i]}
}

# extracting the number of stroke intervals
nb_stroke_intervals = matrix(NA,length(smooth_coords_files),1)
for (i in 1:length(smooth_coords_files)){
  nb_stroke_intervals[i] = stroke_intervals[total_frames[i],3,i]
}
nb_stroke_intervals = as.numeric(nb_stroke_intervals)

# compute duration of stroke intervals
stroke_intervals_duration = array(NA, dim= c((max(nb_stroke_intervals)), 2, length(smooth_coords_files)))
colnames(stroke_intervals_duration) <-  c('ID_stroke_interval', 'duration')
# indexing stroke interval in the first column
for (i in 1:length(smooth_coords_files)){
  for(j in 1:nb_stroke_intervals[i]){
    stroke_intervals_duration[j,1,i] = j
  }
}
# computing time of each stroke interval in the second column
for (i in 1:length(smooth_coords_files)){
  for(j in 1:nb_stroke_intervals[i]){
    stroke_intervals_duration[j,2,i] = length(which(stroke_intervals[,3,i] == j)) * (1/files_ID$fps[i])
  }
}

All_stroke_intervals = matrix(NA, (sum(nb_stroke_intervals)+1), 2)
All_stroke_intervals[,1] = seq(1, nrow(All_stroke_intervals),1)

All_stroke_intervals[1:nb_stroke_intervals[1],2] = stroke_intervals_duration[1:nb_stroke_intervals[1],2,1]
for (i in 2:length(smooth_coords_files)){
  start_now = last(na.omit(All_stroke_intervals))[1] + 1
  All_stroke_intervals[start_now:(start_now + (nb_stroke_intervals[i]-1)),2] =
    stroke_intervals_duration[1:nb_stroke_intervals[i],2,i]
}


hist(All_stroke_intervals[,2], breaks = 5000, xlab = 'time between strokes (seconds)', axes=T)
hist(All_stroke_intervals[,2], breaks = 5000, xlim = c(0,1.5), ylim = c(0,100), xlab = 'time between strokes (seconds)')



## storing mean and dispersion of stroke interval durations
stroke_intervals_data = matrix(NA, length(smooth_coords_files), 7)
stroke_intervals_data[,1:4] = as.matrix(files_ID[,2:5])
colnames(stroke_intervals_data) <-  c('specimen_ID','flight_ID','species','fps','mean_stroke_duration','sd_stroke_duration','max_stroke_duration')

for (i in 1:length(smooth_coords_files)){
  stroke_intervals_data[i,5] = mean(na.omit(stroke_intervals_duration[,2,i]))
  stroke_intervals_data[i,6] = sd(na.omit(stroke_intervals_duration[,2,i]))
  stroke_intervals_data[i,7] = max(na.omit(stroke_intervals_duration[,2,i]))
}


# View(stroke_intervals_data)
# write.csv(stroke_intervals_data,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/stroke_intervals_data.csv", row.names=F)





















#################### separating stroke intervals by Morpho species #################### 

marcus_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='marcus')]
deidamia_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='deidamia')]
helenor_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='helenor')]
achilles_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='achilles')]
menelaus_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='menelaus')]
godartii_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='godartii')]
aurora_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='aurora')]
sulkowskyi_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='sulkowskyi')]
theseus_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='theseus')]
cisseis_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='cisseis')]
rhetenor_stroke_int <- stroke_intervals_duration[,, which(files_ID$species=='rhetenor')]


marcus_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='marcus')])+1), 2)
marcus_All_stroke_int[,1] = seq(1, nrow(marcus_All_stroke_int),1)
marcus_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='marcus')][1],2] = marcus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='marcus')][1],2,1]
for (i in 2:length(which(files_ID$species=='marcus'))){
  start_now = last(na.omit(marcus_All_stroke_int))[1] + 1
  marcus_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='marcus')][i]-1)),2] =
    marcus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='marcus')][i],2,i]
}

deidamia_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='deidamia')])+1), 2)
deidamia_All_stroke_int[,1] = seq(1, nrow(deidamia_All_stroke_int),1)
deidamia_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='deidamia')][1],2] = deidamia_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='deidamia')][1],2,1]
for (i in 2:length(which(files_ID$species=='deidamia'))){
  start_now = last(na.omit(deidamia_All_stroke_int))[1] + 1
  deidamia_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='deidamia')][i]-1)),2] =
    deidamia_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='deidamia')][i],2,i]
}

helenor_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='helenor')])+1), 2)
helenor_All_stroke_int[,1] = seq(1, nrow(helenor_All_stroke_int),1)
helenor_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='helenor')][1],2] = helenor_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='helenor')][1],2,1]
for (i in 2:length(which(files_ID$species=='helenor'))){
  start_now = last(na.omit(helenor_All_stroke_int))[1] + 1
  helenor_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='helenor')][i]-1)),2] =
    helenor_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='helenor')][i],2,i]
}

achilles_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='achilles')])+1), 2)
achilles_All_stroke_int[,1] = seq(1, nrow(achilles_All_stroke_int),1)
achilles_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='achilles')][1],2] = achilles_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='achilles')][1],2,1]
for (i in 2:length(which(files_ID$species=='achilles'))){
  start_now = last(na.omit(achilles_All_stroke_int))[1] + 1
  achilles_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='achilles')][i]-1)),2] =
    achilles_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='achilles')][i],2,i]
}

menelaus_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='menelaus')])+1), 2)
menelaus_All_stroke_int[,1] = seq(1, nrow(menelaus_All_stroke_int),1)
menelaus_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='menelaus')][1],2] = menelaus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='menelaus')][1],2,1]
for (i in 2:length(which(files_ID$species=='menelaus'))){
  start_now = last(na.omit(menelaus_All_stroke_int))[1] + 1
  menelaus_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='menelaus')][i]-1)),2] =
    menelaus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='menelaus')][i],2,i]
}

godartii_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='godartii')])+1), 2)
godartii_All_stroke_int[,1] = seq(1, nrow(godartii_All_stroke_int),1)
godartii_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='godartii')][1],2] = godartii_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='godartii')][1],2,1]
for (i in 2:length(which(files_ID$species=='godartii'))){
  start_now = last(na.omit(godartii_All_stroke_int))[1] + 1
  godartii_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='godartii')][i]-1)),2] =
    godartii_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='godartii')][i],2,i]
}

aurora_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='aurora')])+1), 2)
aurora_All_stroke_int[,1] = seq(1, nrow(aurora_All_stroke_int),1)
aurora_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='aurora')][1],2] = aurora_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='aurora')][1],2,1]
for (i in 2:length(which(files_ID$species=='aurora'))){
  start_now = last(na.omit(aurora_All_stroke_int))[1] + 1
  aurora_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='aurora')][i]-1)),2] =
    aurora_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='aurora')][i],2,i]
}

sulkowskyi_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='sulkowskyi')])+1), 2)
sulkowskyi_All_stroke_int[,1] = seq(1, nrow(sulkowskyi_All_stroke_int),1)
sulkowskyi_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='sulkowskyi')][1],2] = sulkowskyi_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='sulkowskyi')][1],2,1]
for (i in 2:length(which(files_ID$species=='sulkowskyi'))){
  start_now = last(na.omit(sulkowskyi_All_stroke_int))[1] + 1
  sulkowskyi_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='sulkowskyi')][i]-1)),2] =
    sulkowskyi_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='sulkowskyi')][i],2,i]
}

theseus_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='theseus')])+1), 2)
theseus_All_stroke_int[,1] = seq(1, nrow(theseus_All_stroke_int),1)
theseus_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='theseus')][1],2] = theseus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='theseus')][1],2,1]
for (i in 2:length(which(files_ID$species=='theseus'))){
  start_now = last(na.omit(theseus_All_stroke_int))[1] + 1
  theseus_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='theseus')][i]-1)),2] =
    theseus_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='theseus')][i],2,i]
}

cisseis_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='cisseis')])+1), 2)
cisseis_All_stroke_int[,1] = seq(1, nrow(cisseis_All_stroke_int),1)
cisseis_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='cisseis')][1],2] = cisseis_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='cisseis')][1],2,1]
for (i in 2:length(which(files_ID$species=='cisseis'))){
  start_now = last(na.omit(cisseis_All_stroke_int))[1] + 1
  cisseis_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='cisseis')][i]-1)),2] =
    cisseis_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='cisseis')][i],2,i]
}

rhetenor_All_stroke_int = matrix(NA, (sum(nb_stroke_intervals[which(files_ID$species=='rhetenor')])+1), 2)
rhetenor_All_stroke_int[,1] = seq(1, nrow(rhetenor_All_stroke_int),1)
rhetenor_All_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='rhetenor')][1],2] = rhetenor_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='rhetenor')][1],2,1]
for (i in 2:length(which(files_ID$species=='rhetenor'))){
  start_now = last(na.omit(rhetenor_All_stroke_int))[1] + 1
  rhetenor_All_stroke_int[start_now:(start_now + (nb_stroke_intervals[which(files_ID$species=='rhetenor')][i]-1)),2] =
    rhetenor_stroke_int[1:nb_stroke_intervals[which(files_ID$species=='rhetenor')][i],2,i]
}


#################### plotting stroke intervals duration per species #################### 

par(mfrow=c(3,4))
hist(deidamia_All_stroke_int[,2], breaks=(nrow(deidamia_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='deidamia', xlim=c(0,2), col='gray40')
hist(menelaus_All_stroke_int[,2], breaks=(nrow(menelaus_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='menelaus', xlim=c(0,2), col='gray40')
hist(helenor_All_stroke_int[,2], breaks=(nrow(helenor_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='helenor', xlim=c(0,2), col='gray40')
hist(achilles_All_stroke_int[,2], breaks=(nrow(achilles_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='achilles', xlim=c(0,2), col='gray40')
hist(godartii_All_stroke_int[,2], breaks=(nrow(godartii_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='godartii', xlim=c(0,2), col='gray40')
hist(marcus_All_stroke_int[,2], breaks=(nrow(marcus_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='marcus', xlim=c(0,2), col='gray40')
hist(aurora_All_stroke_int[,2], breaks=(nrow(aurora_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='aurora', xlim=c(0,2), col='gray40')
hist(sulkowskyi_All_stroke_int[,2], breaks=(nrow(sulkowskyi_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='sulkowskyi', xlim=c(0,2), col='gray40')
hist(theseus_All_stroke_int[,2], breaks=(nrow(theseus_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='theseus', xlim=c(0,2), col='gray40')
hist(cisseis_All_stroke_int[,2], breaks=(nrow(cisseis_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='cisseis', xlim=c(0,2))
hist(rhetenor_All_stroke_int[,2], breaks=(nrow(rhetenor_All_stroke_int)-1), xlab = 'time between strokes (seconds)', main='rhetenor', xlim=c(0,2), col='gray40')
par(mfrow=c(1,1))


par(mfrow=c(3,4))
hist(deidamia_All_stroke_int[,2], breaks=(nrow(deidamia_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('deidamia (10)','       max=',round(max(na.omit(deidamia_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(menelaus_All_stroke_int[,2], breaks=(nrow(menelaus_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('menelaus (11)','       max=',round(max(na.omit(menelaus_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(helenor_All_stroke_int[,2], breaks=(nrow(helenor_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('helenor (10)','       max=',round(max(na.omit(helenor_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(achilles_All_stroke_int[,2], breaks=(nrow(achilles_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('achilles (10)','       max=',round(max(na.omit(achilles_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(godartii_All_stroke_int[,2], breaks=(nrow(godartii_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('godartii (10)','       max=',round(max(na.omit(godartii_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(marcus_All_stroke_int[,2], breaks=(nrow(marcus_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('marcus (2)','       max=',round(max(na.omit(marcus_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(aurora_All_stroke_int[,2], breaks=(nrow(aurora_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('aurora (1)','       max=',round(max(na.omit(aurora_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(sulkowskyi_All_stroke_int[,2], breaks=(nrow(sulkowskyi_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('sulkowskyi (2)','       max=',round(max(na.omit(sulkowskyi_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
hist(theseus_All_stroke_int[,2], breaks=(nrow(theseus_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('theseus (4)','       max=',round(max(na.omit(theseus_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5))
hist(cisseis_All_stroke_int[,2], breaks=(nrow(cisseis_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('cisseis (11)','       max=',round(max(na.omit(cisseis_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5))
hist(rhetenor_All_stroke_int[,2], breaks=(nrow(rhetenor_All_stroke_int)-1), xlab = 'time between strokes (seconds)',
     main=paste0('rhetenor (8)','       max=',round(max(na.omit(rhetenor_All_stroke_int[,2])),2),'sec'), xlim=c(0,0.5), col='gray40')
par(mfrow=c(1,1))






