
      #     
     # #    
    #   #     The code 'AA_flight_trajectories_formating.R' must be loaded
   #  !  #    before using this code
  #       #         
 # # # # # #


local_path = dirname(rstudioapi::getSourceEditorContext()$path)


#################### delimiting gliding and flapping phases #################### 

# assuming a gliding phase start from 0.1666667 secondes without a wing stroke
# i.e. 40 frames at 240fps and 20 frames at 120fps
gliding_threshold_240fps = 40
gliding_threshold_120fps = gliding_threshold_240fps / 2

gliding = array(NA, dim= c(max(total_frames), 5, length(smooth_coords_files)))
gliding[,1,] = Flight_and_strokes[,1,] # adding the time column
gliding[,2,] = Flight_and_strokes[,6,] # adding the stroke column
colnames(gliding) = c('time', 'stroke', 'X_glide', 'Y_glide', 'Z_glide')

for (t in 1:length(smooth_coords_files)){n=0
for (i in 1:total_frames[t]){
  if(is.na(Flight_and_strokes[i,6,t])==T){gliding[i,3:5,t] = gliding[i,3:5,t] ; n=n+1}
  if(is.na(Flight_and_strokes[i,6,t])==F){gliding[i,3:5,t] = gliding[i,3:5,t] ; n=0}
  if(n==gliding_threshold_240fps){gliding[(i-(gliding_threshold_240fps-1)):i,3:5,t] = Flight_and_strokes[(i-(gliding_threshold_240fps-1)):i,2:4,t] ; n=n+1}
  if(n>gliding_threshold_240fps){gliding[i,3:5,t] = Flight_and_strokes[i,2:4,t] ; n=n+1}
 }
}

### correcting for the 120 fps sequences 
seq_120fps = which(files_ID$fps==120)
for (t in seq_120fps){n=0
for (i in 1:total_frames[t]){
  if(is.na(Flight_and_strokes[i,6,t])==T){gliding[i,3:5,t] = gliding[i,3:5,t] ; n=n+1}
  if(is.na(Flight_and_strokes[i,6,t])==F){gliding[i,3:5,t] = gliding[i,3:5,t] ; n=0}
  if(n==gliding_threshold_120fps){gliding[(i-(gliding_threshold_120fps-1)):i,3:5,t] = Flight_and_strokes[(i-(gliding_threshold_120fps-1)):i,2:4,t] ; n=n+1}
  if(n>gliding_threshold_120fps){gliding[i,3:5,t] = Flight_and_strokes[i,2:4,t] ; n=n+1}
 }
}

# flapping array (mirror of the gliding array)
flapping = array(NA, dim= c(max(total_frames), 5, length(smooth_coords_files)))
flapping[,1,] = Flight_and_strokes[,1,] # adding the time column
flapping[,2,] = Flight_and_strokes[,6,] # adding the stroke column
colnames(flapping) = c('time', 'stroke', 'X_flap', 'Y_flap', 'Z_flap')

for (t in 1:length(smooth_coords_files)){
  for (i in 1:total_frames[t]){
    if(is.na(gliding[i,3,t])==T){flapping[i,3:5,t] = Flight_and_strokes[i,2:4,t]}
  }
}



#################### plotting flight path, stroke positions and gliding phases #################### 


p=184
plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab="X ", ylab="Y ", zlab="Z ", asp=F, box=F)
plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
legend3d("topleft", legend = paste0(p,'__',files_ID$species[p],'_',files_ID$specimen_ID[p],'_',files_ID$flight_ID[p]),bty='n' )

# for (p in 100:200){
#   plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" ", ylab=" ", zlab=" ", asp=F, box=F)
#   plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
#   plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
#   plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
#   legend3d("topleft", legend = paste0(p,'__',files_ID$species[p],'_',files_ID$specimen_ID[p],'_',files_ID$flight_ID[p]), bty='n' )
# }

### plot designed for the paper figure
# p=184
# plot3d(gliding[1:410,3:5,p], type='p',size=4, col="gray8", xlab=" ", ylab=" ", zlab=" ", asp=F, box=F)
# plot3d(flapping[1:410,3:5,p], type='p',size=4, col='gray65', add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
# legend3d("topleft", legend = paste0(p,'__',files_ID$species[p],'_',files_ID$specimen_ID[p],'_',files_ID$flight_ID[p]),bty='n' )
# rgl.postscript("trajectory_example.eps","eps")
# rgl.postscript("trajectory_example.pdf", "pdf")
# rgl.snapshot("trajectory_example3.png","png")
# getwd()




# sequences to checked (due to their negative glide angle!!)
which(gliding_parameters[,7]<0)
# 15 menelaus 05 F07 ascend glide (0.19s)
# 16 menelaus 06 F03 CORRECTED
# 18 menelaus 06 F06 ascend glide (0.21s)
# 19 menelaus 07 F02 ascend glide (0.23s)
# 20 menelaus 07 F09 ascend glide (0.17s)
# 23 cisseis 11 F04 should be NA
# 24 cisseis 11 F05 should be NA
# 33 rhetenor 17 F07 CORRECTED
# 35 achilles 20 F02 ascend glide (0.16s)
# 41 deidamia 22 F04 ascend glide (0.32s)
# 49 achilles 25 F01 ascend glide (0.16s)
# 55 marcus 27 F03 ascend glide (0.16s)
# 69 menelaus 44 F03 CORRECTED
# 93 marcus 53 F10 ascend glide (0.16s)
# 107 menelaus 60 F12 CORRECTED
# 147 theseus 79 F01 glide angle and delta_Z can be approximate to 0 (but check the flight sequence)
# 152 theseus 81 F03 should be NA
# 174 achilles 87 F06 ascend glide (0.38s)
# 181 rhetenor 90 F08 glide angle and delta_Z can be approximate to 0 (but check the flight sequence)
# 194 helenor 98 F03 ascend glide (0.25s)
# 197 helenor 99 F06 glide angle and delta_Z can be approximate to 0 (but check the flight sequence)
# 214 godartii 117 F04 glide angle and delta_Z can be approximate to 0
# 235 godartii 128 F04 ascend glide (0.25s)



#################### indexing gliding and flapping phases ####################

# the gliding and flapping array have to be a bit larger (one more row) for this to work
gliding2 = array(NA, dim= c((max(total_frames)+1), 5, length(smooth_coords_files)))
gliding2[1:max(total_frames),,] = gliding
flapping2 = array(NA, dim= c((max(total_frames)+1), 5, length(smooth_coords_files)))
flapping2[1:max(total_frames),,] = flapping

# the Flight_and_strokes array will have an additional column to be fill either with 'glide' or 'flap'
Flight_and_strokes2 = array(NA, dim= c(max(total_frames), 8, length(smooth_coords_files)))
Flight_and_strokes2[,1:6,] = Flight_and_strokes
colnames(Flight_and_strokes2) <-  c('time', 'X', 'Y', 'Z', 'stroke_value', 'stroke', 'flight_mode','phase_index')

# adding 'flap' or 'glide' in the 7th columns of the 'Flight_and_strokes2' array
for (t in 1:length(smooth_coords_files)){
  for (i in 1:total_frames[t]){
    if(is.na(gliding2[i,3,t])==T){Flight_and_strokes2[i,7,t] = 'flap' }
    if(is.na(gliding2[i,3,t])==F){Flight_and_strokes2[i,7,t] = 'glide'}
  }
}

# indexing the gliding phases
for (t in 1:length(smooth_coords_files)){k=1
  if(is.na(gliding2[1,3,t])==F){Flight_and_strokes2[1,8,t] = 1}
  if(is.na(gliding2[1,3,t])==T){Flight_and_strokes2[1,8,t] = 0 ; k=0}
    for (i in 2:total_frames[t]){
      if(is.na(gliding2[i,3,t])==T){seq=0 ; Flight_and_strokes2[i,8,t] = 0}
      if(is.na(gliding2[i,3,t])==F){seq=k ; Flight_and_strokes2[i,8,t] = k}
      if(seq==0 & is.na(gliding2[i+1,3,t])==F){k=k+1}
 }  
}

# catching the number of gliding phases in each sequence
# this number is placed in the matrix 'phase_count'
phase_count = matrix(NA,length(smooth_coords_files),3)
phase_count[,1] = files_ID[,1]
colnames(phase_count)<- c("order_in_scan_loop", "gliding_phases_nb", "flapping_phases_nb")

for (t in 1:length(smooth_coords_files)){
  for (i in total_frames[t]:1){ # inverting sweeping order
    if(Flight_and_strokes2[i,7,t]=='glide'){phase_count[t,2] = Flight_and_strokes2[i,8,t];break} # catching the last glide index in Flight_and_strokes2
  }
}

# for sequences without gliding parts (i.e. only flapping), we add '0'
for (i in 1:length(smooth_coords_files)){
  if(is.na(phase_count[i,2])==T){phase_count[i,2]=0}
}

### same process for the flapping part (Flight_and_strokes3)

# we use an other array to do this
Flight_and_strokes3 = array(NA, dim= c(max(total_frames), 8, length(smooth_coords_files)))
Flight_and_strokes3[,1:7,] = Flight_and_strokes2[,1:7,]
colnames(Flight_and_strokes3) <-  c('time', 'X', 'Y', 'Z', 'stroke_value', 'stroke', 'flight_mode','phase_index')

# indexing the flapping phases
for (t in 1:length(smooth_coords_files)){k=1
  if(is.na(flapping2[1,3,t])==F){Flight_and_strokes3[1,8,t] = 1}
  if(is.na(flapping2[1,3,t])==T){Flight_and_strokes3[1,8,t] = 0 ; k=0}
    for (i in 2:total_frames[t]){
      if(is.na(flapping2[i,3,t])==T){seq=0 ; Flight_and_strokes3[i,8,t] = 0}
      if(is.na(flapping2[i,3,t])==F){seq=k ; Flight_and_strokes3[i,8,t] = k}
      if(seq==0 & is.na(flapping2[i+1,3,t])==F){k=k+1}
 }  
}

# catching the number of flapping phases in each sequence
for (t in 1:length(smooth_coords_files)){
  for (i in total_frames[t]:1){ # inverting sweeping order
    if(Flight_and_strokes3[i,7,t]=='flap'){phase_count[t,3] = Flight_and_strokes3[i,8,t];break} # catching the last flap index in Flight_and_strokes3
  }
}

# for sequences without flapping parts (i.e. only gliding), we add '0'
for (i in 1:length(smooth_coords_files)){
  if(is.na(phase_count[i,3])==T){phase_count[i,3]=0}
}


# finally, we combine Flight_and_strokes2 and Flight_and_strokes3
for (t in 1:length(smooth_coords_files)){
  for (i in 1:total_frames[t]){
    if(is.na(gliding2[i,3,t])==T){Flight_and_strokes2[i,8,t] = Flight_and_strokes3[i,8,t]}
  }
}

# pasting index and flight mode for having 'flap1, glide1, flap2, glide2...' in column 8
for (t in 1:length(smooth_coords_files)){
  for (i in 1:total_frames[t]){
    Flight_and_strokes2[i,7,t] = paste0(Flight_and_strokes2[i,7,t], Flight_and_strokes2[i,8,t])
  }
}

#     View(phase_count)
#     View(Flight_and_strokes2[,,150])


######################## saving the "Flight_and_strokes2" array for Science submission ########################

flight_trajectories = Flight_and_strokes2[,-c(5,8),]
View(flight_trajectories[,,150])

dimension_names = as.matrix(rep(NA,nrow(files_ID)))
for (i in 1:nrow(files_ID)){
  dimension_names[i] = as.character(paste0(files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i]))
}

dimnames(flight_trajectories)[[3]] = dimension_names

# saveRDS(flight_trajectories, file = "/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/flight_trajectories_array.rds", ascii = FALSE, version = NULL, compress = TRUE, refhook = NULL)
# test = readRDS("/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/flight_trajectories_array.rds")

### save the "flight_trajectories" object for Qiang Chen and Zhigang Deng
for (i in which(files_ID$fps==240)){
  specimen_now = paste0('specimen_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i],'_',files_ID$species[i])
  write.csv(flight_trajectories[,1:5,i], paste0(local_path, '/10_saved_flight_and_strokes_data/',specimen_now,'.csv'), row.names = F)
}



######################## gliding & flapping phases duration ########################

# array intended to receive the frame number for each gliding phase, and for each flight
glide_frame = array(NA, dim= c(max(phase_count[,2]), 2, length(smooth_coords_files)))
colnames(glide_frame)<- c("gliding_phase_ID", "frames_nb")
# for each flight, we put in the first column the index of the gliding phase
for (t in 1:length(smooth_coords_files)){
  if(phase_count[t,2]>0){
    for (i in 1:phase_count[t,2]){
      glide_frame[i,1,t] = i
    }
  }
}

# we put the frame number of each gliding phase in the column 2 of glide_frame
for (t in 1:length(smooth_coords_files)){
  for (i in 1:phase_count[t,2]){
    glide_frame[i,2,t] = length(which(Flight_and_strokes2[,7,t]==paste0('glide',i))==T)
  }
}

# same process for flapping phases
flap_frame = array(NA, dim= c(max(as.numeric(phase_count[,3])), 2, length(smooth_coords_files)))
colnames(flap_frame)<- c("flapping_phase_ID", "frames_nb")
for (t in 1:length(smooth_coords_files)){
  if(phase_count[t,3]>0){
    for (i in 1:phase_count[t,3]){
      flap_frame[i,1,t] = i }}}
for (t in 1:length(smooth_coords_files)){
  for (i in 1:phase_count[t,3]){
    flap_frame[i,2,t] = length(which(Flight_and_strokes2[,7,t]==paste0('flap',i))==T)}}

# we have now a 'glide_frame' array and a 'flap_frame' array, in which containing
# for each gliding/flapping phase of each flight, the frame number of these phases




########################  summarizing glide/flap data ######################## 

glide_and_flap_data = matrix(NA, length(smooth_coords_files), 14)
glide_and_flap_data[,1:4] = as.matrix(files_ID[,2:5])
colnames(glide_and_flap_data) <- c("specimen_ID","flight_ID",'species','fps','gliding_phase_nb','gliding_prop','mean_glide_duration','max_glide_duration',
                                   'min_glide_duration','flapping_phase_nb','flapping_prop','mean_flap_duration','max_flap_duration','min_flap_duration')


# filling the columns of  glide_and_flap_data
glide_and_flap_data[,5] = phase_count[,2] # gliding_phase_nb
glide_and_flap_data[,10] = phase_count[,3] # flapping_phase_nb

for (i in 1:length(smooth_coords_files)){
  glide_and_flap_data[i,7] = (mean(na.omit(glide_frame[,2,i])))*(1/240) # mean_glide_duration
  glide_and_flap_data[i,8] = (max(na.omit(glide_frame[,2,i])))*(1/240) # max_glide_duration
  glide_and_flap_data[i,9] = (min(na.omit(glide_frame[,2,i])))*(1/240) # min_glide_duration
  glide_and_flap_data[i,12] = (mean(na.omit(flap_frame[,2,i])))*(1/240) # mean_flap_duration
  glide_and_flap_data[i,13] = (max(na.omit(flap_frame[,2,i])))*(1/240) # max_flap_duration
  glide_and_flap_data[i,14] = (min(na.omit(flap_frame[,2,i])))*(1/240) # min_flap_duration
}

# correcting for the 120fps sequences
seq_120fps = which(files_ID$fps==120)
for(i in seq_120fps){
  glide_and_flap_data[i,7] = (mean(na.omit(glide_frame[,2,i])))*(1/120) # mean_glide_duration
  glide_and_flap_data[i,8] = (max(na.omit(glide_frame[,2,i])))*(1/120) # max_glide_duration
  glide_and_flap_data[i,9] = (min(na.omit(glide_frame[,2,i])))*(1/120) # min_glide_duration
  glide_and_flap_data[i,12] = (mean(na.omit(flap_frame[,2,i])))*(1/120) # mean_flap_duration
  glide_and_flap_data[i,13] = (max(na.omit(flap_frame[,2,i])))*(1/120) # max_flap_duration
  glide_and_flap_data[i,14] = (min(na.omit(flap_frame[,2,i])))*(1/120) # min_flap_duration
}

# filling the gliding and flapping proportion column
for (i in 1:length(smooth_coords_files)){
  glide_and_flap_data[i,6] = (sum(na.omit(glide_frame[,2,i]))*100)/total_frames[i]
  glide_and_flap_data[i,11] = (sum(na.omit(flap_frame[,2,i]))*100)/total_frames[i]
}


# View(glide_and_flap_data)
# write.csv(glide_and_flap_data,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/glide_and_flap_data.csv", row.names=F)


########################### wingbeat frequency ###########################

# wingbeat number (one wingbeat = two strokes)
wb_number <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  wb_number[i,] = length(which(is.na(Flight_and_strokes2[1:total_frames[i],6,i])==F))/2
}

# wingbeat frequency (i.e. wingbeat number divided by time)
wb_frequency <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  wb_frequency[i] = wb_number[i]/ (as.numeric(Flight_and_strokes2[total_frames[i],1,i]))
}

# extracting sequences duration
seq_length <- matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  seq_length[i] <- as.numeric(Flight_and_strokes2[total_frames[i],1,i])
}


wingbeat_frequency = cbind(files_ID[,2:5], seq_length, wb_number, wb_frequency)
colnames(wingbeat_frequency) <- c('specimen_ID','flight_ID','species','fps',"seq_length", "wb_number", "wb_frequency")

# View(wingbeat_frequency)
# write.csv(wingbeat_frequency,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/wingbeat_frequency.csv", row.names=F)

plot(wingbeat_frequency[,7], pch=16, col='gray40', bty='n')




########################### flight height ###########################

# scaling along the z axis does not always start from zero, to compute flight height we have to correct for it.

Z_axis_coordinate = Flight_and_strokes2[,c(1,4),]

# catching the lowest negative value of flight height
lowest_flight_height_values = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  lowest_flight_height_values[i] = min(na.omit(as.numeric(Z_axis_coordinate[,2,i])))
}
min(lowest_flight_height_values)

# adding this lowest value to every Z coordinates
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Z_axis_coordinate[j,2,i] = as.numeric(Z_axis_coordinate[j,2,i]) + (-min(lowest_flight_height_values))
  }
}


flight_height = matrix(NA, length(smooth_coords_files), 3)
colnames(flight_height) <- c('mean_flight_height','max_flight_height','sd_flight_height')
for (i in 1:length(smooth_coords_files)){
  flight_height[i,1] = mean(as.numeric(na.omit(Z_axis_coordinate[,2,i])))
  flight_height[i,2] = max(as.numeric(na.omit(Z_axis_coordinate[,2,i])))
  flight_height[i,3] = sd(as.numeric(na.omit(Z_axis_coordinate[,2,i])))
}

# View(flight_height)
# write.csv(flight_height,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/flight_height_parameters.csv", row.names=F)



########################### extracting longest gliding/flapping phase coordinates #####################

# extracting the longest gliding phase from each flight in 'Flight_and_strokes2'
max_glide_coords = array(NA, dim = c(max(total_frames), 8, length(smooth_coords_files)))

# creating a second 'glide_frame' object, but with its 0 replaced by NAs
# (it is necessary to access to the max glide sequence without problems)
glide_frame2 <- glide_frame
for (t in 1:length(smooth_coords_files)){if(glide_frame2[1,2,t]==0){
  glide_frame2[1,2,t]=NA} }

for (t in 1:length(smooth_coords_files)){
  if(phase_count[t,2]>0){
    max_glide_coords[1:max(na.omit(glide_frame2[,2,t])),,t] = 
      Flight_and_strokes2[c(which(Flight_and_strokes2[,7,t]==
                         paste0('glide', which(glide_frame2[,2,t]==
                                                max(na.omit(glide_frame2[,2,t])))[1])) # the [1] is because of ex aequo
      ),,t]}                                                    
}

# extracting the longest flapping phase from each flight in 'Flight_and_strokes2'
max_flap_coords = array(NA, dim = c(max(total_frames), 8, length(smooth_coords_files)))

flap_frame2 <- flap_frame
for (t in 1:length(smooth_coords_files)){if(flap_frame2[1,2,t]==0){
  flap_frame2[1,2,t]=NA} }

for (t in 1:length(smooth_coords_files)){
  if(phase_count[t,3]>0){
    max_flap_coords[1:max(na.omit(flap_frame2[,2,t])),,t] = 
      Flight_and_strokes2[c(which(Flight_and_strokes2[,7,t]==
                                    paste0('flap', which(flap_frame2[,2,t]==
                                                            max(na.omit(flap_frame2[,2,t])))[1]))
      ),,t]}                                                    
}

# frame number for each max flap/glide sequences
# for max_glide
total_frames_maxglide <- matrix(NA, length(smooth_coords_files), 1)
for (t in 1:length(smooth_coords_files)){
  total_frames_maxglide[t] = length(which(is.na(max_glide_coords[,1,t])==F)) }
# for max_flap
total_frames_maxflap <- matrix(NA, length(smooth_coords_files), 1)
for (t in 1:length(smooth_coords_files)){
  total_frames_maxflap[t] = length(which(is.na(max_flap_coords[,1,t])==F)) }







########################### wingbeat frequency inside max flapping phases ###########################

# extracting Micro wingbeat number (first column of the matrix 'Micro_wb_frequency')
Micro_wb_number <- matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  Micro_wb_number[i] = length(which(is.na(max_flap_coords[1:total_frames_maxflap[i],6,i])==F))/2
}

# extracting Micro wingbeat frequency
Micro_wb_frequency <- matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  if(phase_count[i,3]>0){
    Micro_wb_frequency[i] = Micro_wb_number[i] / (as.numeric(max_flap_coords[total_frames_maxflap[i],1,i]) - as.numeric(max_flap_coords[1,1,i]))
  }}

# adding a third column containing the micro wingbeat sequences duration
Micro_wb_seq_length <- matrix(NA, length(smooth_coords_files),1)
for (i in 1:length(smooth_coords_files)){
  if(phase_count[i,3]>0){
    Micro_wb_seq_length[i,1] <- (as.numeric(max_flap_coords[total_frames_maxflap[i],1,i]) - as.numeric(max_flap_coords[1,1,i]))
  }}


# saving wingbeat frequency informations of the max flapping phases
Micro_wingbeat_parameters <- cbind(files_ID[,2:5], Micro_wb_seq_length, Micro_wb_number,Micro_wb_frequency)
colnames(Micro_wingbeat_parameters) <- c("specimen_ID","flight_ID","species",'fps', "Micro_seq_length", 'Micro_wb_number', "Micro_wb_frequency")
# View(Micro_wingbeat_parameters)

# write.csv(Micro_wingbeat_parameters,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/Micro_wingbeat_parameters.csv", row.names=F)






########################### gliding efficiency on longest gliding phases ###########################

#  ==> we focus only the longest gliding phases

# gliding efficiency can be estimated by computing the glide ratio:
# glide ratio = vertical height loss over the glide / horizontal distance covered over the glide

gliding_parameters = matrix(NA, length(smooth_coords_files),8)
colnames(gliding_parameters) <- c("specimen_ID","flight_ID","species",'gliding_length_(s)', "delta_z", 'horiz_dist', "glide_ratio", 'glide_angle')
gliding_parameters[,1:3] = as.matrix(files_ID[,2:4]) # adding species and specimens infos
for (i in 1:length(smooth_coords_files)){ # adding length of the gliding phase
  if (glide_and_flap_data[i,5]==0){gliding_parameters[i,4]=NA}
  if (glide_and_flap_data[i,5]>0){
  gliding_parameters[i,4] = as.numeric(max_glide_coords[last(which(is.na(max_glide_coords[,1,i])==F)),1,i]) - as.numeric(max_glide_coords[1,1,i])
  }
}
                   
# computing height loss (called delta z)
for (i in 1:length(smooth_coords_files)){
  if (glide_and_flap_data[i,5]==0){gliding_parameters[i,5]=NA}
  if (glide_and_flap_data[i,5]>0){gliding_parameters[i,5]= as.numeric(max_glide_coords[1,4,i]) -  as.numeric(max_glide_coords[last(which(is.na(max_glide_coords[,4,i])==F)),4,i])}
}

## computing horizontal distance covered
## we compute the distance between the first and the last gliding position projected on a horizontal line from the first position (as if there were no change on the z axis)
## example:
# i=20
# first_pos = as.numeric(max_glide_coords[1,2:4,i])
# last_pos = as.numeric(max_glide_coords[last(which(is.na(max_glide_coords[,4,i])==F)),2:4,i])
# last_pos_Z_adjusted = c(last_pos[1:2],first_pos[3])
# 
# plot3d(max_glide_coords[,2:4,i], typ="p", col="blue", lwd=3, xlab="X", ylab="Y", zlab="Z", aspect=F)
# points3d(first_pos[1],first_pos[2],first_pos[3], size=10, col="green", add=T) # start frame
# points3d(last_pos[1],last_pos[2],last_pos[3], size=10, col="red", add=T) # last frame
# points3d(last_pos_Z_adjusted[1],last_pos_Z_adjusted[2],last_pos_Z_adjusted[3], size=10, col="black", add=T) # projected
# vectors3d(origin = first_pos, last_pos_Z_adjusted, col='red', headlength = 0, lwd=2, add=T)
# vectors3d(origin = last_pos_Z_adjusted, last_pos, col='red', headlength = 0, lwd=2, add=T)
# vectors3d(origin = first_pos, last_pos, col='red', headlength = 0, lwd=2, add=T)


# computing for all trajectories
for (i in 1:length(smooth_coords_files)){
  if (glide_and_flap_data[i,5]==0){gliding_parameters[i,6]=NA}
  if (glide_and_flap_data[i,5]>0){
    first_pos_now = as.numeric(max_glide_coords[1,2:4,i])
    last_pos_now = as.numeric(max_glide_coords[last(which(is.na(max_glide_coords[,4,i])==F)),2:4,i])
    last_pos_Z_adjusted_now = c(last_pos_now[1:2],first_pos_now[3])
    gliding_parameters[i,6] = sqrt(diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,1])^2 +
                                     diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,2])^2 +
                                     diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,3])^2)
  }
}

# sequences for which delta_Z is negative and very close to 0 (n=4) are replace with 0
for (i in c(49,147,181,197,214)){
  gliding_parameters[i,5] = 0
}

# sequences for which it is definitively not a glide (visually assessed) are not considered and replace with NA
for (i in c(23,24,152)){
  gliding_parameters[i,5] = NA
}

# some gliding phases are in the upward direction !!
which(gliding_parameters[,5]<0)
# 15 menelaus 05 F07 ascend glide (0.19s)
# 18 menelaus 06 F06 ascend glide (0.21s)
# 19 menelaus 07 F02 ascend glide (0.23s)
# 20 menelaus 07 F09 ascend glide (0.17s)
# 35 achilles 20 F02 ascend glide (0.16s)
# 41 deidamia 22 F04 ascend glide (0.32s)
# 49 achilles 25 F01 ascend glide (0.16s)
# 55 marcus 27 F03 ascend glide (0.16s)
# 93 marcus 53 F10 ascend glide (0.16s)
# 174 achilles 87 F06 ascend glide (0.38s)
# 194 helenor 98 F03 ascend glide (0.25s)
# 235 godartii 128 F04 ascend glide (0.25s)      REPLACE BY NA ?

# remove gliding phases in the upward direction
for (i in c(15,18,19,20,35,41,59,55,93,174,194,235)){
  gliding_parameters[i,5] = NA
}

# computing glide ratio
for (i in 1:length(smooth_coords_files)){
  gliding_parameters[i,7] = as.numeric(gliding_parameters[i,5]) / as.numeric(gliding_parameters[i,6])
}

# computing glide angle
for (i in 1:length(smooth_coords_files)){
  gliding_parameters[i,8] = atan(as.numeric(gliding_parameters[i,5]) / as.numeric(gliding_parameters[i,6]))*180/pi
}

# View(gliding_parameters)
# write.csv(gliding_parameters,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/gliding_parameters.csv", row.names=F)




###### Does glide ratio computed at each frame gives different result?

## compute series of instantaneouse glide ratio and glide angle
gliding_parameters_inst_arr = array(NA, dim= c(max(total_frames_maxglide), 2, length(smooth_coords_files)))
colnames(gliding_parameters_inst_arr) = c('glide_ratio_inst', 'glide_angle_inst')
for (j in 1:length(smooth_coords_files)){
  for (i in 1:total_frames_maxglide[j]){
    m = as.numeric(max_glide_coords[i,2:4,j])
    n = as.numeric(max_glide_coords[i+1,2:4,j])
    n_adjusted = c(n[1:2],m[3])
    horiz_dist = as.numeric( sqrt(diff(as.matrix(rbind(m,n_adjusted))[,1])^2 +
                                    diff(as.matrix(rbind(m,n_adjusted))[,2])^2 +
                                    diff(as.matrix(rbind(m,n_adjusted))[,3])^2) )
    delta_z = as.numeric(max_glide_coords[i,4,j]) - as.numeric(max_glide_coords[(i+1),4,j])
    gliding_parameters_inst_arr[i,1,j] = delta_z / horiz_dist
    gliding_parameters_inst_arr[i,2,j] = atan(as.numeric(delta_z) / as.numeric(horiz_dist))*180/pi
  }
}

## compute the mean glide ratio and glide angle from the instantaneous series
gliding_parameters_inst = matrix(NA, length(smooth_coords_files), 6)
colnames(gliding_parameters_inst) = c('specimen_ID', 'flight_ID', 'species', 'gliding_length_(s)', 'glide_ratio_inst', 'glide_angle_inst')
gliding_parameters_inst[,1:4] = gliding_parameters[,1:4]
for (i in 1:length(smooth_coords_files)){
  gliding_parameters_inst[i,5] = mean(na.omit(gliding_parameters_inst_arr[,1,i]))
  gliding_parameters_inst[i,6] = mean(na.omit(gliding_parameters_inst_arr[,2,i]))
}
## change the 'NaN' into 'NA'
for (i in 1:length(smooth_coords_files)){
  if (gliding_parameters_inst[i,5] =='NaN') {gliding_parameters_inst[i,5] = NA}
  if (gliding_parameters_inst[i,6] =='NaN') {gliding_parameters_inst[i,6] = NA}
}


#### homogenize 'gliding_parameter' and 'gliding_parameter_inst' (in order to have common 0, and 'NA' lines)
for (i in c(49,147,181,197,214)){
  gliding_parameters_inst[i,5:6] = 0 # negative delta_Z and very close to 0 (n=4) are replace with 0
}
for (i in c(23,24,152)){
  gliding_parameters_inst[i,5:6] = NA # not a glide (visually assessed) replaced with NA
}
for (i in c(15,18,19,20,35,41,59,55,93,174,194,235)){ # remove gliding phases in the upward direction
  gliding_parameters_inst[i,5:6] = NA
}

## compare 'overall' values with thos obtained by averaging the instantaneous glide angle and glide ratio
plot(as.numeric(gliding_parameters[,8]), as.numeric(gliding_parameters_inst[,6]), xlab='global glide angle', ylab='glide angle computed at each frame')

## => it looks very similar...




########################### gliding efficiency on ALL gliding phases ########################### 

gliding_phase_NB = as.numeric(glide_and_flap_data[,5])

## create an ID for all gliding phase
gliding_phase_ID = as.data.frame(matrix(NA,sum(gliding_phase_NB),1))
for (i in 1:length(smooth_coords_files)){
  row_index_now = sum(gliding_phase_NB[1:i]) - (gliding_phase_NB[i]-1)
  range_index_now = row_index_now : (row_index_now + (gliding_phase_NB[i]-1) )
  for (g in 1:gliding_phase_NB[i]){gliding_phase_ID[range_index_now[g],1] =
    paste0(files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i],'_GLIDE_',g) }
}
gliding_phase_ID = gliding_phase_ID[-c(368),]# don't know why but there is an extra unwanted line that we remove

### IDs for gliding phases have been saved and can be directly loaded:
gliding_phase_IDs <- read.csv(paste0(local_path,'/all_gliding_phases_ID.csv'), h=T, sep=",")

## array to receive the 3D coords of all gliding phases
glide_coords_allphases = array(NA, dim = c(max(total_frames_maxglide), 8, sum(gliding_phase_NB)))
dimnames(glide_coords_allphases)[[2]] = dimnames(Flight_and_strokes2)[[2]]
dimnames(glide_coords_allphases)[[3]] = gliding_phase_ID

gliding_phase_IDs$flight_ID = factor(gliding_phase_IDs$flight_ID, levels = levels(files_ID$flight_ID))

for (i in 1:length(smooth_coords_files)){
  index_now = which(gliding_phase_IDs$species==files_ID$species[i] &
                      gliding_phase_IDs$specimen_ID==files_ID$specimen_ID[i] &
                      gliding_phase_IDs$flight_ID==files_ID$flight_ID[i])
  for (j in index_now){
    glide_coords_allphases[1:length(which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j]))),,j] =
      Flight_and_strokes2[which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j])),,i]}
}

## get frame number for all gliding phases
total_frames_all_glides <- matrix(NA, nrow(gliding_phase_IDs), 1)
for (t in 1:nrow(gliding_phase_IDs)){
  total_frames_all_glides[t] = length(which(is.na(glide_coords_allphases[,1,t])==F))}


## gliding efficiency can be estimated by computing the glide ratio:
## glide ratio = vertical height loss over the glide / horizontal distance covered over the glide

gliding_parameters_allphases = as.data.frame(matrix(NA, nrow(gliding_phase_IDs),10))
colnames(gliding_parameters_allphases) <- c("species","specimen_ID","flight_ID","gliding_phase_ID",'gliding_duration_(s)', "delta_z", 'horiz_dist', "glide_ratio", 'glide_angle','ecology')
gliding_parameters_allphases[,1:4] = gliding_phase_IDs[,1:4]
gliding_parameters_allphases[,10] = gliding_phase_IDs$ecology

# computing the duration of gliding phase
for (i in 1:nrow(gliding_phase_IDs)){ 
  gliding_parameters_allphases[i,5] = as.numeric(glide_coords_allphases[last(which(is.na(glide_coords_allphases[,1,i])==F)),1,i]) - as.numeric(glide_coords_allphases[1,1,i])
}

# computing height loss (delta z)
for (i in 1:nrow(gliding_phase_IDs)){
  gliding_parameters_allphases[i,6]= as.numeric(glide_coords_allphases[1,4,i]) -  as.numeric(glide_coords_allphases[last(which(is.na(glide_coords_allphases[,4,i])==F)),4,i])
}

## computing horizontal distance covered during the glide
for (i in 1:nrow(gliding_phase_IDs)){
    first_pos_now = as.numeric(glide_coords_allphases[1,2:4,i])
    last_pos_now = as.numeric(glide_coords_allphases[last(which(is.na(glide_coords_allphases[,4,i])==F)),2:4,i])
    last_pos_Z_adjusted_now = c(last_pos_now[1:2],first_pos_now[3])
    gliding_parameters_allphases[i,7] = sqrt(diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,1])^2 +
                                     diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,2])^2 +
                                     diff(as.matrix(rbind(first_pos_now,last_pos_Z_adjusted_now))[,3])^2)
}

## sequences for which delta_Z is only 0.01 meter negative are replace with 0
for (i in which(gliding_parameters_allphases$delta_z < 0 & gliding_parameters_allphases$delta_z > -0.01)){
  gliding_parameters_allphases[i,6] = 0
}
## we remove all other negative delta z (i.e. glides in the upward direction)
gliding_parameters_allphases = gliding_parameters_allphases[-c(which(gliding_parameters_allphases$delta_z < 0)),]

# computing glide ratio
for (i in 1:nrow(gliding_parameters_allphases)){
  gliding_parameters_allphases[i,8] = as.numeric(gliding_parameters_allphases[i,6]) / as.numeric(gliding_parameters_allphases[i,7])
}
# computing glide angle
for (i in 1:nrow(gliding_parameters_allphases)){
  gliding_parameters_allphases[i,9] = atan(as.numeric(gliding_parameters_allphases[i,6]) / as.numeric(gliding_parameters_allphases[i,7]))*180/pi
}

## remove one very high glide angle:
gliding_parameters_allphases = gliding_parameters_allphases[-c(which(gliding_parameters_allphases$glide_angle>50)),]

## select only glide of duration > to a threshold
gliding_parameters_allphases = gliding_parameters_allphases[
  which(gliding_parameters_allphases$`gliding_duration_(s)`>0.2),]



##### plot glide angle: C vs U
library(ggplot2)
glide_angle_plot = ggplot(gliding_parameters_allphases, aes(x = ecology, y = glide_angle, fill=ecology)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values=c("cornflowerblue", "chartreuse3")) +
  geom_jitter(position = position_jitter(0.15), cex = 1.2, color ='black') +
  labs(y = 'glide angle (degree)', x='ecology') + ylim(0,40) + labs(fill = '') +
  ggtitle('') +
  theme_classic() + theme(legend.position = "none")
glide_angle_plot

anova(lm(gliding_parameters_allphases$glide_angle ~ gliding_parameters_allphases$ecology))

mean(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='U')])
mean(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='C')])
se <- function(vecteur) { sd(vecteur)/sqrt(length(vecteur)) }

# write.csv(gliding_parameters_allphases,'/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/gliding_parameters_allphases.csv', row.names = F)
length(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='C')])
length(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='U')])

##### reduce the 'gliding_parameters_allphases' dataset to the number of flights (i.e. n=241)
# gliding_parameters_allphases_reduced = as.data.frame(matrix(NA, nrow(files_ID), 6))
# gliding_parameters_allphases_reduced[,1:5] = files_ID[,2:6]
# colnames(gliding_parameters_allphases_reduced)=c(colnames(files_ID)[2:6],'mean_glide_angle')
# 
# for(i in 1:nrow(files_ID)){
#   if (gliding_phase_NB[i]==0){gliding_parameters_allphases_reduced[i,6]=NA}
#   if (gliding_phase_NB[i]>0){
#   index_now = which(gliding_parameters_allphases$species==files_ID$species[i] &
#                       gliding_parameters_allphases$specimen_ID==files_ID$specimen_ID[i] &
#                       gliding_parameters_allphases$flight_ID==files_ID$flight_ID[i])
#   gliding_parameters_allphases_reduced[i,6] = mean(
#     gliding_parameters_allphases$glide_angle[index_now]
#   )
# }}
# 
# 
# glide_angle_plot = ggplot(gliding_parameters_allphases_reduced, aes(x = ecology, y = mean_glide_angle, fill=ecology)) +
#   geom_boxplot(outlier.shape = NA) + scale_fill_manual(values=c("cornflowerblue", "chartreuse3")) +
#   geom_jitter(position = position_jitter(0.15), cex = 1.2, color ='black') +
#   labs(y = 'glide angle (degree)', x='ecology') + ylim(0,40) + labs(fill = '') +
#   ggtitle('') +
#   theme_classic() + theme(legend.position = "none")
# glide_angle_plot
# 
# anova(lm(gliding_parameters_allphases_reduced$mean_glide_angle ~ gliding_parameters_allphases_reduced$ecology))

# write.csv(gliding_parameters_allphases_reduced,'/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/gliding_parameters_allphases_flight_levels.csv', row.names = F)






########################### Plot simultaneously all flight trajectories ########################### 

## indexing flight direction (based on the camera 1 view, i.e. along the x axis)
flight_direction = matrix(NA, length(smooth_coords_files),1)
for (i in 1:length(smooth_coords_files)){
  if (as.numeric(Flight_and_strokes2[total_frames[i],2,i]) > 0) {flight_direction[i] = as.character('right to left')
  } else {flight_direction[i] = as.character('left to right')}
}

## setting all trajectories to the same direction (we keep the right to left direction)
Flight_and_strokes2_unidirectional = Flight_and_strokes2
for (i in 1:length(smooth_coords_files)){
  if (flight_direction[i] == 'left to right'){
  for (j in 1:total_frames[i]){
    Flight_and_strokes2_unidirectional[j,2,i] = -as.numeric(Flight_and_strokes2_unidirectional[j,2,i])
    }
  }
}


## all trajectories have to start from a same position: the origin (0,0,0)
# removing the first value of x, y and z on all trajectory points
for (i in 1:length(smooth_coords_files)){
  X1 = as.numeric(Flight_and_strokes2_unidirectional[1,2,i])
  Y1 = as.numeric(Flight_and_strokes2_unidirectional[1,3,i])
  Z1 = as.numeric(Flight_and_strokes2_unidirectional[1,4,i])
  for (j in 1:total_frames[i]){
    Flight_and_strokes2_unidirectional[j,2,i] = as.numeric(Flight_and_strokes2_unidirectional[j,2,i]) - X1
    Flight_and_strokes2_unidirectional[j,3,i] = as.numeric(Flight_and_strokes2_unidirectional[j,3,i]) - Y1
    Flight_and_strokes2_unidirectional[j,4,i] = as.numeric(Flight_and_strokes2_unidirectional[j,4,i]) - Z1
  }
}


###### Plot all trajectories

## we do not consider the trajectory where the flight path is a back and forth (for clarity purpose)
which(files_ID$back_and_forth=='no')

## set colors
colors_species = c('gray60', # achilles
                   'hotpink',    # aurora
                   'limegreen', # cisseis
                   'brown3', # deidamia
                   'lightslateblue', # godartii
                   'black', # helenor
                   'magenta3', # marcus
                   'orange', # menelaus
                   'deepskyblue2', # rhetenor
                   'gold1', # sulkowskyi
                   'sienna') # theseus

colo_species <- colors_species[match(files_ID$species,levels(files_ID$species))]

colors_ecology = c('#0094D6', # canopy
                   '#75B638')    # understory
colo_ecology <- colors_ecology[match(files_ID$ecology,levels(files_ID$ecology))]


### grouping by species
plot3d(Flight_and_strokes2_unidirectional[,2:4,1], type='l',lwd=2, col=colo_species[1], xlab="X ", ylab="Y ", zlab="Z ", asp=F, box=F)
for (i in which(files_ID$back_and_forth=='no')){
  plot3d(Flight_and_strokes2_unidirectional[,2:4,i], type='l',lwd=2, col=colo_species[i], add=T)
}

### grouping by ecology
plot3d(Flight_and_strokes2_unidirectional[,2:4,1], type='l',lwd=3, col=colo_ecology[1], xlab="X ", ylab="Y ", zlab="Z ", asp=F, box=F)
for (i in which(files_ID$back_and_forth=='no')){
  plot3d(Flight_and_strokes2_unidirectional[,2:4,i], type='l',lwd=3, col=colo_ecology[i], add=T)
  # legend3d("topright", legend = paste0(files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i]), bty='n', cex=2, inset=c(0.02))
}


### trial for paper figure
plot3d(Flight_and_strokes2_unidirectional[,2:4,1], type='p',size=2, col='steelblue4', xlab=" ", ylab=" ", zlab=" ", asp=F, box=F)
for (i in which(files_ID$back_and_forth=='no')){
  plot3d(Flight_and_strokes2_unidirectional[,2:4,i], type='p',size=2, col='steelblue4', add=T)
  # legend3d("topright", legend = paste0(files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i]), bty='n', cex=2, inset=c(0.02))
}

# rgl.snapshot("All_trajectories.png","png")
# getwd()

### get color as function of instantaneous speed (or other quantities)
range01 <- function(x)(x-min(x))/diff(range(x)) # scale speed between 0 and 1
cRamp <- function(x){
  cols <- colorRamp(rainbow(7))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[3], xt[2], xt[1], maxColorValue=255))
}  

## with color as function of speed:
plot3d(Flight_and_strokes2_unidirectional[,2:4,1], type='p',size=3,col=cRamp(na.omit(velo_inst[,,1])), xlab=" ", ylab=" ", zlab=" ", asp=F, box='n')
for (i in which(files_ID$back_and_forth=='no')){
  plot3d(Flight_and_strokes2_unidirectional[,2:4,i], type='p',size=3, col=cRamp(na.omit(velo_inst[,,i])), add=T)
 }

## get heat map legend:

b = seq(1,300,by=1)
plot(b, pch=15, cex=4, col=cRamp(b), bty='n', axes=F, xlab='',ylab='')










### plot only on the X vs Z axis

Flight_and_strokes2_unidirectional_2D = array(NA, dim = c(max(total_frames), 2, length(smooth_coords_files)))
colnames(Flight_and_strokes2_unidirectional_2D) <-  c('sqrt(X?+Y?)','Z')
for (i in 1:length(smooth_coords_files)){
  Flight_and_strokes2_unidirectional_2D[,2,i] = Flight_and_strokes2_unidirectional[,4,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Flight_and_strokes2_unidirectional_2D[j,1,i] = sqrt( (as.numeric(Flight_and_strokes2_unidirectional[j,2,i]))^2 +
                                                         (as.numeric(Flight_and_strokes2_unidirectional[j,3,i]))^2 )
  }
}

#### distinguish species
plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=3, col=colo_species[1], xlab=" sqrt(X^2+Y^2) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
for (i in sample(which(files_ID$back_and_forth=='no'), length(which(files_ID$back_and_forth=='no')))){ # randomize the plotting order
  points(Flight_and_strokes2_unidirectional_2D[,1:2,i], type='l',lwd=3, col=colo_species[i], asp=T)
}
legend("topright", pch=16, pt.cex=1, c(levels(files_ID$species)), col=colors_species, bty='n', cex=0.7)

## simplified trajectories with arrows
plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=0.01, col='white', xlab=" sqrt(X^2+Y^2) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
for (i in which(files_ID$back_and_forth=='no')){ 
  arrows(0,0, as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[i],1,i]),
              as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[i],2,i]),
         col=colo_species[i], length=0.1, lwd=2)
}


#### distinguish ecology
plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=3, col=colo_ecology[1], xlab=" sqrt(X^2+Y^2) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
for (i in sample(which(files_ID$back_and_forth=='no'), length(which(files_ID$back_and_forth=='no')))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,i], type='l',lwd=3, col=colo_ecology[i], asp=T)
}
legend("topleft", pch=16, pt.cex=1, c('canopy','understory'), col=colors_ecology, bty='n', cex=0.7)

plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=0.01, col='white', xlab=" sqrt(X^2+Y^2) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
for (i in which(files_ID$back_and_forth=='no')){ 
  arrows(0,0, as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[i],1,i]),
              as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[i],2,i]),
         col=colo_ecology[i], length=0.1, lwd=2)
}


### plot only on the X vs Y axis

Flight_and_strokes2_unidirectional_2D_XY = array(NA, dim = c(max(total_frames), 2, length(smooth_coords_files)))
colnames(Flight_and_strokes2_unidirectional_2D_XY) <-  c('sqrt(X^2+Y^2)','Y')
for (i in 1:length(smooth_coords_files)){
  Flight_and_strokes2_unidirectional_2D_XY[,2,i] = Flight_and_strokes2_unidirectional[,3,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Flight_and_strokes2_unidirectional_2D_XY[j,1,i] = sqrt( (as.numeric(Flight_and_strokes2_unidirectional[j,2,i]))^2 +
                                                           (as.numeric(Flight_and_strokes2_unidirectional[j,4,i]))^2 )
  }
}

#### distinguish ecology
plot(Flight_and_strokes2_unidirectional_2D_XY[,1:2,1], type='l',lwd=3, col=colo_ecology[1], xlab=" sqrt(X^2+Y^2) ", ylab=" Y ", asp=T, xlim=c(0,7), ylim=c(-2,2), bty='n')
for (i in sample(which(files_ID$back_and_forth=='no'), length(which(files_ID$back_and_forth=='no')))){
  points(Flight_and_strokes2_unidirectional_2D_XY[,1:2,i], type='l',lwd=3, col=colo_ecology[i], asp=T)
}
legend("topleft", pch=16, pt.cex=1, c('canopy','understory'), col=colors_ecology, bty='n', cex=0.7)







#### species one by one

plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=0.01, col='white', xlab=" sqrt(X?+Y?) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
## achilles
for (i in 1:length(which(files_ID$species == 'achilles'))){
points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'achilles')[i]], col='gray60', type='l',lwd=3, asp=T)
}
## helenor
for (i in 1:length(which(files_ID$species == 'helenor'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'helenor')[i]], col='black', type='l',lwd=3, asp=T)
}
## deidamia
for (i in 1:length(which(files_ID$species == 'deidamia'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'deidamia')[i]], col='brown3', type='l',lwd=3, asp=T)
}
## menelaus
for (i in 1:length(which(files_ID$species == 'menelaus'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'menelaus')[i]], col='orange', type='l',lwd=3, asp=T)
}
## godartii
for (i in 1:length(which(files_ID$species == 'godartii'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'godartii')[i]], col='lightslateblue', type='l',lwd=3, asp=T)
}
## sulkowskyi
for (i in 1:length(which(files_ID$species == 'sulkowskyi'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'sulkowskyi')[i]], col='gold1', type='l',lwd=3, asp=T)
}
## aurora
for (i in 1:length(which(files_ID$species == 'aurora'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'aurora')[i]], col='hotpink', type='l',lwd=3, asp=T)
}
## marcus
for (i in 1:length(which(files_ID$species == 'marcus'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'marcus')[i]], col='magenta3', type='l',lwd=3, asp=T)
}
## cisseis
for (i in 1:length(which(files_ID$species == 'cisseis'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'cisseis')[i]], col='limegreen', type='l',lwd=3, asp=T)
}
## theseus
for (i in 1:length(which(files_ID$species == 'theseus'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'theseus')[i]], col='sienna', type='l',lwd=3, asp=T)
}
## rhetenor
for (i in 1:length(which(files_ID$species == 'rhetenor'))){
  points(Flight_and_strokes2_unidirectional_2D[,1:2,which(files_ID$species == 'rhetenor')[i]], col='deepskyblue2', type='l',lwd=3, asp=T)
}


#### compute mean path direction per species
mean_2Dpath_direction = matrix(NA, length(levels(files_ID$species)), 3)
colnames(mean_2Dpath_direction) = c('species', 'sqrt(X?+Y?)','Z')
mean_2Dpath_direction[,1] = levels(files_ID$species)

## achilles
achilles_endpoint = matrix(NA, length(which(files_ID$species == 'achilles')), 2) ; colnames(achilles_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'achilles'))){
  achilles_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'achilles')[i]],
                           1,which(files_ID$species == 'achilles')[i]])
  achilles_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'achilles')[i]],
                           2,which(files_ID$species == 'achilles')[i]])
}
mean_2Dpath_direction[1,2] = mean(achilles_endpoint[,1])
mean_2Dpath_direction[1,3] = mean(achilles_endpoint[,2])

## aurora
aurora_endpoint = matrix(NA, length(which(files_ID$species == 'aurora')), 2) ; colnames(aurora_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'aurora'))){
  aurora_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'aurora')[i]],
                                                                            1,which(files_ID$species == 'aurora')[i]])
  aurora_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'aurora')[i]],
                                                                            2,which(files_ID$species == 'aurora')[i]])
}
mean_2Dpath_direction[2,2] = mean(aurora_endpoint[,1])
mean_2Dpath_direction[2,3] = mean(aurora_endpoint[,2])

## cisseis
cisseis_endpoint = matrix(NA, length(which(files_ID$species == 'cisseis')), 2) ; colnames(cisseis_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'cisseis'))){
  cisseis_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'cisseis')[i]],
                                                                          1,which(files_ID$species == 'cisseis')[i]])
  cisseis_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'cisseis')[i]],
                                                                          2,which(files_ID$species == 'cisseis')[i]])
}
mean_2Dpath_direction[3,2] = mean(cisseis_endpoint[,1])
mean_2Dpath_direction[3,3] = mean(cisseis_endpoint[,2])

## deidamia
deidamia_endpoint = matrix(NA, length(which(files_ID$species == 'deidamia')), 2) ; colnames(deidamia_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'deidamia'))){
  deidamia_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'deidamia')[i]],
                                                                           1,which(files_ID$species == 'deidamia')[i]])
  deidamia_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'deidamia')[i]],
                                                                           2,which(files_ID$species == 'deidamia')[i]])
}
mean_2Dpath_direction[4,2] = mean(deidamia_endpoint[,1])
mean_2Dpath_direction[4,3] = mean(deidamia_endpoint[,2])

## godartii
godartii_endpoint = matrix(NA, length(which(files_ID$species == 'godartii')), 2) ; colnames(godartii_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'godartii'))){
  godartii_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'godartii')[i]],
                                                                            1,which(files_ID$species == 'godartii')[i]])
  godartii_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'godartii')[i]],
                                                                            2,which(files_ID$species == 'godartii')[i]])
}
mean_2Dpath_direction[5,2] = mean(godartii_endpoint[,1])
mean_2Dpath_direction[5,3] = mean(godartii_endpoint[,2])

## helenor
helenor_endpoint = matrix(NA, length(which(files_ID$species == 'helenor')), 2) ; colnames(helenor_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'helenor'))){
  helenor_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'helenor')[i]],
                                                                            1,which(files_ID$species == 'helenor')[i]])
  helenor_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'helenor')[i]],
                                                                            2,which(files_ID$species == 'helenor')[i]])
}
mean_2Dpath_direction[6,2] = mean(helenor_endpoint[,1])
mean_2Dpath_direction[6,3] = mean(helenor_endpoint[,2])

## marcus
marcus_endpoint = matrix(NA, length(which(files_ID$species == 'marcus')), 2) ; colnames(marcus_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'marcus'))){
  marcus_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'marcus')[i]],
                                                                           1,which(files_ID$species == 'marcus')[i]])
  marcus_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'marcus')[i]],
                                                                           2,which(files_ID$species == 'marcus')[i]])
}
mean_2Dpath_direction[7,2] = mean(marcus_endpoint[,1])
mean_2Dpath_direction[7,3] = mean(marcus_endpoint[,2])

## menelaus
menelaus_endpoint = matrix(NA, length(which(files_ID$species == 'menelaus')), 2) ; colnames(menelaus_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'menelaus'))){
  menelaus_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'menelaus')[i]],
                                                                          1,which(files_ID$species == 'menelaus')[i]])
  menelaus_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'menelaus')[i]],
                                                                          2,which(files_ID$species == 'menelaus')[i]])
}
mean_2Dpath_direction[8,2] = mean(menelaus_endpoint[,1])
mean_2Dpath_direction[8,3] = mean(menelaus_endpoint[,2])

## rhetenor
rhetenor_endpoint = matrix(NA, length(which(files_ID$species == 'rhetenor')), 2) ; colnames(rhetenor_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'rhetenor'))){
  rhetenor_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'rhetenor')[i]],
                                                                            1,which(files_ID$species == 'rhetenor')[i]])
  rhetenor_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'rhetenor')[i]],
                                                                            2,which(files_ID$species == 'rhetenor')[i]])
}
mean_2Dpath_direction[9,2] = mean(rhetenor_endpoint[,1])
mean_2Dpath_direction[9,3] = mean(rhetenor_endpoint[,2])

## sulkowskyi
sulkowskyi_endpoint = matrix(NA, length(which(files_ID$species == 'sulkowskyi')), 2) ; colnames(sulkowskyi_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'sulkowskyi'))){
  sulkowskyi_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'sulkowskyi')[i]],
                                                                            1,which(files_ID$species == 'sulkowskyi')[i]])
  sulkowskyi_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'sulkowskyi')[i]],
                                                                            2,which(files_ID$species == 'sulkowskyi')[i]])
}
mean_2Dpath_direction[10,2] = mean(sulkowskyi_endpoint[,1])
mean_2Dpath_direction[10,3] = mean(sulkowskyi_endpoint[,2])

## theseus
theseus_endpoint = matrix(NA, length(which(files_ID$species == 'theseus')), 2) ; colnames(theseus_endpoint) = c('sqrt(X?+Y?)','Z')
for (i in 1:length(which(files_ID$species == 'theseus'))){
  theseus_endpoint[i,1] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'theseus')[i]],
                                                                              1,which(files_ID$species == 'theseus')[i]])
  theseus_endpoint[i,2] = as.numeric(Flight_and_strokes2_unidirectional_2D[total_frames[which(files_ID$species == 'theseus')[i]],
                                                                              2,which(files_ID$species == 'theseus')[i]])
}
mean_2Dpath_direction[11,2] = mean(theseus_endpoint[,1])
mean_2Dpath_direction[11,3] = mean(theseus_endpoint[,2])




plot(Flight_and_strokes2_unidirectional_2D[,1:2,1], type='l',lwd=0.01, col='white', xlab=" sqrt(X?+Y?) ", ylab=" Z ", asp=T, xlim=c(0,7), ylim=c(-0.5,2), bty='n')
## achilles
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='achilles'),2]),
            as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='achilles'),3]),
            col='gray60', length=0.1, lwd=2)
## aurora
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='aurora'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='aurora'),3]),
       col='hotpink', length=0.1, lwd=2)
## cisseis
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='cisseis'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='cisseis'),3]),
       col='limegreen', length=0.1, lwd=2)
## deidamia
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='deidamia'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='deidamia'),3]),
       col='brown3', length=0.1, lwd=2)
## godartii
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='godartii'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='godartii'),3]),
       col='lightslateblue', length=0.1, lwd=2)
## helenor
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='helenor'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='helenor'),3]),
       col='black', length=0.1, lwd=2)
## marcus
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='marcus'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='marcus'),3]),
       col='magenta3', length=0.1, lwd=2)
## menelaus
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='menelaus'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='menelaus'),3]),
       col='orange', length=0.1, lwd=2)
## rhetenor
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='rhetenor'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='rhetenor'),3]),
       col='deepskyblue2', length=0.1, lwd=2)
## sulkowskyi
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='sulkowskyi'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='sulkowskyi'),3]),
       col='gold1', length=0.1, lwd=2)
## theseus
arrows(0,0, as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='theseus'),2]),
       as.numeric(mean_2Dpath_direction[which(mean_2Dpath_direction[,1]=='theseus'),3]),
       col='sienna', length=0.1, lwd=2)
legend("topright", pch=16, pt.cex=1, c(levels(files_ID$species)), col=colors_species, bty='n', cex=0.7)







# ### get color as function of instantaneous speed (or other quantities)
# range01 <- function(x)(x-min(x))/diff(range(x)) # scale speed between 0 and 1
# cRamp <- function(x){
#   cols <- colorRamp(rainbow(7))(range01(x))
#   apply(cols, 1, function(xt)rgb(xt[3], xt[2], xt[1], maxColorValue=255))
# }  
# 
# ### code 'AC_flight_trajectories_velocity-accel-sinuosity' must be loaded to get the instantaneous velocity
# 
# plot3d(Flight_and_strokes2_unidirectional[,2:4,1], type='l',lwd=3, col=cRamp(na.omit(velo_inst[,,1])), xlab="X ", ylab="Y ", zlab="Z ", asp=F, box=F)
# for (i in which(files_ID$back_and_forth=='no')){
#   plot3d(Flight_and_strokes2_unidirectional[,2:4,i], type='l',lwd=3, col=cRamp(na.omit(velo_inst[,,i])), add=T)
# }























