
      #     
     # #    
    #   #     The codes 'AA_flight_trajectories_formating.R' and 'AB_flight_trajectories_flap_vs_glide.R' 
   #  !  #    must be loaded before running this code
  #       #         
 # # # # # #



local_path = dirname(rstudioapi::getSourceEditorContext()$path)


############################# importing/formating velocity vectors ############################# 

# this process is similar as the first step of the code 'AA_flight_trajectories_formating.R', but here we consider velocity vectors instead of coordinates.
# see in this code for further comments on code lines.

mypath = paste0(local_path,'/00_Digitized_Coordinates_only3D/kalmanFiltered_velocity_vectors/All_fps/')

smooth_coords_files = dir(mypath, pattern = glob2rx("*.csv"))
total_frames = rep(NA, length(smooth_coords_files))
for(i in 1:(length(smooth_coords_files) )){
  scan_loop= c(mypath, smooth_coords_files[i])
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=',', dec='.'))
  total_frames[i] = length(((i-1)*length(read.smooth_coords_files[,1])+1):(i*length(read.smooth_coords_files[,1])))}

velocity_vect = matrix(NA, sum(total_frames), 5)

i=1
scan_loop = c(mypath, smooth_coords_files[i])
read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
velocity_vect[1:total_frames[i],] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)

for(i in 2:(length(smooth_coords_files))){
  scan_loop = c(mypath, smooth_coords_files[i]) 
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
  velocity_vect[(sum(total_frames[1:(i-1)]) +1):((sum(total_frames[1:(i-1)])) + total_frames[i]),] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)
}

velocity_vect <- velocity_vect[,c(4,1,2,3,5)]
colnames(velocity_vect) <-  c('time', 'U', 'V', 'W', 'index')

seq_120fps <- which(files_ID$fps == 120)
for(i in seq_120fps){
  velocity_vect[which(velocity_vect[,5]==i), 1] = seq(from=0, to=(1/120)*(total_frames[i]-1), by=1/120)
}

Velocity_vect = array(NA, dim= c(max(total_frames), 4, length(smooth_coords_files)))
colnames(Velocity_vect) <-  c('time', 'U', 'V', 'W')

for(i in 1:length(smooth_coords_files)){
  Velocity_vect[1:total_frames[i],,i] = velocity_vect[velocity_vect[,5] == i, 1:4]
}



############################# importing/formating acceleration vectors ############################# 

# this process is similar as the first step of the code 'AA_flight_trajectories_formating.R', but here we consider acceleration vectors instead of coordinates.
# see in this code for further comments on code lines.

mypath = paste0(local_path, '/00_Digitized_Coordinates_only3D/kalmanFiltered_acceleration_vectors/All_fps/')

smooth_coords_files = dir(mypath, pattern = glob2rx("*.csv"))
total_frames = rep(NA, length(smooth_coords_files))
for(i in 1:(length(smooth_coords_files) )){
  scan_loop= c(mypath, smooth_coords_files[i])
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=',', dec='.'))
  total_frames[i] = length(((i-1)*length(read.smooth_coords_files[,1])+1):(i*length(read.smooth_coords_files[,1])))}

accel_vect = matrix(NA, sum(total_frames), 5)

i=1
scan_loop = c(mypath, smooth_coords_files[i])
read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
accel_vect[1:total_frames[i],] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)

for(i in 2:(length(smooth_coords_files))){
  scan_loop = c(mypath, smooth_coords_files[i]) 
  read.smooth_coords_files = as.matrix(read.csv(paste(scan_loop, collapse= ""), h=T, sep=",", dec="."))
  accel_vect[(sum(total_frames[1:(i-1)]) +1):((sum(total_frames[1:(i-1)])) + total_frames[i]),] = cbind(read.smooth_coords_files, seq(from=0, to=(1/240)*(total_frames[i]-1), by=1/240), i)
}

accel_vect <- accel_vect[,c(4,1,2,3,5)]
colnames(accel_vect) <-  c('time', 'ax', 'ay', 'az', 'index')

seq_120fps <- which(files_ID$fps == 120)
for(i in seq_120fps){
  accel_vect[which(accel_vect[,5]==i), 1] = seq(from=0, to=(1/120)*(total_frames[i]-1), by=1/120)
}

Accel_vect = array(NA, dim= c(max(total_frames), 4, length(smooth_coords_files)))
colnames(Accel_vect) <-  c('time', 'ax', 'ay', 'az')

for(i in 1:length(smooth_coords_files)){
  Accel_vect[1:total_frames[i],,i] = accel_vect[accel_vect[,5] == i, 1:4]
}


# ############################ cutting noisy frames ############################
# number of (first and last) frame to remove
c = 30
# remove first frame
Velocity_vect = Velocity_vect[c:max(total_frames),,]
Accel_vect = Accel_vect[c:max(total_frames),,]
# update total_frames
for (i in 1:length(smooth_coords_files)){
  if(is.na(last(Velocity_vect[,1,i]))==F){total_frames[i] = which(Velocity_vect[,1,i]==last(Velocity_vect[,1,i]))}
  else {
    total_frames[i] = which(is.na(Velocity_vect[,1,i])==T)[1] - 1
  }
}
# remove last frames
for (i in 1:length(smooth_coords_files)){
  Velocity_vect[(total_frames[i] - (c-1)):total_frames[i],,i] = NA
  Accel_vect[(total_frames[i] - (c-1)):total_frames[i],,i] = NA
}
# update total_frames (again)
for (i in 1:length(smooth_coords_files)){
  if(is.na(last(Velocity_vect[,1,i]))==F){total_frames[i] = which(Velocity_vect[,1,i]==last(Velocity_vect[,1,i]))}
  else {
    total_frames[i] = which(is.na(Velocity_vect[,1,i])==T)[1] - 1
  }
}
Velocity_vect = Velocity_vect[1:max(total_frames),,]
Accel_vect = Accel_vect[1:max(total_frames),,]




############################# distances and sinuosity ############################# 

## using Pythagore theorem, distance between point a and b is sqrt[(Xa-Xb)?+(Ya-Yb)?+(Za-Zb)?]
## example:
# a = as.numeric(Flight_and_strokes[1,2:4,1])
# b = as.numeric(Flight_and_strokes[2,2:4,1])
# ab = as.matrix(rbind(a,b))
# dist_a_b = sqrt( (diff(ab[,1]))^2 + (diff(ab[,2]))^2 + (diff(ab[,3]))^2)
# 
# diff(as.numeric(Flight_and_strokes[,2:4,1])) # compute the difference between n1 and n2, n2 and n3, n3 and n4 etc. along the coords


# compute the consecutive distance between all positions
consec_dist = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
prog_bar <- txtProgressBar(min = 0, max = length(smooth_coords_files), style = 3)
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:(total_frames[i])){
  consec_dist[j,1,i] = sqrt( (diff(as.numeric(na.omit(Flight_and_strokes2[,2,i])))[j])^2 + 
                             (diff(as.numeric(na.omit(Flight_and_strokes2[,3,i])))[j])^2 +
                             (diff(as.numeric(na.omit(Flight_and_strokes2[,4,i])))[j])^2 )
  }
  Sys.sleep(0.1)
  setTxtProgressBar(prog_bar, i)
}
close(prog_bar)

# compute the total distance of all consecutive positions (i.e. the covered distance in each flight)
covered_dist = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  covered_dist[i,] = sum(na.omit(consec_dist[,,i]))
}

# compute the distance between starting to ending position, 'direct distance'
first_to_last_pos = array(NA, dim= c(2, 3, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  first_to_last_pos[,,i] = rbind(Flight_and_strokes2[1,2:4,i], Flight_and_strokes2[total_frames[i],2:4,i])
}

direct_dist = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
direct_dist[i,] = sqrt( (diff(as.numeric(first_to_last_pos[,1,i])))^2 +
                       (diff(as.numeric(first_to_last_pos[,2,i])))^2 +
                       (diff(as.numeric(first_to_last_pos[,3,i])))^2 )
}

### Sinuosity
# computed as the ratio of the actual distance covered over
# the distance between starting and ending position (a straight path have a sinuosoty equal to 1)
sinuosity = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
  sinuosity[i] = covered_dist[i] / direct_dist[i]
}




### compute sinuosity only on the XY plan (same process as above, but only considering x and y columns)
consec_dist_horiz = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
prog_bar <- txtProgressBar(min = 0, max = length(smooth_coords_files), style = 3)
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:(total_frames[i])){
    consec_dist_horiz[j,1,i] = sqrt( (diff(as.numeric(na.omit(Flight_and_strokes2[,2,i])))[j])^2 + 
                                 (diff(as.numeric(na.omit(Flight_and_strokes2[,3,i])))[j])^2 )
  }
  Sys.sleep(0.1)
  setTxtProgressBar(prog_bar, i)
}
close(prog_bar)
covered_dist_horiz = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  covered_dist_horiz[i,] = sum(na.omit(consec_dist_horiz[,,i]))
}
first_to_last_pos_horiz = array(NA, dim= c(2, 2, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  first_to_last_pos_horiz[,,i] = rbind(Flight_and_strokes2[1,2:3,i], Flight_and_strokes2[total_frames[i],2:3,i])
}
direct_dist_horiz = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
  direct_dist_horiz[i,] = sqrt( (diff(as.numeric(first_to_last_pos_horiz[,1,i])))^2 +
                            (diff(as.numeric(first_to_last_pos_horiz[,2,i])))^2)
}
sinuosity_horiz = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
  sinuosity_horiz[i] = covered_dist_horiz[i] / direct_dist_horiz[i]
}




### compute sinuosity in the Z - combined XY plan (this allows considering only the vertical sinuosity)

### bulding the Z vs combined-XY axes
Z_XY_coords = array(NA, dim= c(max(total_frames), 2, length(smooth_coords_files)))
colnames(Z_XY_coords) <-  c('sqrt(X?+Y?)','Z')

for (i in 1:length(smooth_coords_files)){
  Z_XY_coords[1:total_frames[i],2,i] = Flight_and_strokes2[1:total_frames[i],4,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Z_XY_coords[j,1,i] = sqrt( (as.numeric(Flight_and_strokes2[j,2,i]))^2 +
                                 (as.numeric(Flight_and_strokes2[j,3,i]))^2 )
  }
}

### same process as above, but only in the Z - combined XY plan
consec_dist_verti = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
prog_bar <- txtProgressBar(min = 0, max = length(smooth_coords_files), style = 3)
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:(total_frames[i])){
    consec_dist_verti[j,1,i] = sqrt( (diff(as.numeric(na.omit(Z_XY_coords[,1,i])))[j])^2 + 
                                       (diff(as.numeric(na.omit(Z_XY_coords[,2,i])))[j])^2 )
  }
  Sys.sleep(0.1)
  setTxtProgressBar(prog_bar, i)
}
close(prog_bar)
covered_dist_verti = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  covered_dist_verti[i,] = sum(na.omit(consec_dist_verti[,,i]))
}
first_to_last_pos_verti = array(NA, dim= c(2, 2, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  first_to_last_pos_verti[,,i] = rbind(Z_XY_coords[1,1:2,i], Z_XY_coords[total_frames[i],1:2,i])
}
direct_dist_verti = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
  direct_dist_verti[i,] = sqrt( (diff(as.numeric(first_to_last_pos_verti[,1,i])))^2 +
                                  (diff(as.numeric(first_to_last_pos_verti[,2,i])))^2)
}
sinuosity_verti = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files) ){
  sinuosity_verti[i] = covered_dist_verti[i] / direct_dist_verti[i]
}





############################## Velocity ############################

# the instantaneous speed is the norme (or 'magnitude') of the velocity vector
# function to compute the norm of a vector
norma <- function(x){
  sqrt(x[1]^2 + x[2]^2 + x[3]^2)
}

# instantaneous velocity, i.e. velocity computed at each position
velo_inst = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    velo_inst[j,1,i] = norma(Velocity_vect[j,2:4,i])
  }
}

### get color as function of instantaneous speed (or other quantities)
range01 <- function(x)(x-min(x))/diff(range(x)) # scale speed between 0 and 1
cRamp <- function(x){
  cols <- colorRamp(rainbow(7))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[3], xt[2], xt[1], maxColorValue=255))
}  


# plot variation in velocity
p=1
plot(Velocity_vect[1:total_frames[p],1,p], velo_inst[1:total_frames[p],,p], col=cRamp(na.omit(velo_inst[,,p])),lwd=2, pch=16, ylab='velocity (m/s)', xlab='time (s)')
## add stroke positions
# lines(Velocity_vect[which(is.na(ds_coords[,1,p])==F),1,p], velo_inst[which(is.na(ds_coords[,1,p])==F),1,p], col='red', typ='p')
# lines(Velocity_vect[which(is.na(us_coords[,1,p])==F),1,p], velo_inst[which(is.na(us_coords[,1,p])==F),1,p], col='forestgreen', typ='p')
# legend("topleft", c('downstroke','upstroke'), pch=1, col=c('red','forestgreen'), bty='n')

# ## correcting for speed of deidamia 47 F02 (ID79) because first frames are too high
# velo_inst[1:10,,79] = velo_inst[11,,79]


## plot variation of each component of velocity (X, Y and Z)
plot(Velocity_vect[1:total_frames[p],1,p], Velocity_vect[1:total_frames[p],2,p], typ='l', col=2, ylab='velocity (m/s)', xlab='time (s)', lwd=1,
     ylim=c(min(na.omit(c(Velocity_vect[1:total_frames[p],2,p], Velocity_vect[1:total_frames[p],3,p], Velocity_vect[1:total_frames[p],4,p]))),
            max(na.omit(c(Velocity_vect[1:total_frames[p],2,p], Velocity_vect[1:total_frames[p],3,p], Velocity_vect[1:total_frames[p],4,p])))))
lines(Velocity_vect[1:total_frames[p],1,p], Velocity_vect[1:total_frames[p],3,p], typ='l', col=3, lwd=1)
lines(Velocity_vect[1:total_frames[p],1,p], Velocity_vect[1:total_frames[p],4,p], typ='l', col=4, lwd=1)
legend("topleft", c('x','y','z'), lty=1, lwd=1, col=2:4, bty='n')
## xyz velocity
# lines(Velocity_vect[1:total_frames[p],1,p], velo_inst[1:total_frames[p],,p], typ='l', ylab='velocity (m/s)', xlab='time (s)', lwd=1)
# legend("top", c('xyz'), lty=1, lwd=1, col='black', bty='n')


## plot velocity vector tangent to the curve on trajectory
# p=215
# plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" ", ylab=" ", zlab=" ", asp=F, box=F)
# plot3d(flapping[,3:5,p], type='p',col='gray65', size=3, add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
# 
# for (i in seq(1,(total_frames[p]),1) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Velocity_vect[i,2:4,p]))/5), col='gray40', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }

# compute UNIT velocity vector tangent to the curve
Ta_velocity_vect = array(NA, dim= c((max(total_frames)), 4, length(smooth_coords_files)))
Ta_velocity_vect[,1,] = Velocity_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    Ta_velocity_vect[j,2:4,i] = as.numeric(Velocity_vect[j,2:4,i]) / velo_inst[j,,i]
  }
}

## plot UNIT velocity vector tangent to the curve on trajectory
# p=2
# plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)

# for (i in seq(1,(total_frames[p]),5) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Ta_velocity_vect[i,2:4,p]))/5), col='gray40', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }


############################## Acceleration ############################## 

# instantaneous acceleration, i.e. acceleration computed at each position
accel_inst = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    accel_inst[j,1,i] = norma(Accel_vect[j,2:4,i])
  }
}

# plot variation in acceleration
p=79
plot(Accel_vect[1:total_frames[p],1,p], accel_inst[1:total_frames[p],,p], typ='l', lwd=1, xlab=c('time (s)'), ylab=c('acceleration (m/s?)'))
# legend("topleft", c('ax_ay_az'), lty=1, lwd=1, col='black', bty='n')
# add stroke position
lines(Accel_vect[which(is.na(ds_coords[,1,p])==F),1,p], accel_inst[which(is.na(ds_coords[,1,p])==F),1,p], col='red', typ='p')
lines(Accel_vect[which(is.na(us_coords[,1,p])==F),1,p], accel_inst[which(is.na(us_coords[,1,p])==F),1,p], col='forestgreen', typ='p')
legend("topleft", c('downstroke','upstroke'), pch=1, col=c('red','forestgreen'), bty='n')


## plot variation in acceleration of X, Y and Z components
plot(Accel_vect[1:total_frames[p],1,p], Accel_vect[1:total_frames[p],2,p], typ='l', col=2, ylab='acceleration (m/s?)', xlab='time', lwd=1,
     ylim=c(min(na.omit(c(Accel_vect[1:total_frames[p],2,p], Accel_vect[1:total_frames[p],3,p], Accel_vect[1:total_frames[p],4,p]))),
            max(na.omit(c(Accel_vect[1:total_frames[p],2,p], Accel_vect[1:total_frames[p],3,p], Accel_vect[1:total_frames[p],4,p])))))
lines(Accel_vect[1:total_frames[p],1,p], Accel_vect[1:total_frames[p],3,p], typ='l', col=3, lwd=1)
lines(Accel_vect[1:total_frames[p],1,p], Accel_vect[1:total_frames[p],4,p], typ='l', col=4, lwd=1)
legend("topleft", c('ax','ay','az'), lty=1, lwd=1, col=2:4, bty='n')
## ax_ay_az acceleration
# lines(Accel_vect[1:total_frames[p],1,p], accel_inst[1:total_frames[p],,p], typ='l', lwd=1)
# legend("top", c('ax_ay_az'), lty=1, lwd=1, col='black', bty='n')


## plot acceleration vector tangent to the curve on trajectory
# p=4
# plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)

for (i in seq(1,(total_frames[p]),1) ){
  vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Accel_vect[i,2:4,p]))/40), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
}


## compute UNIT acceleration vector tangent to the curve
Ta_accel_vect = array(NA, dim= c((max(total_frames)), 4, length(smooth_coords_files)))
Ta_accel_vect[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    Ta_accel_vect[j,2:4,i] = as.numeric(Accel_vect[j,2:4,i]) / accel_inst[j,,i]
  }
}


## plot UNIT acceleration vector tangent to the curve on trajectory
# p=84
# plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
# 
# for (i in seq(1,(total_frames[p]),2) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Ta_accel_vect[i,2:4,p]))/5), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }




############################## Turning acceleration #########################

# 'turning acceleration' is the component of acceleration normal to the movement
# it is the component of acceleration strictly attributable to changes in direction.
# mathematically, it is the orthogonal projection of the acceleration vector
# in the plan orthogonal to the velocity vector ("normal to the movement").

# function to scalar product
scal <- function(x,y){
  x[1] * y[1] + x[2] * y[2] + x[3] * y[3]
}

# turning acceleration vector
N_accel_vect = array(NA, dim= c((max(total_frames)), 4, length(smooth_coords_files)))
N_accel_vect[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    N_accel_vect[j,2:4,i] = Accel_vect[j,2:4,i] - (scal(Accel_vect[j,2:4,i] , Ta_velocity_vect[j,2:4,i]))*Ta_velocity_vect[j,2:4,i]
  }
}

# magnitude of turning acceleration
N_accel = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
N_accel[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    N_accel[j,2,i] = norma(N_accel_vect[j,2:4,i])
  }
}

## plot variation in turning acceleration
p=4
plot(N_accel[1:total_frames[p],1,p], N_accel[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('turning acceleration'))

# lines(N_accel[which(is.na(ds_coords[,1,p])==F),1,p], N_accel[which(is.na(ds_coords[,1,p])==F),2,p], col='red', typ='p')
# lines(N_accel[which(is.na(us_coords[,1,p])==F),1,p], N_accel[which(is.na(us_coords[,1,p])==F),2,p], col='forestgreen', typ='p')
# legend("tobpleft", c('turning ax_ay_az'), lty=1, lwd=1, col='black', bty='n')

## plot turning acceleration on trajectory
# p=11
# plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[,3:5,p], type='p',col='gray65',size=3, add=T)
# plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
# plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
# 
# for (i in seq(1,total_frames[p],1) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(N_accel_vect[i,2:4,p]))/40), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }


## typical example
# p=84
# plot3d(gliding[1:200,3:5,p], type='l',lwd=4, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[1:200,3:5,p], type='p',col='gray65',size=3, add=T)
# plot3d(ds_coords[1:200,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.7, add=T)
# plot3d(us_coords[1:200,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)
# 
# for (i in seq(1,200,1) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(N_accel_vect[i,2:4,p]))/40), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }

############################## Turning acceleration in the horizontal plane #########################

norma2D <- function(x){
  sqrt(x[1]^2 + x[2]^2)
}
scal_horiz <- function(x,y){
  x[1] * y[1] + x[2] * y[2]
}

# turning acceleration vector in the X Y plan
N_accel_vect_horiz = array(NA, dim= c((max(total_frames)), 3, length(smooth_coords_files)))
N_accel_vect_horiz[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    N_accel_vect_horiz[j,2:3,i] = Accel_vect[j,2:3,i] - (scal_horiz(Accel_vect[j,2:3,i] , Ta_velocity_vect[j,2:3,i]))*Ta_velocity_vect[j,2:3,i]
  }
}

# magnitude of turning acceleration in the horizontal plane
N_accel_horiz = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
N_accel_horiz[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    N_accel_horiz[j,2,i] = norma2D(N_accel_vect[j,2:3,i])
  }
}

# plot variation in turning acceleration in the X Y plan
p=4
plot(N_accel_horiz[1:total_frames[p],1,p], N_accel_horiz[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('turning acceleration horizontal plane'))



# plot trajectory in in the X Y plan
## p= 113 is a turning trajectory
p=58
plot(Flight_and_strokes2[,2:3,p], type='l',lwd=2, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
points(ds_coords[,1:2,p], pch=16, col="red", xlab="", ylab="", zlab="", size=0.3)
points(us_coords[,1:2,p], pch=16, col="green", xlab="", ylab="", zlab="", size=0.3)

for (i in seq(1,(total_frames[p]),1) ){
  vectors(origin = as.numeric(Flight_and_strokes2[i,2:3,p]), (as.numeric(Flight_and_strokes2[i,2:3,p]) + (as.numeric(N_accel_vect_horiz[i,2:3,p]))/40), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
}



############################## Rate of change of heading #########################

### computed as the number of times turning acceleration vector is passing from one to another side from the velocity vector

# NB: mathematically, turning acceleration vector is the orthogonal projection of the acceleration vector
# in the plan orthogonal to the velocity vector ("normal to the movement").

### according to Ludo, we can detect whether the turning acceleration vector
### change of direction relative to the velocity vector in the following way:
### a[x]^2 + a[y]^2 < or > epsilon
### epsilon being a parameter we need to adjust depending one the temporal resolution

turning = array(NA, dim= c((max(total_frames)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    turning[j,1,i] = sqrt(N_accel_vect[j,2,i]^2 + N_accel_vect[j,3,i]^2)  # sqrt(a[x]^2 + a[y]^2)
  }
}
plot(N_accel_vect[1:total_frames[p],1,p], turning[1:total_frames[p],1,p], typ='l', xlab=c('time (s)'), ylab=c('turning'))

## we need to catch the number of time that turning rate is passing through the epsilon threshold
epsilon = 0.5

nb_change_heading = matrix(NA, length(smooth_coords_files), 1)
for (p in 1:length(smooth_coords_files)){
  list_now = matrix(NA, total_frames[p], 1)
  for (i in 1:total_frames[p]){ list_now[i] = turning[i,1,p] > epsilon }
  count = 0  # count the number of changes:
  for (i in 1:(total_frames[p]-1)){
    if (list_now[i]==T & list_now[i+1]==F) {count = count + 1}
  }
  nb_change_heading[p] = count
}

### the number of change of heading must be standardized by the length of each trajectory
rate_change_heading = matrix(NA, length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  rate_change_heading[i] = nb_change_heading[i] / seq_length[i]
}



## typical example
# p=84
# plot3d(gliding[1:200,3:5,p], type='p',size=5, col="black", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=F, box=F)
# plot3d(flapping[1:200,3:5,p], type='p',col='black',size=5, add=T)
# ## plot unit velocity vector on the flight path:
# for (i in seq(1,total_frames[p],1) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Ta_velocity_vect[i,2:4,p]))/5), col='darkgreen', radius = 1/300, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }
# ## plot unit turning acceleration vector on the flight path:
# for (i in seq(1,total_frames[p],1) ){
#   vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(N_accel_vect[i,2:4,p]))/40), col='darkred', radius = NULL, headlength = 0.005, ref.length=1, lwd=1, add=T)
# }
# ## horizontal view
# plot(Flight_and_strokes2[,2:3,p], type='l',lwd=2, col="gray8", xlab=" X ", ylab=" Y ", zlab=" Z ", asp=T, box=F)
# for (i in seq(1,(total_frames[p]),1) ){
#   vectors(origin = as.numeric(Flight_and_strokes2[i,2:3,p]), (as.numeric(Flight_and_strokes2[i,2:3,p]) + (as.numeric(N_accel_vect_horiz[i,2:3,p]))/40), col='darkred', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
# }





############################## Turning rate (as in Combes et al. 2012) #########################

# Turning rate is a measure of how quickly an animal turns (Combes et al. 2012)
# (it's another attempt to capture manoeuvrability with a different measurement).
# following Combes et al. 2012, we calculate it as the angular change in the direction
# of subsequent velocity vectors in three-dimensional space.


## plot velocity vector tangent to the curve on trajectory
p=2
plot3d(gliding[,3:5,p], type='l',lwd=4, col="gray8", xlab=" ", ylab=" ", zlab=" ", asp=F, box=F)
plot3d(flapping[,3:5,p], type='p',col='gray65', size=3, add=T)
plot3d(ds_coords[,1:3,p], typ="s", col="red", xlab="", ylab="", zlab="", size=0.3, add=T)
plot3d(us_coords[,1:3,p], typ="s", col="green", xlab="", ylab="", zlab="", size=0.3, add=T)

for (i in seq(1,(total_frames[p]),1) ){
  vectors3d(origin = as.numeric(Flight_and_strokes2[i,2:4,p]), (as.numeric(Flight_and_strokes2[i,2:4,p]) + (as.numeric(Velocity_vect[i,2:4,p]))/5), col='gray40', radius = 1/100, headlength = 0.005, ref.length=5, lwd=1, add=T)
}


## function to compute angle between 3D vectors
angle_btw_vectors <- function(x,y){
  acos(scal(x, y) / (norma(x)*norma(y)))
}

## compute variation in turning rate within each trajectory
turning_rate = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files)){turning_rate[1:max(total_frames),1,i] = Accel_vect[1:max(total_frames),1,i]} # add time in first column
for (i in 1:length(smooth_coords_files)) {
  for (j in 1:(max(total_frames[i])-1)){
    turning_rate[j,2,i] = (angle_btw_vectors(Velocity_vect[j,2:4,i], Velocity_vect[(j+1),2:4,i])*180/pi) / (turning_rate[(j+1),1,i] - turning_rate[j,1,i])
    
  }
}

## extract mean and peak turning rate for each flight
turning_rate_summarized = as.data.frame(matrix(NA, length(smooth_coords_files), 5))
colnames(turning_rate_summarized) = c('species', 'specimen', 'flight_ID','mean_turning_rate', 'peak_turning_rate')
turning_rate_summarized[,1] = files_ID[,4] ; turning_rate_summarized[,2] = files_ID[,2] ; turning_rate_summarized[,3] = files_ID[,3]
for (i in 1:length(smooth_coords_files)){
  turning_rate_summarized[i,4] = mean(na.omit(turning_rate[,2,i]))
  turning_rate_summarized[i,5] = max(na.omit(turning_rate[,2,i]))
}



############################## Radius of curvature #########################

# Curvature measure how sharply an animal turns (Combes et al. 2012).
# The instantaneous radius of curvature at a point on a curve is equivalent
# to the radius of the circle that best approximates the curve at that point
# (Greenwood DT (1988) Principles of Dynamics (Prentice-Hall, Englewood Cliffs, N.J.)
# 
# The 'strength' of the curvature can be expressed as 1/radius of curvature
# which is equal to the component of acceleration normal to the movement / speed squared

curvature = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
curvature[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    curvature[j,2,i] = norma(N_accel_vect[j,2:4,i]) / (velo_inst[j,1,i])^2
  }
}

p=184
plot(curvature[1:total_frames[p],1,p], curvature[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('curvature'))
# add stroke position
lines(curvature[which(is.na(ds_coords[,1,p])==F),1,p], curvature[which(is.na(ds_coords[,1,p])==F),2,p], col='red', typ='p')
lines(curvature[which(is.na(us_coords[,1,p])==F),1,p], curvature[which(is.na(us_coords[,1,p])==F),2,p], col='forestgreen', typ='p')
legend("topleft", c('downstroke','upstroke'), pch=1, col=c('red','forestgreen'), bty='n')



############################## Radius of curvature in the horizontal / vertical plane ############################## 

#### horizontal curvature
curvature_horiz = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
curvature_horiz[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    curvature_horiz[j,2,i] = norma2D(N_accel_vect[j,2:3,i]) / (norma2D(Velocity_vect[j,2:3,i]))^2
  }
}
p=184
plot(curvature_horiz[1:total_frames[p],1,p], curvature_horiz[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('curvature XY plan'))

#### vertical curvature? velocity and acceleration vector in the Z vs combined XY plane are needed
#### the 3d curvature is mostly driven by the vertical curvature anyway




############################## Radius of curvature inside gliding phases #########################

# extracting the velocity and acceleration vectors corresponding to max gliding phases coordinates
Velocity_vect_in_max_glide = array(NA, dim = c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,2])>0) ){
  Velocity_vect_in_max_glide[1:total_frames_maxglide[i],,i] = Velocity_vect[which(Flight_and_strokes2[,7,i] == max_glide_coords[1,7,i]) , 2:4,i]
}

# instantaneous velocity during the longest gliding phase
velo_inst_in_max_glide = array(NA, dim= c((max(total_frames_maxglide)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames_maxglide[i]){
    velo_inst_in_max_glide[j,1,i] = norma(Velocity_vect_in_max_glide[j,1:3,i])
  }
}



Accel_vect_in_max_glide = array(NA, dim = c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,2])>0) ){
  Accel_vect_in_max_glide[1:total_frames_maxglide[i],,i] = Accel_vect[which(Flight_and_strokes2[,7,i] == max_glide_coords[1,7,i]) , 2:4,i]
}

# turning acceleration vector in max gliding phases
N_accel_vect_in_max_glide = array(NA, dim = c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,2])>0) ){
  N_accel_vect_in_max_glide[1:total_frames_maxglide[i],,i] = N_accel_vect[which(Flight_and_strokes2[,7,i] == max_glide_coords[1,7,i]) , 2:4,i]
}

# computing curvature in max gliding phases
curvature_in_max_glide = array(NA, dim= c((max(total_frames_maxglide)), 2, length(smooth_coords_files)))
curvature_in_max_glide[,1,] = Accel_vect[ 1:max(total_frames_maxglide) ,1,] # add time in first column
for (i in which(as.numeric(phase_count[,2])>0)){
  for (j in 1:total_frames_maxglide[i]){
    curvature_in_max_glide[j,2,i] = norma(N_accel_vect_in_max_glide[j,,i]) / norma(Velocity_vect_in_max_glide[j,,i])^2
  }
}

# computing curvature in max gliding phases in the XY plan
curvature_horiz_in_max_glide = array(NA, dim= c((max(total_frames_maxglide)), 2, length(smooth_coords_files)))
curvature_in_max_glide[,1,] = Accel_vect[ 1:max(total_frames_maxglide) ,1,] # add time in first column
for (i in which(as.numeric(phase_count[,2])>0) ){
  for (j in 1:total_frames_maxglide[i]){
    curvature_horiz_in_max_glide[j,2,i] = norma2D(N_accel_vect_in_max_glide[j,1:2,i]) / norma2D(Velocity_vect_in_max_glide[j,1:2,i])^2
  }
}


############################## Ascent angle #########################
# from the matlab code 'analyse_3Dtracks' from Florian Muijres
# ascent_angle = 180/pi*atan2(w, sqrt( u.^2 + v.^2))
# see also in 'Muijres et al. 2017. Escaping blood-fed malaria mosquitoes minimize tactile detection without compromising on take-off speed'

# computing ascent angle
ascent_angle = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
ascent_angle[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    ascent_angle[j,2,i] = 180/pi*atan2(Velocity_vect[j,2:4,i][3], sqrt( (Velocity_vect[j,2:4,i][1])^2 + (Velocity_vect[j,2:4,i][2])^2))
  }
}

############################## Ascent angle on longest flapping phases #########################

## first we extract the velocity vector on longest max flap phases
## extracting the velocity vectors corresponding to max flapping phases coordinates
Velocity_vect_in_max_flap = array(NA, dim = c((max(total_frames_maxflap)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,3])>0) ){
  Velocity_vect_in_max_flap[1:total_frames_maxflap[i],,i] = Velocity_vect[which(Flight_and_strokes2[,7,i] == max_flap_coords[1,7,i]) , 2:4,i]
}

## compute anscent angle on max flap phase
ascent_angle_max_flap <- array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
ascent_angle_max_flap[,1,] = max_flap_coords[,1,] # add time in first column
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames_maxflap[i]){
    ascent_angle_max_flap[j,2,i] = 180/pi*atan2(Velocity_vect_in_max_flap[j,1:3,i][3], sqrt( (Velocity_vect_in_max_flap[j,1:3,i][1])^2 + (Velocity_vect_in_max_flap[j,1:3,i][2])^2))
  }
}

# plot variation in ascent angle
p=184
plot(ascent_angle[1:total_frames[p],1,p], ascent_angle[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('ascent angle'))
# add stroke positions
lines(ascent_angle[which(is.na(ds_coords[,1,p])==F),1,p], ascent_angle[which(is.na(ds_coords[,1,p])==F),2,p], col='red', typ='p')
lines(ascent_angle[which(is.na(us_coords[,1,p])==F),1,p], ascent_angle[which(is.na(us_coords[,1,p])==F),2,p], col='forestgreen', typ='p')
legend("topright", c('downstroke','upstroke'), pch=1, col=c('red','forestgreen'), bty='n')

# superimpose gliding phases
for (p in 1:200){
  plot(ascent_angle[1:total_frames[p],1,p], ascent_angle[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('ascent angle'))
  lines(ascent_angle[which(is.na(gliding[,3,p])==F),1,p], gliding[which(is.na(gliding[,3,p])==F),1,p], col='red', typ='p')
}

## plot ascent angle in max flap:
p=184
plot(ascent_angle_max_flap[1:total_frames_maxflap[p],1,p], ascent_angle_max_flap[1:total_frames_maxflap[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('ascent angle max flap'))



############################## Ascent angle  on ALL flapping phases #########################

flapping_phase_NB = as.numeric(glide_and_flap_data[,10])

## create an ID for all gliding phase
flapping_phase_ID = matrix(NA,sum(flapping_phase_NB),1)
for (i in 1:length(smooth_coords_files)){
  row_index_now = sum(flapping_phase_NB[1:i]) - (flapping_phase_NB[i]-1)
  range_index_now = row_index_now : (row_index_now + (flapping_phase_NB[i]-1) )
  for (g in 1:flapping_phase_NB[i]){flapping_phase_ID[range_index_now[g],1] =
    paste0(files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i],'_FLAP_',g) }
}
# flapping_phase_ID = flapping_phase_ID[-c(578),]# don't know why but there is an extra unwanted line that we remove

### IDs for gliding phases have been saved and can be directly loaded:
flapping_phase_IDs <- read.csv(paste0(local_path,'/all_flapping_phases_ID.csv'), h=T, sep=";")

## array to receive the 3D coords of all flapping phases
flap_coords_allphases = array(NA, dim = c(max(total_frames_maxflap), 8, sum(flapping_phase_NB)))
dimnames(flap_coords_allphases)[[2]] = dimnames(Flight_and_strokes2)[[2]]
dimnames(flap_coords_allphases)[[3]] = factor(flapping_phase_ID)

flapping_phase_IDs$flight_ID = factor(flapping_phase_IDs$flight_ID, levels = levels(files_ID$flight_ID))

for (i in 1:length(smooth_coords_files)){
  index_now = which(flapping_phase_IDs$species==files_ID$species[i] &
                      flapping_phase_IDs$specimen_ID==files_ID$specimen_ID[i] &
                      flapping_phase_IDs$flight_ID==files_ID$flight_ID[i])
  for (j in index_now){
    flap_coords_allphases[1:length(which(Flight_and_strokes2[,7,i]== paste0('flap',flapping_phase_IDs$flapping_phase_ID[j]))),,j] =
      Flight_and_strokes2[which(Flight_and_strokes2[,7,i]== paste0('flap',flapping_phase_IDs$flapping_phase_ID[j])),,i]}
}

## get frame number for all flapping phases
total_frames_all_flaps <- matrix(NA, nrow(flapping_phase_IDs), 1)
for (t in 1:nrow(flapping_phase_IDs)){
  total_frames_all_flaps[t] = length(which(is.na(flap_coords_allphases[,1,t])==F))}


### extracting the velocity vectors corresponding to all flapping phases
## array to receive the velocity vectors of all flapping phases
Velocity_vect_all_flap_phases = array(NA, dim = c(max(total_frames_maxflap), 4, sum(flapping_phase_NB)))
dimnames(Velocity_vect_all_flap_phases)[[2]] = dimnames(Flight_and_strokes2)[[2]][1:4]
dimnames(Velocity_vect_all_flap_phases)[[3]] = factor(flapping_phase_ID)

for (i in 1:length(smooth_coords_files)){
  index_now = which(flapping_phase_IDs$species==files_ID$species[i] &
                      flapping_phase_IDs$specimen_ID==files_ID$specimen_ID[i] &
                      flapping_phase_IDs$flight_ID==files_ID$flight_ID[i])
  for (j in index_now){
    Velocity_vect_all_flap_phases[1:length(which(Flight_and_strokes2[,7,i]== paste0('flap',flapping_phase_IDs$flapping_phase_ID[j]))),1:4,j] =
      Velocity_vect[which(Flight_and_strokes2[,7,i]== paste0('flap',flapping_phase_IDs$flapping_phase_ID[j])),1:4,i]}
}


# compute variation in ascent angle over all flapping phases
ascent_angle_all_flap_phase <- array(NA, dim= c((max(total_frames_all_flaps)), 2, sum(flapping_phase_NB)))
ascent_angle_all_flap_phase[,1,] = flap_coords_allphases[,1,] # add time in first column
for (i in 1:nrow(flapping_phase_IDs)){
  for (j in 1:total_frames_all_flaps[i]){
    ascent_angle_all_flap_phase[j,2,i] = 180/pi*atan2(Velocity_vect_all_flap_phases[j,2:4,i][3], sqrt( (Velocity_vect_all_flap_phases[j,2:4,i][1])^2 + (Velocity_vect_all_flap_phases[j,2:4,i][2])^2))
  }
}


# store the mean ascent angle from all flapping phases
flapping_parameters_allphases = as.data.frame(matrix(NA, nrow(flapping_phase_IDs),7))
colnames(flapping_parameters_allphases) <- c("species","specimen_ID","flight_ID","flapping_phase_ID",'flapping_duration_(s)','ascent_angle','ecology')
flapping_parameters_allphases[,1:4] = flapping_phase_IDs[,1:4]
flapping_parameters_allphases[,7] = flapping_phase_IDs$ecology

# computing the duration of flapping phase
for (i in 1:nrow(flapping_phase_IDs)){ 
  flapping_parameters_allphases[i,5] = as.numeric(flap_coords_allphases[last(which(is.na(flap_coords_allphases[,1,i])==F)),1,i]) - as.numeric(flap_coords_allphases[1,1,i])
}
# computing the mean ascent angle on flapping phases
for (i in 1:nrow(flapping_phase_IDs)){ 
  flapping_parameters_allphases[i,6] = mean(as.numeric(na.omit(ascent_angle_all_flap_phase[,2,i])))
}


##### plot ascent angle: C vs U
library(ggplot2)
ascent_angle_plot = ggplot(flapping_parameters_allphases, aes(x = ecology, y = ascent_angle, fill=ecology)) + 
  geom_boxplot(outlier.shape = NA) + scale_fill_manual(values=c("cornflowerblue", "chartreuse3")) +
  geom_jitter(position = position_jitter(0.15), cex = 1.2, color ='black') +
  labs(y = 'ascent angle (degree)', x='ecology')  + labs(fill = '') +
  ggtitle('') +
  theme_classic() + theme(legend.position = "none")
ascent_angle_plot

anova(lm(gliding_parameters_allphases$glide_angle ~ gliding_parameters_allphases$ecology))

mean(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='U')])
mean(gliding_parameters_allphases$glide_angle[which(gliding_parameters_allphases$ecology=='C')])







############################## g-force (F/mg) #########################

# Fmg = sqrt( ax.^2 + ay.^2 + (az+g).^2 )/g
# it is the net aerodynamic force produced by the butterfly (F), normalized by the weight of the animal (mg),
# defined as F/mg. Calculating net aerodynamic forces from accelerations
# is called "rigid-body inverse dynamics" analysis, and is simply based on newton's second law of motion (F=ma)
#
# if the animal does not accelerate (norm(A)=0), F/mg =1: the animal produces a net aerodynamic force
# equal to the weight of the animal (in order to stay up in the air).
# F/mg is sometimes also called g-force (which is 1 at zero acceleration).
# 
# if F/mg=2, the animal produces a net aerodynamic force equal to twice the weight of the animal,
# i.e. the animal produces a g-force of 2.

# computing g-force
g = 9.81
g_force = array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
g_force[,1,] = Accel_vect[,1,] # add time in first column
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames[i]){
    g_force[j,2,i] = sqrt( (Accel_vect[j,2:4,i][1])^2 + (Accel_vect[j,2:4,i][2])^2 + (Accel_vect[j,2:4,i][3]-g)^2)/g
  }
}

# computing g-force over max flap phase (it will not work if max flap vectors are not yet computed)
g_force_max_flap <- array(NA, dim= c((max(total_frames)), 2, length(smooth_coords_files)))
g_force_max_flap[,1,] = max_flap_coords[,1,] # add time in first column
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames_maxflap[i]){
    g_force_max_flap[j,2,i] = sqrt( (accel_vect_in_max_flap[j,1:3,i][1])^2 + (accel_vect_in_max_flap[j,1:3,i][2])^2 + (accel_vect_in_max_flap[j,1:3,i][3]-g)^2)/g
  }
}

# plot variation in g-force
p=184
plot(g_force[1:total_frames[p],1,p], g_force[1:total_frames[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('g-force'))
# add stroke positions
lines(g_force[which(is.na(ds_coords[,1,p])==F),1,p], g_force[which(is.na(ds_coords[,1,p])==F),2,p], col='red', typ='p')
lines(g_force[which(is.na(us_coords[,1,p])==F),1,p], g_force[which(is.na(us_coords[,1,p])==F),2,p], col='forestgreen', typ='p')
legend("topleft", c('downstroke','upstroke'), pch=1, col=c('red','forestgreen'), bty='n')

# plot variation in g-force max flap
plot(g_force_max_flap[1:total_frames_maxflap[p],1,p], g_force_max_flap[1:total_frames_maxflap[p],2,p], typ='l', xlab=c('time (s)'), ylab=c('g-force in max flap'))





########################### Flapping efficiency (Advance ratio) ###########################

# flapping efficiency can be estimated by computing the advance ratio
# defined as the ratio of the incoming air velocity to the wing flapping velocity
# the advance ratio is zero in hovering flight and increase with forward airspeed.
# see:
# Dahmaniet al. (2018). Effects of advance ratio on elytra-hindwing interaction in forward flying Coleopteran beetle.
# Spedding, G. R. (1993). On the significance of unsteady effects in the aerodynamic performance of flying animals. Contemp. Math. 141, 247-272
# Taylor 2003. Flying and swimming animals cruise at a Strouhal number tuned for high power efficiency

# advance ratio (J) is equal to the forward velocity (Vf) divided by the wing length * stroke amplitude * stroke frequency
# stroke amplitude can be approximate with the wing length ? (Msc thesis 2017)


# let's take the Advance ratio sensus Ellington (1984c, The Aerodynamics of Hovering Insect Flight. III. Kinematics)
# simply computed as the ratio of forward velocity to the flapping velocity (approximated by the wingbeat frequency)

#### advance ratio computed over the entire trajectory
advance_ratio <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  advance_ratio[i] = mean(na.omit(velo_inst[,,i])) / wb_frequency[i]
}

#### advance ratio computed only over the longest flapping phases (makes more sense!)

# extracting the velocity vectors corresponding to max flapping phases coordinates
Velocity_vect_in_max_flap = array(NA, dim = c((max(total_frames_maxflap)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,3])>0) ){
  Velocity_vect_in_max_flap[1:total_frames_maxflap[i],,i] = Velocity_vect[which(Flight_and_strokes2[,7,i] == max_flap_coords[1,7,i]) , 2:4,i]
}


# extracting the acceleration vectors corresponding to max flapping phases coordinates
accel_vect_in_max_flap = array(NA, dim = c((max(total_frames_maxflap)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,3])>0) ){
  accel_vect_in_max_flap[1:total_frames_maxflap[i],,i] = Accel_vect[which(Flight_and_strokes2[,7,i] == max_flap_coords[1,7,i]) , 2:4,i]
}


# instantaneous velocity during the longest flapping phase
velo_inst_in_max_flap = array(NA, dim= c((max(total_frames_maxflap)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames_maxflap[i]){
    velo_inst_in_max_flap[j,1,i] = norma(Velocity_vect_in_max_flap[j,1:3,i])
  }
}

advance_ratio_max_flap <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  advance_ratio_max_flap[i] =  mean(na.omit(velo_inst_in_max_flap[,,i])) / Micro_wb_frequency[i]  ## Micro_wb_frequency has been computed in the code 'AB_flight_trajectories_flap_vs_glide.R'
}


# ## save the flapping velocity vectors for F. Muijres
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(Velocity_vect_in_max_flap[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_velocity_vectors_max_flap/',ID_now,'.csv'), row.names = F)
# }
## save the flapping coordinates for F. Muijres
# dimnames(max_flap_coords)[[2]] = dimnames(Flight_and_strokes2)[[2]]
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(max_flap_coords[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_coordinates_max_flap/',ID_now,'.csv'), row.names = F)
# }
# ## save the flapping acceleration vectors for F. Muijres
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(accel_vect_in_max_flap[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_acceleration_vectors_max_flap/',ID_now,'.csv'), row.names = F)
# }



#### advance ratio normalized to size

# getting forewing length (flight levels)
FW_length = read.csv(paste0(local_path,'/relative_velocity_flights_level.csv'),h=T, sep=";")[,4]


## relative advance ratio, i.e. (flight speed / wing length) / wingbeat frequency
relative_advance_ratio <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  relative_advance_ratio[i] = ( mean(na.omit(velo_inst[,,i])) / FW_length[i]) / wb_frequency[i]
}

## relative advance ratio in max flap, i.e. (flight speed in max flap / wing length) / Micro wingbeat frequency
relative_advance_ratio_max_flap <- matrix(NA,length(smooth_coords_files), 1)
for (i in 1:length(smooth_coords_files)){
  relative_advance_ratio_max_flap[i] = ( mean(na.omit(velo_inst_in_max_flap[,,i])) / FW_length[i]) / Micro_wb_frequency[i]
}


# plot(relative_advance_ratio ~ relative_advance_ratio_max_flap)
# cor.test(relative_advance_ratio , relative_advance_ratio_max_flap)



##############################  Gliding efficiency (lift-to-drag ratio) longest gliding phase ############################## 

# LD ratio is computed from the velocity and acceleration vectors (according to Dario's python code: 'data_postprocessing_@damadori_08-19.py')
# this is done only over the longest gliding phases of each flight

# extracting the velocity vectors corresponding to max gliding phases coordinates
Velocity_vect_in_max_glide = array(NA, dim = c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,2])>0) ){
  Velocity_vect_in_max_glide[1:total_frames_maxglide[i],,i] = Velocity_vect[which(Flight_and_strokes2[,7,i] == max_glide_coords[1,7,i]) , 2:4,i]
}


# instantaneous velocity during the longest gliding phases
velo_inst_in_max_glide = array(NA, dim= c((max(total_frames_maxglide)), 1, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files) ){
  for (j in 1:total_frames_maxglide[i]){
    velo_inst_in_max_glide[j,1,i] = norma(Velocity_vect_in_max_glide[j,1:3,i])
  }
}

# computing the UNIT velocity vector in the longest gliding phases
UNIT_velo_vect_in_max_glide = array(NA, dim= c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames_maxglide[i]){
    UNIT_velo_vect_in_max_glide[j,,i] = as.numeric(Velocity_vect_in_max_glide[j,,i]) / velo_inst_in_max_glide[j,,i]
  }
}

# extracting the acceleration vectors corresponding to max gliding phases coordinates
accel_vect_in_max_glide = array(NA, dim = c((max(total_frames_maxglide)), 3, length(smooth_coords_files)))
for (i in which(as.numeric(phase_count[,2])>0) ){
  accel_vect_in_max_glide[1:total_frames_maxglide[i],,i] = Accel_vect[which(Flight_and_strokes2[,7,i] == max_glide_coords[1,7,i]) , 2:4,i]
}

# ## save the gliding velocity vectors for F. Muijres
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(Velocity_vect_in_max_glide[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_velocity_vectors_max_glide/',ID_now,'.csv'), row.names = F)
# }
# save the gliding coordinates for F. Muijres
# dimnames(max_glide_coords)[[2]] = dimnames(Flight_and_strokes2)[[2]]
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(max_glide_coords[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_coordinates_max_glide/',ID_now,'.csv'), row.names = F)
# }
# ## save the gliding acceleration vectors for F. Muijres
# for (i in 1:length(smooth_coords_files)){
#   ID_now = paste0(files_ID$order_in_scan_loop[i],'_',files_ID$species[i],'_',files_ID$specimen_ID[i],'_',files_ID$flight_ID[i])
#   write.csv(accel_vect_in_max_glide[,,i],paste0('/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/00_Digitized_Coordinates_only3D/kalmanFiltered_acceleration_vectors_max_glide/',ID_now,'.csv'), row.names = F)
# }

  
### first we try with one flight
# p=84
# n = total_frames_maxglide[p]
# 
# # different notation:
# #  Dario's            Mine
# velocityField = na.omit(Velocity_vect_in_max_glide[,,p])
# accelerationField = na.omit(accel_vect_in_max_glide[,,p])
# velocityVersor = na.omit(UNIT_velo_vect_in_max_glide[,,p])
# 
# ## defining given physical quantities
# mass = 0.0006 # why this value ??
# gravityField = matrix(NA, n, 3) ; for (i in 1:n){gravityField[i,] = c(0,0,-9.81)}
# F_aero = as.matrix(mass*(accelerationField - gravityField))
# 
# # all explicit computations
# drag = matrix(NA, n, 1)
# for (i in 1:n){drag[i] = velocityVersor[i,]  %*% F_aero[i,]}
# MeanDragExpl = mean(drag)
# 
# lift = matrix(NA, n, 1)
# for (i in 1:n){
#   lift[i] = sqrt( (F_aero[i,1] - drag[i]*velocityVersor[i,1])^2 + (F_aero[i,2] - drag[i]*velocityVersor[i,2])^2 + (F_aero[i,3] - drag[i]*velocityVersor[i,3])^2 )
# }
# MeanLiftExpl = mean(lift)
# 
# L_D_ratio = lift/abs(drag) 





## array to store variation in LD ratio for each glide:
LD_ratio_array = array(NA, dim= c((max(total_frames_maxglide)), 1, length(smooth_coords_files)))

## using the appropriated specimen mass  => it does not change anything when using the appropriated mass...
# mass_data = files_ID$tot_weight
mass_data = rep( mean(files_ID$tot_weight),nrow(files_ID))

## filling the L/D ratio array:
for (p in which(as.numeric(phase_count[,2])>0) ){
  
  n = total_frames_maxglide[p]
  
  velocityField = na.omit(Velocity_vect_in_max_glide[,,p])
  accelerationField = na.omit(accel_vect_in_max_glide[,,p])
  velocityVersor = na.omit(UNIT_velo_vect_in_max_glide[,,p])
  
  mass = as.numeric(mass_data[p]) / 1000     # /1000 to stay in kilogrammes
  
  gravityField = matrix(NA, n, 3) ; for (i in 1:n){gravityField[i,] = c(0,0,-9.81)}
  F_aero = as.matrix(mass*(accelerationField - gravityField))
  
  drag = matrix(NA, n, 1)
  for (i in 1:n){drag[i] = velocityVersor[i,]  %*% F_aero[i,]}
  
  lift = matrix(NA, n, 1)
  for (i in 1:n){
    lift[i] = sqrt( (F_aero[i,1] - drag[i]*velocityVersor[i,1])^2 + (F_aero[i,2] - drag[i]*velocityVersor[i,2])^2 + (F_aero[i,3] - drag[i]*velocityVersor[i,3])^2 )
  }
  
  LD_ratio_array[1:n,,p] = lift/abs(drag)
}

## L/D values higher than 10 are probably noise. This may be due to the influence of the preceding or upcoming wingbeat.
## we relpace all values > 10 with NA
for (i in which(as.numeric(phase_count[,2])>0)){
  for (j in 1:total_frames_maxglide[i]){
    if(LD_ratio_array[j,1,i]>10) {LD_ratio_array[j,1,i] = NA}
  }
}


lift_to_drag_ratio = matrix(NA, length(smooth_coords_files), 1)
colnames(lift_to_drag_ratio) = c('mean_LD_ratio')
for (i in which(as.numeric(phase_count[,2])>0)){
  lift_to_drag_ratio[i,1] = mean(na.omit(LD_ratio_array[,1,i]))
}
lift_to_drag_ratio[which(lift_to_drag_ratio=='NaN')] = NA





############################## Gliding efficiency (lift-to-drag ratio) ALL gliding phases ############################## 

## we first need to extract the velocity and acceleration vectors corresponding to all the gliding phases

## velocity vectors of all gliding phases
Velocity_vect_all_glides = array(NA, dim = c(max(total_frames_maxglide), 4, sum(gliding_phase_NB)))
dimnames(Velocity_vect_all_glides)[[2]] = c('time','U','V','W')
dimnames(Velocity_vect_all_glides)[[3]] = gliding_phase_ID

for (i in 1:length(smooth_coords_files)){
  index_now = which(gliding_phase_IDs$species==files_ID$species[i] &
                      gliding_phase_IDs$specimen_ID==files_ID$specimen_ID[i] &
                      gliding_phase_IDs$flight_ID==files_ID$flight_ID[i])
  for (j in index_now){
    Velocity_vect_all_glides[1:length(which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j]))),,j] =
      Velocity_vect[which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j])),,i]}
}

## intantaneous velocity in all gliding phases
velo_inst_all_glides = array(NA, dim = c(max(total_frames_maxglide), 1, sum(gliding_phase_NB)))
for (i in 1:nrow(gliding_phase_IDs) ){
  for (j in 1:total_frames_all_glides[i]){
    velo_inst_all_glides[j,1,i] = norma(Velocity_vect_all_glides[j,2:4,i])
  }
}

## computing the UNIT velocity vector in all gliding phases
UNIT_velocity_vect_all_glides = array(NA, dim = c(max(total_frames_maxglide), 4, sum(gliding_phase_NB)))
UNIT_velocity_vect_all_glides[,1,] = glide_coords_allphases[,1,] # add time in first column
for (i in 1:nrow(gliding_phase_IDs)){
  for (j in 1:total_frames_all_glides[i]){
    UNIT_velocity_vect_all_glides[j,2:4,i] = as.numeric(Velocity_vect_all_glides[j,2:4,i]) / velo_inst_all_glides[j,,i]
  }
}

## acceleration vectors of all gliding phases
Accel_vect_all_glides = array(NA, dim = c(max(total_frames_maxglide), 4, sum(gliding_phase_NB)))
dimnames(Accel_vect_all_glides)[[2]] = c('time','ax','ay','az')
dimnames(Accel_vect_all_glides)[[3]] = gliding_phase_ID

for (i in 1:length(smooth_coords_files)){
  index_now = which(gliding_phase_IDs$species==files_ID$species[i] &
                      gliding_phase_IDs$specimen_ID==files_ID$specimen_ID[i] &
                      gliding_phase_IDs$flight_ID==files_ID$flight_ID[i])
  for (j in index_now){
    Accel_vect_all_glides[1:length(which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j]))),,j] =
      Accel_vect[which(Flight_and_strokes2[,7,i]== paste0('glide',gliding_phase_IDs$gliding_phase_ID[j])),,i]}
}




## array to store variation in LD ratio for each glide:
LD_ratio_array = array(NA, dim= c((max(total_frames_maxglide)), 1, sum(gliding_phase_NB)))

## using the appropriated specimen mass  => it does not seems to change anything when using the appropriated mass...
mass_data = rep( mean(files_ID$tot_weight),nrow(gliding_phase_IDs))

## filling the L/D ratio array:
for (p in 1:nrow(gliding_phase_IDs)){
  
  n = total_frames_all_glides[p]
  
  velocityField = na.omit(Velocity_vect_all_glides[,2:4,p])
  accelerationField = na.omit(Accel_vect_all_glides[,2:4,p])
  velocityVersor = na.omit(UNIT_velocity_vect_all_glides[,2:4,p])
  
  mass = as.numeric(mass_data[p]) / 1000     # /1000 to stay in kilogrammes
  
  gravityField = matrix(NA, n, 3) ; for (i in 1:n){gravityField[i,] = c(0,0,-9.81)}
  F_aero = as.matrix(mass*(accelerationField - gravityField))
  
  drag = matrix(NA, n, 1)
  for (i in 1:n){drag[i] = as.numeric(velocityVersor[i,])  %*% F_aero[i,]}
  
  lift = matrix(NA, n, 1)
  for (i in 1:n){
    lift[i] = sqrt( (F_aero[i,1] - drag[i]*as.numeric(velocityVersor[i,1]))^2 + (F_aero[i,2] - drag[i]*as.numeric(velocityVersor[i,2]))^2 + (F_aero[i,3] - drag[i]*as.numeric(velocityVersor[i,3]))^2 )
  }
  
  LD_ratio_array[1:n,,p] = lift/abs(drag)
}


## L/D values higher than 10 are probably noise. This may be due to the influence of the preceding or upcoming wingbeat.
## we relpace all values > 10 with NA
for (i in 1:nrow(gliding_phase_IDs)){
  for (j in 1:total_frames_all_glides[i]){
    if(LD_ratio_array[j,1,i]>10) {LD_ratio_array[j,1,i] = NA}
  }
}

lift_to_drag_ratio_all_glides = matrix(NA, nrow(gliding_phase_IDs), 2)
colnames(lift_to_drag_ratio_all_glides) = c('mean_LD_ratio','peak_LD_ratio')
for (i in 1:nrow(gliding_phase_IDs)){
  lift_to_drag_ratio_all_glides[i,1] = mean(na.omit(LD_ratio_array[,1,i]))
  lift_to_drag_ratio_all_glides[i,2] = max(na.omit(LD_ratio_array[,1,i]))
}
lift_to_drag_ratio_all_glides[which(lift_to_drag_ratio_all_glides=='NaN')] = NA
lift_to_drag_ratio_all_glides[which(lift_to_drag_ratio_all_glides=='-Inf')] = NA


## compile with row IDs
lift_to_drag_ratio_all_glides = as.data.frame(cbind(gliding_phase_IDs,lift_to_drag_ratio_all_glides))


## in order to compre LD with glide angle, that was computed in the code 'AB_flight_trajectories_flap_vs_glide.R'
## we need to match the two table (because many not proper glides were removed in the 'gliding_parameters_allphases')

LD_to_be_matched = matrix(NA, nrow(gliding_parameters_allphases), 1)
for (i in 1:nrow(gliding_parameters_allphases)){
  LD_to_be_matched[i,1] = lift_to_drag_ratio_all_glides[
    which(lift_to_drag_ratio_all_glides$species==gliding_parameters_allphases$species[i] &
            lift_to_drag_ratio_all_glides$specimen_ID==gliding_parameters_allphases$specimen_ID[i] &
            lift_to_drag_ratio_all_glides$flight_ID==gliding_parameters_allphases$flight_ID[i] &
            lift_to_drag_ratio_all_glides$gliding_phase_ID==gliding_parameters_allphases$gliding_phase_ID[i])
    ,6]
}

gliding_parameters_allphases2 = as.data.frame(cbind(gliding_parameters_allphases, LD_to_be_matched))
colnames(gliding_parameters_allphases2)[11] = c('LD_ratio')

plot(gliding_parameters_allphases2$glide_angle ~ gliding_parameters_allphases2$LD_ratio, pch=16,
     col=as.numeric(gliding_parameters_allphases2$ecology), main='actual data')

gliding_parameters_allphases3= gliding_parameters_allphases2[-c(
  which(gliding_parameters_allphases2$glide_angle<10 & gliding_parameters_allphases2$LD_ratio<6)
),]

plot(gliding_parameters_allphases3$glide_angle ~ gliding_parameters_allphases3$LD_ratio, pch=16,
     col=as.numeric(gliding_parameters_allphases3$ecology), main='ballistic manoeuvers removed')









############################## Re-projection #########################


### Z axis vs combined-XY axes

Z_XY_coords = array(NA, dim= c(max(total_frames), 2, length(smooth_coords_files)))
colnames(Z_XY_coords) <-  c('sqrt(X?+Y?)','Z')

for (i in 1:length(smooth_coords_files)){
  Z_XY_coords[1:total_frames[i],2,i] = Flight_and_strokes2[1:total_frames[i],4,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Z_XY_coords[j,1,i] = sqrt( (as.numeric(Flight_and_strokes2[j,2,i]))^2 +
                                 (as.numeric(Flight_and_strokes2[j,3,i]))^2 )
  }
}

plot(Z_XY_coords[,,p], type='l',lwd=2, col="gray8", xlab=" sqrt(X?+Y?) ", ylab=" Z ", asp=T)
plot(Flight_and_strokes[,c(3,4),p], type='l',lwd=2, col="gray8", xlab=" Y ", ylab=" Z ", asp=T)


### X axis vs combined-YZ axes

X_YZ_coords = array(NA, dim= c(max(total_frames), 2, length(smooth_coords_files)))
colnames(X_YZ_coords) <-  c('X','sqrt(Y?+Z?)')

for (i in 1:length(smooth_coords_files)){
  X_YZ_coords[1:total_frames[i],1,i] = Flight_and_strokes2[1:total_frames[i],2,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    X_YZ_coords[j,2,i] = sqrt( (as.numeric(Flight_and_strokes2[j,3,i]))^2 +
                                 (as.numeric(Flight_and_strokes2[j,4,i]))^2 )
  }
}

plot(X_YZ_coords[,,p], type='l',lwd=2, col="gray8", xlab=" X ", ylab=" sqrt(Y?+Z?) ", asp=T)
plot(Flight_and_strokes[,c(2,4),p], type='l',lwd=2, col="gray8", xlab=" X ", ylab=" Z ", asp=T)



### Y axis vs combined-XZ axes

Y_XZ_coords = array(NA, dim= c(max(total_frames), 2, length(smooth_coords_files)))
colnames(Y_XZ_coords) <-  c('Y','sqrt(X?+Z?)')

for (i in 1:length(smooth_coords_files)){
  Y_XZ_coords[1:total_frames[i],1,i] = Flight_and_strokes2[1:total_frames[i],3,i]
}
for (i in 1:length(smooth_coords_files)){
  for (j in 1:total_frames[i]){
    Y_XZ_coords[j,2,i] = sqrt( (as.numeric(Flight_and_strokes2[j,2,i]))^2 +
                                 (as.numeric(Flight_and_strokes2[j,4,i]))^2 )
  }
}

plot(Y_XZ_coords[,,p], type='l',lwd=2, col="gray8", xlab=" Y ", ylab=" sqrt(X?+Z?) ", asp=T)
plot(Flight_and_strokes[,c(2,3),p], type='l',lwd=2, col="gray8", xlab=" X ", ylab=" Y ", asp=T)








############################## Storing all datas #########################

movement_data = matrix(NA, length(smooth_coords_files), 37)
movement_data[,1:4] = as.matrix(files_ID[,2:5])
colnames(movement_data) <-  c('specimen_ID','flight_ID','species','fps',
                              'covered_distance','sinuosity',
                              'mean_velocity','peak_velocity','sd_velocity',
                              'mean_acceleration','peak_acceleration','sd_acceleration',
                              'mean_turning_acceleration','peak_turning_acceleration','sd_turning_acceleration',
                              'mean_turning_acceleration_horiz','peak_turning_acceleration_horiz','sd_turning_acceleration_horiz',
                              'mean_curvature','peak_curvature','sd_curvature',
                              'mean_curvature_horiz','peak_curvature_horiz','sd_curvature_horiz',
                              'mean_gliding_curvature','peak_gliding_curvature','sd_gliding_curvature',
                              'mean_gliding_curvature_horiz','peak_gliding_curvature_horiz','sd_gliding_curvature_horiz',
                              'mean_ascent_angle','peak_ascent_angle','sd_ascent_angle',
                              'mean_g_force','peak_g_force','sd_g_force',
                              'mean_gliding_velocity')


movement_data[,5] = covered_dist
movement_data[,6] = sinuosity

for (i in 1:length(smooth_coords_files)){
  movement_data[i,7] = mean(na.omit(velo_inst[,,i]))
  movement_data[i,8] = max(na.omit(velo_inst[,,i]))
  movement_data[i,9] = sd(na.omit(velo_inst[,,i]))
  
  movement_data[i,10] = mean(na.omit(accel_inst[,,i]))
  movement_data[i,11] = max(na.omit(accel_inst[,,i]))
  movement_data[i,12] = sd(na.omit(accel_inst[,,i]))
  
  movement_data[i,13] = mean(na.omit(N_accel[,2,i]))
  movement_data[i,14] = max(na.omit(N_accel[,2,i]))
  movement_data[i,15] = sd(na.omit(N_accel[,2,i]))
  
  movement_data[i,16] = mean(na.omit(N_accel_horiz[,2,i]))
  movement_data[i,17] = max(na.omit(N_accel_horiz[,2,i]))
  movement_data[i,18] = sd(na.omit(N_accel_horiz[,2,i]))
  
  movement_data[i,19] = mean(na.omit(curvature[,2,i]))
  movement_data[i,20] = max(na.omit(curvature[,2,i]))
  movement_data[i,21] = sd(na.omit(curvature[,2,i]))
  
  movement_data[i,22] = mean(na.omit(curvature_horiz[,2,i]))
  movement_data[i,23] = max(na.omit(curvature_horiz[,2,i]))
  movement_data[i,24] = sd(na.omit(curvature_horiz[,2,i]))
  
  movement_data[i,25] = mean(na.omit(curvature_in_max_glide[,2,i]))
  movement_data[i,26] = max(na.omit(curvature_in_max_glide[,2,i]))
  movement_data[i,27] = sd(na.omit(curvature_in_max_glide[,2,i]))
  
  movement_data[i,28] = mean(na.omit(curvature_horiz_in_max_glide[,2,i]))
  movement_data[i,29] = max(na.omit(curvature_horiz_in_max_glide[,2,i]))
  movement_data[i,30] = sd(na.omit(curvature_horiz_in_max_glide[,2,i]))
  
  movement_data[i,31] = mean(na.omit(ascent_angle[,2,i]))
  movement_data[i,32] = max(na.omit(ascent_angle[,2,i]))
  movement_data[i,33] = sd(na.omit(ascent_angle[,2,i]))
  
  movement_data[i,34] = mean(na.omit(g_force[,2,i]))
  movement_data[i,35] = max(na.omit(g_force[,2,i]))
  movement_data[i,36] = sd(na.omit(g_force[,2,i]))
  
  movement_data[i,37] = mean(na.omit(velo_inst_in_max_glide[,1,i]))
}


movement_data[which(movement_data=='-Inf')] = NA
movement_data[which(movement_data=='NaN')] = NA

# View(movement_data)
# write.csv(movement_data,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/movement_data.csv", row.names=F)


## ad hoc sinuosity in the horizontal and vertical plane
# write.csv(sinuosity_horiz,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/sinuosity_horiz.csv", row.names=F)
# write.csv(sinuosity_verti,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/sinuosity_verti.csv", row.names=F)

## ad hoc advance ratio
# write.csv(advance_ratio,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/advance_ratio.csv", row.names=F)
# write.csv(advance_ratio_max_flap,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/advance_ratio_max_flap.csv", row.names=F)

## ad hoc lift-to-drag ratio
# write.csv(lift_to_drag_ratio,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/lift_to_drag_ratio.csv", row.names=F)

## ad hoc rate of change of heading
# write.csv(rate_change_heading,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/rate_change_heading.csv", row.names=F)

## ad hoc relative advance ratio and relative advance ratio in max flap
# write.csv(relative_advance_ratio,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_flight_levels.csv", row.names=F)
# write.csv(relative_advance_ratio_max_flap,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_max_flap_flight_levels.csv", row.names=F)

## ad hoc turning rate (as in Combes etal. 2012)
# write.csv(turning_rate_summarized,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/turning_rate_flights_levels.csv", row.names=F)

## final dataset
flightdata <- read.csv(paste0(local_path,'/flight_parameters_CL17_formatted.csv'),h=T,sep=";")

## ad hoc sinking speed, i.e the glide angle multiplied by gliding velocity (Ennos 1989)
# sinking_speed = matrix(NA, length(smooth_coords_files), 1)
# for (i in 1:length(smooth_coords_files)){
#   sinking_speed[i] = flightdata$glide_angle[i] * flightdata$mean_gliding_velocity[i]
# }
# write.csv(sinking_speed,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/sink_speed_flights_levels.csv", row.names=F)


## ad hoc ascent angle in max flap (flight levels)
# mean_ascent_angle_max_flap_per_flight = matrix(NA, length(smooth_coords_files), 1)
# for (i in 1:length(smooth_coords_files)){
#   mean_ascent_angle_max_flap_per_flight[i,1] = mean(na.omit(as.numeric(ascent_angle_max_flap[,2,i])))
# }
# write.csv(mean_ascent_angle_max_flap_per_flight,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/ascent_angle_max_flap_flights_levels.csv", row.names=F)


## ad hoc g-force in max flap (flight levels)
# mean_g_force_max_flap_per_flight = matrix(NA, length(smooth_coords_files), 1)
# for (i in 1:length(smooth_coords_files)){
#   mean_g_force_max_flap_per_flight[i,1] = mean(na.omit(as.numeric(g_force_max_flap[,2,i])))
# }
# write.csv(mean_g_force_max_flap_per_flight,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/g_force_max_flap_flights_levels.csv", row.names=F)



############################### Reducing dataset (mean/max per specimen) ######################################

## up to now, data contains 82 specimens, with ~ 3 flights per specimen (241 rows). Here we reduce data with the mean value for each specimen

## IDs
IDs_reduced_dataset <- read.csv(paste0(local_path,'/flight_parameters_reduced_IDs.csv'),h=T,sep=";")

flight_parameters_MEANvalues <- matrix(NA, nrow(IDs_reduced_dataset), 42)
flight_parameters_MEANvalues[,1:6] = as.matrix(IDs_reduced_dataset)
colnames(flight_parameters_MEANvalues) = c('filming_date','specimen_ID','species','ecology','pch_ecology','fps','seq_length','sinuosity','sinuosity_horiz',
                                          'mean_flight_height','max_flight_height','wb_frequency','mean_stroke_duration','max_stroke_duration',
                                          'gliding_phase_nb','gliding_prop','mean_glide_duration','max_glide_duration','glide_ratio','glide_angle',
                                          'flapping_phase_nb','flapping_prop','mean_flap_duration','max_flap_duration','Micro_wb_frequency',
                                          'mean_velocity','peak_velocity','sd_velocity','mean_acceleration','peak_acceleration','mean_turning_acceleration',
                                          'peak_turning_acceleration','mean_turning_acceleration_horiz','peak_turning_acceleration_horiz','mean_curvature',
                                          'mean_curvature_horiz','mean_ascent_angle','mean_g_force','advance_ratio','advance_ratio_max_flap')


species <- factor(IDs_reduced_dataset[,3])
specimen <- factor(IDs_reduced_dataset[,2])


flight_parameters_MEANvalues[,7] = tapply(flightdata$seq_length, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,8] = tapply(flightdata$sinuosity, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,9] = tapply(flightdata$sinuosity_horiz, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,10] = tapply(flightdata$mean_flight_height, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,11] = tapply(flightdata$max_flight_height, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,12] = tapply(flightdata$wb_frequency, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,13] = tapply(flightdata$mean_stroke_duration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,14] = tapply(flightdata$max_stroke_duration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,15] = tapply(flightdata$gliding_phase_nb, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,16] = tapply(flightdata$gliding_prop, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,17] = tapply(flightdata$mean_glide_duration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,18] = tapply(flightdata$max_glide_duration, flightdata$specimen_ID, mean)

flight_parameters_MEANvalues[,19] = matrix(NA, nrow(flight_parameters_MEANvalues), 1) ## trick to handle NA in glide_ratio
for (i in 1:length(specimen)){
  flight_parameters_MEANvalues[i,19] = mean(na.omit(flightdata$glide_ratio[which(flightdata$specimen_ID == specimen[i])]))
} ; flight_parameters_MEANvalues[,19][which(flight_parameters_MEANvalues[,19]=='NaN')] = NA

flight_parameters_MEANvalues[,20] = matrix(NA, nrow(flight_parameters_MEANvalues), 1) ## trick to handle NA in glide_angle
for (i in 1:length(specimen)){
  flight_parameters_MEANvalues[i,20] = mean(na.omit(flightdata$glide_angle[which(flightdata$specimen_ID == specimen[i])]))
} ; flight_parameters_MEANvalues[,20][which(flight_parameters_MEANvalues[,20]=='NaN')] = NA

flight_parameters_MEANvalues[,21] = tapply(flightdata$flapping_phase_nb, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,22] = tapply(flightdata$flapping_prop, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,23] = tapply(flightdata$mean_flap_duration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,24] = tapply(flightdata$max_flap_duration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,25] = tapply(flightdata$Micro_wb_frequency, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,26] = tapply(flightdata$mean_velocity, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,27] = tapply(flightdata$peak_velocity, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,28] = tapply(flightdata$sd_velocity, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,29] = tapply(flightdata$mean_acceleration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,30] = tapply(flightdata$peak_acceleration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,31] = tapply(flightdata$mean_turning_acceleration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,32] = tapply(flightdata$peak_turning_acceleration, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,33] = tapply(flightdata$mean_turning_acceleration_horiz, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,34] = tapply(flightdata$peak_turning_acceleration_horiz, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,35] = tapply(flightdata$mean_curvature, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,36] = tapply(flightdata$mean_curvature_horiz, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,37] = tapply(flightdata$mean_ascent_angle, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,38] = tapply(flightdata$mean_g_force, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,39] = tapply(flightdata$advance_ratio, flightdata$specimen_ID, mean)
flight_parameters_MEANvalues[,40] = tapply(flightdata$advance_ratio_max_flap, flightdata$specimen_ID, mean)


# View(flight_parameters_MEANvalues)

# write.csv(flight_parameters_MEANvalues,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/flight_CL17_parameters_MEANvalues.csv", row.names=F)


### ad hoc: extract minimum value for the glide performance parameters
glide_ratio_min_values= matrix(NA, length(specimen), 1)
for (i in 1:length(specimen)){
  glide_ratio_min_values[i,1] = min(na.omit(flightdata$glide_ratio[which(flightdata$specimen_ID == specimen[i])]))
} ; glide_ratio_min_values[which(glide_ratio_min_values=='Inf')] = NA ## WARNING ARE EXPECTED

glide_angle_min_values= matrix(NA, length(specimen), 1)
for (i in 1:length(specimen)){
  glide_angle_min_values[i,1] = min(na.omit(flightdata$glide_angle[which(flightdata$specimen_ID == specimen[i])]))
} ; glide_angle_min_values[which(glide_angle_min_values=='Inf')] = NA ## WARNING ARE EXPECTED

gliding_parameters_min_values_specimen_level = as.data.frame(matrix(NA, length(specimen), 4))
colnames(gliding_parameters_min_values_specimen_level) = c('specimen_ID', 'species', 'glide_ratio_min_values', 'glide_angle_min_values')
gliding_parameters_min_values_specimen_level[,1:2] = flight_parameters_MEANvalues[,c(8,9)]
gliding_parameters_min_values_specimen_level[,3] = glide_ratio_min_values
gliding_parameters_min_values_specimen_level[,4] = glide_angle_min_values

# write.csv(gliding_parameters_min_values_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/glide_angle&ratio_min_values_specimen_level.csv", row.names=F)


## ad hoc: lift-to-drag ratio (mean an max per specimen)
LD_ratio_specimen_level= matrix(NA, length(specimen), 2)
colnames(LD_ratio_specimen_level) = c('mean_LD_ratio', 'peak_LD_ratio')
for (i in 1:length(specimen)){
  LD_ratio_specimen_level[i,1] = mean(na.omit(flightdata$mean_LD_ratio[which(flightdata$specimen_ID == specimen[i])]))
  LD_ratio_specimen_level[i,2] = max(na.omit(flightdata$mean_LD_ratio[which(flightdata$specimen_ID == specimen[i])]))
}  ## WARNING ARE EXPECTED
LD_ratio_specimen_level[which(LD_ratio_specimen_level=='NaN'),] = NA 
LD_ratio_specimen_level[which(LD_ratio_specimen_level=='Inf'),] = NA 
# write.csv(LD_ratio_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/lift_to_drag_ratio_specimen_level.csv", row.names=F)

## ad hoc: rate of change of heading (mean per specimen)
rate_change_heading_specimen_level = matrix(NA, length(specimen), 1)
for (i in 1:length(specimen)){
  rate_change_heading_specimen_level[i] = mean(flightdata$rate_change_heading[which(flightdata$specimen_ID == specimen[i])])
} 
# write.csv(rate_change_heading_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/rate_change_heading_specimen_level.csv", row.names=F)


## ad hoc: relative advance ratio (mean per specimen)
relative_advance_ratio_specimen_level = matrix(NA, length(specimen), 1)
for (i in 1:length(specimen)){
  relative_advance_ratio_specimen_level[i] = mean(flightdata$relative_advance_ratio[which(flightdata$specimen_ID == specimen[i])])
} 
# write.csv(relative_advance_ratio_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_specimen_level.csv", row.names=F)


## ad hoc: relative advance ratio in max flap (mean per specimen)
relative_advance_ratio_maxflap_specimen_level = matrix(NA, length(specimen), 1)
for (i in 1:length(specimen)){
  relative_advance_ratio_maxflap_specimen_level[i] = mean(flightdata$relative_advance_ratio_max_flap[which(flightdata$specimen_ID == specimen[i])])
} 
# write.csv(relative_advance_ratio_maxflap_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_maxflap_specimen_level.csv", row.names=F)


## ad hoc: turning_rate (mean and max per specimen)
turning_rate_specimen_level = matrix(NA, length(specimen), 2)
colnames(turning_rate_specimen_level) = c('mean_turning_rate', 'peak_turning_rate')
for (i in 1:length(specimen)){
  turning_rate_specimen_level[i,1] = mean(flightdata$mean_turning_rate[which(flightdata$specimen_ID == specimen[i])])
  turning_rate_specimen_level[i,2] = mean(flightdata$peak_turning_rate[which(flightdata$specimen_ID == specimen[i])])
} 
# write.csv(turning_rate_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/turning_rate_specimen_level.csv", row.names=F)

## ad hoc sink speed (mean per specimen)
sink_speed_specimen_level = matrix(NA, length(specimen), 1)
colnames(sink_speed_specimen_level) = c('sink_speed')
for (i in 1:length(specimen)){
  sink_speed_specimen_level[i,1] = mean(na.omit(flightdata$sink_speed[which(flightdata$specimen_ID == specimen[i])]))
} 
sink_speed_specimen_level[which(sink_speed_specimen_level=='NaN'),] = NA 
# write.csv(sink_speed_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/sink_speed_specimen_level.csv", row.names=F)

## ad hoc ascent angle max flap (mean per specimen)
ascent_angle_max_flap_specimen_level = matrix(NA, length(specimen), 1)
colnames(ascent_angle_max_flap_specimen_level) = c('ascent_angle_max_flap')
for (i in 1:length(specimen)){
  ascent_angle_max_flap_specimen_level[i,1] = mean(na.omit(flightdata$ascent_angle_max_flap[which(flightdata$specimen_ID == specimen[i])]))
} 
# write.csv(ascent_angle_max_flap_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/ascent_angle_max_flap_specimen_level.csv", row.names=F)

## ad hoc g-force max flap (mean per specimen)
g_force_max_flap_specimen_level = matrix(NA, length(specimen), 1)
colnames(g_force_max_flap_specimen_level) = c('g_force')
for (i in 1:length(specimen)){
  g_force_max_flap_specimen_level[i,1] = mean(na.omit(flightdata$g_force_max_flap[which(flightdata$specimen_ID == specimen[i])]))
}
# write.csv(g_force_max_flap_specimen_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/g_force_max_flap_specimen_level.csv", row.names=F)






############################### Reducing dataset (mean/max per species) ######################################

# up to now, the mean values dataset contains 82 specimens. Here we reduce it to one row per species (we have 11 species)

flight_parameters_MEANvalues <- read.csv(paste0(local_path,'/flight_parameters_CL17_MEANvalues_formatted.csv'), sep=';')
species <- factor(flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "species")])
specimen_ID <- factor(flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "specimen_ID")])

## correcting for the back-and-forth trajectory creating outlier in sinuosity
for (i in which(flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "sinuosity")] > 1.8)){ flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "sinuosity")][i] = NA }
for (i in which(flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "sinuosity_horiz")] > 1.8)){ flight_parameters_MEANvalues[,which(colnames(flight_parameters_MEANvalues) == "sinuosity_horiz")][i] = NA }

## removing theseus 079 because it has no photo and no morphological data:
flight_parameters_MEANvalues = flight_parameters_MEANvalues[-c(which(flight_parameters_MEANvalues$specimen_ID == 79)),]

## load wing area
wing_area <- read.csv(paste0(substr(local_path,1,71), '/08-Wing Digitizing/wing_area.csv'),h=T, sep=";")

## compute wing loading
tot_wing_area = matrix(NA, nrow(wing_area), 1)
for (i in 1:nrow(wing_area)){
  tot_wing_area[i] = sum(wing_area[i,3], wing_area[i,4]) # sum the area of fore and hindwings
}
plot(tot_wing_area ~ flight_parameters_MEANvalues$species)
wing_loading = matrix(NA, nrow(wing_area), 1)
for (i in 1:nrow(wing_area)){
  wing_loading[i] = flight_parameters_MEANvalues$total_weight[i] / tot_wing_area[i]
}
plot(wing_loading ~ flight_parameters_MEANvalues$species)




flight_parameters_BySpecies <- as.data.frame(matrix(NA, 11, 44))

colnames(flight_parameters_BySpecies) = c('species','ecology','pch_ecology','mean_total_weight','mean_thorax_weight','mean_abdomen_weight','mean_wing_loading','mean_seq_length','mean_sinuosity','mean_sinuosity_horiz',
                                           'mean_flight_height','max_flight_height','mean_wb_frequency','mean_stroke_duration','max_stroke_duration',
                                           'mean_gliding_phase_nb','mean_gliding_prop','mean_glide_duration','max_glide_duration','mean_glide_ratio','mean_glide_angle',
                                           'mean_flapping_phase_nb','mean_flapping_prop','mean_flap_duration','max_flap_duration','mean_Micro_wb_frequency',
                                           'mean_velocity','peak_velocity','sd_velocity','mean_acceleration','peak_acceleration','mean_turning_acceleration',
                                           'peak_turning_acceleration','mean_turning_acceleration_horiz','peak_turning_acceleration_horiz','mean_curvature',
                                           'mean_curvature_horiz','mean_ascent_angle','mean_g_force','mean_advance_ratio','mean_advance_ratio_max_flap',
                                           'mean_wing_area','glide_ratio_min_values','glide_angle_min_values')


flight_parameters_BySpecies[,1] = levels(flight_parameters_MEANvalues$species)
flight_parameters_BySpecies[,2] = c('U','U','C','U','U','U','U','U','C','U','C')
flight_parameters_BySpecies[,3] = c(16,16,1,16,16,16,16,16,1,16,1)
flight_parameters_BySpecies[,4] = tapply(flight_parameters_MEANvalues$total_weight, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,5] = tapply(flight_parameters_MEANvalues$thorax_weight[which(is.na(flight_parameters_MEANvalues$thorax_weight)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$thorax_weight)==F)], mean)
flight_parameters_BySpecies[,6] = tapply(flight_parameters_MEANvalues$abdomen_weight[which(is.na(flight_parameters_MEANvalues$abdomen_weight)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$abdomen_weight)==F)], mean)
flight_parameters_BySpecies[,7] = tapply(wing_loading, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,8] = tapply(flight_parameters_MEANvalues$seq_length, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,9] = tapply(flight_parameters_MEANvalues$sinuosity[which(is.na(flight_parameters_MEANvalues$sinuosity)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$sinuosity)==F)], mean)
flight_parameters_BySpecies[,10] = tapply(flight_parameters_MEANvalues$sinuosity_horiz[which(is.na(flight_parameters_MEANvalues$sinuosity_horiz)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$sinuosity_horiz)==F)], mean)
flight_parameters_BySpecies[,11] = tapply(flight_parameters_MEANvalues$mean_flight_height, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,12] = tapply(flight_parameters_MEANvalues$max_flight_height, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,13] = tapply(flight_parameters_MEANvalues$wb_frequency, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,14] = tapply(flight_parameters_MEANvalues$mean_stroke_duration, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,15] = tapply(flight_parameters_MEANvalues$max_stroke_duration, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,16] = tapply(flight_parameters_MEANvalues$gliding_phase_nb, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,17] = tapply(flight_parameters_MEANvalues$gliding_prop, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,18] = tapply(flight_parameters_MEANvalues$mean_glide_duration, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,19] = tapply(flight_parameters_MEANvalues$max_glide_duration, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,20] = tapply(flight_parameters_MEANvalues$glide_ratio[which(is.na(flight_parameters_MEANvalues$glide_ratio)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$glide_ratio)==F)], mean)
flight_parameters_BySpecies[,21] = tapply(flight_parameters_MEANvalues$glide_angle[which(is.na(flight_parameters_MEANvalues$glide_angle)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$glide_angle)==F)], mean)
flight_parameters_BySpecies[,22] = tapply(flight_parameters_MEANvalues$flapping_phase_nb, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,23] = tapply(flight_parameters_MEANvalues$flapping_prop, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,24] = tapply(flight_parameters_MEANvalues$mean_flap_duration, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,25] = tapply(flight_parameters_MEANvalues$max_flap_duration, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,26] = tapply(flight_parameters_MEANvalues$Micro_wb_frequency, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,27] = tapply(flight_parameters_MEANvalues$mean_velocity, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,28] = tapply(flight_parameters_MEANvalues$peak_velocity, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,29] = tapply(flight_parameters_MEANvalues$sd_velocity, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,30] = tapply(flight_parameters_MEANvalues$mean_acceleration, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,31] = tapply(flight_parameters_MEANvalues$peak_acceleration, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,32] = tapply(flight_parameters_MEANvalues$mean_turning_acceleration, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,33] = tapply(flight_parameters_MEANvalues$peak_turning_acceleration, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,34] = tapply(flight_parameters_MEANvalues$mean_turning_acceleration_horiz, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,35] = tapply(flight_parameters_MEANvalues$peak_turning_acceleration_horiz, flight_parameters_MEANvalues$species, max)
flight_parameters_BySpecies[,36] = tapply(flight_parameters_MEANvalues$mean_curvature, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,37] = tapply(flight_parameters_MEANvalues$mean_curvature_horiz, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,38] = tapply(flight_parameters_MEANvalues$mean_ascent_angle, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,39] = tapply(flight_parameters_MEANvalues$mean_g_force, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,40] = tapply(flight_parameters_MEANvalues$advance_ratio, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,41] = tapply(flight_parameters_MEANvalues$advance_ratio_max_flap, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,42] = tapply(tot_wing_area, flight_parameters_MEANvalues$species, mean)
flight_parameters_BySpecies[,43] = tapply(flight_parameters_MEANvalues$glide_ratio_min_values[which(is.na(flight_parameters_MEANvalues$glide_ratio_min_values)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$glide_ratio_min_values)==F)], mean)
flight_parameters_BySpecies[,44] = tapply(flight_parameters_MEANvalues$glide_angle_min_values[which(is.na(flight_parameters_MEANvalues$glide_angle_min_values)==F)], flight_parameters_MEANvalues$species[which(is.na(flight_parameters_MEANvalues$glide_angle_min_values)==F)], mean)

# write.csv(flight_parameters_BySpecies,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/flight_CL17_parameters_MEANvaluesBySpecies.csv", row.names=F)





## ad hoc: mean lift-to-drag ratio per species
LD_ratio_species_level = matrix(NA, length(levels(species)), 1)
for (i in 1:length(levels(species))){
  LD_ratio_species_level[i,1] = mean(na.omit(flight_parameters_MEANvalues$mean_LD_ratio[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
}
# write.csv(LD_ratio_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/lift_to_drag_species_level.csv", row.names=F)


## ad hoc: mean lift-to-drag ratio per species
LD_ratio_species_level = matrix(NA, length(levels(species)), 1)
for (i in 1:length(levels(species))){
  LD_ratio_species_level[i,1] = mean(na.omit(flight_parameters_MEANvalues$mean_LD_ratio[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
}

## ad hoc: rate of change of heading per species
rate_change_heading_species_level = matrix(NA, length(levels(species)), 1)
for (i in 1:length(levels(species))){
  rate_change_heading_species_level[i] = mean(na.omit(flight_parameters_MEANvalues$rate_change_heading[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
}
# write.csv(rate_change_heading_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/rate_change_heading_species_level.csv", row.names=F)


## ad hoc: relative_advance_ratio per species
relative_advance_ratio_species_level = matrix(NA, length(levels(species)), 1)
for (i in 1:length(levels(species))){
  relative_advance_ratio_species_level[i] = mean(na.omit(flight_parameters_MEANvalues$relative_advance_ratio[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
}
# write.csv(relative_advance_ratio_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_species_level.csv", row.names=F)

## ad hoc: relative_advance_ratio_maxflap per species
relative_advance_ratio_maxflap_species_level = matrix(NA, length(levels(species)), 1)
for (i in 1:length(levels(species))){
  relative_advance_ratio_maxflap_species_level[i] = mean(na.omit(flight_parameters_MEANvalues$relative_advance_ratio_max_flap[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
}
# write.csv(relative_advance_ratio_maxflap_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/relative_advance_ratio_max_flap_species_level.csv", row.names=F)

## ad hoc: turning_rate (mean and max per species)
turning_rate_species_level = matrix(NA, length(levels(species)), 2)
colnames(turning_rate_species_level) = c('mean_turning_rate', 'peak_turning_rate')
for (i in 1:length(levels(species))){
  turning_rate_species_level[i,1] = mean(flight_parameters_MEANvalues$mean_turning_rate[which(flight_parameters_MEANvalues$species == levels(species)[i])])
  turning_rate_species_level[i,2] = mean(flight_parameters_MEANvalues$peak_turning_rate[which(flight_parameters_MEANvalues$species == levels(species)[i])])
} 
# write.csv(turning_rate_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/turning_rate_species_level.csv", row.names=F)

## ad hoc sink speed (mean per species)
sink_speed_species_level = matrix(NA, length(levels(species)), 1)
colnames(sink_speed_species_level) = c('sink_speed')
for (i in 1:length(levels(species))){
  sink_speed_species_level[i,1] = mean(na.omit(flight_parameters_MEANvalues$sink_speed[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
} 
# write.csv(sink_speed_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/sink_speed_species_level.csv", row.names=F)


## ad hoc ascent angle max flap (mean per species)
ascent_angle_max_flap_species_level = matrix(NA, length(levels(species)), 1)
colnames(ascent_angle_max_flap_species_level) = c('ascent_angle')
for (i in 1:length(levels(species))){
  ascent_angle_max_flap_species_level[i,1] = mean(na.omit(flight_parameters_MEANvalues$ascent_angle_max_flap[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
} 
# write.csv(ascent_angle_max_flap_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/ascent_angle_max_flap_species_level.csv", row.names=F)

## ad hoc g_force max flap (mean per species)
g_force_max_flap_species_level = matrix(NA, length(levels(species)), 1)
colnames(g_force_max_flap_species_level) = c('g_force')
for (i in 1:length(levels(species))){
  g_force_max_flap_species_level[i,1] = mean(na.omit(flight_parameters_MEANvalues$g_force_max_flap[which(flight_parameters_MEANvalues$species == levels(species)[i])]))
} 
# write.csv(g_force_max_flap_species_level,"/Users/camille_le_roy/OneDrive - Wageningen University & Research/PhD_Camille/05-Flight Reconstruction/g_force_max_flap_species_level.csv", row.names=F)



