###############################################################################	
#
#  preprocessed data for openface 
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:perez.xochicale@gmail.com
# please email me directly if you see any errors or have any questions
# last update: 21 February 2018
#
###############################################################################	
	# OUTLINE:
	# (0) Loading libraries and functions
 	# (1) Definifing paths and Reading data
	# (2) Data Filtering
		# (2.1) Windowing
	# (3) Postprocessing
		# (3.0) Principal Component Analysis
		# (3.1) Zero Mean Unit Variance
		# (3.2) Savitzky-Golay Filter
		# (3.3) Creating Low Frequency Components
		# (3.4) Creating High Frequency Components
		# (3.5) Smoothing data with hf sg zmuv
	# (4) Plotting
		# (4.0) Plot features
		# (4.1) Creating and changing plot path
	# (5) Creating path to write postprocessed data




#################
# Start the clock!
start.time <- Sys.time()


################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 

library(signal)# for butterworth filter and sgolay
source('~/mxochicale/github/R/functions/ollin_cencah.R')




################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()

setwd("../../../")
main_repository_path <- getwd()
setwd("../")
github_path <- getwd()


main_data_path <- paste( main_repository_path, '/data/razor_imu',sep="")
outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
relativeplotpathtimeseries <- "/utde/openface/timeseries"
relativeodatapath <- "/datatables"

relativeplotpath4utde_openface_ed <- "/utde/openface/euclideandistances"
utde_sensor_path <- paste(outcomes_path,'/utde/openface',sep='')

################################################################################
# (1) Setting DataSets paths and reading data


odatapath <- paste( outcomes_path, relativeodatapath, sep="" )
setwd(odatapath)
datatable <- fread("rawopenfacedata-v00.datatable", header=TRUE)








################################################################################
# (2) Data Filtering


################################
### (2.1) Windowing Data [xdata[,.SD[1:2],by=.(Participant,Activity,Sensor)]]

#windowframe = 0:1500;
#windowframe = 400:1000;
windowframe = 500:1000;
xdata <- datatable[,.SD[windowframe],by=.(participant,trial)];





################################################################################
# (3) Postprocessing

featurenames <- names(xdata[,5:18])
#> names(xdata[,5:18])
# [1] "confidence" "success"    "gaze_0_x"   "gaze_0_y"   "gaze_0_z"  
# [6] "gaze_1_x"   "gaze_1_y"   "gaze_1_z"   "pose_Tx"    "pose_Ty"   
#[11] "pose_Tz"    "pose_Rx"    "pose_Ry"    "pose_Rz"   

gazeFnames <- names(xdata[,7:12])

poseFnames <- names(xdata[,13:18])




################################################################################
### (3.0) Principal Components
###

# principal components
#> names(xdata[,7:12]) #[1] "gaze_0_x" "gaze_0_y" "gaze_0_z" "gaze_1_x" "gaze_1_y" "gaze_1_z"
pc <- prcomp(  xdata[, 7:12 ] , center = T, scale. = T)
xdata[, `:=`(pc1_gaze, pc$x[, 1])]
xdata[, `:=`(pc2_gaze, pc$x[, 2])]
xdata[, `:=`(pc3_gaze, pc$x[, 3])]

#>names(xdata[,13:18]) #[1] "pose_Tx" "pose_Ty" "pose_Tz" "pose_Rx" "pose_Ry" "pose_Rz"
pc <- prcomp(  xdata[, 13:18 ] , center = T, scale. = T)
xdata[, `:=`(pc1_pose, pc$x[, 1])]
xdata[, `:=`(pc2_pose, pc$x[, 2])]
xdata[, `:=`(pc3_pose, pc$x[, 3])]



#> names(xdata[,19:86])
# [1] "x_0"  "x_1"  "x_2"  "x_3"  "x_4"  "x_5"  "x_6"  "x_7"  "x_8"  "x_9" 
#[11] "x_10" "x_11" "x_12" "x_13" "x_14" "x_15" "x_16" "x_17" "x_18" "x_19"
#[21] "x_20" "x_21" "x_22" "x_23" "x_24" "x_25" "x_26" "x_27" "x_28" "x_29"
#[31] "x_30" "x_31" "x_32" "x_33" "x_34" "x_35" "x_36" "x_37" "x_38" "x_39"
#[41] "x_40" "x_41" "x_42" "x_43" "x_44" "x_45" "x_46" "x_47" "x_48" "x_49"
#[51] "x_50" "x_51" "x_52" "x_53" "x_54" "x_55" "x_56" "x_57" "x_58" "x_59"
#[61] "x_60" "x_61" "x_62" "x_63" "x_64" "x_65" "x_66" "x_67"
pc <- prcomp(  xdata[, 19:86 ] , center = T, scale. = T)
xdata[, `:=`(pc1_xlm, pc$x[, 1])]
xdata[, `:=`(pc2_xlm, pc$x[, 2])]
xdata[, `:=`(pc3_xlm, pc$x[, 3])]



#> names(xdata[,87:154])
# [1] "y_0"  "y_1"  "y_2"  "y_3"  "y_4"  "y_5"  "y_6"  "y_7"  "y_8"  "y_9" 
#[11] "y_10" "y_11" "y_12" "y_13" "y_14" "y_15" "y_16" "y_17" "y_18" "y_19"
#[21] "y_20" "y_21" "y_22" "y_23" "y_24" "y_25" "y_26" "y_27" "y_28" "y_29"
#[31] "y_30" "y_31" "y_32" "y_33" "y_34" "y_35" "y_36" "y_37" "y_38" "y_39"
#[41] "y_40" "y_41" "y_42" "y_43" "y_44" "y_45" "y_46" "y_47" "y_48" "y_49"
#[51] "y_50" "y_51" "y_52" "y_53" "y_54" "y_55" "y_56" "y_57" "y_58" "y_59"
#[61] "y_60" "y_61" "y_62" "y_63" "y_64" "y_65" "y_66" "y_67"
pc <- prcomp(  xdata[, 87:155 ] , center = T, scale. = T)
xdata[, `:=`(pc1_ylm, pc$x[, 1])]
xdata[, `:=`(pc2_ylm, pc$x[, 2])]
xdata[, `:=`(pc3_ylm, pc$x[, 3])]





################################################################################
### (3.1) Zero mean and unit Variance
###
xdata[,c(
	'zmuvconfidence', 'zmuvsuccess',
	'zmuvgaze_0_x', 'zmuvgaze_0_y', 'zmuvgaze_0_z', 'zmuvgaze_1_x', 'zmuvgaze_1_y', 'zmuvgaze_1_z',
	'zmuvpose_Tx', 'zmuvpose_Ty', 'zmuvpose_Tz', 'zmuvpose_Rx', 'zmuvpose_Ry', 'zmuvpose_Rz',
	'zmuvx_0', 'zmuvx_1', 'zmuvx_2',
	'zmuvy_0', 'zmuvy_1', 'zmuvy_2',
	'zmuvp_scale', 'zmuvp_rx', 'zmuvp_ry', 'zmuvp_rz', 'zmuvp_tx', 'zmuvp_ty',
	'zmuvpc1_gaze', 'zmuvpc2_gaze', 'zmuvpc3_gaze', 
	'zmuvpc1_pose', 'zmuvpc2_pose', 'zmuvpc3_pose', 
	'zmuvpc1_xlm', 'zmuvpc2_xlm', 'zmuvpc3_xlm',
	'zmuvpc1_ylm', 'zmuvpc2_ylm', 'zmuvpc3_ylm' 
	) :=
       lapply(.(
	confidence, success,
	gaze_0_x, gaze_0_y, gaze_0_z, gaze_1_x, gaze_1_y, gaze_1_z,
	pose_Tx, pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz,
	x_0, x_1, x_2,
	y_0, y_1, y_2,
	p_scale, p_rx, p_ry, p_rz, p_tx, p_ty,
	pc1_gaze, pc2_gaze, pc3_gaze, 
	pc1_pose, pc2_pose, pc3_pose, 
	pc1_xlm, pc2_xlm, pc3_xlm,
	pc1_ylm, pc2_ylm, pc3_ylm 
	), function(x) ( zeromean_unitvariance(x)  ) )
	]




################################################################################
### (3.2) Smoothing data with Savitzky-Golay Filter
###
SavitzkyGolayCoeffs <- sgolay(p=5,n=155 ,m=0)

### FUNCTON TO SMOOTH THE DATA
SGolay <- function(xinput,sgCoeffs){
  output <- filter(sgCoeffs, xinput)
  return(output)
}


xdata[,c(
	'sgconfidence', 'sgsuccess',
	'sggaze_0_x', 'sggaze_0_y', 'sggaze_0_z', 'sggaze_1_x', 'sggaze_1_y', 'sggaze_1_z',
	'sgpose_Tx', 'sgpose_Ty', 'sgpose_Tz', 'sgpose_Rx', 'sgpose_Ry', 'sgpose_Rz',
	'sgzmuvconfidence', 'sgzmuvsuccess',
	'sgzmuvgaze_0_x', 'sgzmuvgaze_0_y', 'sgzmuvgaze_0_z', 'sgzmuvgaze_1_x', 'sgzmuvgaze_1_y', 'sgzmuvgaze_1_z',
	'sgzmuvpose_Tx', 'sgzmuvpose_Ty', 'sgzmuvpose_Tz', 'sgzmuvpose_Rx', 'sgzmuvpose_Ry', 'sgzmuvpose_Rz',
	'sgzmuvx_0', 'sgzmuvx_1', 'sgzmuvx_2',
	'sgzmuvy_0', 'sgzmuvy_1', 'sgzmuvy_2',
	'sgzmuvp_scale', 'sgzmuvp_rx', 'sgzmuvp_ry', 'sgzmuvp_rz', 'sgzmuvp_tx', 'sgzmuvp_ty',
	'sgzmuvpc1_gaze', 'sgzmuvpc2_gaze', 'sgzmuvpc3_gaze', 
	'sgzmuvpc1_pose', 'sgzmuvpc2_pose', 'sgzmuvpc3_pose', 
	'sgzmuvpc1_xlm', 'sgzmuvpc2_xlm', 'sgzmuvpc3_xlm',
	'sgzmuvpc1_ylm', 'sgzmuvpc2_ylm', 'sgzmuvpc3_ylm' 
	) 
:=lapply(
	.(
	confidence, success,
	gaze_0_x, gaze_0_y, gaze_0_z, gaze_1_x, gaze_1_y, gaze_1_z,
	pose_Tx, pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz,
	zmuvconfidence, zmuvsuccess,
	zmuvgaze_0_x, zmuvgaze_0_y, zmuvgaze_0_z, zmuvgaze_1_x, zmuvgaze_1_y, zmuvgaze_1_z,
	zmuvpose_Tx, zmuvpose_Ty, zmuvpose_Tz, zmuvpose_Rx, zmuvpose_Ry, zmuvpose_Rz,
	zmuvx_0, zmuvx_1, zmuvx_2,
	zmuvy_0, zmuvy_1, zmuvy_2,
	zmuvp_scale, zmuvp_rx, zmuvp_ry, zmuvp_rz, zmuvp_tx, zmuvp_ty,
	zmuvpc1_gaze, zmuvpc2_gaze, zmuvpc3_gaze, 
	zmuvpc1_pose, zmuvpc2_pose, zmuvpc3_pose, 
	zmuvpc1_xlm, zmuvpc2_xlm, zmuvpc3_xlm,
	zmuvpc1_ylm, zmuvpc2_ylm, zmuvpc3_ylm
	), 
	function(x) ( SGolay(x,SavitzkyGolayCoeffs)  ))
	]



#
#
#################################################################################
#### (3.3) Creating Low Frequency Components 
####
#cutoffHZ <- 6
#sampleHz <- 50
#nyqHZ = sampleHz/2 #nyquist
#f <- butter(9, cutoffHZ/nyqHZ)
##create lowfreq components
#xdata[,c(
#	'lfsgYaw', 'lfsgPitch', 'lfsgRoll',
#	'lfsgpc1_AG', 'lfsgpc2_AG', 'lfsgpc3_AG'
#	)
#:=lapply(
#	.(
#	sgYaw, sgPitch, sgRoll,
#	sgpc1_AG, sgpc2_AG, sgpc3_AG
#	),
#	function(x) ( filtfilt(f, x)) )
#	]
#
#
#
#
#
#
#################################################################################
#### (3.4) Creating Hight Frequency Components
####
#
#xdata[, hfsgYaw := sgYaw - lfsgYaw]
#xdata[, hfsgPitch := sgPitch - lfsgPitch]
#xdata[, hfsgRoll := sgRoll - lfsgRoll]
#
#xdata[, hfsgzmuvGyroX := sgzmuvGyroX - lfsgzmuvGyroX]
#xdata[, hfsgzmuvGyroY := sgzmuvGyroY - lfsgzmuvGyroY]
#xdata[, hfsgzmuvGyroZ := sgzmuvGyroZ - lfsgzmuvGyroZ]
#
#
#














#
################################################################################
# (4) Plotting
#


PLOTTINGTIMESERIES <- FALSE


if (PLOTTINGTIMESERIES == TRUE) 
{

#################
# (4.0) Plots Features
tag <- 'pre-of-timeseries'
image_width <- 2500
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"

plotlinewidth <- 1


################################################################################
# (4.1) Creating  and Changing to PlotPath
plot_path <- paste(outcomes_path,relativeplotpathtimeseries,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



#####################
## (4.2) Plotting data


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvconfidence, col='zmuvconfidence'), size=plotlinewidth)+
	geom_line( aes(y=zmuvsuccess, col='zmuvsuccess'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZMUV data ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_zmuvbase.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgconfidence, col='sgconfidence'), size=plotlinewidth)+
#	geom_line( aes(y=sgsuccess, col='sgsuccess'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgbase.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#

plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvconfidence, col='sgzmuvconfidence'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvsuccess, col='sgzmuvsuccess'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvbase.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=pose_Tx, col='pose_Tx'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Ty, col='pose_Ty'), size=plotlinewidth)+
#	#geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Pose Position Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseTxTy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	#geom_line( aes(y=pose_Tx, col='pose_Tx'), size=plotlinewidth)+
#	#geom_line( aes(y=pose_Ty, col='pose_Ty'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Pose Position Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseTz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Tx, col='zmuvpose_Tx'), size=plotlinewidth)+
	geom_line( aes(y=zmuvpose_Ty, col='zmuvpose_Ty'), size=plotlinewidth)+
	geom_line( aes(y=zmuvpose_Tz, col='zmuvpose_Tz'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Raw Pose Position Data ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_zmuvposeT.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvpose_Tx, col='sgzmuvpose_Tx'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpose_Ty, col='sgzmuvpose_Ty'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpose_Tz, col='sgzmuvpose_Tz'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvposeT.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Tx, col='sgpose_Tx'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Ty, col='sgpose_Ty'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeTxy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Tz, col='sgpose_Tz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeTz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#



#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=pose_Rx, col='pose_Rx'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Ry, col='pose_Ry'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Rz, col='pose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Poste Rotation Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseR.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#v
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Rx, col='sgpose_Rx'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Ry, col='sgpose_Ry'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeRxyz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#

plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Rx, col='zmuvpose_Rx'), size=plotlinewidth)+
	geom_line( aes(y=zmuvpose_Ry, col='zmuvpose_Ry'), size=plotlinewidth)+
	geom_line( aes(y=zmuvpose_Rz, col='zmuvpose_Rz'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_zmuvposeR.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=frame))+  
geom_line( aes(y=sgzmuvpose_Rx, col='sgzmuvpose_Rx'), size=plotlinewidth)+
geom_line( aes(y=sgzmuvpose_Ry, col='sgzmuvpose_Ry'), size=plotlinewidth)+
geom_line( aes(y=sgzmuvpose_Rz, col='sgzmuvpose_Rz'), size=plotlinewidth)+

facet_grid(participant~.)+
coord_cartesian(xlim=NULL, ylim=NULL  )+
ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
xlab('Sample')+
labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvposeR.png",sep=''),
width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sggaze_0_x, col='sggaze_0_x'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_0_y, col='sggaze_0_y'), size=plotlinewidth)+
#	#geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_x, col='sggaze_1_x'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_y, col='sggaze_1_y'), size=plotlinewidth)+
#	#geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sggaze_xy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#

#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sggaze_z.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#



#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgzmuvgaze_0_x, col='sgzmuvgaze_0_x'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_0_y, col='sgzmuvgaze_0_y'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_0_z, col='sgzmuvgaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_x, col='sgzmuvgaze_1_x'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_y, col='sgzmuvgaze_1_y'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_z, col='sgzmuvgaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgzmuvgaze.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#





} # if PLOTTINGTIMESERIES












################################################################################
################################################################################
# (4) TIME_DELAY EMBEDDING
################################################################################
################################################################################


UTDE <- FALSE

if (UTDE == TRUE) {





#### Embedding Creating Preprossede Data Path
embedding_path <- paste(utde_sensor_path,"/embeddings",sep="")
if (file.exists(embedding_path)){
setwd(file.path(embedding_path))
} else {
dir.create(embedding_path, recursive=TRUE)
setwd(file.path(embedding_path))
}






pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')
axis <- c('zmuvconfidence', 'zmuvsuccess', 'sgzmuvconfidence', 'sgzmuvsuccess', 'zmuvpose_Rx', 'zmuvpose_Ry', 'zmuvpose_Rz', 'sgzmuvpose_Rx', 'sgzmuvpose_Ry', 'sgzmuvpose_Rz', 'zmuvpose_Tx', 'zmuvpose_Ty', 'zmuvpose_Tz', 'sgzmuvpose_Tx', 'sgzmuvpose_Ty', 'sgzmuvpose_Tz')

xd <- xdata[,.(zmuvconfidence, zmuvsuccess, sgzmuvconfidence, sgzmuvsuccess, zmuvpose_Rx, zmuvpose_Ry, zmuvpose_Rz, sgzmuvpose_Rx, sgzmuvpose_Ry, sgzmuvpose_Rz, zmuvpose_Tx, zmuvpose_Ty, zmuvpose_Tz, sgzmuvpose_Tx, sgzmuvpose_Ty, sgzmuvpose_Tz), by=. (participant,trial,frame)]


ED <- NULL # Euclidean Distances data.table object!
for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]






#time_lags_p <- NULL
ED_a<-NULL
for (axis_k in c(1:length(axis))){ #for (axis_k in c(1:12)){


	message('#### axis:' , axis[axis_k])
	inputtimeseries <- xdp[,  get(axis[axis_k]) ]




      #### Embedding Creating Preprossede Data Path
      axis_embedding_path <- paste(embedding_path, '/',axis[axis_k],sep="")
      if (file.exists(axis_embedding_path)){
      setwd(file.path(axis_embedding_path))
      } else {
      dir.create(axis_embedding_path, recursive=TRUE)
      setwd(file.path(axis_embedding_path))
      }




################################################################################
################################################################################
## buildTakens

### Computed Embedding parameters:  m=7 tau=4
# delays <- tau
# dimensions <- dimension



#delays <- c(2)
#dimensions <- c(10)

#delays <- c(2,8)
#dimensions <- c(10, 100)

delays <- c(2,5,10)
dimensions <- c(10,25,50)

#delays <- c(4,5,6,7,8,9,10)
#dimensions <- c(3,5,7,10,20,30,40,50,60,70,80,90,100)


################################################################################

ed_dimtau_dta <-NULL
for (dim_i in (1:500)[dimensions]){
    
	ed_tau_dta <- NULL
	for (tau_j in (1:500)[delays]){


#      #### Embedding Creating Preprossede Data Path
#      TAU_embedding_path <- paste(axis_embedding_path,"/tau_", formatC(tau_j,width=2,flag='0'),sep="")
#      if (file.exists(TAU_embedding_path)){
#      setwd(file.path(TAU_embedding_path))
#      } else {
#      dir.create(TAU_embedding_path, recursive=TRUE)
#      setwd(file.path(TAU_embedding_path))
#      }



message('Embedding parameters:  m=',dim_i,' tau=',d=tau_j)



#autde <- buildTakens(hinputtimeseries,embedding.dim= dim_i, time.lag= tau_j)
#X<-bT[,]

a_utde <- Takens_Theorem(inputtimeseries, dim_i, tau_j, 1)

a_rss <- PCA( a_utde ,0)





image_width =  1000#2000 #3508 #595 #877
image_height = 1000#700#2480 #842 #620

imagefilename <- paste('pc_rotateddata_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', '.png', sep='')
png(filename=imagefilename, width=image_width, height=image_height, units="px", bg="white")
plotRSS2D_rotateddata(a_rss,05)
dev.off()





## Euclidean Distances
ed_dta <- as.data.table(euclidean.distances_rotateddata(a_rss))
fun <- function(x) {list( axis[axis_k] )}
ed_dta[,c('axis'):= fun(), ]





#
#image_width_p3d =  2000#2000 #3508 #595 #877
#image_height_p3d = 500#700#2480 #842 #620
#[[
##imagefilename <- paste('utde_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', sensors[sensor_k], '.jpeg', sep='')
##jpeg(filename=imagefilename, width=image_width_p3d, height=image_height_p3d, units="px", bg="white")
#
#
#imagefilename <- paste('utde_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', sensors[sensor_k], '.png', sep='')
#png(filename=imagefilename, width=image_width_p3d, height=image_height_p3d, units="px", bg="white")
#
#plotRSS3D2D(a_rss)
#dev.off()
#




fun <- function(x) {list(  formatC(tau_j, width=3, flag='0')   )}
ed_dta[,c('tau'):= fun(), ]


ed_tau_dta <- rbind(ed_tau_dta,ed_dta)


}##for (tau_j in (1:500)[delays]){

fun <- function(x) {list(  formatC(dim_i, width=3, flag='0')   )}
ed_tau_dta[,c('dim'):= fun(), ]

ed_dimtau_dta <- rbind(ed_dimtau_dta, ed_tau_dta)

}##for (dim_i in (1:500)[dimensions]){


#fun <- function(x) {list( tau_j )}
#ed_a[,c('axis'):= fun(), ]






ED_a <- rbind(ED_a, ed_dimtau_dta)




}###for (axis_k in c(1:12)){ #for (axis_k in c(1:12)){



 # Particpant Number

if (participants_k == 1){
fsNNtmp <-function(x) {list("p01")}
} else if (participants_k == 2){
fsNNtmp <-function(x) {list("p02")}
} else if (participants_k == 3){
fsNNtmp <-function(x) {list("p03")}
} else if (participants_k == 4){
fsNNtmp <-function(x) {list("p04")}
} else if (participants_k == 5){
fsNNtmp <-function(x) {list("p05")}
} else if (participants_k == 6){
fsNNtmp <-function(x) {list("p06")}
} else if (participants_k == 7){
fsNNtmp <-function(x) {list("p07")}
} 

ED_a[,c("participant"):=fsNNtmp(), ]
ED <- rbind(ED, ED_a)





} ###for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {






names(ED) <- gsub("V1", "EuclideanDistances", names(ED))
setcolorder( ED, c(5,2,3,4,1) )





plot_path <- paste(outcomes_path,relativeplotpath4utde_openface_ed,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}






for (pd_k in c(1:length(delays))) {

tau_value <-  formatC(delays[pd_k],width=3,flag='0')
dED <- ED[tau== tau_value, .SDcols=cols  ]


pbox <- ggplot(dED, aes(x=participant, y=EuclideanDistances) )+
    	geom_point(aes(fill=participant),
                alpha=0.9,
                size=0.5,
                shape=21,
                position=position_jitter(width=0.25, height=0)  )+
       	geom_boxplot(lwd=0.5,outlier.colour=NA, fill=NA)+ 
	facet_grid(dim~axis)+
	labs(x= "Participant", y="")+
        theme_bw(15)+
        theme(panel.grid.minor= element_blank(),
               panel.border=element_rect(color="black"),
               legend.position="none")+
        coord_cartesian( ylim=c(0,20) )+
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"),
              axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
              axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain")
              )



## Setting up plots_path

### Save Picture
width = 2000
height = 1000
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi



filenameimage <- paste("edhumans_", 'tau_', tau_value, ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	pbox)


} ###for (pd_k in c(1:length(delays))) {






} #### if (UTDE == TRUE) {





#
##
##################################################################################
### (5) Creating Preprossed Data Path and Writing Data
##
##odata_path <- paste(outcomes_path,relativeodatapath,sep="")
##if (file.exists(odata_path)){
##    setwd(file.path(odata_path))
##} else {
##  dir.create(odata_path, recursive=TRUE)
##  setwd(file.path(odata_path))
##}
##
##
##
##
##################################################################################
######  (5)  Writing Data
##write.table(xdata, "rawimudata-v00.datatable", row.name=FALSE)
##
##message('datatable file has been created at '  )
##message (odata_path)
##
##
#

#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


