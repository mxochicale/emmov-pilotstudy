###############################################################################	
#
#  preprocessed data for razor IMU sensors 
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:@gmail.com
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
source('~/mxochicale/github/r-code_repository/functions/ollin_cencah.R')




################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()

setwd("../../../")
main_repository_path <- getwd()
setwd("../")
github_path <- getwd()


main_data_path <- paste( main_repository_path, '/data/razor_imu',sep="")
outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
relativeplotpath <- "/plots_timeseries/of-postprocessing"
relativeodatapath <- "/datatables"

odatapath <- paste( outcomes_path, relativeodatapath, sep="" )


################################################################################
# (1) Setting DataSets paths and reading data


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
#	'lfsgAccX', 'lfsgAccY', 'lfsgAccZ',
#	'lfsgGyroX', 'lfsgGyroY', 'lfsgGyroZ',
#	'lfsgzmuvYaw', 'lfsgzmuvPitch', 'lfsgzmuvRoll',
#	'lfsgzmuvAccX', 'lfsgzmuvAccY', 'lfsgzmuvAccZ',
#	'lfsgzmuvGyroX', 'lfsgzmuvGyroY', 'lfsgzmuvGyroZ',
#	'lfsgpc1_AG', 'lfsgpc2_AG', 'lfsgpc3_AG'
#	)
#:=lapply(
#	.(
#	sgYaw, sgPitch, sgRoll,
#	sgAccX, sgAccY, sgAccZ,
#	sgGyroX, sgGyroY, sgGyroZ,
#	sgzmuvYaw, sgzmuvPitch, sgzmuvRoll,
#	sgzmuvAccX, sgzmuvAccY, sgzmuvAccZ,
#	sgzmuvGyroX, sgzmuvGyroY, sgzmuvGyroZ,
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
#xdata[, hfsgAccX := sgAccX - lfsgAccX]
#xdata[, hfsgAccY := sgAccY - lfsgAccY]
#xdata[, hfsgAccZ := sgAccZ - lfsgAccZ]
#
#xdata[, hfsgGyroX := sgGyroX - lfsgGyroX]
#xdata[, hfsgGyroY := sgGyroY - lfsgGyroY]
#xdata[, hfsgGyroZ := sgGyroZ - lfsgGyroZ]
#
#xdata[, hfsgzmuvYaw := sgzmuvYaw - lfsgzmuvYaw]
#xdata[, hfsgzmuvPitch := sgzmuvPitch - lfsgzmuvPitch]
#xdata[, hfsgzmuvRoll := sgzmuvRoll - lfsgzmuvRoll]
#
#xdata[, hfsgzmuvAccX := sgzmuvAccX - lfsgzmuvAccX]
#xdata[, hfsgzmuvAccY := sgzmuvAccY - lfsgzmuvAccY]
#xdata[, hfsgzmuvAccZ := sgzmuvAccZ - lfsgzmuvAccZ]
#
#xdata[, hfsgzmuvGyroX := sgzmuvGyroX - lfsgzmuvGyroX]
#xdata[, hfsgzmuvGyroY := sgzmuvGyroY - lfsgzmuvGyroY]
#xdata[, hfsgzmuvGyroZ := sgzmuvGyroZ - lfsgzmuvGyroZ]
#
#
#
#################################################################################
#### (3.5) Smoothing data with hf sg zmuv
####
#SavitzkyGolayCoeffs <- sgolay(p=5,n=155 ,m=0)
#
#### FUNCTON TO SMOOTH THE DATA
#SGolay <- function(xinput,sgCoeffs){
#  output <- filter(sgCoeffs, xinput)
#  return(output)
#}
#
#
#xdata[,c(
#	'sghfsgzmuvAccX', 'sghfsgzmuvAccY', 'sghfsgzmuvAccZ',
#	'sghfsgzmuvGyroX', 'sghfsgzmuvGyroY', 'sghfsgzmuvGyroZ'
#	) 
#:=lapply(
#	.(
#	hfsgzmuvAccX,  hfsgzmuvAccY, hfsgzmuvAccZ,
#	hfsgzmuvGyroX, hfsgzmuvGyroY, hfsgzmuvGyroZ
#	), 
#	function(x) ( SGolay(x,SavitzkyGolayCoeffs)  ))
#	]
#
#
#
#
#
#
#
#
#
################################################################################
# (4) Plotting
#

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
plot_path <- paste(outcomes_path,relativeplotpath,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



#####################
## (4.2) Plotting data


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=pc1_gaze, col='pc1_gaze'), size=plotlinewidth)+
	geom_line( aes(y=pc2_gaze, col='pc2_gaze'), size=plotlinewidth)+
	geom_line( aes(y=pc3_gaze, col='pc3_gaze'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Three principal components for gaze_0_x_y_z gaze_1_x_y_z ') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_PCgaze.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=pc1_pose, col='pc1_pose'), size=plotlinewidth)+
	geom_line( aes(y=pc2_pose, col='pc2_pose'), size=plotlinewidth)+
	geom_line( aes(y=pc3_pose, col='pc3_pose'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Three principal components for pose_T_x_y_z pose_R_x_y_z ') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_PCpose.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#'sgzmuvpc1_gaze', 'sgzmuvpc2_gaze', 'sgzmuvpc3_gaze', 
#	'sgzmuvpc1_pose', 'sgzmuvpc2_pose', 'sgzmuvpc3_pose', 
#	'sgzmuvpc1_lm', 'sgzmuvpc2_lm', 'sgzmuvpc3_lm' 



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvpc1_gaze, col='sgzmuvpc1_gaze'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc2_gaze, col='sgzmuvpc2_gaze'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc3_gaze, col='sgzmuvpc3_gaze'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance, PC') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_sgzmuvPCgaze.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvpc1_pose, col='sgzmuvpc1_pose'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc2_pose, col='sgzmuvpc2_pose'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc3_pose, col='sgzmuvpc3_pose'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance, PC') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_sgzmuvPCpose.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvpc1_xlm, col='sgzmuvpc1_xlm'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc2_xlm, col='sgzmuvpc2_xlm'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc3_xlm, col='sgzmuvpc3_xlm'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance, PC') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_sgzmuvPCxlm.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvpc1_ylm, col='sgzmuvpc1_ylm'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc2_ylm, col='sgzmuvpc2_ylm'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvpc3_ylm, col='sgzmuvpc3_ylm'), size=plotlinewidth)+

	facet_grid(participant~.)+
	#scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance, PC') + 
	xlab('Sample')+
	labs(colour = 'pc')


png(filename= paste(tag,"_sgzmuvPCylm.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgconfidence, col='sgconfidence'), size=plotlinewidth)+
	geom_line( aes(y=sgsuccess, col='sgsuccess'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_base.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sggaze_0_x, col='sggaze_0_x'), size=plotlinewidth)+
	geom_line( aes(y=sggaze_0_y, col='sggaze_0_y'), size=plotlinewidth)+
	#geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
	geom_line( aes(y=sggaze_1_x, col='sggaze_1_x'), size=plotlinewidth)+
	geom_line( aes(y=sggaze_1_y, col='sggaze_1_y'), size=plotlinewidth)+
	#geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+


	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sggaze_xy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
	geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+


	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sggaze_z.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgpose_Tx, col='sgpose_Tx'), size=plotlinewidth)+
	geom_line( aes(y=sgpose_Ty, col='sgpose_Ty'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgposeTxy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgpose_Tz, col='sgpose_Tz'), size=plotlinewidth)+
	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgposeTz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgpose_Rx, col='sgpose_Rx'), size=plotlinewidth)+
	geom_line( aes(y=sgpose_Ry, col='sgpose_Ry'), size=plotlinewidth)+
	geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay  ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgposeRxyz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




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



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvgaze_0_x, col='sgzmuvgaze_0_x'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvgaze_0_y, col='sgzmuvgaze_0_y'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvgaze_0_z, col='sgzmuvgaze_0_z'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvgaze_1_x, col='sgzmuvgaze_1_x'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvgaze_1_y, col='sgzmuvgaze_1_y'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvgaze_1_z, col='sgzmuvgaze_1_z'), size=plotlinewidth)+


	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvgaze.png",sep=''),
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


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvx_0, col='sgzmuvx_0'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvx_1, col='sgzmuvx_1'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvx_2, col='sgzmuvx_2'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvx012.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvy_0, col='sgzmuvy_0'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvy_1, col='sgzmuvy_1'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvy_2, col='sgzmuvy_2'), size=plotlinewidth)+

	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvy012.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=sgzmuvp_scale, col='sgzmuvp_scale'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvp_rx, col='sgzmuvp_rx'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvp_ry, col='sgzmuvp_ry'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvp_rz, col='sgzmuvp_rz'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvp_tx, col='sgzmuvp_tx'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvp_ty, col='sgzmuvp_ty'), size=plotlinewidth)+


	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvshape.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





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


