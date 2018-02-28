###############################################################################	
#
#  preprocessed data for razor IMU sensors 
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
relativeplotpath <- "/plots_timeseries/razor/utde"
relativeodatapath <- "/datatables"

odatapath <- paste( outcomes_path, relativeodatapath, sep="" )






################################################################################
# (1) Setting DataSets paths and reading data


setwd(odatapath)

#datatable <- fread("rawimudata-v00.datatable", header=TRUE)
datatable <- fread("semialigned-rawimudata-v00.datatable", header=TRUE)








################################################################################
# (2) Data Filtering


################################
### (2.1) Windowing Data [xdata[,.SD[1:2],by=.(Participant,Activity,Sensor)]]

#windowframe = 400:1000;
windowframe = 100:1100;
xdata <- datatable[,.SD[windowframe],by=.(participant,trial,sensor)];





################################################################################
# (3) Postprocessing


################################################################################
### (3.0) Principal Components
###

# principal components
pc <- prcomp(  xdata[, .(AccX, AccY, AccZ) ] , center = T, scale. = T)
xdata[, `:=`(pc1_Acc, pc$x[, 1])]
xdata[, `:=`(pc2_Acc, pc$x[, 2])]
xdata[, `:=`(pc3_Acc, pc$x[, 3])]

# principal components
pc <- prcomp(  xdata[, .(GyroX, GyroY, GyroZ) ] , center = T, scale. = T)
xdata[, `:=`(pc1_Gyro, pc$x[, 1])]
xdata[, `:=`(pc2_Gyro, pc$x[, 2])]
xdata[, `:=`(pc3_Gyro, pc$x[, 3])]

# principal components
pc <- prcomp(  xdata[, .(AccX, AccY, AccZ, GyroX, GyroY, GyroZ) ] , center = T, scale. = T)
xdata[, `:=`(pc1_AG, pc$x[, 1])]
xdata[, `:=`(pc2_AG, pc$x[, 2])]
xdata[, `:=`(pc3_AG, pc$x[, 3])]




################################################################################
### (3.1) Zero mean and unit Variance
###
xdata[,c(
	'zmuvYaw', 'zmuvPitch', 'zmuvRoll',
	'zmuvAccX', 'zmuvAccY', 'zmuvAccZ',
	'zmuvGyroX', 'zmuvGyroY', 'zmuvGyroZ'
	) :=
       lapply(.(
	Yaw, Pitch, Roll, AccX, AccY, AccZ, GyroX, GyroY, GyroZ
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
	'sgYaw', 'sgPitch', 'sgRoll',
	'sgAccX', 'sgAccY', 'sgAccZ',
	'sgGyroX', 'sgGyroY', 'sgGyroZ',
	'sgzmuvYaw', 'sgzmuvPitch', 'sgzmuvRoll',
	'sgzmuvAccX', 'sgzmuvAccY', 'sgzmuvAccZ',
	'sgzmuvGyroX', 'sgzmuvGyroY', 'sgzmuvGyroZ',
	'sgpc1_AG', 'sgpc2_AG', 'sgpc3_AG'
	) 
:=lapply(
	.(
	Yaw, Pitch, Roll, AccX, AccY, AccZ, GyroX, GyroY, GyroZ,
	zmuvYaw, zmuvPitch, zmuvRoll,
	zmuvAccX, zmuvAccY, zmuvAccZ,
	zmuvGyroX, zmuvGyroY, zmuvGyroZ,
	pc1_AG, pc2_AG, pc3_AG
	), 
	function(x) ( SGolay(x,SavitzkyGolayCoeffs)  ))
	]





################################################################################
### (3.3) Creating Low Frequency Components 
###
cutoffHZ <- 6
sampleHz <- 50
nyqHZ = sampleHz/2 #nyquist
f <- butter(9, cutoffHZ/nyqHZ)
#create lowfreq components
xdata[,c(
	'lfsgYaw', 'lfsgPitch', 'lfsgRoll',
	'lfsgAccX', 'lfsgAccY', 'lfsgAccZ',
	'lfsgGyroX', 'lfsgGyroY', 'lfsgGyroZ',
	'lfsgzmuvYaw', 'lfsgzmuvPitch', 'lfsgzmuvRoll',
	'lfsgzmuvAccX', 'lfsgzmuvAccY', 'lfsgzmuvAccZ',
	'lfsgzmuvGyroX', 'lfsgzmuvGyroY', 'lfsgzmuvGyroZ',
	'lfsgpc1_AG', 'lfsgpc2_AG', 'lfsgpc3_AG'
	)
:=lapply(
	.(
	sgYaw, sgPitch, sgRoll,
	sgAccX, sgAccY, sgAccZ,
	sgGyroX, sgGyroY, sgGyroZ,
	sgzmuvYaw, sgzmuvPitch, sgzmuvRoll,
	sgzmuvAccX, sgzmuvAccY, sgzmuvAccZ,
	sgzmuvGyroX, sgzmuvGyroY, sgzmuvGyroZ,
	sgpc1_AG, sgpc2_AG, sgpc3_AG
	),
	function(x) ( filtfilt(f, x)) )
	]






################################################################################
### (3.4) Creating Hight Frequency Components
###

xdata[, hfsgYaw := sgYaw - lfsgYaw]
xdata[, hfsgPitch := sgPitch - lfsgPitch]
xdata[, hfsgRoll := sgRoll - lfsgRoll]

xdata[, hfsgAccX := sgAccX - lfsgAccX]
xdata[, hfsgAccY := sgAccY - lfsgAccY]
xdata[, hfsgAccZ := sgAccZ - lfsgAccZ]

xdata[, hfsgGyroX := sgGyroX - lfsgGyroX]
xdata[, hfsgGyroY := sgGyroY - lfsgGyroY]
xdata[, hfsgGyroZ := sgGyroZ - lfsgGyroZ]

xdata[, hfsgzmuvYaw := sgzmuvYaw - lfsgzmuvYaw]
xdata[, hfsgzmuvPitch := sgzmuvPitch - lfsgzmuvPitch]
xdata[, hfsgzmuvRoll := sgzmuvRoll - lfsgzmuvRoll]

xdata[, hfsgzmuvAccX := sgzmuvAccX - lfsgzmuvAccX]
xdata[, hfsgzmuvAccY := sgzmuvAccY - lfsgzmuvAccY]
xdata[, hfsgzmuvAccZ := sgzmuvAccZ - lfsgzmuvAccZ]

xdata[, hfsgzmuvGyroX := sgzmuvGyroX - lfsgzmuvGyroX]
xdata[, hfsgzmuvGyroY := sgzmuvGyroY - lfsgzmuvGyroY]
xdata[, hfsgzmuvGyroZ := sgzmuvGyroZ - lfsgzmuvGyroZ]



################################################################################
### (3.5) Smoothing data with hf sg zmuv
###
SavitzkyGolayCoeffs <- sgolay(p=5,n=155 ,m=0)

### FUNCTON TO SMOOTH THE DATA
SGolay <- function(xinput,sgCoeffs){
  output <- filter(sgCoeffs, xinput)
  return(output)
}


xdata[,c(
	'sghfsgzmuvAccX', 'sghfsgzmuvAccY', 'sghfsgzmuvAccZ',
	'sghfsgzmuvGyroX', 'sghfsgzmuvGyroY', 'sghfsgzmuvGyroZ'
	) 
:=lapply(
	.(
	hfsgzmuvAccX,  hfsgzmuvAccY, hfsgzmuvAccZ,
	hfsgzmuvGyroX, hfsgzmuvGyroY, hfsgzmuvGyroZ
	), 
	function(x) ( SGolay(x,SavitzkyGolayCoeffs)  ))
	]




################################################################################
# (4) Plotting
#


printflag <- FALSE


if (printflag == TRUE) {

#################
# (4.0) Plots Features
tag <- 'razor-timeseries'
image_width <- 2500
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"

plotlinewidth <- 0.7


################################################################################
# (4.1) Creating  and Changing to PlotPath
plot_path <- paste(outcomes_path,relativeplotpath,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}






######################
### (4.2) Plots Data from Razor sensor




#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sgzmuvAccX, col='sgzmuvAccX'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvAccY, col='sgzmuvAccY'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvAccZ, col='sgzmuvAccZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
##png(filename= paste(tag,"_sgzmuvAccXYZ.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()
#
#

plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccX, col='zmuvAccX'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvAccX, col='sgzmuvAccX'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvAccX.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccY, col='zmuvAccY'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvAccY, col='sgzmuvAccY'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvAccY.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccZ, col='zmuvAccZ'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvAccZ, col='sgzmuvAccZ'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvAccZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()










#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sgzmuvGyroX, col='sgzmuvGyroX'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvGyroY, col='sgzmuvGyroY'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvGyroZ, col='sgzmuvGyroZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
##png(filename= paste(tag,"_sgzmuvGyroXYZ.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()
#


plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroX, col='zmuvGyroX'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvGyroX, col='sgzmuvGyroX'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvGyroX.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroY, col='zmuvGyroY'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvGyroY, col='sgzmuvGyroY'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvGyroY.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroZ, col='zmuvGyroZ'), size=plotlinewidth)+
	geom_line( aes(y=sgzmuvGyroZ, col='sgzmuvGyroZ'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_zmuv-sgzmuvGyroZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()













#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc1_Acc, col='pc1_Acc'), size=plotlinewidth)+
#	geom_line( aes(y=pc2_Acc, col='pc2_Acc'), size=plotlinewidth)+
#	geom_line( aes(y=pc3_Acc, col='pc3_Acc'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('principal components for Acc') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
##png(filename= paste(tag,"_pcAcc.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()
#


#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc1_Gyro, col='pc1_Gyro'), size=plotlinewidth)+
#	geom_line( aes(y=pc2_Gyro, col='pc2_Gyro'), size=plotlinewidth)+
#	geom_line( aes(y=pc3_Gyro, col='pc3_Gyro'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('principal components for Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
#png(filename= paste(tag,"_pcGyro.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#




#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc1_AG, col='pc1_AG'), size=plotlinewidth)+
#	geom_line( aes(y=pc2_AG, col='pc2_AG'), size=plotlinewidth)+
#	geom_line( aes(y=pc3_AG, col='pc3_AG'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('principal components for Acc and Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
#png(filename= paste(tag,"_pcAG.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sgpc1_AG, col='sgpc1_AG'), size=plotlinewidth)+
#	geom_line( aes(y=sgpc2_AG, col='sgpc2_AG'), size=plotlinewidth)+
#	geom_line( aes(y=sgpc3_AG, col='sgpc3_AG'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay filtering for principal components for Acc and Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
##png(filename= paste(tag,"_sgpcAG.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()
#






#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc1_AG, col='pc1_AG'), size=plotlinewidth)+
#	geom_line( aes(y=sgpc1_AG, col='sgpc1_AG'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay filtering for principal components for Acc and Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
##png(filename= paste(tag,"_sgpcAG.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()


#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc2_AG, col='pc2_AG'), size=plotlinewidth)+
#	geom_line( aes(y=sgpc2_AG, col='sgpc2_AG'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay filtering for principal components for Acc and Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
##png(filename= paste(tag,"_sgpcAG.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()
#

#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=pc3_AG, col='pc3_AG'), size=plotlinewidth)+
#	geom_line( aes(y=sgpc3_AG, col='sgpc3_AG'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay filtering for principal components for Acc and Gyro') + 
#	xlab('Sample')+
#	labs(colour = 'pc')
#
#
##png(filename= paste(tag,"_sgpcAG.png",sep=''),
##   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
##dev.off()

































#
#
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=zmuvAccX, col='zmuvAccX'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvAccY, col='zmuvAccY'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvAccZ, col='zmuvAccZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
#	ylab('ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#
#png(filename= paste(tag,"_zmuvAccXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sgAccX, col='sgAccX'), size=plotlinewidth)+
#	geom_line( aes(y=sgAccY, col='sgAccY'), size=plotlinewidth)+
#	geom_line( aes(y=sgAccZ, col='sgAccZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#
#png(filename= paste(tag,"_sgAccXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#



#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sgGyroX, col='sgGyroX'), size=plotlinewidth)+
#	geom_line( aes(y=sgGyroY, col='sgGyroY'), size=plotlinewidth)+
#	geom_line( aes(y=sgGyroZ, col='sgGyroZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#
#png(filename= paste(tag,"_sgGyroXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=zmuvGyroX, col='zmuvGyroX'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvGyroY, col='zmuvGyroY'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvGyroZ, col='zmuvGyroZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#png(filename= paste(tag,"_zmuvGyroXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#



















#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=hfsgzmuvAccX, col='hfsgzmuvAccX'), size=plotlinewidth)+
#	geom_line( aes(y=hfsgzmuvAccY, col='hfsgzmuvAccY'), size=plotlinewidth)+
#	geom_line( aes(y=hfsgzmuvAccZ, col='hfsgzmuvAccZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-0.05,0.05)  )+
#	ylab('High Frequency Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#png(filename= paste(tag,"_hfsgzmuvAccXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=hfsgzmuvGyroX, col='hfsgzmuvGyroX'), size=plotlinewidth)+
#	geom_line( aes(y=hfsgzmuvGyroY, col='hfsgzmuvGyroY'), size=plotlinewidth)+
#	geom_line( aes(y=hfsgzmuvGyroZ, col='hfsgzmuvGyroZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-0.05,0.05)  )+
#	ylab('High Frequency Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#png(filename= paste(tag,"_hfsgzmuvGyroXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sghfsgzmuvAccX, col='sghfsgzmuvAccX'), size=plotlinewidth)+
#	geom_line( aes(y=sghfsgzmuvAccY, col='sghfsgzmuvAccY'), size=plotlinewidth)+
#	geom_line( aes(y=sghfsgzmuvAccZ, col='sghfsgzmuvAccZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-0.0001,0.0001)  )+
#	ylab('Savitzky-Golay High Frequency Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#png(filename= paste(tag,"_sghfsgzmuvAccXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=sghfsgzmuvGyroX, col='sghfsgzmuvGyroX'), size=plotlinewidth)+
#	geom_line( aes(y=sghfsgzmuvGyroY, col='sghfsgzmuvGyroY'), size=plotlinewidth)+
#	geom_line( aes(y=sghfsgzmuvGyroZ, col='sghfsgzmuvGyroZ'), size=plotlinewidth)+
#	facet_grid(participant~sensor)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-0.0001,0.0001)  )+
#	ylab('Savitzky-Golay High Frequency Savitzky-Golay and ZeroMeanUnitVariance') + 
#	xlab('Sample')+
#	labs(colour = 'Feature')
#
#png(filename= paste(tag,"_sghfsgzmuvGyroXYZ.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#







} # end if (printflag == TRUE)













#
#################################################################################
## (5) Creating Preprossed Data Path and Writing Data
#
#odata_path <- paste(outcomes_path,relativeodatapath,sep="")
#if (file.exists(odata_path)){
#    setwd(file.path(odata_path))
#} else {
#  dir.create(odata_path, recursive=TRUE)
#  setwd(file.path(odata_path))
#}
#
#
#
#
#################################################################################
#####  (5)  Writing Data
#write.table(xdata, "rawimudata-v00.datatable", row.name=FALSE)
#
#message('datatable file has been created at '  )
#message (odata_path)
#
#


#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


