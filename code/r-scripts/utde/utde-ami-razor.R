###############################################################################	
#
#  Average Mutual Information
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
	# (4) Compute Avarage Mutual Infomration (AMI)
	# (5) Create path and plot AMIs




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
relativeplotpathforEmbeddingValues <- "/plots_timeseries/razor/utde/embedding-values"
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








##############################################################################
##############################################################################
###############################################################################
# (4) Average Mutual Information

library(devtools)
load_all('~/mxochicale/github/nonlinearTseries')



maxtau <- 100
numberOFpartitions <- 100
#
xd <- xdata[,.(zmuvAccX,zmuvAccY,zmuvAccZ,sgzmuvAccX,sgzmuvAccY,sgzmuvAccZ,zmuvGyroX,zmuvGyroY,zmuvGyroZ,sgzmuvGyroX,sgzmuvGyroY,sgzmuvGyroZ), by=. (participant,trial,sensor,sample)]

pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')
axis <- c("zmuvAccX","zmuvAccY","zmuvAccZ","sgzmuvAccX","sgzmuvAccY","sgzmuvAccZ","zmuvGyroX","zmuvGyroY","zmuvGyroZ","sgzmuvGyroX","sgzmuvGyroY","sgzmuvGyroZ")



AMI <- NULL
for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]
hxdp <- xdp[sensor=='imu-human', .SDcols=cols  ]
rxdp <- xdp[sensor=='imu-robot', .SDcols=cols  ]

amip<-NULL
for (axis_k in c(1:12)){ #for (axis_k in c(1:12)){

	message('#### axis:' , axis[axis_k])



	########################
	message('     #human')
	inputtimeseries <- hxdp[,  get(axis[axis_k]) ]


	## tau-delay estimation based on the mutual information function
	tau.ami <- timeLag(time.series = inputtimeseries, technique = "ami",
                  selection.method = "first.minimum", lag.max = maxtau,
                  do.plot = F, n.partitions = numberOFpartitions,
                  units = "Bits")

	tauamilag <- tau.ami[[1]]
	ami <- tau.ami[[2]]

	amidt <- as.data.table(ami)
	amidt[, tau := 0:(.N-1), ]
	
	ftag <-function(x) {list("imu-human")}
	amidt[,c("sensor"):=ftag(), ]
	amih <- amidt

	
	#######################
	message('     #robot')
	inputtimeseries <- rxdp[,  get(axis[axis_k]) ]
	
	## tau-delay estimation based on the mutual information function
	tau.ami <- timeLag(time.series = inputtimeseries, technique = "ami",
                  selection.method = "first.minimum", lag.max = maxtau,
                  do.plot = F, n.partitions = numberOFpartitions,
                  units = "Bits")

	tauamilag <- tau.ami[[1]]
	ami <- tau.ami[[2]]

	amidt <- as.data.table(ami)
	amidt[, tau := 0:(.N-1), ]
	
	ftag <-function(x) {list("imu-robot")}
	amidt[,c("sensor"):=ftag(), ]
	amir <- amidt

	amis<-rbind(amih,amir)


	if (axis_k == 1){
    	ftag <-function(x) {list("zmuvAccX")}
    	} else if (axis_k == 2){
	ftag <-function(x) {list("zmuvAccY")}
    	} else if (axis_k == 3){
    	ftag <-function(x) {list("zmuvAccZ")}
       	} else if (axis_k == 4){
	ftag <-function(x) {list("sgzmuvAccX")}
    	} else if (axis_k == 5){
    	ftag <-function(x) {list("sgzmuvAccY")}
    	} else if (axis_k == 6){
	ftag <-function(x) {list("sgzmuvAccZ")}
    	} else if (axis_k == 7){
    	ftag <-function(x) {list("zmuvGyroX")}
    	} else if (axis_k == 8){
	ftag <-function(x) {list("zmuvGyroY")}
    	} else if (axis_k == 9){
    	ftag <-function(x) {list("zmuvGyroZ")}
    	} else if (axis_k == 10){
	ftag <-function(x) {list("sgzmuvGyroX")}
    	} else if (axis_k == 11){
    	ftag <-function(x) {list("sgzmuvGyroY")}
     	} else if (axis_k == 12){
    	ftag <-function(x) {list("sgzmuvGyroZ")}
    	} 

	
	amis[,c("axis"):=ftag(), ]

	amip <- rbind(amip,amis)

	}#for (axis_k in c(1:12)){


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

amip[,c("participant"):=fsNNtmp(), ]


AMI <- rbind(AMI, amip)

}#for (pNN_k in c(1:1)) {



setcolorder(AMI,c(5,4,3,2,1) )




################################################################################
### (5) Plot Avarage Mutual Information 


print_AMI_flag <- TRUE



if (print_AMI_flag == TRUE) {

### Save Picture
width = 2000
height = 1000
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi


plotlinewidth <- 0.9
#filenameimage <- paste("Evalues", t, ".png",sep="")




hAMI <- AMI[sensor=='imu-human', .SDcols=cols  ]
hami <- ggplot(hAMI, aes(x=tau) ) + geom_line( aes(y=ami ),lwd = plotlinewidth, alpha=0.7 ) + facet_grid(participant~axis) + ylab('AMI') + xlab('Tau')
#+ labs(colour = 'tau')


rAMI <- AMI[sensor=='imu-robot', .SDcols=cols  ]
rami <- ggplot(rAMI, aes(x=tau) ) + geom_line( aes(y=ami ),lwd = plotlinewidth, alpha=0.7 ) + facet_grid(participant~axis) + ylab('AMI') + xlab('Tau')


#    geom_point( aes(x=dim,y=E1, shape=factor(tau), colour=factor(tau)), size=5, stroke =1 )+
#    scale_color_manual(values = colorRampPalette(brewer.pal(n = 8, name="Greens"))(maxtau) ) +
#    scale_shape_manual(values= 1:(maxtau))+
#    labs(x='Embedding dimension, m', y='E1(m)' )+





## Setting up plots_path

plot_path <- paste(outcomes_path,relativeplotpathforEmbeddingValues,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



filenameimage <- paste("amih_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	hami)


filenameimage <- paste("amir_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	rami)






}















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


