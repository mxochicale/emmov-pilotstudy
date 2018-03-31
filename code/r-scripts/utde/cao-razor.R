###############################################################################	
#
#  preprocessed data for razor IMU sensors 
#  it takes around 30 minutes 29march2018 
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
	# (4) CAO's Algoring
	# (5) Plot E1 and E2 values (print_EVALUES_flag <- TRUE)



#################
# Start the clock!
start.time <- Sys.time()


################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 

library(signal)# for butterworth filter and sgolay
source('../../../../tavand/functions/ollin_cencah.R')





################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()

setwd("../../../")
main_repository_path <- getwd()
setwd("../")
github_path <- getwd()


main_data_path <- paste( main_repository_path, '/data/razor_imu',sep="")
outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
relativeplotpath <- "/utde/razor/timeseries"
relativeplotpathforEmbeddedValues <- "/utde/razor/minimum-embedding-values"
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









###############################################################################
###############################################################################
################################################################################
## (4) CAO's Algorithm
##
source(paste(github_path,'/tavand/functions/embedding_parameters/withCao1997/cao97_functions.R', sep=''))




maxtau <- 20
maxdim <- 21




xcao <- xdata[,.(zmuvAccX,zmuvAccY,zmuvAccZ,sgzmuvAccX,sgzmuvAccY,sgzmuvAccZ,zmuvGyroX,zmuvGyroY,zmuvGyroZ,sgzmuvGyroX,sgzmuvGyroY,sgzmuvGyroZ), by=. (participant,trial,sensor,sample)]

pNN <- c('p01')
#pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')

axis <- c("zmuvAccX","sgzmuvAccX")
#axis <- c("zmuvAccX","zmuvAccY","zmuvAccZ","sgzmuvAccX","sgzmuvAccY","sgzmuvAccZ","zmuvGyroX","zmuvGyroY","zmuvGyroZ","sgzmuvGyroX","sgzmuvGyroY","sgzmuvGyroZ")

EE <- NULL
for (participants_k in c( 1:(length(pNN))  )) {#for (pNN_k in c(1:1)) {

setkey(xcao, participant)
xcao1 <- xcao[.( pNN[participants_k] )]
hxcao1 <- xcao1[sensor=='imu-human', .SDcols=cols  ]
rxcao1 <- xcao1[sensor=='imu-robot', .SDcols=cols  ]






Ep <- NULL


for (axis_k in c(1:(length(axis)) )){ #for (axis_k in c(1:12)){

message('#### axis:' , axis[axis_k])

message('     #human')
inputtimeseries <- hxcao1[,  get(axis[axis_k]) ]
E <- data.table()
for (tau_i in 1:maxtau){
    message( 'tau: ', tau_i )
    Et<- as.data.table(cao97sub(inputtimeseries,maxdim,tau_i) )
    func <-function(x) {list( tau_i )}
    Et[,c("tau"):=func(), ]
    Et[,dim:=seq(.N)]
    setcolorder(Et, c(3,4,1:2))
    E <- rbind(E, Et )
}
names(E) <- gsub("V1", "E1", names(E))
names(E) <- gsub("V2", "E2", names(E))

ftag <-function(x) {list("imu-human")}
E[,c("sensor"):=ftag(), ]
Eh <- E

E <- NULL

message('    #robot')

inputtimeseries <- rxcao1[,  get(axis[axis_k]) ]
E <- data.table()
for (tau_i in 1:maxtau){
    message( 'tau: ', tau_i )
    Et<- as.data.table(cao97sub(inputtimeseries,maxdim,tau_i) )
    func <-function(x) {list( tau_i )}
    Et[,c("tau"):=func(), ]
    Et[,dim:=seq(.N)]
    setcolorder(Et, c(3,4,1:2))
    E <- rbind(E, Et )
}
names(E) <- gsub("V1", "E1", names(E))
names(E) <- gsub("V2", "E2", names(E))

ftag <-function(x) {list("imu-robot")}
E[,c("sensor"):=ftag(), ]
Er <-E
E<-NULL



Ea <- rbind(Eh,Er)

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



	
Ea[,c("axis"):=ftag(), ]

Ep <- rbind(Ep,Ea)
}# for (axis_k in c(1:12)){ 




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

Ep[,c("participant"):=fsNNtmp(), ]

EE <- rbind(EE,Ep)



}#for (pNN_k in c(1:1)) {


setcolorder(EE,c(7,5,6,1:4) )






################################################################################
### (5) Plot E values



print_EVALUES_flag <- TRUE
#print_EVALUES_flag <- FALSE


if (print_EVALUES_flag == TRUE) {

### Save Picture
width = 2000
height = 1000
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi


plotlinewidth <- 0.9
#filenameimage <- paste("Evalues", t, ".png",sep="")




hEE <- EE[sensor=='imu-human', .SDcols=cols  ]
he1 <- ggplot(hEE, aes(x=dim) ) + 
	geom_line( aes(y=E1, colour=factor(tau) ),lwd = plotlinewidth, alpha=0.5 ) + 
	facet_grid(participant~axis) + 
	theme_bw(20)+	
	ylab('E1') + 
	xlab('Dimension, m')+ 
	labs(colour = 'tau')

he2 <- ggplot(hEE, aes(x=dim) ) + 
	geom_line( aes(y=E2, colour=factor(tau) ),lwd = plotlinewidth, alpha=0.5 ) + 
	facet_grid(participant~axis) + 
	theme_bw(20)+	
	ylab('E2') + 
	xlab('Dimension, m')+ 
	labs(colour = 'tau')

rEE <- EE[sensor=='imu-robot', .SDcols=cols  ]
re1 <- ggplot(rEE, aes(x=dim) ) + 
	geom_line( aes(y=E1, colour=factor(tau) ),lwd = plotlinewidth, alpha=0.5 ) + 
	facet_grid(participant~axis) + 
	theme_bw(20)+	
	ylab('E1') + 
	xlab('Dimension, m')+ 
	labs(colour = 'tau')

re2 <- ggplot(rEE, aes(x=dim) ) + 
	geom_line( aes(y=E2, colour=factor(tau) ),lwd = plotlinewidth, alpha=0.5 ) + 
	facet_grid(participant~axis) + 
	ylab('E2') + 
	xlab('Dimension, m')+ 
	labs(colour = 'tau')


#    geom_point( aes(x=dim,y=E1, shape=factor(tau), colour=factor(tau)), size=5, stroke =1 )+
#    scale_color_manual(values = colorRampPalette(brewer.pal(n = 8, name="Greens"))(maxtau) ) +
#    scale_shape_manual(values= 1:(maxtau))+
#    labs(x='Embedding dimension, m', y='E1(m)' )+





## Setting up plots_path

plot_path <- paste(outcomes_path,relativeplotpathforEmbeddedValues,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



filenameimage <- paste("e1h_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	he1)


filenameimage <- paste("e2h_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	he2)


filenameimage <- paste("e1r_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	re1)


filenameimage <- paste("e2r_", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	re2)




}




#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


