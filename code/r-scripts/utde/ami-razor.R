###############################################################################	
#
#  Average Mutual Information
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:@gmail.com
# please email me directly if you see any errors or have any questions
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
relativeplotpathforEmbeddingValues <- "/utde/razor/minimum-embedding-values"
relativeodatapath <- "/datatables"

odatapath <- paste( outcomes_path, relativeodatapath, sep="" )






################################################################################
# (2) Setting DataSets paths and reading data


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
load_all( paste(github_path,'/nonlinearTseries',sep='') )



maxtau <- 100
numberOFpartitions <- 100

#Method used for selecting a concrete time lag. Available methods are "first.zero", "first.e.decay" (default), "first.minimum" and "first.value".
ami_timelag_selection_method <- 'first.minimum' # 'first.minimum'


#ami_timelag_selection_method <- 'first.value' # 'first.minimum'

##Numeric value indicating the value that the autocorrelation/AMI function must cross in order to select the time lag. It is used only with the "first.value" selection method.
ami_numeric_value <- 1/exp(0)


#
xd <- xdata[,.(
	zmuvAccX,zmuvAccY,zmuvAccZ,sgzmuvAccX,sgzmuvAccY,sgzmuvAccZ,
	zmuvGyroX,zmuvGyroY,zmuvGyroZ,sgzmuvGyroX,sgzmuvGyroY,sgzmuvGyroZ), 
	by=. (participant,trial,sensor,sample)]

pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')
axis <- c("zmuvAccX","zmuvAccY","zmuvAccZ","sgzmuvAccX","sgzmuvAccY","sgzmuvAccZ",
	"zmuvGyroX","zmuvGyroY","zmuvGyroZ","sgzmuvGyroX","sgzmuvGyroY","sgzmuvGyroZ")



AMI <- NULL
time_lags <- NULL

for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]
hxdp <- xdp[sensor=='imu-human', .SDcols=cols  ]
rxdp <- xdp[sensor=='imu-robot', .SDcols=cols  ]

time_lags_p <- NULL
amip<-NULL
for (axis_k in c(1:12)){ #for (axis_k in c(1:12)){

	message('#### axis:' , axis[axis_k])



	########################
	message('     #human')
	inputtimeseries <- hxdp[,  get(axis[axis_k]) ]


	## tau-delay estimation based on the mutual information function
	tau.ami <- timeLag(time.series = inputtimeseries, technique = "ami",
                selection.method = ami_timelag_selection_method, value = ami_numeric_value,
		lag.max = maxtau,
                do.plot = F, n.partitions = numberOFpartitions,
                units = "Bits")

	tauamilag_h <- as.data.table(tau.ami[[1]])
	ami <- tau.ami[[2]]

	amidt <- as.data.table(ami)
	amidt[, tau := 0:(.N-1), ]
	
	ftag <-function(x) {list("imu-human")}
	amidt[,c("sensor"):=ftag(), ]
	amih <- amidt

	tauamilag_h[,c("sensor"):=ftag(), ]
	
	
	#######################
	message('     #robot')
	inputtimeseries <- rxdp[,  get(axis[axis_k]) ]
	
	## tau-delay estimation based on the mutual information function
	tau.ami <- timeLag(time.series = inputtimeseries, technique = "ami",
                selection.method = ami_timelag_selection_method, value = ami_numeric_value,
		lag.max = maxtau,
                do.plot = F, n.partitions = numberOFpartitions,
                units = "Bits")

	tauamilag_r <- as.data.table(tau.ami[[1]])
	ami <- tau.ami[[2]]

	amidt <- as.data.table(ami)
	amidt[, tau := 0:(.N-1), ]
	
	ftag <-function(x) {list("imu-robot")}
	amidt[,c("sensor"):=ftag(), ]
	amir <- amidt

	tauamilag_r[,c("sensor"):=ftag(), ]


	time_lags_a <- rbind(tauamilag_h, tauamilag_r)


	amis<-rbind(amih,amir)

	
	## function for axis
	fa <-function(x) {  axis[axis_k]  }

	time_lags_a[,c("axis"):=fa(), ]
	time_lags_p <- rbind(time_lags_p,time_lags_a)


	amis[,c("axis"):=fa(), ]
	amip <- rbind(amip,amis)


	}#for (axis_k in c(1:12)){


# function for Particpant Number
fp <-function(x) {  pNN[participants_k]   }


amip[,c("participant"):=fp(), ]
AMI <- rbind(AMI, amip)


time_lags_p[,c("participant"):=fp(), ]
time_lags <- rbind(time_lags,time_lags_p)


}#for (pNN_k in c(1:1)) {



setcolorder(AMI,c(5,4,3,2,1) )

setcolorder(time_lags,c(4,2,3,1) )
names(time_lags)[4] <- 'timelags'



################################################################################
### (5) Plot Avarage Mutual Information 


#print_AMI_flag <- FALSE
print_AMI_flag <- TRUE



if (print_AMI_flag == TRUE) { #if (print_AMI_flag == TRUE) {


plotlinewidth <- 0.9
#filenameimage <- paste("Evalues", t, ".png",sep="")




htl <- time_lags[sensor=='imu-human', .SDcols=cols  ]
htlp <- ggplot(htl, aes(x=participant,y=timelags) ) + 
	geom_point( aes(fill=participant, colour=participant, shape=participant), size=5 ) + 
	facet_grid(.~axis) + ylab("First Minimum AMI") + 
	coord_cartesian(xlim=NULL, ylim=c(0,35)  ) +
	theme_bw(20) +	
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain")  )


rtl <- time_lags[sensor=='imu-robot', .SDcols=cols  ]
rtlp <- ggplot(rtl, aes(x=participant,y=timelags) ) + 
	geom_point( aes(fill=participant, colour=participant, shape=participant), size=5 ) + 
	facet_grid(.~axis) + ylab("First Minimum AMI") + 
	coord_cartesian(xlim=NULL, ylim=c(0,35)  ) +
	theme_bw(20) +	
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain") )





hAMI <- AMI[sensor=='imu-human', .SDcols=cols  ]
hami <- ggplot(hAMI, aes(x=tau) ) + 
	geom_line( aes(y=ami ),lwd = plotlinewidth, alpha=0.7 ) + 
	facet_grid(participant~axis) + 
	ylab('AMI') + xlab('Tau') +
	theme_bw(20)+
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))

rAMI <- AMI[sensor=='imu-robot', .SDcols=cols  ]
rami <- ggplot(rAMI, aes(x=tau) ) + 
	geom_line( aes(y=ami ),lwd = plotlinewidth, alpha=0.7 ) + 
	facet_grid(participant~axis) + 
	ylab('AMI') + xlab('Tau') +
	theme_bw(20)+
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))



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

### Save Picture
width = 2000
height = 1000
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi



filenameimage <- paste("ami-human", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	hami)


filenameimage <- paste("ami-robot", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	rami)



### Save Picture
width = 2100
height = 400
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi


filenameimage <- paste("ami-human-mintau", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	htlp)


filenameimage <- paste("ami-robot-mintau", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	rtlp)




} #if (print_AMI_flag == TRUE) {















#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


