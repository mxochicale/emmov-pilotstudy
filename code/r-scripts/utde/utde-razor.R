###############################################################################	
#
#  Uniform Time-Delay Embedding UTDE for razor
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:@gmail.com
# please email me directly if you see any errors or have any questions
# 
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
	# (4) Plotting time series: (PLOTTING_TIME_SERIES = FALSE/TRUE)
		# (4.0) Plots features	 
		# (4.1) Creating and chanding plot path	 
		# (4.2) Plots data for razor sensor
	# (5) Uniform Time Delay Embedding: (TIME_DELAY_EMBEDDING_COMPUTATIONS = FALSE/TRUE)
		# (5.0) Embedding Creating Preprossede Data Path
		# (5.1) Filtering Axis for Analysis of the UTDE
		# (5.2) Computing Eucliddean Distances from the PC1 and PC2
		# (5.3) buildTakens
		# (5.4) Creating path for razor euclidean distances



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
utde_sensor_path <- paste(outcomes_path,'/utde/razor',sep='')




relativeplotpathtimeseries <- "/utde/razor/timeseries"
#relativeplotpath <- "/plots_timeseries/razor/utde"
relativeplotpath4utde_razor_euclidean_distances <- "/utde/razor/euclideandistances"

relativeodatapath <- "/datatables"

odatapath <- paste( outcomes_path, relativeodatapath, sep="" )


utde_sensor_path <- paste(outcomes_path,'/utde/razor',sep='')






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
	Yaw, Pitch, Roll, 
	AccX, AccY, AccZ, 
	GyroX, GyroY, GyroZ,
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
# (4) plotting
#


################################################################################
################################################################################
################################################################################
################################################################################



PLOTTING_TIME_SERIES = FALSE
#PLOTTING_TIME_SERIES = TRUE





if ( PLOTTING_TIME_SERIES == TRUE ) {






#################
# (4.0) plots features
tag <- 'postprocessing'
image_width <- 2500
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"



plotlinewidth_z <- 1
alpha_z <- 0.9

plotlinewidth_sg <- 2
alpha_sg <- 0.7





################################################################################
# (4.1) creating  and changing to plotpath
plot_path <- paste(outcomes_path,relativeplotpathtimeseries,sep="")
                                #relativeplotpathtimeseries <- "/utde/razor/timeseries"

if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



######################
### (4.2) Plots Data from Razor sensor



plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccX, col='zmuvAccX'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvAccX, col='sgzmuvAccX'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_AccX.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccY, col='zmuvAccY'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvAccY, col='sgzmuvAccY'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_AccY.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvAccZ, col='zmuvAccZ'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvAccZ, col='sgzmuvAccZ'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_AccZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroX, col='zmuvGyroX'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvGyroX, col='sgzmuvGyroX'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_GyroX.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroY, col='zmuvGyroY'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvGyroY, col='sgzmuvGyroY'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_GyroY.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=zmuvGyroZ, col='zmuvGyroZ'), size=plotlinewidth_z,alpha=alpha_z)+
	geom_line( aes(y=sgzmuvGyroZ, col='sgzmuvGyroZ'), size=plotlinewidth_sg, alpha=alpha_sg)+
	facet_grid(participant~sensor)+
	theme_bw(20)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-5,5)  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_GyroZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






} ###if ( PLOTTING_TIME_SERIES = TRUE ) {







################################################################################
################################################################################
# (5)  UNIFORM TIME DELAY EMBEDDING
################################################################################
################################################################################


################################################################################
################################################################################
################################################################################
################################################################################



TIME_DELAY_EMBEDDING_COMPUTATIONS = FALSE
#TIME_DELAY_EMBEDDING_COMPUTATIONS = TRUE



if ( TIME_DELAY_EMBEDDING_COMPUTATIONS == TRUE ) {


### (5.0) Embedding Creating Preprossede Data Path
embedding_path <- paste(utde_sensor_path,"/embeddings",sep="")
if (file.exists(embedding_path)){
setwd(file.path(embedding_path))
} else {
dir.create(embedding_path, recursive=TRUE)
setwd(file.path(embedding_path))
}



### (5.1) Filtering Axis for Analysis of the UTDE

xd <- xdata[,.(zmuvAccX,zmuvAccY,zmuvAccZ,sgzmuvAccX,sgzmuvAccY,sgzmuvAccZ,zmuvGyroX,zmuvGyroY,zmuvGyroZ,sgzmuvGyroX,sgzmuvGyroY,sgzmuvGyroZ), by=. (participant,trial,sensor,sample)]

pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')
axis <- c("zmuvAccX","zmuvAccY","zmuvAccZ","sgzmuvAccX","sgzmuvAccY","sgzmuvAccZ","zmuvGyroX","zmuvGyroY","zmuvGyroZ","sgzmuvGyroX","sgzmuvGyroY","sgzmuvGyroZ")
sensors <- c('imu-human','imu-robot')




### (5.2) Computing Eucliddean Distances from the PC1 and PC2

ED <- NULL # Euclidean Distances data.table object!
for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]


#hxdp <- xdp[sensor=='imu-human', .SDcols=cols  ]
#rxdp <- xdp[sensor=='imu-robot', .SDcols=cols  ]




ED_sa <- NULL
for (sensor_k in c(1:2)) {

hrxdp <- xdp[sensor== sensors[sensor_k], .SDcols=cols  ]




#time_lags_p <- NULL
ED_a<-NULL
for (axis_k in c(1:12)){ #for (axis_k in c(1:12)){

	message('#### axis:' , axis[axis_k])



	########################
	message('     #sensor',sensors[sensor_k])
	hrinputtimeseries <- hrxdp[,  get(axis[axis_k]) ]




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
## (5.3) buildTakens

### Computed Embedding parameters:  m=7 tau=4
# delays <- tau
# dimensions <- dimension



## For testing purposes 
#delays <- c(2)
#dimensions <- c(5)



#delays <- c(2,8)
#dimensions <- c(10, 100)

delays <- c(2,5,10)
dimensions <- c(10,50,100)

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

a_utde <- Takens_Theorem(hrinputtimeseries, dim_i, tau_j, 1)

a_rss <- PCA( a_utde ,0)





image_width =  1000#2000 #3508 #595 #877
image_height = 1000#700#2480 #842 #620

imagefilename <- paste('pc_rotateddata_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', sensors[sensor_k], '.png', sep='')
png(filename=imagefilename, width=image_width, height=image_height, units="px", bg="white")
plotRSS2D_rotateddata(a_rss,15)
dev.off()





## Euclidean Distances
ed_dta <- as.data.table(euclidean.distances_rotateddata(a_rss))
fun <- function(x) {list( axis[axis_k] )}
ed_dta[,c('axis'):= fun(), ]





#
#image_width_p3d =  2000#2000 #3508 #595 #877
#image_height_p3d = 500#700#2480 #842 #620
#
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



########################################################################
########### START OF  ....   THIS PART CREATES SOME PROBLEMS WITH THE IMAGE RESOLUTION
#### Save Picture
#width = 500
#height = 500
#text.factor = 1
#dpi <- text.factor * 100
#width_calc <- width / dpi
#height_calc <- height / dpi
#
#imagefilename <- paste('utde_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', sensors[sensor_k], '.jpeg', sep='')
##png(filename=imagefilename, width=width_calc, height=height_calc, units="px", res = dpi, bg="white")
#jpeg(filename=imagefilename, width=width_calc, height=height_calc, units="px", res = dpi, bg="white")
#
#
#p <- plotRSS3D2D(a_rss)
#
#dev.off()
#
##
##ggsave(
##	plot=plotRSS3D2D(a_rss),
##	filename = imagefilename,
##        dpi = dpi,
##        width = width.calc,
##        height = height.calc,
##        units = 'in',
##        bg = "transparent",
##        device = "png"
##	)	
##
##
#
########### END OF ....   THIS PART CREATES SOME PROBLEMS WITH THE IMAGE RESOLUTION
########################################################################



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


}##for (axis_k in c(1:12)){


fun <- function(x) {list( sensors[sensor_k] )}
ED_a[,c('sensor'):= fun(), ]


ED_sa <- rbind(ED_sa, ED_a)


}###for (sensor_k in c(1:2))







 # Particpant Number
fp <-function(x) {  pNN[participants_k]   }
ED_sa[,c("participant"):=fp(), ]
ED <- rbind(ED, ED_sa)



}##for (pNN_k in c(1:1)) {





names(ED) <- gsub("V1", "EuclideanDistances", names(ED))
setcolorder( ED, c(6,5,2,3,4,1) )








### (5.4) Creating path for razor euclidean distances



plot_path <- paste(outcomes_path,relativeplotpath4utde_razor_euclidean_distances,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



#########
message('plotting euclidean distances')



for (pd_k in c(1:length(delays))) {

tau_value <-  formatC(delays[pd_k],width=3,flag='0')
dED <- ED[tau== tau_value, .SDcols=cols  ]






hED <- dED[sensor=='imu-human', .SDcols=cols  ]

hpbox <- ggplot(hED, aes(x=participant, y=EuclideanDistances) )+
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



rED <- dED[sensor=='imu-robot', .SDcols=cols  ]

rpbox <- ggplot(rED, aes(x=participant, y=EuclideanDistances) )+
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
	hpbox)


filenameimage <- paste("edrobot_", 'tau_', tau_value, ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	rpbox)


} ###for (pd_k in c(1:length(delays))) {






} ## if ( TIME_DELAY_EMBEDDING_COMPUTATIONS == TRUE ) {













#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path

