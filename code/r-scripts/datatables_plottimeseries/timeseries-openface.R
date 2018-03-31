###############################################################################	
#
# Time series plots for the openface landmarks 
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email: @gmail.com
# please email me directly if you see any errors or have any questions
#
###############################################################################	
	# OUTLINE:
	# (0) Loading libraries and functions
 	# (1) Definifing paths and Reading data
	# (2) Data Filtering
		# (2.1) Windowing
	# (3) Plotting (PLOTTING_TIMESERIES = TRUE/FALSE)
		# (3.1) Creating and changing plotting paths
		# (3.2) Plots features
		# (3.3) Plots data from openface
	# (4) Creating Preprossed Data Path
	# (5) Writing Data



#################
# Start the clock!
start.time <- Sys.time()


################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 




################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()

setwd("../../../")
main_repository_path <- getwd()
setwd("../")
github_path <- getwd()


main_data_path <- paste( main_repository_path, '/data/openface',sep="")
outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
relativeplotpath <- "/plots_timeseries/openface"
relativeodatapath <- "/datatables"


################################################################################
# (1) Setting DataSets paths and reading data
setwd(main_data_path)
data_path_list <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

participantsNN <- 6
trialsNN <- 1
participant_index <- c(2,4,6,8,10,12)


pNN_tmp  <- NULL ## initialise variable
#pNN_tNN_tmp  <- NULL ## initialise variable


#forSTART......... to read data from participants paths
for(participants_k in 1:participantsNN)
{

	participant_NN_path <-  substring( (toString(data_path_list[ participant_index[participants_k] ])) , 2, last = 1000000L)
	full_participant_NN_path <- paste(main_data_path, participant_NN_path, "/",sep="")
	message(' PATH for PARTICIPANT ', participants_k, '  = ', full_participant_NN_path )
	setwd( full_participant_NN_path )

	details = file.info(list.files(pattern=""))
	files = rownames(details)


    	# Particpant Number
	pNN_ <-  paste("p", participants_k, sep="")
    	assign (pNN_, fread(  files[2] , header = TRUE, sep=',') )
    	temp <- get(pNN_)


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


 
	temp[,c("participant"):=fsNNtmp(), ]
	setcolorder(temp,c(432,1:431) )


        #forSTART...trials
        for(trial_k in 1:trialsNN )
        {

           #### adding trial tags to each data.table
           if (trial_k == 1){
           fsNNtmp <-function(x) {list("t01")}
           } else if (trial_k == 2){
           fsNNtmp <-function(x) {list("t02")}
           } else if (trial_k == 3){
           fsNNtmp <-function(x) {list("t03")}
           } else if (trial_k == 4){
           fsNNtmp <-function(x) {list("t04")}
           } else if (trial_k == 5){
           fsNNtmp <-function(x) {list("t05")}
           } else if (trial_k == 6){
           fsNNtmp <-function(x) {list("t06")}
           }
           temp[,c("trial"):=fsNNtmp(), ]
           setcolorder(temp,c(1,433,2:432) )

           pNN_tmp <- rbind(pNN_tmp, temp)
        }
        #forEND...trials


}
#forEND......... to read data from participants paths


##### dataTable
datatable <- pNN_tmp




################################################################################
# (2) Data Filtering


################################
### (2.1) Windowing Data [xdata[,.SD[1:2],by=.(Participant,Activity,Sensor)]]

windowframe = 000:2000;
xdata <- datatable[,.SD[windowframe],by=.(participant,trial)];



################################################################################
# (3) Plotting
#

#PLOTTING_TIMESERIES = FALSE
PLOTTING_TIMESERIES = TRUE

if (PLOTTING_TIMESERIES == TRUE) {


################################################################################
# (3.1) Creating  and Changing to PlotPath
#
plot_path <- paste(outcomes_path,relativeplotpath,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}




#################
# (3.2) Plots Features
tag <- 'openface-timeseries'
image_width <- 2000
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"

plotlinewidth <- 1



#####################
## (3.3) Plots Data from OpenFace



#Confidence and success
plot <- ggplot(xdata, aes(x=frame) ) +	
	geom_line( aes(y=confidence, col='confidence'), size=plotlinewidth )+
	geom_line( aes(y=as.numeric(success), col='success'), size=plotlinewidth )+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	
	ylab('Raw Values') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_confidence_success.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




# Gaze 0
plot <- ggplot(xdata, aes(x=frame) ) +	
  	geom_line( aes(y=gaze_0_x, color='gaze_0_x'), size=plotlinewidth)+
  	geom_line( aes(y=gaze_0_y, color='gaze_0_y'), size=plotlinewidth)+
  	geom_line( aes(y=gaze_0_z, color='gaze_0_z'), size=plotlinewidth)+
  	facet_grid(participant~.)+
  	theme_bw(20) +

	ylab('Raw Values') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_gaze_0_xyz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




# Gaze 1
plot <- ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=gaze_1_x, color='gaze_1_x'), size=plotlinewidth)+
  geom_line( aes(y=gaze_1_y, color='gaze_1_y'), size=plotlinewidth)+
  geom_line( aes(y=gaze_1_z, color='gaze_1_z'), size=plotlinewidth)+
  facet_grid(participant~.)+
	theme_bw(20) +

	ylab('Raw Values') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_gaze_1_xyz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





## Pose estimation with respect to the camera


plot <- ggplot(xdata, aes(x=frame) ) +	
   geom_line( aes(y=pose_Tx, col='pose_Tx'), size=plotlinewidth)+
   geom_line( aes(y=pose_Ty, col='pose_Ty'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=c(-150,150))+
	ylab('Location of the head with respect to the camera [millimetre]') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_pose_TxTy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




plot <- ggplot(xdata, aes(x=frame) ) +	
   geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=c(800,1200))+
	ylab('Location of the head with respect to the camera [millimetre]') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_pose_Tz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




plot <- ggplot(xdata, aes(x=frame) ) +	
   geom_line( aes(y=pose_Rx, col='pose_Rx'), size=plotlinewidth)+
   geom_line( aes(y=pose_Ry, col='pose_Ry'), size=plotlinewidth)+
   geom_line( aes(y=pose_Rz, col='pose_Rz'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=c(-0.5,0.5))+
	ylab('Rotation of the head [Radians]') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_pose_RxRyRz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





#x_48, x_49, x_50, x_51, x_52, x_53, x_54, x_55, x_56, x_57, x_58, x_59, x_60, x_61, x_62, x_63, x_64, x_65, x_66, x_67, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=x_48, col='x_48'), size=plotlinewidth)+
 	geom_line( aes(y=x_49, col='x_49'), size=plotlinewidth)+
 	geom_line( aes(y=x_50, col='x_50'), size=plotlinewidth)+
 	geom_line( aes(y=x_51, col='x_51'), size=plotlinewidth)+
 	geom_line( aes(y=x_52, col='x_52'), size=plotlinewidth)+
 	geom_line( aes(y=x_53, col='x_53'), size=plotlinewidth)+
 	geom_line( aes(y=x_54, col='x_54'), size=plotlinewidth)+
 	geom_line( aes(y=x_55, col='x_55'), size=plotlinewidth)+
 	geom_line( aes(y=x_56, col='x_56'), size=plotlinewidth)+
 	geom_line( aes(y=x_57, col='x_57'), size=plotlinewidth)+
 	geom_line( aes(y=x_58, col='x_58'), size=plotlinewidth)+
 	geom_line( aes(y=x_59, col='x_59'), size=plotlinewidth)+
 	geom_line( aes(y=x_60, col='x_60'), size=plotlinewidth)+
 	geom_line( aes(y=x_61, col='x_61'), size=plotlinewidth)+
 	geom_line( aes(y=x_62, col='x_62'), size=plotlinewidth)+
 	geom_line( aes(y=x_63, col='x_63'), size=plotlinewidth)+
 	geom_line( aes(y=x_64, col='x_64'), size=plotlinewidth)+
 	geom_line( aes(y=x_65, col='x_65'), size=plotlinewidth)+
 	geom_line( aes(y=x_66, col='x_66'), size=plotlinewidth)+
 	geom_line( aes(y=x_67, col='x_67'), size=plotlinewidth)+


	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_x_48_to_x_67_mouth.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


##y_48, y_49, y_50, y_51, y_52, y_53, y_54, y_55, y_56, y_57, y_58, y_59, y_60, y_61, y_62, y_63, y_64, y_65, y_66, y_67, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=y_48, col='y_48'), size=plotlinewidth)+
 	geom_line( aes(y=y_49, col='y_49'), size=plotlinewidth)+
 	geom_line( aes(y=y_50, col='y_50'), size=plotlinewidth)+
 	geom_line( aes(y=y_51, col='y_51'), size=plotlinewidth)+
 	geom_line( aes(y=y_52, col='y_52'), size=plotlinewidth)+
 	geom_line( aes(y=y_53, col='y_53'), size=plotlinewidth)+
 	geom_line( aes(y=y_54, col='y_54'), size=plotlinewidth)+
 	geom_line( aes(y=y_55, col='y_55'), size=plotlinewidth)+
 	geom_line( aes(y=y_56, col='y_56'), size=plotlinewidth)+
 	geom_line( aes(y=y_57, col='y_57'), size=plotlinewidth)+
 	geom_line( aes(y=y_58, col='y_58'), size=plotlinewidth)+
 	geom_line( aes(y=y_59, col='y_59'), size=plotlinewidth)+
 	geom_line( aes(y=y_60, col='y_60'), size=plotlinewidth)+
 	geom_line( aes(y=y_61, col='y_61'), size=plotlinewidth)+
 	geom_line( aes(y=y_62, col='y_62'), size=plotlinewidth)+
 	geom_line( aes(y=y_63, col='y_63'), size=plotlinewidth)+
 	geom_line( aes(y=y_64, col='y_64'), size=plotlinewidth)+
 	geom_line( aes(y=y_65, col='y_65'), size=plotlinewidth)+
 	geom_line( aes(y=y_66, col='y_66'), size=plotlinewidth)+
 	geom_line( aes(y=y_67, col='y_67'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_y_48_to_y_67_mouth.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




#
##x_0, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11, x_12, x_13, x_14, x_15, x_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=x_0, col='x_00'), size=plotlinewidth)+
 	geom_line( aes(y=x_1, col='x_01'), size=plotlinewidth)+
 	geom_line( aes(y=x_2, col='x_02'), size=plotlinewidth)+
 	geom_line( aes(y=x_3, col='x_03'), size=plotlinewidth)+
 	geom_line( aes(y=x_4, col='x_04'), size=plotlinewidth)+
 	geom_line( aes(y=x_5, col='x_05'), size=plotlinewidth)+
 	geom_line( aes(y=x_6, col='x_06'), size=plotlinewidth)+
 	geom_line( aes(y=x_7, col='x_07'), size=plotlinewidth)+
 	geom_line( aes(y=x_8, col='x_08'), size=plotlinewidth)+
 	geom_line( aes(y=x_9, col='x_09'), size=plotlinewidth)+
 	geom_line( aes(y=x_10, col='x_10'), size=plotlinewidth)+
 	geom_line( aes(y=x_11, col='x_11'), size=plotlinewidth)+
 	geom_line( aes(y=x_12, col='x_12'), size=plotlinewidth)+
 	geom_line( aes(y=x_13, col='x_13'), size=plotlinewidth)+
 	geom_line( aes(y=x_14, col='x_14'), size=plotlinewidth)+
 	geom_line( aes(y=x_15, col='x_15'), size=plotlinewidth)+
 	geom_line( aes(y=x_16, col='x_16'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_x_01_to_x_16_roundface.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



#
##y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8, y_9, y_10, y_11, y_12, y_13, y_14, y_15, y_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=y_0, col='y_00'), size=plotlinewidth)+
 	geom_line( aes(y=y_1, col='y_01'), size=plotlinewidth)+
 	geom_line( aes(y=y_2, col='y_02'), size=plotlinewidth)+
 	geom_line( aes(y=y_3, col='y_03'), size=plotlinewidth)+
 	geom_line( aes(y=y_4, col='y_04'), size=plotlinewidth)+
 	geom_line( aes(y=y_5, col='y_05'), size=plotlinewidth)+
 	geom_line( aes(y=y_6, col='y_06'), size=plotlinewidth)+
 	geom_line( aes(y=y_7, col='y_07'), size=plotlinewidth)+
 	geom_line( aes(y=y_8, col='y_08'), size=plotlinewidth)+
 	geom_line( aes(y=y_9, col='y_09'), size=plotlinewidth)+
 	geom_line( aes(y=y_10, col='y_10'), size=plotlinewidth)+
 	geom_line( aes(y=y_11, col='y_11'), size=plotlinewidth)+
 	geom_line( aes(y=y_12, col='y_12'), size=plotlinewidth)+
 	geom_line( aes(y=y_13, col='y_13'), size=plotlinewidth)+
 	geom_line( aes(y=y_14, col='y_14'), size=plotlinewidth)+
 	geom_line( aes(y=y_15, col='y_15'), size=plotlinewidth)+
 	geom_line( aes(y=y_16, col='y_16'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_y_01_to_y_16_roundface.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#
##x_17, x_18, x_19, x_20, x_21, x_22, x_23, x_24, x_25, x_26
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=x_17, col='x_17'), size=plotlinewidth)+
 	geom_line( aes(y=x_18, col='x_18'), size=plotlinewidth)+
 	geom_line( aes(y=x_19, col='x_19'), size=plotlinewidth)+
 	geom_line( aes(y=x_20, col='x_20'), size=plotlinewidth)+
 	geom_line( aes(y=x_21, col='x_21'), size=plotlinewidth)+
 	geom_line( aes(y=x_22, col='x_22'), size=plotlinewidth)+
 	geom_line( aes(y=x_23, col='x_23'), size=plotlinewidth)+
 	geom_line( aes(y=x_24, col='x_24'), size=plotlinewidth)+
 	geom_line( aes(y=x_25, col='x_25'), size=plotlinewidth)+
 	geom_line( aes(y=x_26, col='x_26'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +

	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_x_17_to_x_26_eyerbrows.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





#
##y_17, y_18, y_19, y_20, y_21, y_22, y_23, y_24, y_25, y_26, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=y_17, col='y_17'), size=plotlinewidth)+
 	geom_line( aes(y=y_18, col='y_18'), size=plotlinewidth)+
 	geom_line( aes(y=y_19, col='y_19'), size=plotlinewidth)+
 	geom_line( aes(y=y_20, col='y_20'), size=plotlinewidth)+
 	geom_line( aes(y=y_21, col='y_21'), size=plotlinewidth)+
 	geom_line( aes(y=y_22, col='y_22'), size=plotlinewidth)+
 	geom_line( aes(y=y_23, col='y_23'), size=plotlinewidth)+
 	geom_line( aes(y=y_24, col='y_24'), size=plotlinewidth)+
 	geom_line( aes(y=y_25, col='y_25'), size=plotlinewidth)+
 	geom_line( aes(y=y_26, col='y_26'), size=plotlinewidth)+

	facet_grid(participant ~ . )+
  	theme_bw(20) +
	
	coord_cartesian(xlim=NULL, ylim=NULL)+
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_y_17_to_y_26_eyerbrows.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()














##x_0,...x_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=x_0, col='x_00'), size=plotlinewidth)+
 	geom_line( aes(y=x_1, col='x_01'), size=plotlinewidth)+
 	geom_line( aes(y=x_2, col='x_02'), size=plotlinewidth)+
 	geom_line( aes(y=x_3, col='x_03'), size=plotlinewidth)+
 	geom_line( aes(y=x_4, col='x_04'), size=plotlinewidth)+
 	geom_line( aes(y=x_5, col='x_05'), size=plotlinewidth)+
 	geom_line( aes(y=x_6, col='x_06'), size=plotlinewidth)+
 	geom_line( aes(y=x_7, col='x_07'), size=plotlinewidth)+
 	geom_line( aes(y=x_8, col='x_08'), size=plotlinewidth)+
 	geom_line( aes(y=x_9, col='x_09'), size=plotlinewidth)+
 	geom_line( aes(y=x_10, col='x_10'), size=plotlinewidth)+
 	geom_line( aes(y=x_11, col='x_11'), size=plotlinewidth)+
 	geom_line( aes(y=x_12, col='x_12'), size=plotlinewidth)+
 	geom_line( aes(y=x_13, col='x_13'), size=plotlinewidth)+
 	geom_line( aes(y=x_14, col='x_14'), size=plotlinewidth)+
 	geom_line( aes(y=x_15, col='x_15'), size=plotlinewidth)+
 	geom_line( aes(y=x_16, col='x_16'), size=plotlinewidth)+
 	geom_line( aes(y=x_17, col='x_17'), size=plotlinewidth)+
 	geom_line( aes(y=x_18, col='x_18'), size=plotlinewidth)+
 	geom_line( aes(y=x_19, col='x_19'), size=plotlinewidth)+
 	geom_line( aes(y=x_20, col='x_20'), size=plotlinewidth)+
 	geom_line( aes(y=x_21, col='x_21'), size=plotlinewidth)+
 	geom_line( aes(y=x_22, col='x_22'), size=plotlinewidth)+
 	geom_line( aes(y=x_23, col='x_23'), size=plotlinewidth)+
 	geom_line( aes(y=x_24, col='x_24'), size=plotlinewidth)+
 	geom_line( aes(y=x_25, col='x_25'), size=plotlinewidth)+
 	geom_line( aes(y=x_26, col='x_26'), size=plotlinewidth)+
 	geom_line( aes(y=x_27, col='x_27'), size=plotlinewidth)+
 	geom_line( aes(y=x_28, col='x_28'), size=plotlinewidth)+
 	geom_line( aes(y=x_29, col='x_29'), size=plotlinewidth)+
 	geom_line( aes(y=x_30, col='x_30'), size=plotlinewidth)+
 	geom_line( aes(y=x_31, col='x_31'), size=plotlinewidth)+
 	geom_line( aes(y=x_32, col='x_32'), size=plotlinewidth)+
 	geom_line( aes(y=x_33, col='x_33'), size=plotlinewidth)+
 	geom_line( aes(y=x_34, col='x_34'), size=plotlinewidth)+
 	geom_line( aes(y=x_35, col='x_35'), size=plotlinewidth)+
 	geom_line( aes(y=x_36, col='x_36'), size=plotlinewidth)+
 	geom_line( aes(y=x_37, col='x_37'), size=plotlinewidth)+
 	geom_line( aes(y=x_38, col='x_38'), size=plotlinewidth)+
 	geom_line( aes(y=x_39, col='x_39'), size=plotlinewidth)+
 	geom_line( aes(y=x_40, col='x_40'), size=plotlinewidth)+
 	geom_line( aes(y=x_41, col='x_41'), size=plotlinewidth)+
 	geom_line( aes(y=x_42, col='x_42'), size=plotlinewidth)+
 	geom_line( aes(y=x_43, col='x_43'), size=plotlinewidth)+
 	geom_line( aes(y=x_44, col='x_44'), size=plotlinewidth)+
 	geom_line( aes(y=x_45, col='x_45'), size=plotlinewidth)+
 	geom_line( aes(y=x_46, col='x_46'), size=plotlinewidth)+
	geom_line( aes(y=x_47, col='x_47'), size=plotlinewidth)+
 	geom_line( aes(y=x_48, col='x_48'), size=plotlinewidth)+
 	geom_line( aes(y=x_49, col='x_49'), size=plotlinewidth)+
 	geom_line( aes(y=x_50, col='x_50'), size=plotlinewidth)+
 	geom_line( aes(y=x_51, col='x_51'), size=plotlinewidth)+
 	geom_line( aes(y=x_52, col='x_52'), size=plotlinewidth)+
 	geom_line( aes(y=x_53, col='x_53'), size=plotlinewidth)+
 	geom_line( aes(y=x_54, col='x_54'), size=plotlinewidth)+
 	geom_line( aes(y=x_55, col='x_55'), size=plotlinewidth)+
 	geom_line( aes(y=x_56, col='x_56'), size=plotlinewidth)+
 	geom_line( aes(y=x_57, col='x_57'), size=plotlinewidth)+
 	geom_line( aes(y=x_58, col='x_58'), size=plotlinewidth)+
 	geom_line( aes(y=x_59, col='x_59'), size=plotlinewidth)+
 	geom_line( aes(y=x_60, col='x_60'), size=plotlinewidth)+
 	geom_line( aes(y=x_61, col='x_61'), size=plotlinewidth)+
 	geom_line( aes(y=x_62, col='x_62'), size=plotlinewidth)+
 	geom_line( aes(y=x_63, col='x_63'), size=plotlinewidth)+
 	geom_line( aes(y=x_64, col='x_64'), size=plotlinewidth)+
 	geom_line( aes(y=x_65, col='x_65'), size=plotlinewidth)+
 	geom_line( aes(y=x_66, col='x_66'), size=plotlinewidth)+
 	geom_line( aes(y=x_67, col='x_67'), size=plotlinewidth)+



	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_x_all.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()








##y_0,...y_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=y_0, col='y_00'), size=plotlinewidth)+
 	geom_line( aes(y=y_1, col='y_01'), size=plotlinewidth)+
 	geom_line( aes(y=y_2, col='y_02'), size=plotlinewidth)+
 	geom_line( aes(y=y_3, col='y_03'), size=plotlinewidth)+
 	geom_line( aes(y=y_4, col='y_04'), size=plotlinewidth)+
 	geom_line( aes(y=y_5, col='y_05'), size=plotlinewidth)+
 	geom_line( aes(y=y_6, col='y_06'), size=plotlinewidth)+
 	geom_line( aes(y=y_7, col='y_07'), size=plotlinewidth)+
 	geom_line( aes(y=y_8, col='y_08'), size=plotlinewidth)+
 	geom_line( aes(y=y_9, col='y_09'), size=plotlinewidth)+
 	geom_line( aes(y=y_10, col='y_10'), size=plotlinewidth)+
 	geom_line( aes(y=y_11, col='y_11'), size=plotlinewidth)+
 	geom_line( aes(y=y_12, col='y_12'), size=plotlinewidth)+
 	geom_line( aes(y=y_13, col='y_13'), size=plotlinewidth)+
 	geom_line( aes(y=y_14, col='y_14'), size=plotlinewidth)+
 	geom_line( aes(y=y_15, col='y_15'), size=plotlinewidth)+
 	geom_line( aes(y=y_16, col='y_16'), size=plotlinewidth)+
 	geom_line( aes(y=y_17, col='y_17'), size=plotlinewidth)+
 	geom_line( aes(y=y_18, col='y_18'), size=plotlinewidth)+
 	geom_line( aes(y=y_19, col='y_19'), size=plotlinewidth)+
 	geom_line( aes(y=y_20, col='y_20'), size=plotlinewidth)+
 	geom_line( aes(y=y_21, col='y_21'), size=plotlinewidth)+
 	geom_line( aes(y=y_22, col='y_22'), size=plotlinewidth)+
 	geom_line( aes(y=y_23, col='y_23'), size=plotlinewidth)+
 	geom_line( aes(y=y_24, col='y_24'), size=plotlinewidth)+
 	geom_line( aes(y=y_25, col='y_25'), size=plotlinewidth)+
 	geom_line( aes(y=y_26, col='y_26'), size=plotlinewidth)+
 	geom_line( aes(y=y_27, col='y_27'), size=plotlinewidth)+
 	geom_line( aes(y=y_28, col='y_28'), size=plotlinewidth)+
 	geom_line( aes(y=y_29, col='y_29'), size=plotlinewidth)+
 	geom_line( aes(y=y_30, col='y_30'), size=plotlinewidth)+
 	geom_line( aes(y=y_31, col='y_31'), size=plotlinewidth)+
 	geom_line( aes(y=y_32, col='y_32'), size=plotlinewidth)+
 	geom_line( aes(y=y_33, col='y_33'), size=plotlinewidth)+
 	geom_line( aes(y=y_34, col='y_34'), size=plotlinewidth)+
 	geom_line( aes(y=y_35, col='y_35'), size=plotlinewidth)+
 	geom_line( aes(y=y_36, col='y_36'), size=plotlinewidth)+
 	geom_line( aes(y=y_37, col='y_37'), size=plotlinewidth)+
 	geom_line( aes(y=y_38, col='y_38'), size=plotlinewidth)+
 	geom_line( aes(y=y_39, col='y_39'), size=plotlinewidth)+
 	geom_line( aes(y=y_40, col='y_40'), size=plotlinewidth)+
 	geom_line( aes(y=y_41, col='y_41'), size=plotlinewidth)+
 	geom_line( aes(y=y_42, col='y_42'), size=plotlinewidth)+
 	geom_line( aes(y=y_43, col='y_43'), size=plotlinewidth)+
 	geom_line( aes(y=y_44, col='y_44'), size=plotlinewidth)+
 	geom_line( aes(y=y_45, col='y_45'), size=plotlinewidth)+
 	geom_line( aes(y=y_46, col='y_46'), size=plotlinewidth)+
	geom_line( aes(y=y_47, col='y_47'), size=plotlinewidth)+
 	geom_line( aes(y=y_48, col='y_48'), size=plotlinewidth)+
 	geom_line( aes(y=y_49, col='y_49'), size=plotlinewidth)+
 	geom_line( aes(y=y_50, col='y_50'), size=plotlinewidth)+
 	geom_line( aes(y=y_51, col='y_51'), size=plotlinewidth)+
 	geom_line( aes(y=y_52, col='y_52'), size=plotlinewidth)+
 	geom_line( aes(y=y_53, col='y_53'), size=plotlinewidth)+
 	geom_line( aes(y=y_54, col='y_54'), size=plotlinewidth)+
 	geom_line( aes(y=y_55, col='y_55'), size=plotlinewidth)+
 	geom_line( aes(y=y_56, col='y_56'), size=plotlinewidth)+
 	geom_line( aes(y=y_57, col='y_57'), size=plotlinewidth)+
 	geom_line( aes(y=y_58, col='y_58'), size=plotlinewidth)+
 	geom_line( aes(y=y_59, col='y_59'), size=plotlinewidth)+
 	geom_line( aes(y=y_60, col='y_60'), size=plotlinewidth)+
 	geom_line( aes(y=y_61, col='y_61'), size=plotlinewidth)+
 	geom_line( aes(y=y_62, col='y_62'), size=plotlinewidth)+
 	geom_line( aes(y=y_63, col='y_63'), size=plotlinewidth)+
 	geom_line( aes(y=y_64, col='y_64'), size=plotlinewidth)+
 	geom_line( aes(y=y_65, col='y_65'), size=plotlinewidth)+
 	geom_line( aes(y=y_66, col='y_66'), size=plotlinewidth)+
 	geom_line( aes(y=y_67, col='y_67'), size=plotlinewidth)+



	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_y_all.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()









##X_0,...X_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=X_0, col='X_00'), size=plotlinewidth)+
 	geom_line( aes(y=X_1, col='X_01'), size=plotlinewidth)+
 	geom_line( aes(y=X_2, col='X_02'), size=plotlinewidth)+
 	geom_line( aes(y=X_3, col='X_03'), size=plotlinewidth)+
 	geom_line( aes(y=X_4, col='X_04'), size=plotlinewidth)+
 	geom_line( aes(y=X_5, col='X_05'), size=plotlinewidth)+
 	geom_line( aes(y=X_6, col='X_06'), size=plotlinewidth)+
 	geom_line( aes(y=X_7, col='X_07'), size=plotlinewidth)+
 	geom_line( aes(y=X_8, col='X_08'), size=plotlinewidth)+
 	geom_line( aes(y=X_9, col='X_09'), size=plotlinewidth)+
 	geom_line( aes(y=X_10, col='X_10'), size=plotlinewidth)+
 	geom_line( aes(y=X_11, col='X_11'), size=plotlinewidth)+
 	geom_line( aes(y=X_12, col='X_12'), size=plotlinewidth)+
 	geom_line( aes(y=X_13, col='X_13'), size=plotlinewidth)+
 	geom_line( aes(y=X_14, col='X_14'), size=plotlinewidth)+
 	geom_line( aes(y=X_15, col='X_15'), size=plotlinewidth)+
 	geom_line( aes(y=X_16, col='X_16'), size=plotlinewidth)+
 	geom_line( aes(y=X_17, col='X_17'), size=plotlinewidth)+
 	geom_line( aes(y=X_18, col='X_18'), size=plotlinewidth)+
 	geom_line( aes(y=X_19, col='X_19'), size=plotlinewidth)+
 	geom_line( aes(y=X_20, col='X_20'), size=plotlinewidth)+
 	geom_line( aes(y=X_21, col='X_21'), size=plotlinewidth)+
 	geom_line( aes(y=X_22, col='X_22'), size=plotlinewidth)+
 	geom_line( aes(y=X_23, col='X_23'), size=plotlinewidth)+
 	geom_line( aes(y=X_24, col='X_24'), size=plotlinewidth)+
 	geom_line( aes(y=X_25, col='X_25'), size=plotlinewidth)+
 	geom_line( aes(y=X_26, col='X_26'), size=plotlinewidth)+
 	geom_line( aes(y=X_27, col='X_27'), size=plotlinewidth)+
 	geom_line( aes(y=X_28, col='X_28'), size=plotlinewidth)+
 	geom_line( aes(y=X_29, col='X_29'), size=plotlinewidth)+
 	geom_line( aes(y=X_30, col='X_30'), size=plotlinewidth)+
 	geom_line( aes(y=X_31, col='X_31'), size=plotlinewidth)+
 	geom_line( aes(y=X_32, col='X_32'), size=plotlinewidth)+
 	geom_line( aes(y=X_33, col='X_33'), size=plotlinewidth)+
 	geom_line( aes(y=X_34, col='X_34'), size=plotlinewidth)+
 	geom_line( aes(y=X_35, col='X_35'), size=plotlinewidth)+
 	geom_line( aes(y=X_36, col='X_36'), size=plotlinewidth)+
 	geom_line( aes(y=X_37, col='X_37'), size=plotlinewidth)+
 	geom_line( aes(y=X_38, col='X_38'), size=plotlinewidth)+
 	geom_line( aes(y=X_39, col='X_39'), size=plotlinewidth)+
 	geom_line( aes(y=X_40, col='X_40'), size=plotlinewidth)+
 	geom_line( aes(y=X_41, col='X_41'), size=plotlinewidth)+
 	geom_line( aes(y=X_42, col='X_42'), size=plotlinewidth)+
 	geom_line( aes(y=X_43, col='X_43'), size=plotlinewidth)+
 	geom_line( aes(y=X_44, col='X_44'), size=plotlinewidth)+
 	geom_line( aes(y=X_45, col='X_45'), size=plotlinewidth)+
 	geom_line( aes(y=X_46, col='X_46'), size=plotlinewidth)+
	geom_line( aes(y=X_47, col='X_47'), size=plotlinewidth)+
 	geom_line( aes(y=X_48, col='X_48'), size=plotlinewidth)+
 	geom_line( aes(y=X_49, col='X_49'), size=plotlinewidth)+
 	geom_line( aes(y=X_50, col='X_50'), size=plotlinewidth)+
 	geom_line( aes(y=X_51, col='X_51'), size=plotlinewidth)+
 	geom_line( aes(y=X_52, col='X_52'), size=plotlinewidth)+
 	geom_line( aes(y=X_53, col='X_53'), size=plotlinewidth)+
 	geom_line( aes(y=X_54, col='X_54'), size=plotlinewidth)+
 	geom_line( aes(y=X_55, col='X_55'), size=plotlinewidth)+
 	geom_line( aes(y=X_56, col='X_56'), size=plotlinewidth)+
 	geom_line( aes(y=X_57, col='X_57'), size=plotlinewidth)+
 	geom_line( aes(y=X_58, col='X_58'), size=plotlinewidth)+
 	geom_line( aes(y=X_59, col='X_59'), size=plotlinewidth)+
 	geom_line( aes(y=X_60, col='X_60'), size=plotlinewidth)+
 	geom_line( aes(y=X_61, col='X_61'), size=plotlinewidth)+
 	geom_line( aes(y=X_62, col='X_62'), size=plotlinewidth)+
 	geom_line( aes(y=X_63, col='X_63'), size=plotlinewidth)+
 	geom_line( aes(y=X_64, col='X_64'), size=plotlinewidth)+
 	geom_line( aes(y=X_65, col='X_65'), size=plotlinewidth)+
 	geom_line( aes(y=X_66, col='X_66'), size=plotlinewidth)+
 	geom_line( aes(y=X_67, col='X_67'), size=plotlinewidth)+



	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Landmarks Location in 3D [Milimetres]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_X_all.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


##Y_0,...Y_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=Y_0, col='Y_00'), size=plotlinewidth)+
 	geom_line( aes(y=Y_1, col='Y_01'), size=plotlinewidth)+
 	geom_line( aes(y=Y_2, col='Y_02'), size=plotlinewidth)+
 	geom_line( aes(y=Y_3, col='Y_03'), size=plotlinewidth)+
 	geom_line( aes(y=Y_4, col='Y_04'), size=plotlinewidth)+
 	geom_line( aes(y=Y_5, col='Y_05'), size=plotlinewidth)+
 	geom_line( aes(y=Y_6, col='Y_06'), size=plotlinewidth)+
 	geom_line( aes(y=Y_7, col='Y_07'), size=plotlinewidth)+
 	geom_line( aes(y=Y_8, col='Y_08'), size=plotlinewidth)+
 	geom_line( aes(y=Y_9, col='Y_09'), size=plotlinewidth)+
 	geom_line( aes(y=Y_10, col='Y_10'), size=plotlinewidth)+
 	geom_line( aes(y=Y_11, col='Y_11'), size=plotlinewidth)+
 	geom_line( aes(y=Y_12, col='Y_12'), size=plotlinewidth)+
 	geom_line( aes(y=Y_13, col='Y_13'), size=plotlinewidth)+
 	geom_line( aes(y=Y_14, col='Y_14'), size=plotlinewidth)+
 	geom_line( aes(y=Y_15, col='Y_15'), size=plotlinewidth)+
 	geom_line( aes(y=Y_16, col='Y_16'), size=plotlinewidth)+
 	geom_line( aes(y=Y_17, col='Y_17'), size=plotlinewidth)+
 	geom_line( aes(y=Y_18, col='Y_18'), size=plotlinewidth)+
 	geom_line( aes(y=Y_19, col='Y_19'), size=plotlinewidth)+
 	geom_line( aes(y=Y_20, col='Y_20'), size=plotlinewidth)+
 	geom_line( aes(y=Y_21, col='Y_21'), size=plotlinewidth)+
 	geom_line( aes(y=Y_22, col='Y_22'), size=plotlinewidth)+
 	geom_line( aes(y=Y_23, col='Y_23'), size=plotlinewidth)+
 	geom_line( aes(y=Y_24, col='Y_24'), size=plotlinewidth)+
 	geom_line( aes(y=Y_25, col='Y_25'), size=plotlinewidth)+
 	geom_line( aes(y=Y_26, col='Y_26'), size=plotlinewidth)+
 	geom_line( aes(y=Y_27, col='Y_27'), size=plotlinewidth)+
 	geom_line( aes(y=Y_28, col='Y_28'), size=plotlinewidth)+
 	geom_line( aes(y=Y_29, col='Y_29'), size=plotlinewidth)+
 	geom_line( aes(y=Y_30, col='Y_30'), size=plotlinewidth)+
 	geom_line( aes(y=Y_31, col='Y_31'), size=plotlinewidth)+
 	geom_line( aes(y=Y_32, col='Y_32'), size=plotlinewidth)+
 	geom_line( aes(y=Y_33, col='Y_33'), size=plotlinewidth)+
 	geom_line( aes(y=Y_34, col='Y_34'), size=plotlinewidth)+
 	geom_line( aes(y=Y_35, col='Y_35'), size=plotlinewidth)+
 	geom_line( aes(y=Y_36, col='Y_36'), size=plotlinewidth)+
 	geom_line( aes(y=Y_37, col='Y_37'), size=plotlinewidth)+
 	geom_line( aes(y=Y_38, col='Y_38'), size=plotlinewidth)+
 	geom_line( aes(y=Y_39, col='Y_39'), size=plotlinewidth)+
 	geom_line( aes(y=Y_40, col='Y_40'), size=plotlinewidth)+
 	geom_line( aes(y=Y_41, col='Y_41'), size=plotlinewidth)+
 	geom_line( aes(y=Y_42, col='Y_42'), size=plotlinewidth)+
 	geom_line( aes(y=Y_43, col='Y_43'), size=plotlinewidth)+
 	geom_line( aes(y=Y_44, col='Y_44'), size=plotlinewidth)+
 	geom_line( aes(y=Y_45, col='Y_45'), size=plotlinewidth)+
 	geom_line( aes(y=Y_46, col='Y_46'), size=plotlinewidth)+
	geom_line( aes(y=Y_47, col='Y_47'), size=plotlinewidth)+
 	geom_line( aes(y=Y_48, col='Y_48'), size=plotlinewidth)+
 	geom_line( aes(y=Y_49, col='Y_49'), size=plotlinewidth)+
 	geom_line( aes(y=Y_50, col='Y_50'), size=plotlinewidth)+
 	geom_line( aes(y=Y_51, col='Y_51'), size=plotlinewidth)+
 	geom_line( aes(y=Y_52, col='Y_52'), size=plotlinewidth)+
 	geom_line( aes(y=Y_53, col='Y_53'), size=plotlinewidth)+
 	geom_line( aes(y=Y_54, col='Y_54'), size=plotlinewidth)+
 	geom_line( aes(y=Y_55, col='Y_55'), size=plotlinewidth)+
 	geom_line( aes(y=Y_56, col='Y_56'), size=plotlinewidth)+
 	geom_line( aes(y=Y_57, col='Y_57'), size=plotlinewidth)+
 	geom_line( aes(y=Y_58, col='Y_58'), size=plotlinewidth)+
 	geom_line( aes(y=Y_59, col='Y_59'), size=plotlinewidth)+
 	geom_line( aes(y=Y_60, col='Y_60'), size=plotlinewidth)+
 	geom_line( aes(y=Y_61, col='Y_61'), size=plotlinewidth)+
 	geom_line( aes(y=Y_62, col='Y_62'), size=plotlinewidth)+
 	geom_line( aes(y=Y_63, col='Y_63'), size=plotlinewidth)+
 	geom_line( aes(y=Y_64, col='Y_64'), size=plotlinewidth)+
 	geom_line( aes(y=Y_65, col='Y_65'), size=plotlinewidth)+
 	geom_line( aes(y=Y_66, col='Y_66'), size=plotlinewidth)+
 	geom_line( aes(y=Y_67, col='Y_67'), size=plotlinewidth)+



	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Landmarks Location in 3D [Milimetres]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_Y_all.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



##Z_0,...Z_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
 	geom_line( aes(y=Z_0, col='Z_00'), size=plotlinewidth)+
 	geom_line( aes(y=Z_1, col='Z_01'), size=plotlinewidth)+
 	geom_line( aes(y=Z_2, col='Z_02'), size=plotlinewidth)+
 	geom_line( aes(y=Z_3, col='Z_03'), size=plotlinewidth)+
 	geom_line( aes(y=Z_4, col='Z_04'), size=plotlinewidth)+
 	geom_line( aes(y=Z_5, col='Z_05'), size=plotlinewidth)+
 	geom_line( aes(y=Z_6, col='Z_06'), size=plotlinewidth)+
 	geom_line( aes(y=Z_7, col='Z_07'), size=plotlinewidth)+
 	geom_line( aes(y=Z_8, col='Z_08'), size=plotlinewidth)+
 	geom_line( aes(y=Z_9, col='Z_09'), size=plotlinewidth)+
 	geom_line( aes(y=Z_10, col='Z_10'), size=plotlinewidth)+
 	geom_line( aes(y=Z_11, col='Z_11'), size=plotlinewidth)+
 	geom_line( aes(y=Z_12, col='Z_12'), size=plotlinewidth)+
 	geom_line( aes(y=Z_13, col='Z_13'), size=plotlinewidth)+
 	geom_line( aes(y=Z_14, col='Z_14'), size=plotlinewidth)+
 	geom_line( aes(y=Z_15, col='Z_15'), size=plotlinewidth)+
 	geom_line( aes(y=Z_16, col='Z_16'), size=plotlinewidth)+
 	geom_line( aes(y=Z_17, col='Z_17'), size=plotlinewidth)+
 	geom_line( aes(y=Z_18, col='Z_18'), size=plotlinewidth)+
 	geom_line( aes(y=Z_19, col='Z_19'), size=plotlinewidth)+
 	geom_line( aes(y=Z_20, col='Z_20'), size=plotlinewidth)+
 	geom_line( aes(y=Z_21, col='Z_21'), size=plotlinewidth)+
 	geom_line( aes(y=Z_22, col='Z_22'), size=plotlinewidth)+
 	geom_line( aes(y=Z_23, col='Z_23'), size=plotlinewidth)+
 	geom_line( aes(y=Z_24, col='Z_24'), size=plotlinewidth)+
 	geom_line( aes(y=Z_25, col='Z_25'), size=plotlinewidth)+
 	geom_line( aes(y=Z_26, col='Z_26'), size=plotlinewidth)+
 	geom_line( aes(y=Z_27, col='Z_27'), size=plotlinewidth)+
 	geom_line( aes(y=Z_28, col='Z_28'), size=plotlinewidth)+
 	geom_line( aes(y=Z_29, col='Z_29'), size=plotlinewidth)+
 	geom_line( aes(y=Z_30, col='Z_30'), size=plotlinewidth)+
 	geom_line( aes(y=Z_31, col='Z_31'), size=plotlinewidth)+
 	geom_line( aes(y=Z_32, col='Z_32'), size=plotlinewidth)+
 	geom_line( aes(y=Z_33, col='Z_33'), size=plotlinewidth)+
 	geom_line( aes(y=Z_34, col='Z_34'), size=plotlinewidth)+
 	geom_line( aes(y=Z_35, col='Z_35'), size=plotlinewidth)+
 	geom_line( aes(y=Z_36, col='Z_36'), size=plotlinewidth)+
 	geom_line( aes(y=Z_37, col='Z_37'), size=plotlinewidth)+
 	geom_line( aes(y=Z_38, col='Z_38'), size=plotlinewidth)+
 	geom_line( aes(y=Z_39, col='Z_39'), size=plotlinewidth)+
 	geom_line( aes(y=Z_40, col='Z_40'), size=plotlinewidth)+
 	geom_line( aes(y=Z_41, col='Z_41'), size=plotlinewidth)+
 	geom_line( aes(y=Z_42, col='Z_42'), size=plotlinewidth)+
 	geom_line( aes(y=Z_43, col='Z_43'), size=plotlinewidth)+
 	geom_line( aes(y=Z_44, col='Z_44'), size=plotlinewidth)+
 	geom_line( aes(y=Z_45, col='Z_45'), size=plotlinewidth)+
 	geom_line( aes(y=Z_46, col='Z_46'), size=plotlinewidth)+
	geom_line( aes(y=Z_47, col='Z_47'), size=plotlinewidth)+
 	geom_line( aes(y=Z_48, col='Z_48'), size=plotlinewidth)+
 	geom_line( aes(y=Z_49, col='Z_49'), size=plotlinewidth)+
 	geom_line( aes(y=Z_50, col='Z_50'), size=plotlinewidth)+
 	geom_line( aes(y=Z_51, col='Z_51'), size=plotlinewidth)+
 	geom_line( aes(y=Z_52, col='Z_52'), size=plotlinewidth)+
 	geom_line( aes(y=Z_53, col='Z_53'), size=plotlinewidth)+
 	geom_line( aes(y=Z_54, col='Z_54'), size=plotlinewidth)+
 	geom_line( aes(y=Z_55, col='Z_55'), size=plotlinewidth)+
 	geom_line( aes(y=Z_56, col='Z_56'), size=plotlinewidth)+
 	geom_line( aes(y=Z_57, col='Z_57'), size=plotlinewidth)+
 	geom_line( aes(y=Z_58, col='Z_58'), size=plotlinewidth)+
 	geom_line( aes(y=Z_59, col='Z_59'), size=plotlinewidth)+
 	geom_line( aes(y=Z_60, col='Z_60'), size=plotlinewidth)+
 	geom_line( aes(y=Z_61, col='Z_61'), size=plotlinewidth)+
 	geom_line( aes(y=Z_62, col='Z_62'), size=plotlinewidth)+
 	geom_line( aes(y=Z_63, col='Z_63'), size=plotlinewidth)+
 	geom_line( aes(y=Z_64, col='Z_64'), size=plotlinewidth)+
 	geom_line( aes(y=Z_65, col='Z_65'), size=plotlinewidth)+
 	geom_line( aes(y=Z_66, col='Z_66'), size=plotlinewidth)+
 	geom_line( aes(y=Z_67, col='Z_67'), size=plotlinewidth)+



	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Landmarks Location in 3D [Milimetres]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_Z_all.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



 
#p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, p_16, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_0, col='p_00'), size=plotlinewidth)+
  geom_line( aes(y=p_1, col='p_01'), size=plotlinewidth)+
  geom_line( aes(y=p_2, col='p_02'), size=plotlinewidth)+
  geom_line( aes(y=p_3, col='p_03'), size=plotlinewidth)+
  geom_line( aes(y=p_4, col='p_04'), size=plotlinewidth)+
  geom_line( aes(y=p_5, col='p_05'), size=plotlinewidth)+
  geom_line( aes(y=p_6, col='p_06'), size=plotlinewidth)+
  geom_line( aes(y=p_7, col='p_07'), size=plotlinewidth)+
  geom_line( aes(y=p_8, col='p_08'), size=plotlinewidth)+
  geom_line( aes(y=p_9, col='p_09'), size=plotlinewidth)+
  geom_line( aes(y=p_10, col='p_10'), size=plotlinewidth)+
  geom_line( aes(y=p_11, col='p_11'), size=plotlinewidth)+
  geom_line( aes(y=p_12, col='p_12'), size=plotlinewidth)+
  geom_line( aes(y=p_13, col='p_13'), size=plotlinewidth)+
  geom_line( aes(y=p_14, col='p_14'), size=plotlinewidth)+
  geom_line( aes(y=p_15, col='p_15'), size=plotlinewidth)+
  geom_line( aes(y=p_16, col='p_16'), size=plotlinewidth)+

	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('non-rigid shape parameters') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_p0top16_nonrigidparameters.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



#p_17, p_18, p_19, p_20, p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31, p_32, p_33, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_17, col='p_17'), size=plotlinewidth)+
  geom_line( aes(y=p_18, col='p_18'), size=plotlinewidth)+
  geom_line( aes(y=p_19, col='p_19'), size=plotlinewidth)+
  geom_line( aes(y=p_20, col='p_20'), size=plotlinewidth)+
  geom_line( aes(y=p_21, col='p_21'), size=plotlinewidth)+
  geom_line( aes(y=p_22, col='p_22'), size=plotlinewidth)+
  geom_line( aes(y=p_23, col='p_23'), size=plotlinewidth)+
  geom_line( aes(y=p_24, col='p_24'), size=plotlinewidth)+
  geom_line( aes(y=p_25, col='p_25'), size=plotlinewidth)+
  geom_line( aes(y=p_26, col='p_26'), size=plotlinewidth)+
  geom_line( aes(y=p_27, col='p_27'), size=plotlinewidth)+
  geom_line( aes(y=p_28, col='p_28'), size=plotlinewidth)+
  geom_line( aes(y=p_29, col='p_29'), size=plotlinewidth)+
  geom_line( aes(y=p_30, col='p_30'), size=plotlinewidth)+
  geom_line( aes(y=p_31, col='p_31'), size=plotlinewidth)+
  geom_line( aes(y=p_32, col='p_32'), size=plotlinewidth)+
  geom_line( aes(y=p_33, col='p_33'), size=plotlinewidth)+
 
	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('non-rigid shape parameters') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_p17top33_nonrigidparameters.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




#p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20, p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31, p_32, p_33, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_0, col='p_00'), size=plotlinewidth)+
  geom_line( aes(y=p_1, col='p_01'), size=plotlinewidth)+
  geom_line( aes(y=p_2, col='p_02'), size=plotlinewidth)+
  geom_line( aes(y=p_3, col='p_03'), size=plotlinewidth)+
  geom_line( aes(y=p_4, col='p_04'), size=plotlinewidth)+
  geom_line( aes(y=p_5, col='p_05'), size=plotlinewidth)+
  geom_line( aes(y=p_6, col='p_06'), size=plotlinewidth)+
  geom_line( aes(y=p_7, col='p_07'), size=plotlinewidth)+
  geom_line( aes(y=p_8, col='p_08'), size=plotlinewidth)+
  geom_line( aes(y=p_9, col='p_09'), size=plotlinewidth)+
  geom_line( aes(y=p_10, col='p_10'), size=plotlinewidth)+
  geom_line( aes(y=p_11, col='p_11'), size=plotlinewidth)+
  geom_line( aes(y=p_12, col='p_12'), size=plotlinewidth)+
  geom_line( aes(y=p_13, col='p_13'), size=plotlinewidth)+
  geom_line( aes(y=p_14, col='p_14'), size=plotlinewidth)+
  geom_line( aes(y=p_15, col='p_15'), size=plotlinewidth)+
  geom_line( aes(y=p_16, col='p_16'), size=plotlinewidth)+
  geom_line( aes(y=p_17, col='p_17'), size=plotlinewidth)+
  geom_line( aes(y=p_18, col='p_18'), size=plotlinewidth)+
  geom_line( aes(y=p_19, col='p_19'), size=plotlinewidth)+
  geom_line( aes(y=p_20, col='p_20'), size=plotlinewidth)+
  geom_line( aes(y=p_21, col='p_21'), size=plotlinewidth)+
  geom_line( aes(y=p_22, col='p_22'), size=plotlinewidth)+
  geom_line( aes(y=p_23, col='p_23'), size=plotlinewidth)+
  geom_line( aes(y=p_24, col='p_24'), size=plotlinewidth)+
  geom_line( aes(y=p_25, col='p_25'), size=plotlinewidth)+
  geom_line( aes(y=p_26, col='p_26'), size=plotlinewidth)+
  geom_line( aes(y=p_27, col='p_27'), size=plotlinewidth)+
  geom_line( aes(y=p_28, col='p_28'), size=plotlinewidth)+
  geom_line( aes(y=p_29, col='p_29'), size=plotlinewidth)+
  geom_line( aes(y=p_30, col='p_30'), size=plotlinewidth)+
  geom_line( aes(y=p_31, col='p_31'), size=plotlinewidth)+
  geom_line( aes(y=p_32, col='p_32'), size=plotlinewidth)+
  geom_line( aes(y=p_33, col='p_33'), size=plotlinewidth)+
 
	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('non-rigid shape parameters') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_p0top33_nonrigidparameters.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



##p_tx, p_ty, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_tx, col='p_tx'), size=plotlinewidth)+
  geom_line( aes(y=p_ty, col='p_ty'), size=plotlinewidth)+
 
	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('Translation terms of the Point Distrubution Model') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_ptxy_pdm.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


##p_scale, p_rx, p_ry, p_rz 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_scale, col='p_scale'), size=plotlinewidth)+
  geom_line( aes(y=p_rx, col='p_rx'), size=plotlinewidth)+
  geom_line( aes(y=p_ry, col='p_ry'), size=plotlinewidth)+
  geom_line( aes(y=p_rz, col='p_rz'), size=plotlinewidth)+
 
	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('scale and rotation terms of the Point Distrubution Model') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_pscalerxyz_pdm.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






# The presense (0 absent, 1 present) of 18 AUs:
# `AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c`
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU01_c), col='AU01'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU02_c), col='AU02'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU04_c), col='AU04'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU05_c), col='AU05'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU06_c), col='AU06'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU07_c), col='AU07'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU09_c), col='AU09'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU10_c), col='AU10'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU12_c), col='AU12'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU14_c), col='AU14'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU15_c), col='AU15'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU17_c), col='AU17'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU20_c), col='AU20'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU23_c), col='AU23'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU25_c), col='AU25'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU26_c), col='AU26'), size=plotlinewidth)+ 
  geom_line( aes(y=as.numeric(AU28_c), col='AU28'), size=plotlinewidth)+
  geom_line( aes(y=as.numeric(AU45_c), col='AU45'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=c(-0.5,1.5))+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU presence (0 absent, 1 present)') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_AU_presence.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





# The intensity (from 0 to 5) of 18 AUs:
# `AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, AU45_r`
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=AU01_r, col='AU01'), size=plotlinewidth)+
  geom_line( aes(y=AU02_r, col='AU02'), size=plotlinewidth)+
  geom_line( aes(y=AU04_r, col='AU04'), size=plotlinewidth)+
  geom_line( aes(y=AU05_r, col='AU05'), size=plotlinewidth)+
  geom_line( aes(y=AU06_r, col='AU06'), size=plotlinewidth)+
  geom_line( aes(y=AU07_r, col='AU07'), size=plotlinewidth)+
  geom_line( aes(y=AU09_r, col='AU09'), size=plotlinewidth)+
  geom_line( aes(y=AU10_r, col='AU10'), size=plotlinewidth)+
  geom_line( aes(y=AU12_r, col='AU12'), size=plotlinewidth)+
  geom_line( aes(y=AU14_r, col='AU14'), size=plotlinewidth)+
  geom_line( aes(y=AU15_r, col='AU15'), size=plotlinewidth)+
  geom_line( aes(y=AU17_r, col='AU17'), size=plotlinewidth)+
  geom_line( aes(y=AU20_r, col='AU20'), size=plotlinewidth)+
  geom_line( aes(y=AU23_r, col='AU23'), size=plotlinewidth)+
  geom_line( aes(y=AU25_r, col='AU25'), size=plotlinewidth)+
  geom_line( aes(y=AU26_r, col='AU26'), size=plotlinewidth)+ 
  geom_line( aes(y=AU45_r, col='AU45'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU_intensity.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU01_c), col='AU01_c'), size=plotlinewidth)+
  geom_line( aes(y=AU01_r, col='AU01_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU01_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU02_c), col='AU02_c'), size=plotlinewidth)+
  geom_line( aes(y=AU02_r, col='AU02_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU02_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU04_c), col='AU04_c'), size=plotlinewidth)+
  geom_line( aes(y=AU04_r, col='AU04_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU04_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU05_c), col='AU05_c'), size=plotlinewidth)+
  geom_line( aes(y=AU05_r, col='AU05_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU05_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU06_c), col='AU06_c'), size=plotlinewidth)+
  geom_line( aes(y=AU06_r, col='AU06_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU06_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU07_c), col='AU07_c'), size=plotlinewidth)+
  geom_line( aes(y=AU07_r, col='AU07_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU07_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU09_c), col='AU09_c'), size=plotlinewidth)+
  geom_line( aes(y=AU09_r, col='AU09_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU09_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU10_c), col='AU10_c'), size=plotlinewidth)+
  geom_line( aes(y=AU10_r, col='AU10_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU10_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU12_c), col='AU12_c'), size=plotlinewidth)+
  geom_line( aes(y=AU12_r, col='AU12_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU12_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU14_c), col='AU14_c'), size=plotlinewidth)+
  geom_line( aes(y=AU14_r, col='AU14_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU14_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU15_c), col='AU15_c'), size=plotlinewidth)+
  geom_line( aes(y=AU15_r, col='AU15_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU15_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU17_c), col='AU17_c'), size=plotlinewidth)+
  geom_line( aes(y=AU17_r, col='AU17_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU17_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU20_c), col='AU20_c'), size=plotlinewidth)+
  geom_line( aes(y=AU20_r, col='AU20_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU20_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU23_c), col='AU23_c'), size=plotlinewidth)+
  geom_line( aes(y=AU23_r, col='AU23_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU23_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU25_c), col='AU25_c'), size=plotlinewidth)+
  geom_line( aes(y=AU25_r, col='AU25_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU25_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU26_c), col='AU26_c'), size=plotlinewidth)+
  geom_line( aes(y=AU26_r, col='AU26_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU26_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

######################################
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=as.numeric(AU45_c), col='AU45_c'), size=plotlinewidth)+
  geom_line( aes(y=AU45_r, col='AU45_r'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=NULL )+ 
	facet_grid(participant ~ . )+
  	theme_bw(20) +
	ylab('AU_c presence (0 absent, 1 present); AU_r intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU45_cr.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


} ## if (PLOTTING_TIMESERIES == TRUE) {


################################################################################
# (4) Creating Preprossed Data Path

odata_path <- paste(outcomes_path,relativeodatapath,sep="")
if (file.exists(odata_path)){
    setwd(file.path(odata_path))
} else {
  dir.create(odata_path, recursive=TRUE)
  setwd(file.path(odata_path))
}




################################################################################
####  (5)  Writing Data
write.table(datatable, "rawopenfacedata-v00.datatable", row.name=FALSE)

message('datatable file has been created at '  )
message (odata_path)




#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


