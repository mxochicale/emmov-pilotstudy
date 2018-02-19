
###############################################################################	
#
# Time series plots for the openface landmarks 
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:perez.xochicale@gmail.com
# please email me directly if you see any errors or have any questions
# last update: 17 February 2018
#
###############################################################################	
	# OUTLINE:
	# (0) Loading libraries and functions
 	# (1) Definifing paths and Reading data
	# (2) Data Filtering
		# (2.1) Windowing
	# (3) Plotting
		# (3.1) Creating and changing plotting paths
		# (3.2) Plots features
		# (3.3) Plots data from openface


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




################################################################################
# (1) Setting DataSets paths and reading data
setwd(main_data_path)
data_path_list <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

participantsNN <- 7
trialsNN <- 1
participant_index <- c(2,4,6,8,10,12,14)


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
datable <- pNN_tmp




################################################################################
# (2) Data Filtering


################################
### (2.1) Windowing Data [xdata[,.SD[1:2],by=.(Participant,Activity,Sensor)]]

windowframe = 200:1500;
xdata <- datable[,.SD[windowframe],by=.(participant,trial)];



################################################################################
# (3) Plotting
#


################################################################################
# (3.1) Creating  and Changing to PlotPath
#
plot_path <- paste(outcomes_path,"/plots_timeseries",sep="")
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



 ##Confidence and success
plot <- ggplot(xdata, aes(x=frame) ) +	
	geom_line( aes(y=confidence, col='confidence'), size=plotlinewidth )+
	geom_line( aes(y=success, col='success'), size=plotlinewidth )+

	facet_grid(participant ~ . )+
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

	ylab('Raw Values') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_gaze0_xyz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




# Gaze 1
plot <- ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=gaze_1_x, color='gaze_1_x'), size=plotlinewidth)+
  geom_line( aes(y=gaze_1_y, color='gaze_1_y'), size=plotlinewidth)+
  geom_line( aes(y=gaze_1_z, color='gaze_1_z'), size=plotlinewidth)+
  facet_grid(participant~.)+

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

	coord_cartesian(xlim=NULL, ylim=c(-150,150))+
	facet_grid(participant ~ . )+
	ylab('Location of the head with respect to the camera [millimetre]') + 
	xlab('Sample')+
	labs(colour = 'Feature')

png(filename= paste(tag,"_pose_TxTy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




plot <- ggplot(xdata, aes(x=frame) ) +	
   geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+

	coord_cartesian(xlim=NULL, ylim=c(800,1200))+
	facet_grid(participant ~ . )+
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

	coord_cartesian(xlim=NULL, ylim=c(-0.5,0.5))+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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


	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
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
	ylab('Landmarks Location in 2D [Pixels]') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_y_all.png",sep=''),
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
	ylab('non-rigid shape parameters') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_p0top33_nonrigidparameters.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



##p_scale, p_rx, p_ry, p_rz, p_tx, p_ty, 
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=p_scale, col='p_scale'), size=plotlinewidth)+
  geom_line( aes(y=p_rx, col='p_rx'), size=plotlinewidth)+
  geom_line( aes(y=p_ry, col='p_ry'), size=plotlinewidth)+
  geom_line( aes(y=p_rz, col='p_rz'), size=plotlinewidth)+
  geom_line( aes(y=p_tx, col='p_tx'), size=plotlinewidth)+
  geom_line( aes(y=p_ty, col='p_ty'), size=plotlinewidth)+
 
	coord_cartesian(xlim=NULL, ylim=NULL)+
	facet_grid(participant ~ . )+
	ylab('scale, rotation and translation terms of the Point Distrubution Model') + 
	xlab('Sample')+
	labs(colour = 'Feature')


png(filename= paste(tag,"_pscalerxyztxy_pdm.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()




# The presense (0 absent, 1 present) of 18 AUs:
# `AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c`
plot <-	ggplot(xdata, aes(x=frame) ) +	
  geom_line( aes(y=AU01_c, col='AU01'), size=plotlinewidth)+
  geom_line( aes(y=AU02_c, col='AU02'), size=plotlinewidth)+
  geom_line( aes(y=AU04_c, col='AU04'), size=plotlinewidth)+
  geom_line( aes(y=AU05_c, col='AU05'), size=plotlinewidth)+
  geom_line( aes(y=AU06_c, col='AU06'), size=plotlinewidth)+
  geom_line( aes(y=AU07_c, col='AU07'), size=plotlinewidth)+
  geom_line( aes(y=AU09_c, col='AU09'), size=plotlinewidth)+
  geom_line( aes(y=AU10_c, col='AU10'), size=plotlinewidth)+
  geom_line( aes(y=AU12_c, col='AU12'), size=plotlinewidth)+
  geom_line( aes(y=AU14_c, col='AU14'), size=plotlinewidth)+
  geom_line( aes(y=AU15_c, col='AU15'), size=plotlinewidth)+
  geom_line( aes(y=AU17_c, col='AU17'), size=plotlinewidth)+
  geom_line( aes(y=AU20_c, col='AU20'), size=plotlinewidth)+
  geom_line( aes(y=AU23_c, col='AU23'), size=plotlinewidth)+
  geom_line( aes(y=AU25_c, col='AU25'), size=plotlinewidth)+
  geom_line( aes(y=AU26_c, col='AU26'), size=plotlinewidth)+ 
  geom_line( aes(y=AU28_c, col='AU28'), size=plotlinewidth)+
  geom_line( aes(y=AU45_c, col='AU45'), size=plotlinewidth)+
  

	coord_cartesian(xlim=NULL, ylim=c(-0.5,1.5))+ 
	facet_grid(participant ~ . )+
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
	ylab('AU intensity (from 0 to 5)') + 
	xlab('Sample')+
	labs(colour = 'Feature')



png(filename= paste(tag,"_AU_intensity.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()












#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


