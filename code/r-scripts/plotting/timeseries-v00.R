
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
#preprossededata_path <- paste(outcomes_path,"/preProcessedDataTable_p01_to_p22",sep="")
#setwd(file.path(preprossededata_path))
#feature_path <- "xxxxxxxxxxxxxx"




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
sensortag <- 'plot'
plotlinewidtg <- 0.7
image_width <- 1208
image_height <- 2480
image_dpi <- 300
image_bg <- "transparent"


#####################
## (3.3) Plots Data from OpenFace

#Plot <- ggplot(xdata)+geom_line( aes(x=frame,y=pose_Tx, color=trial), size=1.5)+facet_grid(participant~trial);Plot

# ##Confidence and success
#Plot <- ggplot(xdata)+
#	geom_line( aes(x=frame,y=confidence), size=1.5)+
#	geom_line( aes(x=frame,y=success), size=1.5)+
#	facet_grid(participant ~ . )
#Plot



## Gaze 1
#Plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=gaze_1_x, color='red'), size=1)+
#  geom_line( aes(x=frame,y=gaze_1_y, color='blue'), size=1)+
#  geom_line( aes(x=frame,y=gaze_1_z, color='green'), size=1)+
#  facet_grid(participant~.)
#Plot
#

### Gaze 0
# Plot <- ggplot(xdata)+
#   geom_line( aes(x=frame,y=gaze_0_x, color='red'), size=1)+
#   geom_line( aes(x=frame,y=gaze_0_y, color='blue'), size=1)+
#   geom_line( aes(x=frame,y=gaze_0_z, color='green'), size=1)+
#   facet_grid(participant~.)
#Plot
#

## Pose estimation with respect to the camera

#Plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=pose_Tx, color='blue'), size=2)+
#  scale_color_manual(labels = c("Tx"), values = c("blue")) +
#  coord_cartesian(xlim=NULL, ylim=c(-150,150))+
#  facet_grid(participant~trial);
#png(filename= paste("Tx.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#Plot
#dev.off()



#Plot <- ggplot(xdata)+
#   geom_line( aes(x=frame,y=pose_Tz, color='green2'), size=1)+
#   scale_color_manual(labels = c("Tz"), values = c("green2")) +
#   coord_cartesian(xlim=NULL, ylim=NULL )+
#   facet_grid(participant~.)
#Plot

#Plot <- ggplot(xdata)+
#   geom_line( aes(x=frame,y=pose_Tx, color='red2'), size=1)+
#   geom_line( aes(x=frame,y=pose_Ty, color='blue2'), size=1)+
#   scale_color_manual(labels = c("Tx", "Ty"), values = c("red2", "blue2")) +
#   coord_cartesian(xlim=NULL, ylim=c(-150,150))+
#   facet_grid(participant~.)
#Plot



#
### Pose estimation, rotations around x,y,z axes
#Plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=pose_Rx, color='red'), size=1)+
#  geom_line( aes(x=frame,y=pose_Ry, color='blue'), size=1)+
#  geom_line( aes(x=frame,y=pose_Rz, color='green'), size=1)+
#  coord_cartesian(xlim=c(500,1500), ylim=c(-0.5,0.5))+
#  scale_color_manual(labels = c("Rx", "Ry", 'Rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#Plot
#




##x_48, x_49, x_50, x_51, x_52, x_53, x_54, x_55, x_56, x_57, x_58, x_59, x_60, x_61, x_62, x_63, x_64, x_65, x_66, x_67, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=x_48, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=x_49, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=x_50, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=x_51, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=x_52, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=x_53, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=x_54, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=x_55, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=x_56, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=x_57, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=x_58, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=x_59, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=x_60, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=x_61, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=x_62, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=x_63, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=x_64, colour=colors()[17]), size=1)+
#  geom_line( aes(x=frame,y=x_65, colour=colors()[18]), size=1)+
#  geom_line( aes(x=frame,y=x_66, colour=colors()[19]), size=1)+
#  geom_line( aes(x=frame,y=x_67, colour=colors()[20]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot
#



#
##y_48, y_49, y_50, y_51, y_52, y_53, y_54, y_55, y_56, y_57, y_58, y_59, y_60, y_61, y_62, y_63, y_64, y_65, y_66, y_67, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=y_48, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=y_49, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=y_50, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=y_51, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=y_52, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=y_53, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=y_54, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=y_55, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=y_56, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=y_57, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=y_58, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=y_59, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=y_60, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=y_61, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=y_62, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=y_63, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=y_64, colour=colors()[17]), size=1)+
#  geom_line( aes(x=frame,y=y_65, colour=colors()[18]), size=1)+
#  geom_line( aes(x=frame,y=y_66, colour=colors()[19]), size=1)+
#  geom_line( aes(x=frame,y=y_67, colour=colors()[20]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot
#



#
##x_0, x_1, x_2, x_3, x_4, x_5, x_6, x_7, x_8, x_9, x_10, x_11, x_12, x_13, x_14, x_15, x_16, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=x_0, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=x_1, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=x_2, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=x_3, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=x_4, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=x_5, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=x_6, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=x_7, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=x_8, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=x_9, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=x_10, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=x_11, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=x_12, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=x_13, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=x_14, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=x_15, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=x_16, colour=colors()[17]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot



#
##y_0, y_1, y_2, y_3, y_4, y_5, y_6, y_7, y_8, y_9, y_10, y_11, y_12, y_13, y_14, y_15, y_16, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=y_0, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=y_1, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=y_2, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=y_3, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=y_4, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=y_5, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=y_6, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=y_7, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=y_8, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=y_9, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=y_10, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=y_11, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=y_12, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=y_13, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=y_14, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=y_15, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=y_16, colour=colors()[17]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot




#
##x_17, x_18, x_19, x_20, x_21, x_22, x_23, x_24, x_25, x_26, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=x_17, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=x_18, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=x_19, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=x_20, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=x_21, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=x_22, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=x_23, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=x_24, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=x_25, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=x_26, colour=colors()[10]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot
#
#
#
#



#y_17, y_18, y_19, y_20, y_21, y_22, y_23, y_24, y_25, y_26, 
plot <- ggplot(xdata)+
  geom_line( aes(x=frame,y=y_17, colour=colors()[1]), size=1)+
  geom_line( aes(x=frame,y=y_18, colour=colors()[2]), size=1)+
  geom_line( aes(x=frame,y=y_19, colour=colors()[3]), size=1)+
  geom_line( aes(x=frame,y=y_20, colour=colors()[4]), size=1)+
  geom_line( aes(x=frame,y=y_21, colour=colors()[5]), size=1)+
  geom_line( aes(x=frame,y=y_22, colour=colors()[6]), size=1)+
  geom_line( aes(x=frame,y=y_23, colour=colors()[7]), size=1)+
  geom_line( aes(x=frame,y=y_24, colour=colors()[8]), size=1)+
  geom_line( aes(x=frame,y=y_25, colour=colors()[9]), size=1)+
  geom_line( aes(x=frame,y=y_26, colour=colors()[10]), size=1)+
  coord_cartesian(xlim=NULL, ylim=NULL)+
  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
  facet_grid(participant~.)
plot



#
##p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, p_11, p_12, p_13, p_14, p_15, p_16, p_17, p_18, p_19, p_20, p_21, p_22, p_23, p_24, p_25, p_26, p_27, p_28, p_29, p_30, p_31, p_32, p_33, 
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=p_0, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=p_1, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=p_2, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=p_3, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=p_4, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=p_5, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=p_6, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=p_7, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=p_8, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=p_9, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=p_10, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=p_11, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=p_12, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=p_13, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=p_14, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=p_15, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=p_16, colour=colors()[17]), size=1)+
#  geom_line( aes(x=frame,y=p_17, colour=colors()[18]), size=1)+
#  geom_line( aes(x=frame,y=p_18, colour=colors()[19]), size=1)+
#  geom_line( aes(x=frame,y=p_19, colour=colors()[20]), size=1)+
#  geom_line( aes(x=frame,y=p_20, colour=colors()[21]), size=1)+
#  geom_line( aes(x=frame,y=p_21, colour=colors()[22]), size=1)+
#  geom_line( aes(x=frame,y=p_22, colour=colors()[23]), size=1)+
#  geom_line( aes(x=frame,y=p_23, colour=colors()[24]), size=1)+
#  geom_line( aes(x=frame,y=p_24, colour=colors()[25]), size=1)+
#  geom_line( aes(x=frame,y=p_25, colour=colors()[26]), size=1)+
#  geom_line( aes(x=frame,y=p_26, colour=colors()[27]), size=1)+
#  geom_line( aes(x=frame,y=p_27, colour=colors()[28]), size=1)+
#  geom_line( aes(x=frame,y=p_28, colour=colors()[29]), size=1)+
#  geom_line( aes(x=frame,y=p_29, colour=colors()[30]), size=1)+
#  geom_line( aes(x=frame,y=p_30, colour=colors()[31]), size=1)+
#  geom_line( aes(x=frame,y=p_31, colour=colors()[32]), size=1)+
#  geom_line( aes(x=frame,y=p_32, colour=colors()[33]), size=1)+
#  geom_line( aes(x=frame,y=p_33, colour=colors()[34]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  facet_grid(participant~.)
#plot
#


#
##p_0, p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=p_0, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=p_1, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=p_2, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=p_3, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=p_4, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=p_5, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=p_6, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=p_7, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=p_8, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=p_9, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=p_10, colour=colors()[11]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  facet_grid(participant~.)
#plot
#
#


##p_scale, p_rx, p_ry, p_rz, p_tx, p_ty, 
#plot <- ggplot(xdata)+
##  geom_line( aes(x=frame,y=p_scale, colour=colors()[1]), size=1)+
#
#  geom_line( aes(x=frame,y=p_rx, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=p_ry, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=p_rz, colour=colors()[4]), size=1)+
#
##  geom_line( aes(x=frame,y=p_tx, colour=colors()[5]), size=1)+
#
#
###  geom_line( aes(x=frame,y=p_ty, colour=colors()[6]), size=1)+
#
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  facet_grid(participant~.)
#plot
#














#Plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=AU01_c, color='red'), size=1)+
#  geom_line( aes(x=frame,y=AU02_c, color='blue'), size=1)+
#  geom_line( aes(x=frame,y=AU04_c, color='green'), size=1)+
#  coord_cartesian(xlim=c(500,1500), ylim=c(-0.5,0.5))+
#  scale_color_manual(labels = c("Rx", "Ry", 'Rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#Plot


## The presense (0 absent, 1 present) of 18 AUs:
## `AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c`
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=AU01_c, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=AU02_c, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=AU04_c, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=AU05_c, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=AU06_c, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=AU07_c, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=AU09_c, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=AU10_c, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=AU12_c, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=AU14_c, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=AU15_c, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=AU17_c, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=AU20_c, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=AU23_c, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=AU25_c, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=AU26_c, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=AU45_c, colour=colors()[17]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=c(-0.5,1.5))+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot
#



## The presense (0 absent, 1 present) of 18 AUs:
## `AU01_c, AU02_c, AU04_c, AU05_c, AU06_c, AU07_c, AU09_c, AU10_c, AU12_c, AU14_c, AU15_c, AU17_c, AU20_c, AU23_c, AU25_c, AU26_c, AU28_c, AU45_c`
#plot <- ggplot(xdata)+
#
##  	geom_line( aes(x=frame,y=AU01_c, colour=colors()[1]), size=1)+
##	geom_line( aes(x=frame,y=AU01_r, colour=colors()[2]), size=1)+
#### 
#
##  geom_line( aes(x=frame,y=AU02_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU02_r, colour=colors()[2]), size=1)+
### 
#
#
##  geom_line( aes(x=frame,y=AU04_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU04_r, colour=colors()[2]), size=1)+
####
#
##  geom_line( aes(x=frame,y=AU05_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU05_r, colour=colors()[2]), size=1)+
####
#
##  geom_line( aes(x=frame,y=AU06_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU06_r, colour=colors()[2]), size=1)+
###
#
##  geom_line( aes(x=frame,y=AU07_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU07_r, colour=colors()[2]), size=1)+
####
#
##  geom_line( aes(x=frame,y=AU09_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU09_r, colour=colors()[2]), size=1)+
####
#
##  geom_line( aes(x=frame,y=AU10_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU10_r, colour=colors()[2]), size=1)+
###
## 
##
##  geom_line( aes(x=frame,y=AU12_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU12_r, colour=colors()[2]), size=1)+
###
# 
#
##   geom_line( aes(x=frame,y=AU14_c, colour=colors()[1]), size=1)+
##   geom_line( aes(x=frame,y=AU14_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU15_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU15_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU17_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU17_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU20_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU20_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU23_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU23_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU25_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU25_r, colour=colors()[2]), size=1)+
#
##  geom_line( aes(x=frame,y=AU26_c, colour=colors()[1]), size=1)+
##  geom_line( aes(x=frame,y=AU26_r, colour=colors()[2]), size=1)+
#
#  geom_line( aes(x=frame,y=AU45_c, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=AU45_r, colour=colors()[2]), size=1)+
#
#
#
#  coord_cartesian(xlim=NULL, ylim=NULL)+
#  facet_grid(participant~.)
#plot
#
#
#
#







#
##The system can detect the intensity (from 0 to 5) of 17 AUs:
##`AU01_r, AU02_r, AU04_r, AU05_r, AU06_r, AU07_r, AU09_r, AU10_r, AU12_r, AU14_r, AU15_r, AU17_r, AU20_r, AU23_r, AU25_r, AU26_r, AU45_r`
#plot <- ggplot(xdata)+
#  geom_line( aes(x=frame,y=AU01_r, colour=colors()[1]), size=1)+
#  geom_line( aes(x=frame,y=AU02_r, colour=colors()[2]), size=1)+
#  geom_line( aes(x=frame,y=AU04_r, colour=colors()[3]), size=1)+
#  geom_line( aes(x=frame,y=AU05_r, colour=colors()[4]), size=1)+
#  geom_line( aes(x=frame,y=AU06_r, colour=colors()[5]), size=1)+
#  geom_line( aes(x=frame,y=AU07_r, colour=colors()[6]), size=1)+
#  geom_line( aes(x=frame,y=AU09_r, colour=colors()[7]), size=1)+
#  geom_line( aes(x=frame,y=AU10_r, colour=colors()[8]), size=1)+
#  geom_line( aes(x=frame,y=AU12_r, colour=colors()[9]), size=1)+
#  geom_line( aes(x=frame,y=AU14_r, colour=colors()[10]), size=1)+
#  geom_line( aes(x=frame,y=AU15_r, colour=colors()[11]), size=1)+
#  geom_line( aes(x=frame,y=AU17_r, colour=colors()[12]), size=1)+
#  geom_line( aes(x=frame,y=AU20_r, colour=colors()[13]), size=1)+
#  geom_line( aes(x=frame,y=AU23_r, colour=colors()[14]), size=1)+
#  geom_line( aes(x=frame,y=AU25_r, colour=colors()[15]), size=1)+
#  geom_line( aes(x=frame,y=AU26_r, colour=colors()[16]), size=1)+
#  geom_line( aes(x=frame,y=AU45_r, colour=colors()[17]), size=1)+
#  coord_cartesian(xlim=NULL, ylim=c(-0.1,5.2))+
#  #scale_color_manual(labels = c("rx", "ry", 'rz'), values = c("red", "blue", "green")) +
#  facet_grid(participant~.)
#plot
#




#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


