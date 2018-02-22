###############################################################################	
#
# Time series plots for razor IMU sensors 
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
	# (3) Plotting
		# (3.1) Creating and changing plotting paths
		# (3.2) Plots features
		# (3.3) Plots data 
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


main_data_path <- paste( main_repository_path, '/data/razor_imu',sep="")
outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
relativeplotpath <- "/plots_timeseries/razor"
relativeodatapath <- "/datatables"



################################################################################
# (1) Setting DataSets paths and reading data
setwd(main_data_path)
data_path_list <- list.dirs(path = ".", full.names = TRUE, recursive = TRUE)

participantsNN <- 6
trialsNN <- 1
participant_index <- c(2:(participantsNN+1))
sNN <- 1:4 # number of sensors


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



	# for START sNN_k
	for(sNN_k in sNN)
	{

	##START ifelseNA
	if ( is.na(files[sNN_k]) )
	{
	message('files[', sNN_k,'] is an NA')
	}
	else{	
	#
	# This for non NA sensors
	#
	if ( files[sNN_k] == 's02.csv' ) {
		message('s02.csv')
		pNN_ <-  paste("p", participants_k, sep="")
  		assign (pNN_, fread(  files[ sNN_k ] , header = TRUE, sep=',') )
		temp <- get(pNN_)

		# add column names
        	setnames(temp, c("Yaw", "Pitch", "Roll", "AccX", "AccY", "AccZ", "GyroX", "GyroY",	"GyroZ") )
        	#add sample label
        	temp[,sample:=seq(.N)]
        	setcolorder(temp,c(10,1:9))
        	# add s01 label to sensor
        	functmp <-function(x) {list("imu-human")}
        	temp[,c("sensor"):=functmp(), ]
        	setcolorder(temp,c(11,1:10) )
		ts02<-temp
		}


	if ( files[sNN_k] == 's03.csv' ) {
		message('s03.csv')
		pNN_ <-  paste("p", participants_k, sep="")
  		assign (pNN_, fread(  files[ sNN_k ] , header = TRUE, sep=',') )
		temp <- get(pNN_)

		# add column names
        	setnames(temp, c("Yaw", "Pitch", "Roll", "AccX", "AccY", "AccZ", "GyroX", "GyroY",	"GyroZ") )
        	#add sample label
        	temp[,sample:=seq(.N)]
        	setcolorder(temp,c(10,1:9))
        	# add s01 label to sensor
        	functmp <-function(x) {list("imu-robot")}
        	temp[,c("sensor"):=functmp(), ]
        	setcolorder(temp,c(11,1:10) )
		ts03<-temp
		}

	}##END ifelseNA 
	



	

	}
	# for END sNN_k
	temp<-rbind(ts02,ts03)

	
	


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


 
	temp[,c("participant"):=fsNNtmp(), ]
	setcolorder(temp,c(12,1:11) )


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
           setcolorder(temp,c(1,13,2:12) )

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

windowframe = 400:1100;
xdata <- datatable[,.SD[windowframe],by=.(participant,trial,sensor)];



################################################################################
# (3) Plotting
#


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
tag <- 'razor-timeseries'
image_width <- 2500
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"

plotlinewidth <- 1



######################
### (3.3) Plots Data from Razor sensor


plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=AccX, col='AccX'), size=plotlinewidth)+
	geom_line( aes(y=AccY, col='AccY'), size=plotlinewidth)+
	geom_line( aes(y=AccZ, col='AccZ'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-500,500))

png(filename= paste(tag,"_AccXYZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=sample))+  
	geom_line( aes(y=GyroX, col='GyroX'), size=plotlinewidth)+
	geom_line( aes(y=GyroY, col='GyroY'), size=plotlinewidth)+
	geom_line( aes(y=GyroZ, col='GyroZ'), size=plotlinewidth)+
	facet_grid(participant~sensor)+
	scale_y_continuous()+
	coord_cartesian(xlim=NULL, ylim=c(-3,3))

png(filename= paste(tag,"_GyroXYZ.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



#plot <- ggplot(xdata, aes(x=sample))+  
#	geom_line( aes(y=Yaw, col='Yaw'), size=plotlinewidth)+
#	geom_line( aes(y=Pitch, col='Pitch'), size=plotlinewidth)+
#	geom_line( aes(y=Roll, col='Roll'), size=plotlinewidth)+
#	facet_grid(participant~.)+
#	scale_y_continuous()+
#	coord_cartesian(xlim=NULL, ylim=c(-200,200))
#plot


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
write.table(datatable, "rawimudata-v00.datatable", row.name=FALSE)

message('datatable file has been created at '  )
message (odata_path)




#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path


