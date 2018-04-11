###############################################################################	
#
# Time series for  preprocessed data of openface data
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
	# (2) Setting DataSets paths and reading data
	# (3) Data Filtering
		# (3.1) Windowing
	# (4) Postprocessing
		# (4.0) Principal Component Analysis
		# (4.1) Zero Mean Unit Variance
		# (4.2) Savitzky-Golay Filter
	
	# (5) Plotting Time Series  ## (PLOTTINGTIMESERIES <- FALSE/TRUE)
		# (5.1) Creating  and Changing to PlotPath
		# (5.2) Plotting data

	# (6) TIME_DELAY EMBEDDING   ##  (UTDE <- FALSE/TRUE)
		# (6.1) Embedding Creating Preprossede Data Path
		# (6.2) buildTakens
		# (6.3) Create and save plots for euclidean distances of PC1 and PC2



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


outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
 relativeplotpathtimeseries <- "/utde/openface/timeseries"
relativeodatapath <- "/datatables"

relativeplotpath4utde_openface_ed <- "/utde/openface/euclideandistances"
utde_sensor_path <- paste(outcomes_path,'/utde/openface',sep='')

################################################################################
# (2) Setting DataSets paths and reading data


odatapath <- paste( outcomes_path, relativeodatapath, sep="" )
setwd(odatapath)
datatable <- fread("rawopenfacedata-v00.datatable", header=TRUE)








################################################################################
# (3) Data Filtering


################################
### (3.1) Windowing Data [xdata[,.SD[1:2],by=.(Participant,Activity,Sensor)]]

#windowframe = 0:1500;
#windowframe = 400:1000;
windowframe = 500:1000;
xdata <- datatable[,.SD[windowframe],by=.(participant,trial)];





################################################################################
# (4) Postprocessing

featurenames <- names(xdata[,5:18])
#> names(xdata[,5:18])
# [1] "confidence" "success"    "gaze_0_x"   "gaze_0_y"   "gaze_0_z"  
# [6] "gaze_1_x"   "gaze_1_y"   "gaze_1_z"   "pose_Tx"    "pose_Ty"   
#[11] "pose_Tz"    "pose_Rx"    "pose_Ry"    "pose_Rz"   

gazeFnames <- names(xdata[,7:12])

poseFnames <- names(xdata[,13:18])




################################################################################
### (4.0) Principal Components
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
### (4.1) Zero mean and unit Variance
###
xdata[,c(
	'zmuvconfidence', 'zmuvsuccess',
	'zmuvgaze_0_x', 'zmuvgaze_0_y', 'zmuvgaze_0_z', 'zmuvgaze_1_x', 'zmuvgaze_1_y', 'zmuvgaze_1_z',
	'zmuvpose_Tx', 'zmuvpose_Ty', 'zmuvpose_Tz', 'zmuvpose_Rx', 'zmuvpose_Ry', 'zmuvpose_Rz',
	'zmuvp_scale', 'zmuvp_rx', 'zmuvp_ry', 'zmuvp_rz', 'zmuvp_tx', 'zmuvp_ty',
	'zmuvpc1_gaze', 'zmuvpc2_gaze', 'zmuvpc3_gaze', 
	'zmuvpc1_pose', 'zmuvpc2_pose', 'zmuvpc3_pose', 
	'zmuvpc1_xlm', 'zmuvpc2_xlm', 'zmuvpc3_xlm',
	'zmuvpc1_ylm', 'zmuvpc2_ylm', 'zmuvpc3_ylm',
	"zmuvx_0",  "zmuvx_1",  "zmuvx_2",  "zmuvx_3",  "zmuvx_4",  "zmuvx_5",  "zmuvx_6",  "zmuvx_7",  "zmuvx_8",  "zmuvx_9", 
	"zmuvx_10", "zmuvx_11", "zmuvx_12", "zmuvx_13", "zmuvx_14", "zmuvx_15", "zmuvx_16", "zmuvx_17", "zmuvx_18", "zmuvx_19",
	"zmuvx_20", "zmuvx_21", "zmuvx_22", "zmuvx_23", "zmuvx_24", "zmuvx_25", "zmuvx_26", "zmuvx_27", "zmuvx_28", "zmuvx_29",
	"zmuvx_30", "zmuvx_31", "zmuvx_32", "zmuvx_33", "zmuvx_34", "zmuvx_35", "zmuvx_36", "zmuvx_37", "zmuvx_38", "zmuvx_39",
	"zmuvx_40", "zmuvx_41", "zmuvx_42", "zmuvx_43", "zmuvx_44", "zmuvx_45", "zmuvx_46", "zmuvx_47", "zmuvx_48", "zmuvx_49",
        "zmuvx_50", "zmuvx_51", "zmuvx_52", "zmuvx_53", "zmuvx_54", "zmuvx_55", "zmuvx_56", "zmuvx_57", "zmuvx_58", "zmuvx_59",
	"zmuvx_60", "zmuvx_61", "zmuvx_62", "zmuvx_63", "zmuvx_64", "zmuvx_65", "zmuvx_66", "zmuvx_67",
	"zmuvy_0",  "zmuvy_1",  "zmuvy_2",  "zmuvy_3",  "zmuvy_4",  "zmuvy_5",  "zmuvy_6",  "zmuvy_7",  "zmuvy_8",  "zmuvy_9", 
	"zmuvy_10", "zmuvy_11", "zmuvy_12", "zmuvy_13", "zmuvy_14", "zmuvy_15", "zmuvy_16", "zmuvy_17", "zmuvy_18", "zmuvy_19",
	"zmuvy_20", "zmuvy_21", "zmuvy_22", "zmuvy_23", "zmuvy_24", "zmuvy_25", "zmuvy_26", "zmuvy_27", "zmuvy_28", "zmuvy_29",
	"zmuvy_30", "zmuvy_31", "zmuvy_32", "zmuvy_33", "zmuvy_34", "zmuvy_35", "zmuvy_36", "zmuvy_37", "zmuvy_38", "zmuvy_39",
	"zmuvy_40", "zmuvy_41", "zmuvy_42", "zmuvy_43", "zmuvy_44", "zmuvy_45", "zmuvy_46", "zmuvy_47", "zmuvy_48", "zmuvy_49",
        "zmuvy_50", "zmuvy_51", "zmuvy_52", "zmuvy_53", "zmuvy_54", "zmuvy_55", "zmuvy_56", "zmuvy_57", "zmuvy_58", "zmuvy_59",
	"zmuvy_60", "zmuvy_61", "zmuvy_62", "zmuvy_63", "zmuvy_64", "zmuvy_65", "zmuvy_66", "zmuvy_67"
	) :=
       lapply(.(
	confidence, success,
	gaze_0_x, gaze_0_y, gaze_0_z, gaze_1_x, gaze_1_y, gaze_1_z,
	pose_Tx, pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz,
	p_scale, p_rx, p_ry, p_rz, p_tx, p_ty,
	pc1_gaze, pc2_gaze, pc3_gaze, 
	pc1_pose, pc2_pose, pc3_pose, 
	pc1_xlm, pc2_xlm, pc3_xlm,
	pc1_ylm, pc2_ylm, pc3_ylm,
	x_0,  x_1,  x_2,  x_3,  x_4,  x_5,  x_6,  x_7,  x_8,  x_9, 
	x_10, x_11, x_12, x_13, x_14, x_15, x_16, x_17, x_18, x_19,
	x_20, x_21, x_22, x_23, x_24, x_25, x_26, x_27, x_28, x_29,
	x_30, x_31, x_32, x_33, x_34, x_35, x_36, x_37, x_38, x_39,
	x_40, x_41, x_42, x_43, x_44, x_45, x_46, x_47, x_48, x_49,
        x_50, x_51, x_52, x_53, x_54, x_55, x_56, x_57, x_58, x_59,
	x_60, x_61, x_62, x_63, x_64, x_65, x_66, x_67,
	y_0,  y_1,  y_2,  y_3,  y_4,  y_5,  y_6,  y_7,  y_8,  y_9, 
	y_10, y_11, y_12, y_13, y_14, y_15, y_16, y_17, y_18, y_19,
	y_20, y_21, y_22, y_23, y_24, y_25, y_26, y_27, y_28, y_29,
	y_30, y_31, y_32, y_33, y_34, y_35, y_36, y_37, y_38, y_39,
	y_40, y_41, y_42, y_43, y_44, y_45, y_46, y_47, y_48, y_49,
        y_50, y_51, y_52, y_53, y_54, y_55, y_56, y_57, y_58, y_59,
	y_60, y_61, y_62, y_63, y_64, y_65, y_66, y_67
	), function(x) ( zeromean_unitvariance(x)  ) )
	]






################################################################################
### (4.2) Smoothing data with Savitzky-Golay Filter
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
	'sgzmuvp_scale', 'sgzmuvp_rx', 'sgzmuvp_ry', 'sgzmuvp_rz', 'sgzmuvp_tx', 'sgzmuvp_ty',
	'sgzmuvpc1_gaze', 'sgzmuvpc2_gaze', 'sgzmuvpc3_gaze', 
	'sgzmuvpc1_pose', 'sgzmuvpc2_pose', 'sgzmuvpc3_pose', 
	'sgzmuvpc1_xlm', 'sgzmuvpc2_xlm', 'sgzmuvpc3_xlm',
	'sgzmuvpc1_ylm', 'sgzmuvpc2_ylm', 'sgzmuvpc3_ylm',
	"sgzmuvx_0",  "sgzmuvx_1",  "sgzmuvx_2",  "sgzmuvx_3",  "sgzmuvx_4",  "sgzmuvx_5",  "sgzmuvx_6",  "sgzmuvx_7",  "sgzmuvx_8",  "sgzmuvx_9", 
	"sgzmuvx_10", "sgzmuvx_11", "sgzmuvx_12", "sgzmuvx_13", "sgzmuvx_14", "sgzmuvx_15", "sgzmuvx_16", "sgzmuvx_17", "sgzmuvx_18", "sgzmuvx_19",
	"sgzmuvx_20", "sgzmuvx_21", "sgzmuvx_22", "sgzmuvx_23", "sgzmuvx_24", "sgzmuvx_25", "sgzmuvx_26", "sgzmuvx_27", "sgzmuvx_28", "sgzmuvx_29",
	"sgzmuvx_30", "sgzmuvx_31", "sgzmuvx_32", "sgzmuvx_33", "sgzmuvx_34", "sgzmuvx_35", "sgzmuvx_36", "sgzmuvx_37", "sgzmuvx_38", "sgzmuvx_39",
	"sgzmuvx_40", "sgzmuvx_41", "sgzmuvx_42", "sgzmuvx_43", "sgzmuvx_44", "sgzmuvx_45", "sgzmuvx_46", "sgzmuvx_47", "sgzmuvx_48", "sgzmuvx_49",
        "sgzmuvx_50", "sgzmuvx_51", "sgzmuvx_52", "sgzmuvx_53", "sgzmuvx_54", "sgzmuvx_55", "sgzmuvx_56", "sgzmuvx_57", "sgzmuvx_58", "sgzmuvx_59",
	"sgzmuvx_60", "sgzmuvx_61", "sgzmuvx_62", "sgzmuvx_63", "sgzmuvx_64", "sgzmuvx_65", "sgzmuvx_66", "sgzmuvx_67",
	"sgzmuvy_0",  "sgzmuvy_1",  "sgzmuvy_2",  "sgzmuvy_3",  "sgzmuvy_4",  "sgzmuvy_5",  "sgzmuvy_6",  "sgzmuvy_7",  "sgzmuvy_8",  "sgzmuvy_9", 
	"sgzmuvy_10", "sgzmuvy_11", "sgzmuvy_12", "sgzmuvy_13", "sgzmuvy_14", "sgzmuvy_15", "sgzmuvy_16", "sgzmuvy_17", "sgzmuvy_18", "sgzmuvy_19",
	"sgzmuvy_20", "sgzmuvy_21", "sgzmuvy_22", "sgzmuvy_23", "sgzmuvy_24", "sgzmuvy_25", "sgzmuvy_26", "sgzmuvy_27", "sgzmuvy_28", "sgzmuvy_29",
	"sgzmuvy_30", "sgzmuvy_31", "sgzmuvy_32", "sgzmuvy_33", "sgzmuvy_34", "sgzmuvy_35", "sgzmuvy_36", "sgzmuvy_37", "sgzmuvy_38", "sgzmuvy_39",
	"sgzmuvy_40", "sgzmuvy_41", "sgzmuvy_42", "sgzmuvy_43", "sgzmuvy_44", "sgzmuvy_45", "sgzmuvy_46", "sgzmuvy_47", "sgzmuvy_48", "sgzmuvy_49",
        "sgzmuvy_50", "sgzmuvy_51", "sgzmuvy_52", "sgzmuvy_53", "sgzmuvy_54", "sgzmuvy_55", "sgzmuvy_56", "sgzmuvy_57", "sgzmuvy_58", "sgzmuvy_59",
	"sgzmuvy_60", "sgzmuvy_61", "sgzmuvy_62", "sgzmuvy_63", "sgzmuvy_64", "sgzmuvy_65", "sgzmuvy_66", "sgzmuvy_67"
	) 
:=lapply(
	.(
	confidence, success,
	gaze_0_x, gaze_0_y, gaze_0_z, gaze_1_x, gaze_1_y, gaze_1_z,
	pose_Tx, pose_Ty, pose_Tz, pose_Rx, pose_Ry, pose_Rz,
	zmuvconfidence, zmuvsuccess,
	zmuvgaze_0_x, zmuvgaze_0_y, zmuvgaze_0_z, zmuvgaze_1_x, zmuvgaze_1_y, zmuvgaze_1_z,
	zmuvpose_Tx, zmuvpose_Ty, zmuvpose_Tz, zmuvpose_Rx, zmuvpose_Ry, zmuvpose_Rz,
	zmuvp_scale, zmuvp_rx, zmuvp_ry, zmuvp_rz, zmuvp_tx, zmuvp_ty,
	zmuvpc1_gaze, zmuvpc2_gaze, zmuvpc3_gaze, 
	zmuvpc1_pose, zmuvpc2_pose, zmuvpc3_pose, 
	zmuvpc1_xlm, zmuvpc2_xlm, zmuvpc3_xlm,
	zmuvpc1_ylm, zmuvpc2_ylm, zmuvpc3_ylm,
	zmuvx_0,  zmuvx_1,  zmuvx_2,  zmuvx_3,  zmuvx_4,  zmuvx_5,  zmuvx_6,  zmuvx_7, zmuvx_8, zmuvx_9, 
	zmuvx_10, zmuvx_11, zmuvx_12, zmuvx_13, zmuvx_14, zmuvx_15, zmuvx_16, zmuvx_17, zmuvx_18, zmuvx_19,
	zmuvx_20, zmuvx_21, zmuvx_22, zmuvx_23, zmuvx_24, zmuvx_25, zmuvx_26, zmuvx_27, zmuvx_28, zmuvx_29,
	zmuvx_30, zmuvx_31, zmuvx_32, zmuvx_33, zmuvx_34, zmuvx_35, zmuvx_36, zmuvx_37, zmuvx_38, zmuvx_39,
	zmuvx_40, zmuvx_41, zmuvx_42, zmuvx_43, zmuvx_44, zmuvx_45, zmuvx_46, zmuvx_47, zmuvx_48, zmuvx_49,
        zmuvx_50, zmuvx_51, zmuvx_52, zmuvx_53, zmuvx_54, zmuvx_55, zmuvx_56, zmuvx_57, zmuvx_58, zmuvx_59,
	zmuvx_60, zmuvx_61, zmuvx_62, zmuvx_63, zmuvx_64, zmuvx_65, zmuvx_66, zmuvx_67,
	zmuvy_0, zmuvy_1,  zmuvy_2,  zmuvy_3,  zmuvy_4, zmuvy_5, zmuvy_6, zmuvy_7, zmuvy_8, zmuvy_9, 
	zmuvy_10, zmuvy_11, zmuvy_12, zmuvy_13, zmuvy_14, zmuvy_15, zmuvy_16, zmuvy_17, zmuvy_18, zmuvy_19,
	zmuvy_20, zmuvy_21, zmuvy_22, zmuvy_23, zmuvy_24, zmuvy_25, zmuvy_26, zmuvy_27, zmuvy_28, zmuvy_29,
	zmuvy_30, zmuvy_31, zmuvy_32, zmuvy_33, zmuvy_34, zmuvy_35, zmuvy_36, zmuvy_37, zmuvy_38, zmuvy_39,
	zmuvy_40, zmuvy_41, zmuvy_42, zmuvy_43, zmuvy_44, zmuvy_45, zmuvy_46, zmuvy_47, zmuvy_48, zmuvy_49,
        zmuvy_50, zmuvy_51, zmuvy_52, zmuvy_53, zmuvy_54, zmuvy_55, zmuvy_56, zmuvy_57, zmuvy_58, zmuvy_59,
	zmuvy_60, zmuvy_61, zmuvy_62, zmuvy_63, zmuvy_64, zmuvy_65, zmuvy_66, zmuvy_67
	), 
	function(x) ( SGolay(x,SavitzkyGolayCoeffs)  ))
	]















#
################################################################################
# (5) Plotting Time Series
#


PLOTTINGTIMESERIES <- FALSE
#PLOTTINGTIMESERIES <- TRUE


if (PLOTTINGTIMESERIES == TRUE) 
{

#################
# (4.0) Plots Features
tag <- 'timeseries'
image_width <- 2500
image_height <- 3000
image_dpi <- 300
image_bg <- "transparent"

plotlinewidth <- 1

plotlinewidth_z <- 0.5
alpha_z <- 0.9

plotlinewidth_sg <- 1.5
alpha_sg <- 0.7

################################################################################
# (5.1) Creating  and Changing to PlotPath
plot_path <- paste(outcomes_path,relativeplotpathtimeseries,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}



#####################
## (5.2) Plotting data


plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvconfidence, col='zmuvconfidence'), size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvconfidence, col='sgzmuvconfidence'), size=plotlinewidth_sg, alpha= alpha_sg)+



        theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_confidence.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgconfidence, col='sgconfidence'), size=plotlinewidth)+
#	geom_line( aes(y=sgsuccess, col='sgsuccess'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgbase.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#

plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvsuccess, col='zmuvsuccess'), size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvsuccess, col='sgzmuvsuccess'), size=plotlinewidth_sg, alpha= alpha_sg)+

        theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_sucess.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=pose_Tx, col='pose_Tx'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Ty, col='pose_Ty'), size=plotlinewidth)+
#	#geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Pose Position Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseTxTy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	#geom_line( aes(y=pose_Tx, col='pose_Tx'), size=plotlinewidth)+
#	#geom_line( aes(y=pose_Ty, col='pose_Ty'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Tz, col='pose_Tz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Pose Position Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseTz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()


#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Tx, col='zmuvpose_Tx'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Tx, col='sgzmuvpose_Tx'),  size=plotlinewidth_sg, alpha= alpha_sg)+


        theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseTx.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Ty, col='zmuvpose_Ty'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Ty, col='sgzmuvpose_Ty'),  size=plotlinewidth_sg, alpha= alpha_sg)+


        theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseTy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Tz, col='zmuvpose_Tz'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Tz, col='sgzmuvpose_Tz'),  size=plotlinewidth_sg, alpha= alpha_sg)+


        theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseTz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgzmuvpose_Tx, col='sgzmuvpose_Tx'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvpose_Ty, col='sgzmuvpose_Ty'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvpose_Tz, col='sgzmuvpose_Tz'), size=plotlinewidth)+
#
#        theme_bw(15)+
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgzmuvposeT.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#



#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Tx, col='sgpose_Tx'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Ty, col='sgpose_Ty'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeTxy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Tz, col='sgpose_Tz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#	#geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeTz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#



#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=pose_Rx, col='pose_Rx'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Ry, col='pose_Ry'), size=plotlinewidth)+
#	geom_line( aes(y=pose_Rz, col='pose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Raw Poste Rotation Data ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_poseR.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#v
#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgpose_Rx, col='sgpose_Rx'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Ry, col='sgpose_Ry'), size=plotlinewidth)+
#	geom_line( aes(y=sgpose_Rz, col='sgpose_Rz'), size=plotlinewidth)+
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgposeRxyz.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#

#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=zmuvpose_Rx, col='zmuvpose_Rx'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvpose_Ry, col='zmuvpose_Ry'), size=plotlinewidth)+
#	geom_line( aes(y=zmuvpose_Rz, col='zmuvpose_Rz'), size=plotlinewidth)+
#
#        theme_bw(15)+
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Zero Mean Unit Variance ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_zmuvposeR.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#geom_line( aes(y=sgzmuvpose_Rx, col='sgzmuvpose_Rx'), size=plotlinewidth)+
#geom_line( aes(y=sgzmuvpose_Ry, col='sgzmuvpose_Ry'), size=plotlinewidth)+
#geom_line( aes(y=sgzmuvpose_Rz, col='sgzmuvpose_Rz'), size=plotlinewidth)+
#
#facet_grid(participant~.)+
#coord_cartesian(xlim=NULL, ylim=NULL  )+
#ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
#xlab('Sample')+
#labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgzmuvposeR.png",sep=''),
#width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#

#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Rx, col='zmuvpose_Rx'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Rx, col='sgzmuvpose_Rx'),  size=plotlinewidth_sg, alpha= alpha_sg)+


	theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseRx.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()

#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Ry, col='zmuvpose_Ry'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Ry, col='sgzmuvpose_Ry'),  size=plotlinewidth_sg, alpha= alpha_sg)+


	theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseRy.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()


#
plot <- ggplot(xdata, aes(x=frame))+  
	geom_line( aes(y=zmuvpose_Rz, col='zmuvpose_Rz'),  size=plotlinewidth_z, alpha= alpha_z)+
	geom_line( aes(y=sgzmuvpose_Rz, col='sgzmuvpose_Rz'),  size=plotlinewidth_sg, alpha= alpha_sg)+


	theme_bw(15)+
	facet_grid(participant~.)+
	coord_cartesian(xlim=NULL, ylim=NULL  )+
	ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
	xlab('Sample')+
	labs(colour = 'Features')


png(filename= paste(tag,"_poseRz.png",sep=''),
   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






#
#plot <- ggplot(xdata, aes(x=frame))+  
#geom_line( aes(y=zmuvx_0, col='zmuvx_0'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_10, col='zmuvx_10'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_20, col='zmuvx_20'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_30, col='zmuvx_30'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_40, col='zmuvx_40'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_50, col='zmuvx_50'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_60, col='zmuvx_60'),  size=plotlinewidth_z, alpha= alpha_z)+
#
#geom_line( aes(y=sgzmuvx_0, col='sgzmuvx_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_10, col='sgzmuvx_10'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_20, col='sgzmuvx_20'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_30, col='sgzmuvx_30'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_40, col='sgzmuvx_40'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_50, col='sgzmuvx_50'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvx_60, col='sgzmuvx_60'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#
#theme_bw(15)+
#facet_grid(participant~.)+
#coord_cartesian(xlim=NULL, ylim=NULL  )+
#ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
#xlab('Sample')+
#labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_x0.png",sep=''),
#width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#





#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#geom_line( aes(y=zmuvx_0, col='zmuvx_0'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_1, col='zmuvx_1'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_2, col='zmuvx_2'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_3, col='zmuvx_3'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_4, col='zmuvx_4'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_5, col='zmuvx_5'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_6, col='zmuvx_6'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_7, col='zmuvx_7'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_8, col='zmuvx_8'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_9, col='zmuvx_9'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_10, col='zmuvx_10'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_11, col='zmuvx_11'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_12, col='zmuvx_12'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_13, col='zmuvx_13'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_14, col='zmuvx_14'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_15, col='zmuvx_15'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_16, col='zmuvx_16'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_17, col='zmuvx_17'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_18, col='zmuvx_18'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_19, col='zmuvx_19'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_20, col='zmuvx_20'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_21, col='zmuvx_21'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_22, col='zmuvx_22'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_23, col='zmuvx_23'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_24, col='zmuvx_24'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_25, col='zmuvx_25'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_26, col='zmuvx_26'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_27, col='zmuvx_27'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_28, col='zmuvx_28'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_29, col='zmuvx_29'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_30, col='zmuvx_30'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_31, col='zmuvx_31'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_32, col='zmuvx_32'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_33, col='zmuvx_33'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_34, col='zmuvx_34'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_35, col='zmuvx_35'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_36, col='zmuvx_36'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_37, col='zmuvx_37'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_38, col='zmuvx_38'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_39, col='zmuvx_39'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_40, col='zmuvx_40'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_41, col='zmuvx_41'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_42, col='zmuvx_42'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_43, col='zmuvx_43'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_44, col='zmuvx_44'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_45, col='zmuvx_45'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_46, col='zmuvx_46'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_47, col='zmuvx_47'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_48, col='zmuvx_48'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_49, col='zmuvx_49'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_50, col='zmuvx_50'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_51, col='zmuvx_51'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_52, col='zmuvx_52'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_53, col='zmuvx_53'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_54, col='zmuvx_54'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_55, col='zmuvx_55'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_56, col='zmuvx_56'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_57, col='zmuvx_57'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_58, col='zmuvx_58'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_59, col='zmuvx_59'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_60, col='zmuvx_60'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_61, col='zmuvx_61'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_62, col='zmuvx_62'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_63, col='zmuvx_63'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_64, col='zmuvx_64'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_65, col='zmuvx_65'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_66, col='zmuvx_66'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvx_67, col='zmuvx_67'),  size=plotlinewidth_z, alpha= alpha_z)+
#
#
#theme_bw(15)+
#facet_grid(participant~.)+
#coord_cartesian(xlim=NULL, ylim=NULL  )+
#ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
#xlab('Sample')+
#labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_zmuv-x.png",sep=''),
#width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#
#
#
#
#plot <- ggplot(xdata, aes(x=frame))+  
#geom_line( aes(y=sgzmuvx_0, col='sgzmuvx_0'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_1, col='sgzmuvx_1'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_2, col='sgzmuvx_2'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_3, col='sgzmuvx_3'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_4, col='sgzmuvx_4'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_5, col='sgzmuvx_5'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_6, col='sgzmuvx_6'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_7, col='sgzmuvx_7'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_8, col='sgzmuvx_8'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_9, col='sgzmuvx_9'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_10, col='sgzmuvx_10'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_11, col='sgzmuvx_11'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_12, col='sgzmuvx_12'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_13, col='sgzmuvx_13'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_14, col='sgzmuvx_14'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_15, col='sgzmuvx_15'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_16, col='sgzmuvx_16'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_17, col='sgzmuvx_17'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_18, col='sgzmuvx_18'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_19, col='sgzmuvx_19'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_20, col='sgzmuvx_20'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_21, col='sgzmuvx_21'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_22, col='sgzmuvx_22'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_23, col='sgzmuvx_23'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_24, col='sgzmuvx_24'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_25, col='sgzmuvx_25'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_26, col='sgzmuvx_26'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_27, col='sgzmuvx_27'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_28, col='sgzmuvx_28'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_29, col='sgzmuvx_29'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_30, col='sgzmuvx_30'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_31, col='sgzmuvx_31'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_32, col='sgzmuvx_32'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_33, col='sgzmuvx_33'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_34, col='sgzmuvx_34'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_35, col='sgzmuvx_35'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_36, col='sgzmuvx_36'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_37, col='sgzmuvx_37'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_38, col='sgzmuvx_38'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_39, col='sgzmuvx_39'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_40, col='sgzmuvx_40'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_41, col='sgzmuvx_41'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_42, col='sgzmuvx_42'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_43, col='sgzmuvx_43'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_44, col='sgzmuvx_44'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_45, col='sgzmuvx_45'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_46, col='sgzmuvx_46'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_47, col='sgzmuvx_47'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_48, col='sgzmuvx_48'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_49, col='sgzmuvx_49'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_50, col='sgzmuvx_50'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_51, col='sgzmuvx_51'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_52, col='sgzmuvx_52'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_53, col='sgzmuvx_53'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_54, col='sgzmuvx_54'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_55, col='sgzmuvx_55'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_56, col='sgzmuvx_56'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_57, col='sgzmuvx_57'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_58, col='sgzmuvx_58'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_59, col='sgzmuvx_59'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_60, col='sgzmuvx_60'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_61, col='sgzmuvx_61'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_62, col='sgzmuvx_62'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_63, col='sgzmuvx_63'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_64, col='sgzmuvx_64'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_65, col='sgzmuvx_65'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_66, col='sgzmuvx_66'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=sgzmuvx_67, col='sgzmuvx_67'),  size=plotlinewidth_z, alpha= alpha_z)+
#
#theme_bw(15)+
#facet_grid(participant~.)+
#coord_cartesian(xlim=NULL, ylim=NULL  )+
#ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
#xlab('Sample')+
#labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgzmuv-x.png",sep=''),
#width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#


plotlinewidth_z <- 0.5
alpha_z <- 0.9


plotlinewidth_sg <- 1.5
alpha_sg <- 0.25


plot <- ggplot(xdata, aes(x=frame))+  
geom_line( aes(y=zmuvx_0, col='zmuvx_0'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_1, col='zmuvx_1'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_2, col='zmuvx_2'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_3, col='zmuvx_3'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_4, col='zmuvx_4'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_5, col='zmuvx_5'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_6, col='zmuvx_6'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_7, col='zmuvx_7'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_8, col='zmuvx_8'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_9, col='zmuvx_9'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_10, col='zmuvx_10'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_11, col='zmuvx_11'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_12, col='zmuvx_12'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_13, col='zmuvx_13'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_14, col='zmuvx_14'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_15, col='zmuvx_15'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_16, col='zmuvx_16'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_17, col='zmuvx_17'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_18, col='zmuvx_18'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_19, col='zmuvx_19'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_20, col='zmuvx_20'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_21, col='zmuvx_21'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_22, col='zmuvx_22'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_23, col='zmuvx_23'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_24, col='zmuvx_24'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_25, col='zmuvx_25'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_26, col='zmuvx_26'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_27, col='zmuvx_27'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_28, col='zmuvx_28'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_29, col='zmuvx_29'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_30, col='zmuvx_30'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_31, col='zmuvx_31'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_32, col='zmuvx_32'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_33, col='zmuvx_33'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_34, col='zmuvx_34'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_35, col='zmuvx_35'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_36, col='zmuvx_36'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_37, col='zmuvx_37'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_38, col='zmuvx_38'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_39, col='zmuvx_39'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_40, col='zmuvx_40'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_41, col='zmuvx_41'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_42, col='zmuvx_42'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_43, col='zmuvx_43'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_44, col='zmuvx_44'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_45, col='zmuvx_45'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_46, col='zmuvx_46'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_47, col='zmuvx_47'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_48, col='zmuvx_48'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_49, col='zmuvx_49'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_50, col='zmuvx_50'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_51, col='zmuvx_51'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_52, col='zmuvx_52'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_53, col='zmuvx_53'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_54, col='zmuvx_54'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_55, col='zmuvx_55'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_56, col='zmuvx_56'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_57, col='zmuvx_57'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_58, col='zmuvx_58'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_59, col='zmuvx_59'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_60, col='zmuvx_60'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_61, col='zmuvx_61'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_62, col='zmuvx_62'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_63, col='zmuvx_63'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_64, col='zmuvx_64'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_65, col='zmuvx_65'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_66, col='zmuvx_66'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_67, col='zmuvx_67'),  size=plotlinewidth_z, alpha= alpha_z)+

geom_line( aes(y=sgzmuvx_0, col='sgzmuvx_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_1, col='sgzmuvx_1'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_2, col='sgzmuvx_2'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_3, col='sgzmuvx_3'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_4, col='sgzmuvx_4'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_5, col='sgzmuvx_5'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_6, col='sgzmuvx_6'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_7, col='sgzmuvx_7'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_8, col='sgzmuvx_8'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_9, col='sgzmuvx_9'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_10, col='sgzmuvx_10'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_11, col='sgzmuvx_11'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_12, col='sgzmuvx_12'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_13, col='sgzmuvx_13'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_14, col='sgzmuvx_14'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_15, col='sgzmuvx_15'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_16, col='sgzmuvx_16'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_17, col='sgzmuvx_17'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_18, col='sgzmuvx_18'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_19, col='sgzmuvx_19'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_20, col='sgzmuvx_20'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_21, col='sgzmuvx_21'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_22, col='sgzmuvx_22'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_23, col='sgzmuvx_23'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_24, col='sgzmuvx_24'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_25, col='sgzmuvx_25'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_26, col='sgzmuvx_26'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_27, col='sgzmuvx_27'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_28, col='sgzmuvx_28'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_29, col='sgzmuvx_29'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_30, col='sgzmuvx_30'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_31, col='sgzmuvx_31'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_32, col='sgzmuvx_32'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_33, col='sgzmuvx_33'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_34, col='sgzmuvx_34'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_35, col='sgzmuvx_35'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_36, col='sgzmuvx_36'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_37, col='sgzmuvx_37'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_38, col='sgzmuvx_38'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_39, col='sgzmuvx_39'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_40, col='sgzmuvx_40'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_41, col='sgzmuvx_41'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_42, col='sgzmuvx_42'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_43, col='sgzmuvx_43'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_44, col='sgzmuvx_44'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_45, col='sgzmuvx_45'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_46, col='sgzmuvx_46'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_47, col='sgzmuvx_47'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_48, col='sgzmuvx_48'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_49, col='sgzmuvx_49'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_50, col='sgzmuvx_50'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_51, col='sgzmuvx_51'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_52, col='sgzmuvx_52'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_53, col='sgzmuvx_53'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_54, col='sgzmuvx_54'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_55, col='sgzmuvx_55'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_56, col='sgzmuvx_56'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_57, col='sgzmuvx_57'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_58, col='sgzmuvx_58'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_59, col='sgzmuvx_59'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_60, col='sgzmuvx_60'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_61, col='sgzmuvx_61'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_62, col='sgzmuvx_62'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_63, col='sgzmuvx_63'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_64, col='sgzmuvx_64'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_65, col='sgzmuvx_65'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_66, col='sgzmuvx_66'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_67, col='sgzmuvx_67'),  size=plotlinewidth_sg, alpha= alpha_sg)+

theme_bw(15)+
theme(legend.position="none")+
facet_grid(participant~.)+
coord_cartesian(xlim=NULL, ylim=NULL  )+
ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
xlab('Sample')+
labs(colour = 'Features')


png(filename= paste(tag,"_sg-zmuv-x.png",sep=''),
width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()






plotlinewidth_z <- 0.5
alpha_z <- 0.9

plotlinewidth_sg <- 1.5
alpha_sg <- 0.25


plot <- ggplot(xdata, aes(x=frame))+  
geom_line( aes(y=zmuvy_0, col='zmuvy_0'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_1, col='zmuvy_1'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_2, col='zmuvy_2'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_3, col='zmuvy_3'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_4, col='zmuvy_4'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_5, col='zmuvy_5'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_6, col='zmuvy_6'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_7, col='zmuvy_7'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_8, col='zmuvy_8'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_9, col='zmuvy_9'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_10, col='zmuvy_10'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_11, col='zmuvy_11'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_12, col='zmuvy_12'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_13, col='zmuvy_13'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_14, col='zmuvy_14'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_15, col='zmuvy_15'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_16, col='zmuvy_16'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_17, col='zmuvy_17'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_18, col='zmuvy_18'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_19, col='zmuvy_19'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_20, col='zmuvy_20'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_21, col='zmuvy_21'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_22, col='zmuvy_22'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_23, col='zmuvy_23'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_24, col='zmuvy_24'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_25, col='zmuvy_25'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_26, col='zmuvy_26'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_27, col='zmuvy_27'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_28, col='zmuvy_28'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_29, col='zmuvy_29'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_30, col='zmuvy_30'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_31, col='zmuvy_31'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_32, col='zmuvy_32'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_33, col='zmuvy_33'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_34, col='zmuvy_34'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_35, col='zmuvy_35'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_36, col='zmuvy_36'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_37, col='zmuvy_37'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_38, col='zmuvy_38'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_39, col='zmuvy_39'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_40, col='zmuvy_40'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_41, col='zmuvy_41'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_42, col='zmuvy_42'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_43, col='zmuvy_43'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_44, col='zmuvy_44'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_45, col='zmuvy_45'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_46, col='zmuvy_46'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_47, col='zmuvy_47'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_48, col='zmuvy_48'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_49, col='zmuvy_49'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_50, col='zmuvy_50'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_51, col='zmuvy_51'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_52, col='zmuvy_52'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_53, col='zmuvy_53'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_54, col='zmuvy_54'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_55, col='zmuvy_55'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_56, col='zmuvy_56'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_57, col='zmuvy_57'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_58, col='zmuvy_58'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_59, col='zmuvy_59'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_60, col='zmuvy_60'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_61, col='zmuvy_61'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_62, col='zmuvy_62'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_63, col='zmuvy_63'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_64, col='zmuvy_64'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_65, col='zmuvy_65'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_66, col='zmuvy_66'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_67, col='zmuvy_67'),  size=plotlinewidth_z, alpha= alpha_z)+

geom_line( aes(y=sgzmuvy_0, col='sgzmuvy_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_1, col='sgzmuvy_1'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_2, col='sgzmuvy_2'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_3, col='sgzmuvy_3'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_4, col='sgzmuvy_4'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_5, col='sgzmuvy_5'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_6, col='sgzmuvy_6'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_7, col='sgzmuvy_7'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_8, col='sgzmuvy_8'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_9, col='sgzmuvy_9'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_10, col='sgzmuvy_10'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_11, col='sgzmuvy_11'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_12, col='sgzmuvy_12'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_13, col='sgzmuvy_13'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_14, col='sgzmuvy_14'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_15, col='sgzmuvy_15'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_16, col='sgzmuvy_16'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_17, col='sgzmuvy_17'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_18, col='sgzmuvy_18'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_19, col='sgzmuvy_19'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_20, col='sgzmuvy_20'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_21, col='sgzmuvy_21'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_22, col='sgzmuvy_22'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_23, col='sgzmuvy_23'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_24, col='sgzmuvy_24'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_25, col='sgzmuvy_25'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_26, col='sgzmuvy_26'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_27, col='sgzmuvy_27'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_28, col='sgzmuvy_28'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_29, col='sgzmuvy_29'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_30, col='sgzmuvy_30'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_31, col='sgzmuvy_31'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_32, col='sgzmuvy_32'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_33, col='sgzmuvy_33'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_34, col='sgzmuvy_34'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_35, col='sgzmuvy_35'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_36, col='sgzmuvy_36'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_37, col='sgzmuvy_37'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_38, col='sgzmuvy_38'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_39, col='sgzmuvy_39'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_40, col='sgzmuvy_40'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_41, col='sgzmuvy_41'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_42, col='sgzmuvy_42'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_43, col='sgzmuvy_43'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_44, col='sgzmuvy_44'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_45, col='sgzmuvy_45'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_46, col='sgzmuvy_46'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_47, col='sgzmuvy_47'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_48, col='sgzmuvy_48'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_49, col='sgzmuvy_49'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_50, col='sgzmuvy_50'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_51, col='sgzmuvy_51'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_52, col='sgzmuvy_52'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_53, col='sgzmuvy_53'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_54, col='sgzmuvy_54'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_55, col='sgzmuvy_55'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_56, col='sgzmuvy_56'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_57, col='sgzmuvy_57'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_58, col='sgzmuvy_58'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_59, col='sgzmuvy_59'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_60, col='sgzmuvy_60'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_61, col='sgzmuvy_61'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_62, col='sgzmuvy_62'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_63, col='sgzmuvy_63'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_64, col='sgzmuvy_64'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_65, col='sgzmuvy_65'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_66, col='sgzmuvy_66'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_67, col='sgzmuvy_67'),  size=plotlinewidth_sg, alpha= alpha_sg)+

theme_bw(15)+
theme(legend.position="none")+
facet_grid(participant~.)+
coord_cartesian(xlim=NULL, ylim=NULL  )+
ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
xlab('Sample')+
labs(colour = 'Features')


png(filename= paste(tag,"_sg-zmuv-y.png",sep=''),
width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()





plotlinewidth_z <- 0.5
alpha_z <- 0.9

plotlinewidth_sg <- 1.5
alpha_sg <- 0.5




plot <- ggplot(xdata, aes(x=frame))+  
geom_line( aes(y=zmuvx_0, col='zmuvx_0'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvx_67, col='zmuvx_67'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=sgzmuvx_0, col='sgzmuvx_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvx_67, col='sgzmuvx_67'),  size=plotlinewidth_sg, alpha= alpha_sg)+

theme_bw(15)+
facet_grid(participant~.)+
coord_cartesian(xlim=NULL, ylim=NULL  )+
ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
xlab('Sample')+
labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvx.png",sep=''),
width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()



plot <- ggplot(xdata, aes(x=frame))+  
geom_line( aes(y=zmuvy_0, col='zmuvy_0'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=zmuvy_67, col='zmuvy_67'),  size=plotlinewidth_z, alpha= alpha_z)+
geom_line( aes(y=sgzmuvy_0, col='sgzmuvy_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
geom_line( aes(y=sgzmuvy_67, col='sgzmuvy_67'),  size=plotlinewidth_sg, alpha= alpha_sg)+

theme_bw(15)+
facet_grid(participant~.)+
coord_cartesian(xlim=NULL, ylim=NULL  )+
ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
xlab('Sample')+
labs(colour = 'Features')


png(filename= paste(tag,"_sgzmuvy.png",sep=''),
width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
print(plot)
dev.off()














#
#plot <- ggplot(xdata, aes(x=frame))+  
#geom_line( aes(y=zmuvy_0, col='zmuvy_0'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_10, col='zmuvy_10'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_20, col='zmuvy_20'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_30, col='zmuvy_30'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_40, col='zmuvy_40'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_50, col='zmuvy_50'),  size=plotlinewidth_z, alpha= alpha_z)+
#geom_line( aes(y=zmuvy_60, col='zmuvy_60'),  size=plotlinewidth_z, alpha= alpha_z)+
#
#geom_line( aes(y=sgzmuvy_0, col='sgzmuvy_0'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_10, col='sgzmuvy_10'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_20, col='sgzmuvy_20'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_30, col='sgzmuvy_30'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_40, col='sgzmuvy_40'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_50, col='sgzmuvy_50'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#geom_line( aes(y=sgzmuvy_60, col='sgzmuvy_60'),  size=plotlinewidth_sg, alpha= alpha_sg)+
#
#theme_bw(15)+
#facet_grid(participant~.)+
#coord_cartesian(xlim=NULL, ylim=NULL  )+
#ylab('ZeroMeanUnitVariance and Satikzky-Golay zmuv') + 
#xlab('Sample')+
#labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_y0.png",sep=''),
#width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#
#








#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sggaze_0_x, col='sggaze_0_x'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_0_y, col='sggaze_0_y'), size=plotlinewidth)+
#	#geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_x, col='sggaze_1_x'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_y, col='sggaze_1_y'), size=plotlinewidth)+
#	#geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sggaze_xy.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#

#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sggaze_0_z, col='sggaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sggaze_1_z, col='sggaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay  ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sggaze_z.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#



#
#plot <- ggplot(xdata, aes(x=frame))+  
#	geom_line( aes(y=sgzmuvgaze_0_x, col='sgzmuvgaze_0_x'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_0_y, col='sgzmuvgaze_0_y'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_0_z, col='sgzmuvgaze_0_z'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_x, col='sgzmuvgaze_1_x'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_y, col='sgzmuvgaze_1_y'), size=plotlinewidth)+
#	geom_line( aes(y=sgzmuvgaze_1_z, col='sgzmuvgaze_1_z'), size=plotlinewidth)+
#
#
#	facet_grid(participant~.)+
#	coord_cartesian(xlim=NULL, ylim=NULL  )+
#	ylab('Savitzky-Golay, Zero Mean Unit Variance ') + 
#	xlab('Sample')+
#	labs(colour = 'Features')
#
#
#png(filename= paste(tag,"_sgzmuvgaze.png",sep=''),
#   width=image_width, height=image_height, units="px", res=image_dpi, bg=image_bg)
#print(plot)
#dev.off()
#
#





} # if PLOTTINGTIMESERIES












################################################################################
################################################################################
# (6) TIME_DELAY EMBEDDING
################################################################################
################################################################################


UTDE <- FALSE
#UTDE <- TRUE



if (UTDE == TRUE) {





#### (6.1) Embedding Creating Preprossede Data Path
embedding_path <- paste(utde_sensor_path,"/embeddings",sep="")
if (file.exists(embedding_path)){
setwd(file.path(embedding_path))
} else {
dir.create(embedding_path, recursive=TRUE)
setwd(file.path(embedding_path))
}


#pNN <- c('p01', 'p02')
#axis <- c(
#	'zmuvpose_Tx', 'sgzmuvpose_Tx'
#	)


pNN <- c('p01', 'p02', 'p03', 'p04', 'p05', 'p06')
axis <- c('zmuvconfidence', 'sgzmuvconfidence', 
	'zmuvpose_Rx', 'zmuvpose_Ry', 'zmuvpose_Rz', 'sgzmuvpose_Rx', 'sgzmuvpose_Ry', 'sgzmuvpose_Rz', 
	'zmuvpose_Tx', 'zmuvpose_Ty', 'zmuvpose_Tz', 'sgzmuvpose_Tx', 'sgzmuvpose_Ty', 'sgzmuvpose_Tz',
	'zmuvx_0', 'zmuvx_67', 'sgzmuvx_0', 'sgzmuvx_67',
	'zmuvy_0', 'zmuvy_67', 'sgzmuvy_0', 'sgzmuvy_67'
	)



xd <- xdata[,.(zmuvconfidence, sgzmuvconfidence, 
	zmuvpose_Rx, zmuvpose_Ry, zmuvpose_Rz, sgzmuvpose_Rx, sgzmuvpose_Ry, sgzmuvpose_Rz, 
	zmuvpose_Tx, zmuvpose_Ty, zmuvpose_Tz, sgzmuvpose_Tx, sgzmuvpose_Ty, sgzmuvpose_Tz,
	zmuvx_0, zmuvx_67, sgzmuvx_0, sgzmuvx_67,
	zmuvy_0, zmuvy_67, sgzmuvy_0, sgzmuvy_67
	), 
		by=. (participant,trial,frame)]


ED <- NULL # Euclidean Distances data.table object!
for (participants_k in c(1: (length(pNN)) ) ) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]






#time_lags_p <- NULL
ED_a<-NULL
for (axis_k in c(1:length(axis))){ #for (axis_k in c(1:12)){


	message('#### axis:' , axis[axis_k])
	inputtimeseries <- xdp[,  get(axis[axis_k]) ]




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
## (6.2) buildTakens

### Computed Embedding parameters:  m=7 tau=4
# delays <- tau
# dimensions <- dimension



#delays <- c(2)
#dimensions <- c(10)
#
#delays <- c(2,8)
#dimensions <- c(10, 100)

delays <- c(2,5,10)
dimensions <- c(10,25,50)

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

a_utde <- Takens_Theorem(inputtimeseries, dim_i, tau_j, 1)

a_rss <- PCA( a_utde ,0)





image_width =  1000#2000 #3508 #595 #877
image_height = 1000#700#2480 #842 #620

imagefilename <- paste('pc_rotateddata_p', formatC(participants_k,width=2,flag='0'),'_m',formatC(dim_i,width=2,flag='0'),'d',formatC(tau_j,width=2,flag='0'), '_', '.png', sep='')
png(filename=imagefilename, width=image_width, height=image_height, units="px", bg="white")
plotRSS2D_rotateddata(a_rss,05)
dev.off()





## Euclidean Distances
ed_dta <- as.data.table(euclidean.distances_rotateddata(a_rss))
fun <- function(x) {list( axis[axis_k] )}
ed_dta[,c('axis'):= fun(), ]





#
#image_width_p3d =  2000#2000 #3508 #595 #877
#image_height_p3d = 500#700#2480 #842 #620
#[[
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




}###for (axis_k in c(1:12)){ #for (axis_k in c(1:12)){



 # Particpant Number
fp <-function(x) {  pNN[participants_k]   }
ED_a[,c("participant"):=fp(), ]
ED <- rbind(ED, ED_a)





} ###for (participants_k in c(1:6)) {#for (pNN_k in c(1:1)) {






names(ED) <- gsub("V1", "EuclideanDistances", names(ED))
setcolorder( ED, c(5,2,3,4,1) )



### (6.3) Create and save plots for euclidean distances of PC1 and PC2

plot_path <- paste(outcomes_path,relativeplotpath4utde_openface_ed,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}






for (pd_k in c(1:length(delays))) {

tau_value <-  formatC(delays[pd_k],width=3,flag='0')
dED <- ED[tau== tau_value, .SDcols=cols  ]


pbox <- ggplot(dED, aes(x=participant, y=EuclideanDistances) )+
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
width = 3000
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
	pbox)


} ###for (pd_k in c(1:length(delays))) {






} #### if (UTDE == TRUE) {





#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
