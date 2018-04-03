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


outcomes_path <- paste(github_path,"/DataSets/emmov",sep="")
 relativeplotpathtimeseries <- "/utde/openface/timeseries"
relativeodatapath <- "/datatables"

relativeplotpathforEmbeddedValues <- "/utde/openface/minimum-embedding-values"

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









##############################################################################
##############################################################################
###############################################################################
# (4) Average Mutual Information


#AMI <- FALSE
AMI <- TRUE

if (AMI == TRUE) { #if (AMI == TRUE) {




library(devtools)
load_all( paste(github_path,'/nonlinearTseries',sep='') )



maxtau <- 100
numberOFpartitions <- 100

#Method used for selecting a concrete time lag. Available methods are "first.zero", "first.e.decay" (default), "first.minimum" and "first.value".
ami_timelag_selection_method <- 'first.minimum' # 'first.minimum'


#ami_timelag_selection_method <- 'first.value' # 'first.minimum'

##Numeric value indicating the value that the autocorrelation/AMI function must cross in order to select the time lag. It is used only with the "first.value" selection method.
ami_numeric_value <- 1/exp(0)



#pNN <- c('p01', 'p02')
#axis <- c(
#	'zmuvpose_Tx', 'sgzmuvpose_Tx'
#)
#


#MEMORY ERROR USING THESE AXIS
#pNN <- c('p01', 'p02')
#axis <- c(
#	'zmuvsuccess', 
#	'sgzmuvsuccess'
#	)

#pNN <- c('p01', 'p02')
#axis <- c(
#	'zmuvconfidence', 
#	'sgzmuvconfidence' 
#	)
#

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






AMI <- NULL
TAUlags <- NULL

for (participants_k in c(1:(length(pNN) ) )) {#for (pNN_k in c(1:1)) {

message('####################')
message('# PARTICIPANT: ', participants_k)
setkey(xd, participant)
xdp <- xd[.( pNN[participants_k] )]

tauamilagp <- NULL
amip<-NULL
for (axis_k in c(1:(length(axis) )  )){ #for (axis_k in c(1:12)){

	message('#### axis: ' , axis[axis_k])

	inputtimeseries <- xdp[,  get(axis[axis_k]) ]

	## tau-delay estimation based on the mutual information function
	tau.ami <- timeLag(time.series = inputtimeseries, technique = "ami",
                selection.method = ami_timelag_selection_method, value = ami_numeric_value,
		lag.max = maxtau,
                do.plot = F, n.partitions = numberOFpartitions,
                units = "Bits")

	tauamilag <- as.data.table(tau.ami[[1]])
	ami <- tau.ami[[2]]

	amis <- as.data.table(ami)
	amis[, tau := 0:(.N-1), ]
	
	# function Axis
	fa <-function(x) {  axis[axis_k]  }

	tauamilag[,c("axis"):=fa(), ]
	tauamilagp <- rbind(tauamilagp,tauamilag)

	amis[,c("axis"):=fa(), ]
	amip <- rbind(amip,amis)


	}#for (axis_k in c(1:12)){


# Function Particpant Number
fp <-function(x) {  pNN[participants_k]   }

amip[,c("participant"):=fp(), ]
AMI <- rbind(AMI, amip)

tauamilagp[,c("participant"):=fp(), ]
TAUlags <- rbind(TAUlags,tauamilagp)

}#for (pNN_k in c(1:1)) {



setcolorder(AMI,c(4,3,2,1) )
setcolorder(TAUlags,c(3,2,1) )
names(TAUlags)[3] <- 'timelags'




} ### if (AMI == TRUE) { #if (AMI == TRUE) {






################################################################################
### (5) Plot Avarage Mutual Information 


#print_AMI_flag <- FALSE
print_AMI_flag <- TRUE


if (print_AMI_flag == TRUE) { #if (print_AMI_flag == TRUE) {


plotlinewidth <- 0.9
#filenameimage <- paste("Evalues", t, ".png",sep="")




ptl <- ggplot(TAUlags, aes(x=participant,y=timelags) ) + 
	geom_point( aes(fill=participant, colour=participant, shape=participant), size=5 ) + 
	facet_grid(.~axis) + ylab("First Minimum AMI") + 
	coord_cartesian(xlim=NULL, ylim=c(0,30)  ) +
	theme_bw(20) +	
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain")  )



pami <- ggplot(AMI, aes(x=tau) ) + 
	geom_line( aes(y=ami ),lwd = plotlinewidth, alpha=0.7 ) + 
	facet_grid(participant~axis) + 
	ylab('AMI') + xlab('Tau') +
	theme_bw(20)+
        theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5,face="plain"))




## Setting up plots_path

plot_path <- paste(outcomes_path,relativeplotpathforEmbeddedValues,sep="")
if (file.exists(plot_path)){
    setwd(file.path(plot_path))
} else {
  dir.create(plot_path, recursive=TRUE)
  setwd(file.path(plot_path))
}




### Save Picture
width = 4500
height = 500
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi


filenameimage <- paste("ami-mintau", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	ptl)

### Save Picture
width = 4500
height = 1000
text.factor = 1
dpi <- text.factor * 100
width.calc <- width / dpi
height.calc <- height / dpi



filenameimage <- paste("amis", ".png",sep="")
ggsave(filename = filenameimage,
        dpi = dpi,
        width = width.calc,
        height = height.calc,
        units = 'in',
        bg = "transparent",
        device = "png",
	pami)





} #if (print_AMI_flag == TRUE) {















#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time

################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
