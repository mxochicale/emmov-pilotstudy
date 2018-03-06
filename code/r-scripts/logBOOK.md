logBOOK
---

# TODO




* [ ] For razor and openface timeseries create :
	* compute m and tau for the time-delay embeddging theorem
		* implement cao and ami algorithms for embedding parameter estimations plotting (1march2018: sorted:2march2018)
		* create datatable for minumum tau embedding parameters (created:2march2018, sorted:4march2018,01h25m)
		* create datatable for minumum dim embedding parameters (created:2march2018, sorted:)
	
	* in  ami-razor and cao-razor, For outcomes paht for utde use `outcomes/emmov/utde/razor/...` (created:6mach2018,1h22m sorted:)

	* in utde-razor,  similarly to `plotRSS3D2D(a_rss)` plot PC components plotRSS transformedsignals `plotRSS2D_rotateddata()`  (created: 6march2018,4h08, sorted:6march2018,14h00m)

	* in utde-razor, ED data.table object is created to compute the euclideian distances for different axis, sensors, participants, and embedding parameters (created and sorted: 6march2018, 16h57)

	* create error plots of the ED of utde-razor for different dim values (created:6march2018,17h00m; sorted: )
	* cp utde-razor as utde-openface and compute error bars of ED for different embedding values (created:6march2018,17h00; sorted:)

 

	* state space reconstruction 
	* recurrence plots 
	* recurrence qunatification analysis parameters
	(created:28feb2018, sorted: )	


* [ ] for postprocessing, try different parameters for savitzky-golay filter as the
	current ones `SavitzkyGolayCoeffs <- sgolay(p=5,n=155 ,m=0)`, 
	smoothed the openface time series to the point that no osicllations are 
	presented in the head pose estimation time series.


* [ ] save preprocessed.datatable at `~/data/razor_imu`	(created:22feb2018 sorted:)
* [ ] plot 2d plots with x/y points using pose_tx and pose_ty (created:21february2018, sorted: )
* [ ] plot 3d plots with x/y/z points using xyz values (created:21february2018, sorted: )



# SORTED 

* [x] create doc for timeseries for utde
	* zmuvAccX, sgzmuvAccX
	* zmuvAccY, sgzmuvAccY
	* zmuvAccZ, sgzmuvAccZ
	* zmuvGyroX, sgzmuvAccX
	* zmuvGyroY, sgzmuvGyroY
	* zmuvGyroZ, sgzmuvGyroZ
	(created28feb2018,sorted28feb2018)



* [x] add dates in the `observation.tex` document with the preprocessed data of both 
	razor and openface data
	(created:26feb2018 sorted:28feb2018)




* [x]  create postprocessing data (26feb2018 1531) and document for openface(27feb2018, 1811 )    
	(created:23feb2018 sorted:27feb2018)



* [x] document for razor time series `/home/map479/mxochicale/github/emmov-pilotstudy/docs/plotting_timeseries/razor/razor.tex`
	(created:23feb2018 sorted:23feb2017)



* [x] shift the data for each of the particpants with [:link:](https://github.com/mxochicale/r-code_repository/tree/master/dataDOTtable)
	at: `/home/map479/mxochicale/github/emmov-pilotstudy/code/r-scripts/plotting/timeseries-razor.R`

```
setkey(xdata, participant)
xdp01 <- xdata[.(c('p01'))]
hdp01 <- xdp01[sensor=='imu-human', (anscols):= shift(.SD,75, fill=0, type='lead') , by=.(participant,sensor), .SDcols=cols  ]
rdp01 <- xdp01[sensor=='imu-robot', (anscols):= shift(.SD,00, fill=0, type='lead') , by=.(participant,sensor), .SDcols=cols  ]
dp01 <- rbind(hdp01,rdp01)
```
(created:22feb2018 sorted:23feb2018)




* [x] Write data.table objects in the outcomepath and create scripts for preprocessing techniques (created:21february2018, sorted: 22february)
	`~/github/emmov-pilotstudy/code/r-scripts/preprocessing/preprocessing-razor.R`



* [x] plot data from two sensors (one for the person, one for the robot) for participants 01 to 06 (created:21february2018, sorted:22february2018)
	`/home/map479/mxochicale/github/emmov-pilotstudy/code/r-scripts/plotting/timeseries-razor.R`




* [x] create document with graphical results for openface and razor data (created: 17february2018, sorted:21february2018)



* [x] Plot time series from face landmarks (created:16feb2018, sorted:17february2018)
