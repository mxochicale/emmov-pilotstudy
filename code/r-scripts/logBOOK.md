logBOOK
---


# TODO

* [ ]
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



* [ ]  defining too many datapaths get me dizzy. Use few variables to define these
	(added29march2018, sorted???)





# SORTED 

* [ ] Clean rscripts  (cao,ami,utde - razor,openface)  and use data from all participants and axis sensors
	(added:3april2018 0023, sorted:)

* [x] For razor and openface timeseries create :
	* compute m and tau for the time-delay embeddging theorem
		* implement cao and ami algorithms for embedding parameter estimations plotting (1march2018: sorted:2march2018)
		* create datatable for minumum tau embedding parameters (created:2march2018, sorted:4march2018,01h25m)
		* create datatable for minumum dim embedding parameters (created:2march2018, sorted:1april2018, 2359)
	
	* in  ami-razor and cao-razor, For outcomes paht for utde use `outcomes/emmov/utde/razor/...` (created:6mach2018,1h22m sorted:9march1458)

	* in utde-razor,  similarly to `plotRSS3D2D(a_rss)` plot PC components plotRSS transformedsignals `plotRSS2D_rotateddata()`  
		(created: 6march2018,4h08, sorted:6march2018,14h00m)

	* in utde-razor, ED data.table object is created to compute the euclideian distances for 
		different axis, sensors, participants, and embedding parameters 
		(created and sorted: 6march2018, 16h57)

	* create error plots of the ED of utde-razor for different dim values 
		(created:6march2018,17h00m; sorted: 6march2018,21h34)

	* cp utde-razor as utde-openface and compute error bars of ED for different embedding values 
		(created:6march2018,17h00; sorted:7march2018,02h02m)

 



* [x] rename plotting to `datatables_plottimeseries`
	(added:31marhc2018 1547 sorted:31march2018 16:06)


* [x] create r script to install package dependencies 
	(added:29march2018 2118, sorted:31march2018 1527)


* [x] Updating the paths for libraries and 

```
source('../../../../tavand/functions/ollin_cencah.R')
load_all( paste(github_path,'/nonlinearTseries',sep='') )
source(paste(github_path,'/tavand/functions/embedding_parameters/withCao1997/cao97_functions.R', sep=''))
```

Specific and previous changes:

* in `postprocessing-razor.R`: 
`source('~/mxochicale/github/r-code_repository/functions/ollin_cencah.R')`
to 
`source('~/mxochicale/github/R/functions/ollin_cencah.R')`
(added: 29march2018 2100)

* update the following library paths
for ami-razor.R
	`source('~/mxochicale/github/tavand/ollin_cencah.R')`

for cao-razor.R
source('~/mxochicale/github/R/functions/embedding_parameters/withCao1997/cao97_functions.R')

for ami-razor.R
	`load_all('~/mxochicale/github/nonlinearTseries')``






* [x] time series for utde-razor.tex and utde-openface.tex have been created to do 
	uniform time delay embedding
	(sorted: 27march2018 1703)


* [x] comment the code of utde-razor.R and utde-openface.R
	(added:26march2018, sorted:27march2018)



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
