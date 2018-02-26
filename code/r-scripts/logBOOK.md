logBOOK
---

# TODO


* [ ] add dates in the `observation.tex` document with the preprocessed data of both 
	razor and openface data
	(created:26feb2018 sorted:)



* [ ]  create preprocessing data (26feb2018 1531) and document for openface()    
	(created:23feb2018 sorted:)



* [ ] save preprocessed.datatable at `~/data/razor_imu`
	(created:22feb2018 sorted:)



* [ ] plot 2d plots with x/y points using pose_tx and pose_ty (created:21february2018, sorted: )
* [ ] plot 3d plots with x/y/z points using xyz values (created:21february2018, sorted: )



# SORTED 

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
