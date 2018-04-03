r-scripts
---

# Dependencies

To install the following dependecies, run `install-R-packages.R`
* data.table
* ggplot2
* signal
* tseriesChaos
* git clone `https://github.com/mxochicale/tavand`
* git clone `https://github.com/mxochicale/nonlinearTseries`




# Code Usage

Open a terminal, go to, for instance, `~/rscript/...` and run R as follow
```
$ R
```
once `R` is loaded, explore the history with the up/down arrows and hit enter to run the script.

1. `timeseries-razor.R` reads raw data, shift the data and creates data.tables objects.
2. `postprocessing-razor.R`
3. `cao-razor.R`
4. `ami-razor.R`
5. `utde-razor.R`






# Machine/R version


## Server/R340(yourstupiddarkness)


```
> R.version
               _                           
platform       x86_64-pc-linux-gnu         
arch           x86_64                      
os             linux-gnu                   
system         x86_64, linux-gnu           
status                                     
major          3                           
minor          4.0                         
year           2017                        
month          04                          
day            21                          
svn rev        72570                       
language       R                           
version.string R version 3.4.0 (2017-04-21)
nickname       You Stupid Darkness         
```


## Laptop/R343(kite-eating-tree)


```
$ cat /proc/version
Linux version 4.4.0-36-generic (buildd@lgw01-20) (gcc version 4.8.4 (Ubuntu 4.8.4-2ubuntu1~14.04.3) ) #55~14.04.1-Ubuntu SMP Fri Aug 12 11:49:30 UTC 2016
```

```
$ lscpu
Architecture:          x86_64
CPU op-mode(s):        32-bit, 64-bit
Byte Order:            Little Endian
CPU(s):                4


```


```
> R.version
               _                           
platform       x86_64-pc-linux-gnu         
arch           x86_64                      
os             linux-gnu                   
system         x86_64, linux-gnu           
status                                     
major          3                           
minor          4.3                         
year           2017                        
month          11                          
day            30                          
svn rev        73796                       
language       R                           
version.string R version 3.4.3 (2017-11-30)
nickname       Kite-Eating Tree            
```
