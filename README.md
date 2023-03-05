# ezHCA

ezHCA is an R library for analysing home cage monitoring date for mouse phenotyping studies. 

## Dependencies

You may need to download the rdhf5 package.

```R
BiocManager::install("rhdf5")
```

## Installation

Easiest way to install is using devtools

```R
devtools::install_github('DJFernandes/ezHCA')
```

## Usage

The HDF5 files from Actual Analytics home cage monitoring system contain a treasure trove of mouse phenotyping data. This package should hopefully make it easy to read and analyse it.

```R
library(ezHCA)
library(tidyverse)
```

I uploaded some example HDF5 files (https://wiki.mouseimaging.ca/display/MICePub/Home+Cage+Analysis). I downloaded them and put them all in a directory ('hcadata/'). You can use any HDF5 files from the Actual Analytics home cage monitoring system.

```R
hdf5files = list.files(path = 'hcadata/',pattern = '.hdf5$',full.names=T)
```

This function will read all the HDF5 files. The 'subjectIDs' argument is optional and is a vector of all the known RFIDs. If not supplied, the RFIDs are automatically determined but this is not recommended as it is prone to errors.

```R
subjectdatalist = hcaReadHDF5series( hdf5files , subjectIDs = c("900026000623623", "900026000623624", "900026000623653","900026000623654") )
```

We can create a data.frame that is ready for analysis using the 'hca_to_hcadf' function.

```R
dftoproc = hca_to_hcadf(subjectdatalist)
```

From this point on, you can do whatever you like to the data. I think information theory is a useful way to analyse the data so I created a couple function (with more to come... and I am taking recommendations).

We can calculate entropy, which is a measure of exploration. The higher the entropy (default normalized to a scale 0 to 1) the more 'spread out' a mouse's position is. We can also calculate mutual information, which is a measure of how much information you gain from a mouse's position based on the position of all the cagemates. The higher the mutual information, the more social the mouse is. 

```R
entdf = HCAdf_to_entropy(dftoproc)
midf = HCAdf_to_MI(dftoproc)
```

Tidyverse is great at manipulating the data for plotting. The following is a timeseries of the mutual information.

```R

bootstrapfunc = function(x,n=1000,q=0.5) { 
  unname(
    quantile(
      sapply(1:n,function(y) mean(sample(x,size = length(x),replace=T))) , q))
  }

pldf = midf %>% gather(ID,MI,-t,-dayvec)
summarydf = pldf %>% group_by(ID,dayvec) %>% 
  summarise(
    t = mean(t) , 
    cilowMI = bootstrapfunc(MI,q=0.025) ,
    cihighMI = bootstrapfunc(MI,q=0.975) ,
    MI = mean(MI)
    )
ggplot(pldf,aes(t,MI,colour=ID)) + geom_point(alpha=0.1) + geom_point(data = summarydf) + geom_line(data = summarydf) + geom_errorbar(data = summarydf, aes(ymin = cilowMI , ymax = cihighMI),width=0.2) + ylab('Mutual Information (normalised to total entropy)')

```

## Contributing
Suggestions, concerns, and pull requests are welcome. 


