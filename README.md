# IDInteraction Object Tracking Analysis Tools

*Tools and scripts to analyze object tracking data for the IDInteraction project.*


## Prerequisites

R is required to run the analysis code. All necessary packages are installed using the function *installMissingPackages()*,  available in *dataProcessing.R* and *TreeClassifierCodeResources.R*.

R markdown files contained in Rnotebooks can be rendered into HTML. The *rmarkdown* library is required for the compilation. Check http://rmarkdown.rstudio.com/ for details on how to generate the HTML.


## Configuration of the environment

At the moment certain environment variables need to be configured. Files *dataProcessing.R* and all the R markdown files (inside Rnotebooks, all files with the Rmd extension) contain the following constants.

**projectFolderPath**
The path to the project folder to be used as the root for the files. At the moment the absolute path needs to be defined.

**experimentName**, **videoQuality**, and **trackingMode** are used to locate the necessary resource files inside the project folder.

The resulting path is the following *projectFolderPath/experimentName/videoQuality*.


## Acknowledgements

The IDInteraction Object Tracking Analysis Tools were developed in the IDInteraction project, funded by the Engineering and Physical Sciences Research Council, UK through grant agreement number [EP/M017133/1][gow].

## Licence

Copyright (c) 2015, 2016 The University of Manchester, UK.

Licenced under LGPL version 2.1. See LICENCE for details.

[gow]: http://gow.epsrc.ac.uk/NGBOViewGrant.aspx?GrantRef=EP/M017133/1





