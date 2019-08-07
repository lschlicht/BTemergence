.onLoad <- function(libname, pkgname){
   packageStartupMessage("Dataset and scripts belonging to Schlicht & Kempenaers 2019\n
'Population-level variation in the timing of activity in blue tits\n
(Cyanistes caeruleus): effects of season, sex, age and weather'\n
HOW TO USE:\r\n
To load the full dataset x2, type\n
> data(BTemergenceData)\r\n
A description of the dataset can be found at\n
> ?x2\n
To open the code that runs the models, type \n
> file.edit(paste0(.libPaths(), '/BTemergence/run_models.r'))\n
To open the code that creates the figures, type  \n
> file.edit(paste0(.libPaths(), '/BTemergence/figures.r'))\n
Note that the models contain information necessary for the figures.\n")
}
