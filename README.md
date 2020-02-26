# Biome

Project: Testing the importance of growing season climate for predicting plant vital rates

## Biological Data

#### Helianthella quinquenervis (**HEQU**)
   Iler, A. M., Compagnoni, A., Inouye, D. W., Williams, J. L., CaraDonna, P. J., Anderson, A., & Miller, T. E. (2019). Reproductive losses due to climate change‐induced earlier flowering are not the primary threat to plant population viability in a perennial herb. Journal of Ecology.  
   [Article](https://doi.org/10.1111/1365-2745.13146)  
   [Dryad](https://doi.org/10.5061/dryad.863c8sk)

#### Cryptantha flava (**CRFL**)
   Salguero-Gómez, R., Kempenich, H., Forseth, I. N., & Casper, B. B. (2014). Long‐term individual‐level population dynamics of a native desert chamaephyte: Ecological Archives E095‐050. Ecology, 95(2), 577-577.  
   [Article](https://doi.org/10.1890/13-1256.1)  
   [Figshare](https://doi.org/10.6084/m9.figshare.c.3306537.v1)

#### Cylindriopuntia imbricata (**OPIM**)
Provided by Tom Miller. the data spans from 2004 to 2018. 
Details on data gathering etc can be found in the following articles  
[Eldred & Miller, 2016](https://doi.org/10.1890/15-1526.1)  
[Ohm & Miller, 2014](https://doi.org/10.1890/13-2309.1)    
   
#### Frasera speciosa (**FRSP**)
Provided by David Inouye. See the following websites for more information.  
[The RMBL Phenology project](https://www.bio.fsu.edu/~nunderwood/homepage/RMBLphenologyproject.htmlhttps://www.bio.fsu.edu/~nunderwood/homepage/RMBLphenologyproject.html)  
[Open Science Framework](https://osf.io/jt4n5/)

## Climate data
Data on the climate used for this project is retrieved from two sources:

**For HEQU, CRFL, and FRSP:** Data from NOAA is retrieved using the R package [rnoaa](https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf)  
**For OPIM:** Data from the [SEV-LTER](http://tierra.unm.edu/search/climate/search.php) was used

In case data on specific dates were not available, the values were imputed using 1 or 2 other nearest stations. Any remaining missing data (in case the data was also missing from the other stations) is imputed using the same method as the `Climwin`'s \"method1\" 
   
I used monthly (scaled) data.  

If the climate data is not retrieved using the provided scripts (\"SPECIESCODE\_Get\_Climate\"), the easiest way to use them would be to make sure the following columns are in the file and change the xvar lists in *sliding_EVE.R* to reflect the name of the different climate drivers you want to test. `Climwin` requires a wide format for the imput. Moreover, the file name MUST be in the following format (the submit script uses the file names to retrieve data needed to run the script):  
\"SPECIESCODE\_(anything but without \"\_\" e.g. where it comes from).csv\"  

|Columns|Explanation|
|-------|-----------|
|Year| year of measurement (for monthly data)|
|Month| month of measurement (can either be M or MM; for monthly data)|
|date| date of measurement in \"dd\\mm\\yyyy\" (for daily data)|
|Climate values| Each climate driver needs its own column| 

## Data Management

The scripts require the correct column names and value format for the date. The individually data needs to contain the following columns in the following formats: 

|Columns|Explanation|
|-------|-----------|
|year | The year of measurements (YYYY)|
|month | The month for the measurement (1-12; code can deal with both \"01\" and \"1\")|
|day | The day in the month (1-31; code can deal with both \"01\" and \"1\") of the measurements. If NA, the climwin scripts will use \"01\" as default. |


### HEQU
Metadata for the HEQU dataset

|Columns    |Explanation  |
|---------  |-------------|
|population | There were three distinct study populations. low: Horse Ranch Park (2703 m a.s.l); mid: Maxfield Meadow, at Rocky Mountain Biological Lab (2886 m a.s.l); high: Virginia Basin (3407 m a.s.l)\* |
|plantID    | Unique plant id for each individual (combination of population and plant.ID found in original data)|
|year       | Year of measurements at time T\*|
|month      | Month of measurements at time T|
|day        | Day of measurements at time T|
|presentT   | Presence at time T (0/1)\*|
|presentT1  | Presence at time T+1 (0/1)\*|
|survival   | Survival from year T to year T+1 (0/1)\* |
|seedling   | Is individual seedling in year T (0/1)\* |
|sizeT      | The number of clumps of leaves per plant at time T\* |
|sizeT1     | The number of clumps of leaves per plant at time T+1\* |
|pflowerT   | The probability of flowering (producing at least one stalk) at time T\* |
|pflowerT   | The probability of flowering (producing at least one stalk) at time T+1\* |
|abort.stalks | Number of aborted flower stalks per plant at time T\** |
|fertilityT | Total number of flowering stalks in which the plant invested at time T\*|
|fertilityT1 | Total number of flowering stalks in which the plant invested at time T+1\*| 

\* Same as found in the original data; For details see the original [README](https://doi.org/10.5061/dryad.863c8sk)  

### CRFL
Metadata for the CRFL dataset

|Columns    |Explanation  |
|---------  |-------------|
|Treatment|Treatment applied to the permanent plot. C: Control; D1: Rainout shelter in summer 1998; D2: Rainout shelter in summer 1999\*|
|Block|Location where the treatments were assigned in groups of three plots\*|
|Plot| Plot number. Permanent plots of 5 × 5 m²\*|
|Quadrat|Each permanent plot is conformed by 13 samplable 1 × 1 m² quadrats. Note that in plots 1–9 all quadrats were sampled, but in plots 10–18 only a random subset were sampled.\*|
|plantID|Individual unique identifier, a concatenation of the values for variables "Treatment", "Block", "Plot", "Quadrat", "X" and "Y", separated by "." \*|
|year|Year of census (at time T)\*|
|month| Month of census (at time T)|
|day| Day of census (at time T)|
|survival| survival from time T to time T1| 
|sizeT| The total number of rosettes of individual of _Cryptantha flava_ at time T\*|
|sizeT1| The total number of rosettes of individual of _Cryptantha flava_ at time T1\*|
|age|Age of individual of Cryptantha flava. Individuals with unknown age are set to 999\*|
|pflowerT| Did the individual flower|
|fertilityT| Number of flowering rosettes of individual of Cryptantha flava\*|
|Shrub|Closest shrub to the target individual of Cryptantha flava.\*|
|Compass|Compass direction from the center of the closest shrub to the target individual of Cryptantha flava\*|
|Distance|Closest distance from the target individual of Cryptantha flava to the edge of the shrub. Negative values mean the individual of Cryptantha flava is under the shrub. Positive values mean the individual of Cryptantha flava is not under the shrub.\*|

\* Same as found in the original data. For details see the original [README](https://doi.org/10.6084/m9.figshare.c.3306537.v1)  

### OPIM
Metadata for the OPIM dataset

|Columns    |Explanation  |
|---------  |-------------|
|Plot | Plot or spatial block\*|
|TagID |Unique identifier for each individual\*|
|Transplant |0 or 1. If 1, the individual was transplanted as part of an old experiment\*|
|year|The calendar year\*|
|month| The calendar month of census|
|day| The day of census|
|Height_t | Height of the plant in year t (in cm)\*|
|Width_t | Maximum width of the plant in year t (in cm)\*|
|Perp_t | The width perpendicular to the maximum width in year t (in cm)\*|
|sizeT | The volume of the plant in year t, calculated as a cone, using Height_t, Width_t and Perp_t|
|Goodbuds_t | The number of viable flowerbuds on a plant in year t|
|TotFlowerbuds_t | The total number of flowerbuds (viable + inviable) in year t|
|ABFlowerbuds_t | The number of aborted/inviable flowerbuds in year t|
|Recruit|0 or 1. Estimated if this is a 0 year recruit in year t\*\* |
|Newplant|0 or 1. Is this a new plant in the census|
|Survival_t1| Did the individual survive from year t to year t+1|
|Height_t1 | Height of the plant in year t+1 (in cm)\*|
|Width_t1 | Maximum width of the plant in year t+1 (in cm)\*|
|Perp_t1 | The width perpendicular to the maximum width in year t+1 (in cm)\*|
|sizeT1 | The volume of the plant in year t+1, calculated as a cone, using Height_t1, Width_t1 and Perp_t1|
|NS_t | The number of new stem segments in year t (a measure of vegetative growth)\*|
|Antcount_t | The number of ants counted in 30 seconds|
|Antsp_t | The species identity of the ants|  

\* Same as found in the original data  
\*\* Personal note from Tom - Not super reliable, very difficult to distinguish recruits from small older plants  

### FRSP
I used the reformatting script to reformat the original data sheets into a format usuable for this study

|Columns    |Explanation  |
|---------  |-------------|
|year| calendar year of time T |
|month| month when size was measured at time T. Because I work with a fixed reference date set to 7 |
|day| day when size was measured at time T. Because I work with a fixed reference date set to 15 |
|plantID| Unique identifier of individual.|
|sizeT| Number of leaves in basal rosettes at time T. |
|recruitT| Is the individual a new recruit yes (1) or no (0)| 
|sizeT1|Number of leaves in basal rosettes at time T + 1. |
|survival| if the individual survived from time T to T + 1 (1) or not (0) |
|pFlowerT1| Did the individual flower| 


## Analysis

### Climate effects
For this analysis I used the [Climwin package](https://github.com/LiamDBailey/climwin)

#### Sliding window analyses
The sliding window scripts (Analysis\\Climwin\\Slidingwindow) are set up to run on UFZ's EVE as an array job after which the output is merged back into a normal climwin output format for further analyses.
The random analyses scripts (Analysis\\Climwin\\Randomwindow) are also set up to run on EVE, and require a normal climwin output format (i.e. what the merged format produces), the number of the combination to run the randomization for, as well as the original climate and biological imput

all vital rates (survival, growth, flower probability and flower numbers) of the 4 species can be run with the same scripts.


