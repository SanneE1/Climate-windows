# Biome

Project: Testing the importance of growing season climate for predicting population dynamics

## Datasets

#### Helianthella quinquenervis (**HEQU**)
   Iler, A. M., Compagnoni, A., Inouye, D. W., Williams, J. L., CaraDonna, P. J., Anderson, A., & Miller, T. E. (2019). Reproductive losses due to climate change‐induced earlier flowering are not the primary threat to plant population viability in a perennial herb. Journal of Ecology.  
   [Article](https://doi.org/10.1111/1365-2745.13146)  
   [Dryad](https://doi.org/10.5061/dryad.863c8sk)

#### Cryptantha flava (**CRFL**)
   Salguero-Gómez, R., Kempenich, H., Forseth, I. N., & Casper, B. B. (2014). Long‐term individual‐level population dynamics of a native desert chamaephyte: Ecological Archives E095‐050. Ecology, 95(2), 577-577.  
   [Article](https://doi.org/10.1890/13-1256.1)  
   [Figshare](https://doi.org/10.6084/m9.figshare.c.3306537.v1)

#### Opuntia imbricata (**OPIM**)
   _to be provided by Tom Miller_
   
#### Frasera speciosa (**FRSP**)
   _Hopefully David Inouye is willing to share_
   
## Datamanagement

For now the scripts are very sensitive to having the correct column names and value format (especially date). The individually data needs to at least contain the following columns in the following formats:  
* population = the population the individual belongs to
* plantID = unique code for each individual
* year = the year of measurements
* month = the month for the measurement
* day = the day in the month (1-31) of the measurements. If unknown fill in NA, the climwin scripts will use 1 as default
* survival = survival of the individual from T to T+1
* sizeT = The size of the individual at T with any transformations already done
* sizeT1 = The size of the individual at T+1 with any transformations already done

**add more as code develops**


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
|survival   | Survival from year T to year T+1 (0/1)\** . |
|seedling   | Is individual seedling in year T (0/1)\** |
|sizeT      | The lognormal of the number of clumps of leaves per plant at time T |
|sizeT1     | The lognormal of the number of clumps of leaves per plant at time T+1 |
|pflowerT   | The probability of flowering (producing at least one stalk) at time T\* |
|pflowerT   | The probability of flowering (producing at least one stalk) at time T+1\* |
|abort.stalks | Number of aborted flower stalks per plant at time T\** |
|fertilityT | Total number of flowering stalks in which the plant invested at time T\*|
|fertilityT1 | Total number of flowering stalks in which the plant invested at time T+1\*| 

\* Same as found in the original data  
\** For details see the original [README](https://doi.org/10.5061/dryad.863c8sk)
