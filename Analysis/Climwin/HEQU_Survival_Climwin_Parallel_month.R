# setwd("c:/owncloud/Documents/PhD/Biomes/Biome/")

library(climwin)
library(dplyr)
library(lme4)
library(parallel)

### Prepare data -------------------------------------------------------------
Clim <- read.csv("Data/HEQU_NOAA_supplemented.csv") 
Clim$date <- as.Date(Clim$date)                                    ### get a date that's accepted by climwin
Clim$date <- format(Clim$date, format = "%d/%m/%Y")           

Biol <- read.csv("Data/HEQU_demography data_JEcol_Dryad.csv") %>%
  mutate(sizeT = log(as.numeric(sizeT)),          
         sizeT1 = log(as.numeric(sizeT1))         
  )

Biol$date <- as.Date(paste(Biol$year, "/07/01", sep = "") )        ### get a date that's accepted by climwin
Biol$date <- format(Biol$date, format = "%d/%m/%Y")
Biol <- Biol[which(Biol$seedling != 1),]                           ### Select Adults
Biol <- Biol[which(!is.na(Biol$survival)),]                       
Biol <- Biol[which(!is.na(Biol$sizeT)),]                           



### Climate signal combies ----------------------------------------------------------------

xvar <- c("Clim$Temp", "Clim$Rain")
type <- c("absolute")
stat <- c("mean","slope", "sd")
func <- c("lin", "quad")
upper <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37
lower <- NA            ## LEAVE these as NA, when adding stat = sum, use rbind function on row 37

options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)

# options <- rbind(options, c("Clim$Temp", "absolute", "sum", "lin", 10, NA))  ## example of adding a "sum" combination

### Get ClimWin functions that can run parallel -----------------------------------------

ParSliding <- function(combi) {
  
  x <- list(Clim[,ifelse(options$xvar[combi] == xvar[1], 2, 3)]) 
  names(x) <- ifelse(options$xvar[combi] == "Clim$Temp", "Temp", "Rain")
  
  
  slidingwin(baseline = glmer(formula = survival ~ sizeT + population + (1|year),
                              data = Biol, 
                              family = binomial),
             xvar = x,
             type = "absolute",
             range = c(12,0),
             stat = options$stat[combi], 
             upper = ifelse(options$stat[combi] == "sum", options$upper[combi], NA),
             lower = ifelse(options$stat[combi] == "sum", options$lower[combi], NA),
             func = options$func[combi],
             refday = c(30,6),                             
             cinterval = "month",
             cdate = as.character(Clim$Date), bdate = as.character(Biol$Date)
  )
  
  
} 


Cleanup <- function(obj) {
  df <- obj[[1]]$combos[0,]
  l <- list()
  
  for (i in 1:length(obj)) {
    x <- obj[[i]]$combos
    df <- rbind(df, x )
    l <- append(l, obj[[1]][1])
  }
  
  return(c(l, combos = list(df)))
  
}


### set up parallels ----------------------------------------------------------------------

# detect cores
cores     <- (detectCores() - 2)
# set up as many clusters as detected by 'detectCores()'
cluster   <- parallel::makePSOCKcluster(cores)

# attach packages that will be needed on each cluster
clusterEvalQ(cluster, list(library(climwin),  
                           library(dplyr), library(lme4)) )

# attach objects that will be needed on each cluster
clusterExport(cluster, c('Clim', 'Biol', 'options', 'xvar') )

start <- Sys.time()
SurvMPar <- parLapply(cluster, 1:length(options[,1]), ParSliding)
Sys.time() - start

stopCluster(cluster)


### Merge into one output that can be used with climwin -----------------------

SurvivalMonthly <- Cleanup(SurvMPar)

save(SurvivalMonthly, file = "work/HEQU_Growth_Monthly")



