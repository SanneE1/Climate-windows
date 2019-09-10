# setwd("c:/owncloud/Documents/PhD/Biomes/Biome/")

suppressPackageStartupMessages(library(climwin))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(optparse))



# ------------------------------------------------------------------------------
# parsing arguments
# ------------------------------------------------------------------------------

Parsoptions <- list (
  
  
)

parser <- OptionParser(
  usage       = "Rscript %prog [options] Climate SpeciesInput output",
  option_list = Parsoptions,
  description = "\nan Run slidingwindow analysis",
  epilogue    = ""
)

cli <- parse_args(parser, positional_arguments = 3)

# ------------------------------------------------------------------------------
# assign a few shortcuts
# ------------------------------------------------------------------------------

Climate   <- cli$args[1]
SpeciesInput  <- cli$args[2]
output <- cli$args[3]
taskID <- as.integer(Sys.getenv("SGE_TASK_ID"))

### Prepare data -------------------------------------------------------------
Clim <- read.csv(Climate) 
Clim$date <- paste("15/",Clim$Month, "/", Clim$Year, sep = "")
Clim$date <- as.Date(Clim$date)                                    ### get a date that's accepted by climwin
Clim$date <- format(Clim$date, format = "%d/%m/%Y")           

Biol <- read.csv(SpeciesInput) %>%
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

#### Run function ---------------------------------------------------

x <- list(Clim[,ifelse(options$xvar[taskID] == xvar[1], 2, 3)]) 
names(x) <- ifelse(options$xvar[taskID] == "Clim$Temp", "Temp", "Rain")

result <- slidingwin(baseline = glmer(formula = survival ~ sizeT + population + (1|year),
                            data = Biol, 
                            family = binomial),
           xvar = x,
           type = "absolute",
           range = c(12,0),
           stat = options$stat[taskID], 
           upper = ifelse(options$stat[taskID] == "sum", options$upper[taskID], NA),
           lower = ifelse(options$stat[taskID] == "sum", options$lower[taskID], NA),
           func = options$func[taskID],
           refday = c(30,6),                             
           cinterval = "month",
           cdate = as.character(Clim$Date), bdate = as.character(Biol$Date)
)


### Merge into one output that can be used with climwin -----------------------

saveRDS(result, file = output)
