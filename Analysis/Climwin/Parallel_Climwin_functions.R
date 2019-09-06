## Create functions that split up the slidingwin from Climwin so it can be run parallel using ParLapply
## and a function to clean up the resulting list into a form the basic climwin function can use again
## These functions require a dataframe called "options" that includes column names 'xvar', 'type', 'stat' and 'func'
## In the linked scripts the combos are created seperately and then each combo is run through slidingwin seperately
## through code like:
     # xvar <- c("Clim$Temp")
     # type <- c("absolute")
     # stat <- c("sum")
     # func <- c("lin")
     # upper <- c(10)  ## specify upper limit when stat = sum, else set to NA
     # lower <- NA     ## specify lower limit when stat = sum, else set to NA
     # options <- expand.grid(xvar = xvar, type = type, stat = stat, func = func, upper = upper, lower = lower, stringsAsFactors = F)
     #
     # parLapply(cluster, 1:length(options[,1]), ParSliding)



## Slidingwin --------------------------------------------------------------

ParSliding <- function(combi) {
  
  x <- list(Clim[,ifelse(options$xvar[combi] == xvar[1], 2, 3)]) 
  names(x) <- ifelse(options$xvar[combi] == "Clim$Temp", "Temp", "Rain")
  
  
  slidingwin(baseline = lm(Laydate ~ 1, data = Biol),
             xvar = x,
             type = "absolute",
             range = c(365,0),
             stat = options$stat[combi], 
             upper = ifelse(options$stat[combi] == "sum", options$upper[combi], NA),
             lower = ifelse(options$stat[combi] == "sum", options$lower[combi], NA),
             func = options$func[combi],
             refday = c(30,6),                             
             cinterval = "day",
             cdate = as.character(Clim$Date), bdate = as.character(Biol$Date)
  )
  
  
} 


## Clean up ---------------------------------------------------------------

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




