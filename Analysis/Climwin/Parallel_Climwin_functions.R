## Example of functions to use slidingwin as an intrinsic parallel

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




