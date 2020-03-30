autocor_wrap <- function(species, vitalrate, Climate, SlidingObject, Winner){
  
  enquo(Climate)

  options <- SlidingObject$combos[Winner,]
  
  Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin
  Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

  #### Set Range ----------------------------------------------------------------------------------------------------------------------------
  if(vitalrate == "s") {
    range <- c(24,-12)
  }
  
  if(vitalrate == "g") {
    if(species == "FRSP") {
      print("Range set to 5 years")
      range <- c(48, -12)
    } else {
      range <- c(24,-12)
    }
  }
  
  if(vitalrate == "fp") {
    if(species == "FRSP") {
      print("Range set to 4 years")
      range <- c(36, -12)
    } else {
      range <- c(36, 0)
    }
  }
  
  if(vitalrate == "fn") {
    if(species == "FRSP") {
      print("Range set to 4 years")
      range <- c(48, 0)
    } else {
      range <- c(36, 0)
    }
  }

  #### End data fromattin 
  if(species == "FRSP" & vitalrate == "fn"){
    reference <- SlidingObject[[Winner]]$BestModelData %>%
      select(yearT1, climate) %>%
      unique()    
  } else {
  reference <- SlidingObject[[Winner]]$BestModelData %>%
    select(year, climate) %>%
    unique() 
  }
  reference$date <- as.Date(paste(SlidingObject[[Winner]]$Dataset$Reference.day[1], "/",
                                  SlidingObject[[Winner]]$Dataset$Reference.month[1], "/",
                                  reference$year,
                                  sep = ""), 
                            format = "%d/%m/%Y")
  reference <- reference[order(reference$year),]
  
  Clim <- Clim[,c("date", options$climate)] %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  
  BestWindowOpen <- SlidingObject[[Winner]]$Dataset$WindowOpen[1]
  BestWindowClose <- SlidingObject[[Winner]]$Dataset$WindowClose[1]
  
  range <- list(range_min = SlidingObject[[Winner]]$Dataset$Closest[1],
                range_max = SlidingObject[[Winner]]$Dataset$Furthest[1])
  
  windows <- data.frame(Open = SlidingObject[[Winner]]$Dataset$WindowOpen,
                        Close = SlidingObject[[Winner]]$Dataset$WindowClose)
  
  windows$corr <- NA
  
  for (i in c(1:length(windows$Open))){
    
    ClimMat <- array(data = NA, dim = length(reference$date))
    
    for(j in c(1:length(reference$date))){
      
      seqDate <- seq.Date(from = reference$date[j] %m+% months(-windows$Open[i]),
                          to = reference$date[j] %m+% months(-windows$Close[i]),
                          by = "month")
      ClimMat[j] <- do.call(options$stat,
                            list(Clim[which(lubridate::month(Clim$date) %in% lubridate::month(seqDate) &
                                            lubridate::year(Clim$date) %in% lubridate::year(seqDate)),
                                      options$climate],
                                 na.rm = T))
      
    }
    
    windows$corr[i] <- cor(reference$climate, ClimMat, use = "complete.obs")
    
  }
  windows <- list(correlations = windows,
                  df = data.frame(BestWindowOpen = BestWindowOpen,
                                  BestWindowClose = BestWindowClose)
  )
  return(windows) 
}  



acorplot <- function(cor.output) {

  plot <- ggplot() + 
        geom_tile(data = cor.output$correlations, aes(x = Close, y = Open, fill = corr)) + 
        geom_point(data = cor.output$df, aes(x = BestWindowClose, y = BestWindowOpen, z = NULL), size = 5, shape = 1, stroke = 2) +
        scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", 
                             midpoint = 0, limit = c(-1,1), name = "") + 
        theme_classic() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(size = 0.25, colour = "black"), 
              plot.title = element_text(size = 16, hjust = 0.5)) + 
        ggtitle("Autocorrelation") + 
        ylab("Window open") + 
        xlab("Window close")
  
  return(plot)
  
  }




