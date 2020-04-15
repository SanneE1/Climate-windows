cor_wrap <- function(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto = FALSE, plot = TRUE){
  
  data <- corr_climate(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto)
  
  if (plot == FALSE){
    return(data)
  } else {
    
    data$correlations <- tidyr::pivot_longer(data$correlations, -c(Open, Close), names_to = "Clim_variable", values_to = "corr")
    plot <- corplot(data)
    return(plot)
  }
}


corr_climate <- function(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto = FALSE){
  enquo(Climate)

  options <- SlidingObject$combos[Winner,]
  
  Clim <- read.csv(Climate)                                                ### get a date that's accepted by climwin
  Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "")          

  #### Set Range ----------------------------------------------------------------------------------------------------------------------------
  if(vitalrate == "s") {
    if(species == "FRSP") {
      print("Range set to 6 years")
      range <- c(60, -12)
    } else {
    range <- c(24,-12)
    }
  }
  
  if(vitalrate == "g") {
    if(species == "FRSP") {
      print("Range set to 6 years")
      range <- c(60, -12)
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
  

  if (Only_Auto == FALSE){
  Clim <- Clim %>%
    select(sum_prcp, mean_tavg, mean_tmin, mean_tmax, SPEI, date) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
  } else {
    Clim <- Clim %>%
      select(options$climate, date )
  }
  
  BestWindowOpen <- SlidingObject[[Winner]]$Dataset$WindowOpen[1]
  BestWindowClose <- SlidingObject[[Winner]]$Dataset$WindowClose[1]
  
  range <- list(range_min = SlidingObject[[Winner]]$Dataset$Closest[1],
                range_max = SlidingObject[[Winner]]$Dataset$Furthest[1])
  
  windows <- data.frame(Open = SlidingObject[[Winner]]$Dataset$WindowOpen,
                        Close = SlidingObject[[Winner]]$Dataset$WindowClose)
  
for (x in names(Clim[c(1:length(names(Clim))-1)])) {
  
  for (i in c(1:length(windows$Open))){
    
    ClimMat <- array(data = NA, dim = length(reference$date))
    
    for(j in c(1:length(reference$date))){
      
      seqDate <- seq.Date(from = reference$date[j] %m+% months(-windows$Open[i]),
                          to = reference$date[j] %m+% months(-(windows$Close[i]-1)),
                          by = "day")
      ClimMat[j] <- do.call(options$stat,
                            list(Clim[which(as.Date(Clim$date, format = "%d/%m/%Y") %in% seqDate),
                                      x],
                                 na.rm = T))
      
    }
    
    windows[i, x] <- cor(reference$climate, ClimMat, use = "complete.obs")
    
  }
}
  windows <- list(correlations = windows,
                  df = data.frame(Clim_variable = as.character(options$climate),
                                  BestWindowOpen = BestWindowOpen,
                                  BestWindowClose = BestWindowClose)
  )
  
  return(windows) 
}  


corplot <- function(cor.output) {
  
  plot <- ggplot() + 
        geom_tile(data = cor.output$correlations, aes(x = Close, y = Open, fill = corr)) + 
        geom_point(data = cor.output$df, aes(x = BestWindowClose, y = BestWindowOpen, z = NULL), 
                   size = 5, shape = 1, stroke = 2) +
        scale_fill_gradient2(low = "red", mid = "yellow", high = "blue", 
                             midpoint = 0, limit = c(-1,1), name = "") + 
        theme_classic() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(size = 0.25, colour = "black"), 
              plot.title = element_text(size = 16, hjust = 0.5)) + 
    ggtitle(paste("Correlation between the mean", cor.output$df$Clim_variable, "from \nmonth", cor.output$df$BestWindowOpen, "to month", cor.output$df$BestWindowClose,
                  "with all climate variables", sep = " ")) +
        ylab("Window open") + 
        xlab("Window close") + 
    facet_wrap(vars(Clim_variable))
  
  return(plot)
  
  }




