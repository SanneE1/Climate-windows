## wrapper to calculate and then plot correlations
cor_wrap <- function(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto = FALSE, plot = TRUE){
  
  data <- corr_climate(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto)
  
  if (plot == FALSE){
    return(data)
  } else {
    
    data$correlations <- tidyr::pivot_longer(data$correlations, -c(Open, Close), names_to = "Clim_variable", values_to = "corr")
    data$correlations$Clim_variable <- factor(data$correlations$Clim_variable, 
                                              levels = c("mean_tmin", "mean_tavg", "mean_tmax", 
                                                         "sum_prcp", "sum_snow", "mean_snwd",
                                                         "SPEI"))
    if(species == "HEQU") {
      data$df$Clim_variable <- data$df$Clim_variable %>%
        gsub("ppt", "sum_prcp", x = .) %>%
        gsub("tmin", "mean_tmin", x = .)%>%
        gsub("tmax", "mean_tmax", x = .)%>%
        gsub("tmean", "mean_tmavg", x = .)
    }
    
    data$df$Clim_variable <- factor(data$df$Clim_variable, 
                                              levels = c("mean_tmin", "mean_tavg", "mean_tmax", 
                                                         "sum_prcp", "sum_snow", "mean_snwd",
                                                         "SPEI"))
    variable_title <- c(expression(Correlation~of~T[mean_min]), expression(Correlation~of~T[avg]), expression(Correlation~of~T[mean_max]),
                        expression(Correlation~of~P), expression(Correlation~of~S[fall]), expression(Correlation~of~S[depth]), 
                        expression(Correlation~of~SPEI))
    data$df$Clim_variable_title <- variable_title[as.integer(data$df$Clim_variable)]
    plot <- corplot(data)
    return(plot)
  }
}

# function to calculate climate correlations
corr_climate <- function(species, vitalrate, Climate, SlidingObject, Winner, Only_Auto = FALSE){
  enquo(Climate)

  options <- SlidingObject$combos[Winner,]
  
  
  #### data formatting ----------------------------------
  Clim <- read.csv(Climate)
  Clim$date <- paste("15/",sprintf("%02d", Clim$Month), "/", Clim$Year, sep = "") ### get a date that's accepted by climwin
  
  if(species == "HEQU") {
    Clim <- dplyr::rename(Clim, 
           sum_prcp = ppt,
           mean_tmin = tmin,
           mean_tavg = tmean,
           mean_tmax = tmax)
  }
  #### Set Range ------------------------------
  
  if(vitalrate == "s") {
    if(species == "FRSP"){
      print("Range set to 6 years")
      range <- c(60, -12)
    } else {
      print("Range set to 3 years")
      range <- c(24,-12)
    }
  }
  
  if(vitalrate == "g") {
    if(species == "FRSP") {
      print("Range set to 6 years")
      range <- c(60, -12)
    } else {
      print("Range set to 3 years")
      range <- c(24,-12)
    }
  }
  
  if(vitalrate == "fp") {
    if(species == "FRSP") {
      print("Range set to 4 years")
      range <- c(36, -12)
    } else {
      print("Range set to 3 years")
      range <- c(36, 0)
    }
  }
  
  if(vitalrate == "fn") {
    if(species == "FRSP") {
      print("Range set to 4 years")
      range <- c(48, 0)
    } else {
      print("Range set to 3 years")
      range <- c(36, 0)
    }
  }
  
  
  #### End data fromattin 
  
  # select the right climate window for which to correlate other climate drivers
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
  
# select climate vectors
  if (Only_Auto == FALSE){
    if(species == "FRSP") {
  Clim <- Clim %>%
    select(sum_prcp, mean_tavg, mean_tmin, mean_tmax, SPEI, sum_snow, mean_snwd, date) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y"))
    } else {
      Clim <- Clim %>%
        select(sum_prcp, mean_tavg, mean_tmin, mean_tmax, SPEI, date) %>%
        mutate(date = as.Date(date, format = "%d/%m/%Y"))
    }
  } else {
    Clim <- Clim %>%
      select(options$climate, date )
  }
  
  # get open and closing month of best climate window (in relation to reference month)
  BestWindowOpen <- SlidingObject[[Winner]]$Dataset$WindowOpen[1]
  BestWindowClose <- SlidingObject[[Winner]]$Dataset$WindowClose[1]
  
  range <- list(range_min = SlidingObject[[Winner]]$Dataset$Closest[1],
                range_max = SlidingObject[[Winner]]$Dataset$Furthest[1])
  
  windows <- data.frame(Open = SlidingObject[[Winner]]$Dataset$WindowOpen,
                        Close = SlidingObject[[Winner]]$Dataset$WindowClose)
  
for (x in names(Clim[c(1:length(names(Clim))-1)])) {  # for each climate variable
  
  for (i in c(1:length(windows$Open))){ #  for every possible relative time window
    
    ClimMat <- array(data = NA, dim = length(reference$date))
    
    for(j in c(1:length(reference$date))){ # for each census year
      
      ## create sequence with dates that fall in time window
      seqDate <- seq.Date(from = reference$date[j] %m+% months(-windows$Open[i]),  
                          to = reference$date[j] %m+% months(-(windows$Close[i]-1)),
                          by = "day")
      ## and aggregate climate that falls in this sequence, according to stat column (for now always mean)
      ClimMat[j] <- do.call(as.character(options$stat), 
                            list(Clim[which(as.Date(Clim$date, format = "%d/%m/%Y") %in% seqDate),
                                      x],
                                 na.rm = T))
      
    }
    # test correlation between specific time winows and reference time window across years
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

# plot correlation values along the lines of climwin plots
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
    ggtitle(cor.output$df$Clim_variable_title,
            subtitle = paste("from month", cor.output$df$BestWindowOpen, "to month", cor.output$df$BestWindowClose,
                  "with all climate variables", sep = " ")) +
        ylab("Window open") + 
        xlab("Window close") + 
    facet_wrap(vars(Clim_variable)) +
    theme(plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 20, hjust = 0.5))
  
  return(plot)
  
  }




