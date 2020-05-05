DashSeasons <- function(ggplot) {
  
  Diag <- data.frame(
    x = c(1,1,1.2,1.2), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y = c(0,0,0.25,0.25),
    x2 = c(1.1,1.1,1.3,1.3), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y2 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x3 = c(1.2,1.2,1.4,1.4), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y3 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x4 = c(1.3,1.3,1.45,1.45), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y4 = c(0,0,0.1875,0.1875),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x5 = c(1.4,1.4,1.45,1.45), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y5 = c(0,0,0.0625,0.0625),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x6 = c(0.9,0.9,1.1,1.1), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y6 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x7 = c(.8,.8,1,1), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y7 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x8 = c(0.7,0.7,0.9,0.9), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y8 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x9 = c(.8,.8,1,1), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y9 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x10 = c(.6,.6,0.8,0.8), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y10 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x11 = c(.7,.7,0.9,0.9), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y11 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x12 = c(.6,.6,0.8,0.8), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y12 = c(0,0,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x13 = c(.55,.55,0.7,0.7), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y13 = c(0.0625,0.0625,0.25,0.25),# inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
    x14 = c(.55,.55,0.6,0.6), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
    y14 = c(0.1875,0.1875,0.25,0.25)
  )
  
  DiagP <- Diag
  DiagP[,seq(from = 1, to = 27, by = 2)] <- DiagP[,seq(from = 1, to = 27, by = 2)] + 1
  DiagP[DiagP == 0.25] <- 0.2241379
  DiagP[c(1,2), 28] <- 0.175
  
  # Diag <- data.frame(apply(Diag, c(1,2), function(x) x * (9/20)))
  
  plot <- ggplot + 
    geom_path(data=Diag, aes(x=x, y=y, colour = "Explicitely considers \ndormant season"), size = 1)+  # calls co-or for sig. line & draws
    geom_path(data=Diag, aes(x=x2, y=y2),colour = "black", size = 1)+  # calls co-or for sig. line & draws
    geom_path(data=Diag, aes(x=x3, y=y3),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x4, y=y4),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x5, y=y5),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x6, y=y6),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x7, y=y7),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x8, y=y8),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x9, y=y9),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x10, y=y10),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x11, y=y11),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x12, y=y12),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x13, y=y13),colour = "black", size = 1)+
    geom_path(data=Diag, aes(x=x14, y=y14),colour = "black", size = 1) +
    
    geom_path(data=DiagP, aes(x=x, y=y),colour = "black", size = 1)+  
    geom_path(data=DiagP, aes(x=x2, y=y2),colour = "black", size = 1)+  
    geom_path(data=DiagP, aes(x=x3, y=y3),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x4, y=y4),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x5, y=y5),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x6, y=y6),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x7, y=y7),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x8, y=y8),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x9, y=y9),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x10, y=y10),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x11, y=y11),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x12, y=y12),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x13, y=y13),colour = "black", size = 1)+
    geom_path(data=DiagP, aes(x=x14, y=y14),colour = "black", size = 1)+
    scale_color_manual(values = c("Explicitely considers \ndormant season" = "black"),
                       name = c(""))
  
  return(plot)
}

