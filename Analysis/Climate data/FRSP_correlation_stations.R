library(dplyr)
library(ggplot2)

files = list(
  "1" ="Data/Climate data/FRSP_NOAA_month_1.csv",
  "3" = "Data/Climate data/FRSP_NOAA_month_3.csv",
  "4" = "Data/Climate data/FRSP_NOAA_month_4.csv",
  "5" = "Data/Climate data/FRSP_NOAA_month_5.csv",
  "6" = "Data/Climate data/FRSP_NOAA_month_6.csv",
  "8" = "Data/Climate data/FRSP_NOAA_month_8.csv"
)

Clim <- lapply(files, read.csv)

Clim <- lapply(Clim, function(x) select(.data = x, Month, Year, 
                                        sum_prcp, 
                                        mean_tavg, 
                                        mean_tmin,
                                        mean_tmax, 
                                        min_tmin,
                                        max_tmax))

a <- full_join(Clim[["1"]], Clim[["8"]], by = c("Year", "Month")) 

cor(a$sum_prcp.x, a$sum_prcp.y, use = "complete.obs")
cor(a$mean_tavg.x, a$mean_tavg.y, use = "complete.obs")
cor(a$mean_tmin.x, a$mean_tmin.y, use = "complete.obs")
cor(a$mean_tmax.x, a$mean_tmax.y, use = "complete.obs")
cor(a$min_tmin.x, a$min_tmin.y, use = "complete.obs")
cor(a$max_tmax.x, a$max_tmax.y, use = "complete.obs")

plot(a$sum_prcp.x, a$sum_prcp.y)
plot(a$mean_tavg.x, a$mean_tavg.y)
plot(a$mean_tmin.x, a$mean_tmin.y)
plot(a$mean_tmax.x, a$mean_tmax.y)
plot(a$min_tmin.x, a$min_tmin.y)
plot(a$max_tmax.x, a$max_tmax.y)



b <- full_join(Clim[["1"]], Clim[["3"]], by = c("Year", "Month")) 

cor(b$sum_prcp.x, b$sum_prcp.y, use = "complete.obs")
cor(b$mean_tavg.x, b$mean_tavg.y, use = "complete.obs")
cor(b$mean_tmin.x, b$mean_tmin.y, use = "complete.obs")
cor(b$mean_tmax.x, b$mean_tmax.y, use = "complete.obs")
cor(b$min_tmin.x, b$min_tmin.y, use = "complete.obs")
cor(b$max_tmax.x, b$max_tmax.y, use = "complete.obs")

plot(b$sum_prcp.x, b$sum_prcp.y)
plot(b$mean_tavg.x, b$mean_tavg.y)
plot(b$mean_tmin.x, b$mean_tmin.y)
plot(b$mean_tmax.x, b$mean_tmax.y)
plot(b$min_tmin.x, b$min_tmin.y)
plot(b$max_tmax.x, b$max_tmax.y)


c <- full_join(Clim[["1"]], Clim[["4"]], by = c("Year", "Month")) 

cor(c$sum_prcp.x, c$sum_prcp.y, use = "complete.obs")
cor(c$mean_tavg.x, c$mean_tavg.y, use = "complete.obs")
cor(c$mean_tmin.x, c$mean_tmin.y, use = "complete.obs")
cor(c$mean_tmax.x, c$mean_tmax.y, use = "complete.obs")
cor(c$min_tmin.x, c$min_tmin.y, use = "complete.obs")
cor(c$max_tmax.x, c$max_tmax.y, use = "complete.obs")

plot(c$sum_prcp.x, c$sum_prcp.y)
plot(c$mean_tavg.x, c$mean_tavg.y)
plot(c$mean_tmin.x, c$mean_tmin.y)
plot(c$mean_tmax.x, c$mean_tmax.y)
plot(c$min_tmin.x, c$min_tmin.y)
plot(c$max_tmax.x, c$max_tmax.y)


d <- full_join(Clim[["1"]], Clim[["6"]], by = c("Year", "Month")) 

cor(d$sum_prcp.x, d$sum_prcp.y, use = "complete.obs")
cor(d$mean_tavg.x, d$mean_tavg.y, use = "complete.obs")
cor(d$mean_tmin.x, d$mean_tmin.y, use = "complete.obs")
cor(d$mean_tmax.x, d$mean_tmax.y, use = "complete.obs")
cor(d$min_tmin.x, d$min_tmin.y, use = "complete.obs")
cor(d$max_tmax.x, d$max_tmax.y, use = "complete.obs")

plot(d$sum_prcp.x, d$sum_prcp.y)
plot(d$mean_tavg.x, d$mean_tavg.y)
plot(d$mean_tmin.x, d$mean_tmin.y)
plot(d$mean_tmax.x, d$mean_tmax.y)
plot(d$min_tmin.x, d$min_tmin.y)
plot(d$max_tmax.x, d$max_tmax.y)




e <- full_join(Clim[["1"]], Clim[["5"]], by = c("Year", "Month")) 

cor(e$sum_prcp.x, e$sum_prcp.y, use = "complete.obs")
cor(e$mean_tavg.x, e$mean_tavg.y, use = "complete.obs")
cor(e$mean_tmin.x, e$mean_tmin.y, use = "complete.obs")
cor(e$mean_tmax.x, e$mean_tmax.y, use = "complete.obs")
cor(e$min_tmin.x, e$min_tmin.y, use = "complete.obs")
cor(e$max_tmax.x, e$max_tmax.y, use = "complete.obs")

plot(e$sum_prcp.x, e$sum_prcp.y)
plot(e$mean_tavg.x, e$mean_tavg.y)
plot(e$mean_tmin.x, e$mean_tmin.y)
plot(e$mean_tmax.x, e$mean_tmax.y)
plot(e$min_tmin.x, e$min_tmin.y)
plot(e$max_tmax.x, e$max_tmax.y)

