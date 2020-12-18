my_update <- function(mod, formula = NULL, data = NULL) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }
  
  if (!is.null(data)) call$data <- data
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")
  
  eval(call, env, parent.frame())
}

own.singlewin <- function (xvar, cdate, bdate, baseline, range, stat, func, type, 
          refday, cmissing = FALSE, cinterval = "day", cohort = NULL, 
          spatial = NULL, upper = NA, lower = NA, binary = FALSE, 
          centre = list(NULL, "both"), cutoff.day = NULL, cutoff.month = NULL, 
          furthest = NULL, closest = NULL, thresh = NULL) 
{
  if (getOption("scipen") < 0) {
    current_option <- getOption("scipen")
    options(scipen = 0)
  }
  if (all(is.na(as.Date(cdate, format = "%d/%m/%Y")))) {
    stop("cdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
  }
  if (all(is.na(as.Date(bdate, format = "%d/%m/%Y")))) {
    stop("bdate is not in the correct format. Please provide date data in dd/mm/yyyy.")
  }
  if (is.null(cohort) == TRUE) {
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))
  }
  thresholdQ <- "N"
  if ((!is.na(upper) || !is.na(lower)) && (cinterval == "week" || 
                                           cinterval == "month")) {
    thresholdQ <- readline("You specified a climate threshold using upper and/or lower and are working at a weekly or monthly scale. \n                           Do you want to apply this threshold before calculating weekly/monthly means (i.e. calculate thresholds for each day)? Y/N")
    thresholdQ <- toupper(thresholdQ)
    if (thresholdQ != "Y" & thresholdQ != "N") {
      thresholdQ <- readline("Please specify yes (Y) or no (N)")
    }
  }
  if (is.null(thresh) == FALSE) {
    stop("Parameter 'thresh' is now redundant. Please use parameter 'binary' instead.")
  }
  if (type == "variable" || type == "fixed") {
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  if (is.null(furthest) == FALSE & is.null(closest) == FALSE) {
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  if (is.null(cutoff.day) == FALSE & is.null(cutoff.month) == 
      FALSE) {
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  xvar = xvar[[1]]
  if (stat == "slope" & func == "log" || stat == "slope" & 
      func == "inv") {
    stop("stat = slope cannot be used with func = LOG or I as negative values may be present")
  }
  duration <- (range[1] - range[2]) + 1
  bdate <- as.Date(bdate, format = "%d/%m/%Y")
  if (is.null(spatial) == FALSE) {
    SUB.DATE <- list()
    NUM <- 1
    for (i in levels(as.factor(spatial[[2]]))) {
      SUB <- cdate[which(spatial[[2]] == i)]
      SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB, 
                                                           format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")), 
                                               "days"), spatial = i)
      if (length(SUB.DATE[[NUM]]$Date) != length(unique(SUB.DATE[[NUM]]$Date))) {
        stop("There are duplicate dayrecords in climate data")
      }
      NUM <- NUM + 1
    }
    spatialcdate <- plyr::rbind.fill(SUB.DATE)
    cdate2 <- spatialcdate$Date
    cintno <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 
      1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 
      1
  }
  else {
    cdate2 <- seq(min(as.Date(cdate, format = "%d/%m/%Y")), 
                  max(as.Date(cdate, format = "%d/%m/%Y")), "days")
    cintno <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 
      1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 
      1
    if (length(cintno) != length(unique(cintno))) {
      stop("There are duplicate dayrecords in climate data")
    }
  }
  cdate <- as.Date(cdate, format = "%d/%m/%Y")
  if (is.null(spatial) == FALSE) {
    for (i in levels(as.factor(spatial[[2]]))) {
      SUB <- cdate[which(spatial[[2]] == i)]
      if (min(SUB) > min(bdate) | max(SUB) < max(bdate)) {
        stop("Climate data does not cover all years of biological data. Please increase range of climate data")
      }
    }
  }
  else if (min(cdate) > min(bdate) | max(cdate) < max(bdate)) {
    stop("Climate data does not cover all years of biological data. Please increase range of climate data")
  }
  if (is.null(spatial) == FALSE) {
    xvar <- data.frame(Clim = xvar, spatial = spatial[[2]])
    cdate <- data.frame(Date = cdate, spatial = spatial[[2]])
    split.list <- list()
    NUM <- 1
    for (i in levels(xvar$spatial)) {
      SUB <- subset(xvar, spatial == i)
      SUBcdate <- subset(cdate, spatial == i)
      SUBcdate2 <- subset(spatialcdate, spatial == i)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial)
      NUM <- NUM + 1
    }
    xvar <- (plyr::rbind.fill(split.list))$NewClim
    climspatial <- (plyr::rbind.fill(split.list))$Newspatial
  }
  else {
    xvar <- xvar[match(cdate2, cdate)]
  }
  if (cinterval != "day" && cinterval != "week" && cinterval != 
      "month") {
    stop("cinterval should be either day, week or month")
  }
  if (cinterval == "day") {
    if (type == "absolute") {
      if (is.null(cohort) == FALSE) {
        newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum <- 1
        bintno <- seq(1, length(bdate), 1)
        for (i in levels(as.factor(cohort))) {
          sub <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], 
                                                                        refday[2], min(lubridate::year(sub$bdate)), 
                                                                        sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 
            1
        }
      }
      else {
        bintno <- as.numeric(as.Date(paste(refday[1], 
                                           refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - 
          min(as.numeric(cdate2)) + 1
      }
    }
    else {
      bintno <- realbintno
    }
  }
  else if (cinterval == "week") {
    if (!is.na(thresholdQ) && thresholdQ == "Y") {
      if (binary == T) {
        if (is.na(upper) == FALSE && is.na(lower) == 
            TRUE) {
          xvar <- ifelse(xvar > upper, 1, 0)
        }
        else if (is.na(upper) == TRUE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar < lower, 1, 0)
        }
        else if (is.na(upper) == FALSE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         1, 0)
        }
      }
      else {
        if (is.na(upper) == FALSE && is.na(lower) == 
            TRUE) {
          xvar <- ifelse(xvar > upper, xvar, 0)
        }
        else if (is.na(upper) == TRUE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar < lower, xvar, 0)
        }
        else if (is.na(upper) == FALSE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         xvar, 0)
        }
      }
    }
    cweek <- lubridate::week(cdate2)
    cweek[which(cweek == 53)] <- 52
    cyear <- lubridate::year(cdate2) - min(lubridate::year(cdate2))
    cintno <- cweek + 52 * cyear
    realbintno <- lubridate::week(bdate) + 52 * (lubridate::year(bdate) - 
                                                   min(lubridate::year(cdate2)))
    if (is.null(spatial) == FALSE) {
      newclim <- data.frame(cintno = cintno, xvar = xvar, 
                            spatial = climspatial)
      newclim2 <- melt(newclim, id = c("cintno", "spatial"))%>% filter(!is.na(xvar))
      newclim3 <- cast(newclim2, cintno + spatial ~ variable, 
                       stat, na.rm = T)
      newclim3 <- newclim3[order(newclim3$spatial, newclim3$cintno), 
      ]
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
      climspatial <- newclim3$spatial
    }
    else {
      newclim <- data.frame(cintno = cintno, xvar = xvar)
      newclim2 <- melt(newclim, id = "cintno")%>% filter(!is.na(xvar))
      newclim3 <- cast(newclim2, cintno ~ variable, stat, 
                       na.rm = T)
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
    }
    if (type == "absolute") {
      newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
      datenum <- 1
      bintno <- seq(1, length(bdate), 1)
      for (i in levels(as.factor(cohort))) {
        sub <- subset(newdat, cohort == i)
        bintno[as.numeric(rownames(sub))] <- lubridate::month(as.Date(paste(refday[1], 
                                                                            refday[2], min(lubridate::year(sub$bdate)), 
                                                                            sep = "-"), format = "%d-%m-%Y")) + 53 * (min(lubridate::year(sub$bdate)) - 
                                                                                                                        min(year(cdate2)))
      }
    }
    else {
      bintno <- realbintno
    }
  }
  else if (cinterval == "month") {
    if (!is.na(thresholdQ) && thresholdQ == "Y") {
      if (binary == T) {
        if (is.na(upper) == FALSE && is.na(lower) == 
            TRUE) {
          xvar <- ifelse(xvar > upper, 1, 0)
        }
        else if (is.na(upper) == TRUE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar < lower, 1, 0)
        }
        else if (is.na(upper) == FALSE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         1, 0)
        }
      }
      else {
        if (is.na(upper) == FALSE && is.na(lower) == 
            TRUE) {
          xvar <- ifelse(xvar > upper, xvar, 0)
        }
        else if (is.na(upper) == TRUE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar < lower, xvar, 0)
        }
        else if (is.na(upper) == FALSE && is.na(lower) == 
                 FALSE) {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         xvar, 0)
        }
      }
    }
    cmonth <- lubridate::month(cdate2)
    cyear <- year(cdate2) - min(year(cdate2))
    cintno <- cmonth + 12 * cyear
    realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - 
                                                    min(year(cdate2)))
    if (is.null(spatial) == FALSE) {
      newclim <- data.frame(cintno = cintno, xvar = xvar, 
                            spatial = climspatial)
      newclim2 <- melt(newclim, id = c("cintno", "spatial"))%>% filter(!is.na(xvar))
      newclim3 <- cast(newclim2, cintno + spatial ~ variable, 
                       stat)
      newclim3 <- newclim3[order(newclim3$spatial, newclim3$cintno), 
      ]
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
      climspatial <- newclim3$spatial
    }
    else {
      newclim <- data.frame(cintno = cintno, xvar = xvar)
      newclim2 <- melt(newclim, id = "cintno") %>% filter(!is.na(xvar))
      newclim3 <- cast(newclim2, cintno ~ variable, stat)
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
    }
    if (type == "absolute") {
      newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
      datenum <- 1
      bintno <- seq(1, length(bdate), 1)
      for (i in levels(as.factor(cohort))) {
        sub <- subset(newdat, cohort == i)
        bintno[as.numeric(rownames(sub))] <- refday[2] + 
          12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
      }
    }
    else {
      bintno <- realbintno
    }
  }
  if (cinterval == "day") {
    if ((min(bintno) - range[1]) < min(cintno)) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  # if (cinterval == "month") {
  #   if ((as.numeric(min(bdate) - 
  #                   months(range[1])) - (as.numeric(min(cdate)))) <= 0) {
  #     stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
  #   }
  # }
  if (max(bintno) > max(cintno)) {
    if (type == "absolute") {
      stop("You need more recent biological data. This error may be caused by your choice of refday")
    }
    else {
      stop("You need more recent biological data")
    }
  }
  if (class(baseline)[1] == "lme") {
    baseline <- update(baseline, . ~ .)
  }
  else {
    baseline <- my_update(baseline, . ~ .)
  }
  nullmodel <- MuMIn::AICc(baseline)
  modlist <- list()
  cmatrix <- matrix(ncol = (duration), nrow = length(bdate))
  modeldat <- model.frame(baseline)
  if (attr(baseline, "class")[1] == "lme") {
    if (is.null(baseline$modelStruct$varStruct) == FALSE && 
        !is.null(attr(baseline$modelStruct$varStruct, "groups"))) {
      modeldat <- cbind(modeldat, attr(baseline$modelStruct$varStruct, 
                                       "groups"))
      colnames(modeldat)[ncol(modeldat)] <- strsplit(x = as.character(attr(baseline$modelStruct$varStruct, 
                                                                           "formula"))[2], split = " | ")[[1]][3]
    }
    non_rand <- ncol(modeldat)
    modeldat <- cbind(modeldat, baseline$data[, colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% 
                                                                                   "fixed")]])
    colnames(modeldat)[-(1:non_rand)] <- colnames(baseline$fitted)[-which(colnames(baseline$fitted) %in% 
                                                                            "fixed")]
    modeloutput <- update(baseline, . ~ ., data = modeldat)
  }
  colnames(modeldat)[1] <- "yvar"
  if (is.null(centre[[1]]) == FALSE) {
    func = "centre"
  }
  if (length(modeldat$yvar) != length(bdate)) {
    stop("NA values present in biological response. Please remove NA values")
  }
  if (cinterval == "day" || (!is.na(thresholdQ) && thresholdQ == 
                             "N")) {
    if (is.null(spatial) == FALSE) {
      if (is.na(upper) == FALSE && is.na(lower) == TRUE) {
        if (binary == TRUE) {
          xvar$Clim <- ifelse(xvar$Clim > upper, 1, 
                              0)
        }
        else {
          xvar$Clim <- ifelse(xvar$Clim > upper, xvar$Clim, 
                              0)
        }
      }
      if (is.na(lower) == FALSE && is.na(upper) == TRUE) {
        if (binary == TRUE) {
          xvar$Clim <- ifelse(xvar$Clim < lower, 1, 
                              0)
        }
        else {
          xvar$Clim <- ifelse(xvar$Clim < lower, xvar$Clim, 
                              0)
        }
      }
      if (is.na(lower) == FALSE && is.na(upper) == FALSE) {
        if (binary == TRUE) {
          xvar$Clim <- ifelse(xvar$Clim > lower && xvar$Clim < 
                                upper, 1, 0)
        }
        else {
          xvar$Clim <- ifelse(xvar$Clim > lower && xvar$Clim < 
                                upper, xvar$Clim - lower, 0)
        }
      }
    }
    else {
      if (!is.na(upper) && is.na(lower)) {
        if (binary == TRUE) {
          xvar <- ifelse(xvar > upper, 1, 0)
        }
        else {
          xvar <- ifelse(xvar > upper, xvar, 0)
        }
      }
      if (is.na(lower) == FALSE && is.na(upper) == TRUE) {
        if (binary == TRUE) {
          xvar <- ifelse(xvar < lower, 1, 0)
        }
        else {
          xvar <- ifelse(xvar < lower, xvar, 0)
        }
      }
      if (is.na(lower) == FALSE && is.na(upper) == FALSE) {
        if (binary == TRUE) {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         1, 0)
        }
        else {
          xvar <- ifelse(xvar > lower & xvar < upper, 
                         xvar - lower, 0)
        }
      }
    }
  }
  if (is.null(spatial) == FALSE) {
    cintno = data.frame(Date = cintno, spatial = climspatial)
    bintno = data.frame(Date = bintno, spatial = spatial[[1]])
    xvar = data.frame(Clim = xvar, spatial = climspatial)
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- xvar[which(cintno$spatial %in% bintno$spatial[i] & 
                                   cintno$Date %in% (bintno$Date[i] - c(range[2]:range[1]))), 
                           1]
    }
  }
  else {
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- xvar[which(cintno %in% (bintno[i] - 
                                                c(range[2]:range[1])))]
    }
  }
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  # if (cmissing == FALSE && any(is.na(cmatrix))) {
  #   if (is.null(spatial) == FALSE) {
  #     if (cinterval == "day") {
  #       .GlobalEnv$missing <- as.Date(cintno$Date[is.na(xvar$Clim)], 
  #                                     origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 
  #                                       1)
  #     }
  #     if (cinterval == "month") {
  #       .GlobalEnv$missing <- c(paste("Month:", cintno$Date[is.na(xvar$Clim)] - 
  #                                       (floor(cintno$Date[is.na(xvar$Clim)]/12) * 
  #                                          12), "Year:", lubridate::year(min(as.Date(cdate, 
  #                                                                                    format = "%d/%m/%Y"))) + floor(cintno$Date[is.na(xvar$Clim)]/12)))
  #     }
  #     if (cinterval == "week") {
  #       .GlobalEnv$missing <- c(paste("Week:", cintno$Date[is.na(xvar$Clim)] - 
  #                                       (floor(cintno$Date[is.na(xvar$Clim)]/52) * 
  #                                          52), "Year:", lubridate::year(min(as.Date(cdate, 
  #                                                                                    format = "%d/%m/%Y"))) + floor(cintno$Date[is.na(xvar$Clim)]/52)))
  #     }
  #   }
  #   else {
  #     if (cinterval == "day") {
  #       .GlobalEnv$missing <- as.Date(cintno[is.na(xvar)], 
  #                                     origin = min(as.Date(cdate, format = "%d/%m/%Y")) - 
  #                                       1)
  #     }
  #     if (cinterval == "month") {
  #       .GlobalEnv$missing <- c(paste("Month:", (lubridate::month(min(as.Date(cdate, 
  #                                                                             format = "%d/%m/%Y"))) + (which(is.na(xvar)) - 
  #                                                                                                         1)) - (floor((lubridate::month(min(as.Date(cdate, 
  #                                                                                                                                                    format = "%d/%m/%Y"))) + (which(is.na(xvar)) - 
  #                                                                                                                                                                                1))/12) * 12), "Year:", (floor((which(is.na(xvar)) - 
  #                                                                                                                                                                                                                  1)/12) + lubridate::year(min(as.Date(cdate, 
  #                                                                                                                                                                                                                                                       format = "%d/%m/%Y"))))))
  #     }
  #     if (cinterval == "week") {
  #       .GlobalEnv$missing <- c(paste("Week:", cintno[is.na(xvar)] - 
  #                                       (floor(cintno[is.na(xvar)]/52) * 52), "Year:", 
  #                                     lubridate::year(min(as.Date(cdate, format = "%d/%m/%Y"))) + 
  #                                       floor(cintno[is.na(xvar)]/52)))
  #     }
  #   }
  #   stop(c("Climate data should not contain NA values: ", 
  #          length(.GlobalEnv$missing), " NA value(s) found. Please add missing climate data or set cmissing to `method1` or `method2`.\n           See object 'missing' for all missing climate data"))
  # }
  if (cmissing != FALSE && any(is.na(cmatrix))) {
    message("Missing climate data detected. Please wait while NAs are replaced.")
    for (i in which(is.na(cmatrix))) {
      if (i%%nrow(cmatrix) == 0) {
        col <- i/nrow(cmatrix)
        row <- nrow(cmatrix)
      }
      else {
        col <- i%/%nrow(cmatrix) + 1
        row <- i%%nrow(cmatrix)
      }
      if (cmissing == "method1") {
        if (cinterval == "day") {
          cdate_new <- data.frame(Date = as.Date(cdate, 
                                                 format = "%d/%m/%Y"))
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          missingdate <- bioldate - (col + range[2] - 
                                       1)
          if (is.null(spatial) == FALSE) {
            cdate_new$spatial <- spatial[[2]]
            siteID <- spatial[[1]][row]
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% 
                                                   c(missingdate - (1:2), missingdate + (1:2)) & 
                                                   cdate_new$spatial %in% siteID)], na.rm = T)
          }
          else {
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% 
                                                   c(missingdate - (1:2), missingdate + (1:2)))], 
                                      na.rm = T)
          }
        }
        else if (cinterval == "week" || cinterval == 
                 "month") {
          if (is.null(spatial) == FALSE) {
            cdate_new <- data.frame(Date = cintno$Date, 
                                    spatial = cintno$spatial)
            bioldate <- bintno$Date[row]
            missingdate <- bioldate - (col + range[2] - 
                                         1)
            siteID <- spatial[[1]][row]
            cmatrix[row, col] <- mean(xvar$Clim[which(cdate_new$Date %in% 
                                                        c(missingdate - (1:2), missingdate + (1:2)) & 
                                                        cdate_new$spatial %in% siteID)], na.rm = T)
          }
          else {
            cdate_new <- data.frame(Date = cintno)
            bioldate <- bintno[row]
            missingdate <- bioldate - (col + range[2] - 
                                         1)
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% 
                                                   c(missingdate - (1:2), missingdate + (1:2)))], 
                                      na.rm = T)
          }
        }
        if (is.na(cmatrix[row, col])) {
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
        }
      }
      else if (cmissing == "method2") {
        if (cinterval == "day") {
          cdate_new <- data.frame(Date = as.Date(cdate, 
                                                 format = "%d/%m/%Y"), Month = lubridate::month(as.Date(cdate, 
                                                                                                        format = "%d/%m/%Y")), Day = lubridate::day(as.Date(cdate, 
                                                                                                                                                            format = "%d/%m/%Y")))
          bioldate <- as.Date(bdate[row], format = "%d/%m/%Y")
          missingdate <- bioldate - (col + range[2] - 
                                       1)
          missingdate <- data.frame(Date = missingdate, 
                                    Month = lubridate::month(missingdate), Day = lubridate::day(missingdate))
          if (is.null(spatial) == FALSE) {
            cdate_new$spatial <- spatial[[2]]
            siteID <- spatial[[1]][row]
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% 
                                                   missingdate$Month & cdate_new$Day %in% 
                                                   missingdate$Day & cdate_new$spatial %in% 
                                                   siteID)], na.rm = T)
          }
          else {
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Month %in% 
                                                   missingdate$Month & cdate_new$Day %in% 
                                                   missingdate$Day)], na.rm = T)
          }
        }
        else if (cinterval == "week" || cinterval == 
                 "month") {
          if (is.null(spatial) == FALSE) {
            cdate_new <- data.frame(Date = cintno$Date, 
                                    spatial = cintno$spatial)
            bioldate <- bintno$Date[row]
            missingdate <- bioldate - (col + range[2] - 
                                         1)
            if (cinterval == "week") {
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 
                                                    52)
              cdate_new$Date <- ifelse(cdate_new$Date == 
                                         0, 52, cdate_new$Date)
              missingdate <- missingdate - (floor(missingdate/52) * 
                                              52)
              missingdate <- ifelse(missingdate == 0, 
                                    52, missingdate)
            }
            else {
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 
                                                    12)
              cdate_new$Date <- ifelse(cdate_new$Date == 
                                         0, 12, cdate_new$Date)
              missingdate <- missingdate - (floor(missingdate/12) * 
                                              12)
              missingdate <- ifelse(missingdate == 0, 
                                    12, missingdate)
            }
            siteID <- spatial[[1]][row]
            cmatrix[row, col] <- mean(xvar$Clim[which(cdate_new$Date %in% 
                                                        missingdate & cdate_new$spatial %in% siteID)], 
                                      na.rm = T)
          }
          else {
            cdate_new <- data.frame(Date = cintno)
            bioldate <- bintno[row]
            missingdate <- bioldate - (col + range[2] - 
                                         1)
            if (cinterval == "week") {
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/52) * 
                                                    52)
              cdate_new$Date <- ifelse(cdate_new$Date == 
                                         0, 52, cdate_new$Date)
              missingdate <- missingdate - (floor(missingdate/52) * 
                                              52)
              missingdate <- ifelse(missingdate == 0, 
                                    52, missingdate)
            }
            else {
              cdate_new$Date <- cdate_new$Date - (floor(cdate_new$Date/12) * 
                                                    12)
              cdate_new$Date <- ifelse(cdate_new$Date == 
                                         0, 12, cdate_new$Date)
              missingdate <- missingdate - (floor(missingdate/12) * 
                                              12)
              missingdate <- ifelse(missingdate == 0, 
                                    12, missingdate)
            }
            cmatrix[row, col] <- mean(xvar[which(cdate_new$Date %in% 
                                                   missingdate)], na.rm = T)
          }
        }
        if (is.na(cmatrix[row, col])) {
          stop("There is not enough data to replace missing values using method2. Consider dealing with NA values manually")
        }
      }
      else {
        stop("cmissing should be method1, method2 or FALSE")
      }
    }
  }
  if ("(weights)" %in% colnames(model.frame(baseline))) {
    modeldat$model_weights <- weights(baseline)
    call <- as.character(getCall(baseline))
    weight_name <- call[length(call)]
    names(modeldat)[length(names(modeldat))] <- weight_name
  }
  if (!is.null(attr(class(baseline), "package")) && attr(class(baseline), 
                                                         "package") == "lme4" && class(baseline)[1] == "lmerMod" && 
      baseline@resp$REML == 1) {
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    baseline <- update(baseline, yvar ~ ., data = modeldat, 
                       REML = F)
  }
  if (attr(baseline, "class")[1] == "lme" && baseline$method == 
      "REML") {
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    baseline <- update(baseline, yvar ~ ., data = modeldat, 
                       method = "ML")
  }
  if (!is.null(attr(class(baseline), "package")) && attr(class(baseline), 
                                                         "package") == "lme4" && class(baseline)[1] == "lmerMod" && 
      baseline@resp$REML == 1) {
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    baseline <- update(baseline, yvar ~ ., data = modeldat, 
                       REML = F)
  }
  if (attr(baseline, "class")[1] == "lme" && baseline$method == 
      "REML") {
    message("Linear mixed effects models are run in climwin using maximum likelihood. Baseline model has been changed to use maximum likelihood.")
    baseline <- update(baseline, yvar ~ ., data = modeldat, 
                       method = "ML")
  }
  if (attr(baseline, "class")[1] == "lme") {
    modeldat$climate <- seq(1, nrow(modeldat), 1)
  }
  else {
    modeldat$climate <- 1
  }
  if (func == "lin") {
    modeloutput <- update(baseline, yvar ~ . + climate, 
                          data = modeldat)
  }
  else if (func == "quad") {
    modeloutput <- update(baseline, yvar ~ . + climate + 
                            I(climate^2), data = modeldat)
  }
  else if (func == "cub") {
    modeloutput <- update(baseline, yvar ~ . + climate + 
                            I(climate^2) + I(climate^3), data = modeldat)
  }
  else if (func == "log") {
    modeloutput <- update(baseline, yvar ~ . + log(climate), 
                          data = modeldat)
  }
  else if (func == "inv") {
    modeloutput <- update(baseline, yvar ~ . + I(climate^-1), 
                          data = modeldat)
  }
  else if (func == "centre") {
    if (centre[[2]] == "both") {
      modeldat$wgdev <- matrix(ncol = 1, nrow = nrow(cmatrix), 
                               seq(from = 1, to = nrow(cmatrix), by = 1))
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix), 
                                seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgdev + 
                              wgmean, data = modeldat)
    }
    if (centre[[2]] == "mean") {
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix), 
                                seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgmean, 
                            data = modeldat)
    }
    if (centre[[2]] == "dev") {
      modeldat$wgdev <- matrix(ncol = 1, nrow = nrow(cmatrix), 
                               seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgdev, 
                            data = modeldat)
    }
  }
  else {
    stop("Define func")
  }
  m <- range[2]
  n <- duration
  if (stat == "slope") {
    time <- n:1
    modeldat$climate <- apply(cmatrix, 1, FUN = function(x) coef(lm(x ~ 
                                                                      time))[2])
  }
  else {
    ifelse(n == 1, modeldat$climate <- cmatrix, modeldat$climate <- apply(cmatrix, 
                                                                          1, FUN = stat))
  }
  if (is.null(centre[[1]]) == FALSE) {
    if (centre[[2]] == "both") {
      modeldat$wgdev <- wgdev(modeldat$climate, centre[[1]])
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
    }
    if (centre[[2]] == "mean") {
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
    }
    if (centre[[2]] == "dev") {
      modeldat$wgdev <- wgdev(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ . + wgdev, 
                               data = modeldat)
    }
  }
  else {
    LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
  }
  LocalData <- model.frame(LocalBestModel)
  if (exists("current_option")) {
    options(scipen = current_option)
  }
  return(list(BestModel = LocalBestModel, BestModelData = LocalData, 
              Dataset = data.frame(ModelAICc = MuMIn::AICc(LocalBestModel), 
                                   deltaAICc = MuMIn::AICc(LocalBestModel) - nullmodel, WindowOpen = range[1], 
                                   WindowClose = range[2], Function = func)))
}
