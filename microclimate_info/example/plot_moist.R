plot_moist <- function(ncep){
  # simulation output
  if(ncep == 1){
    metout <- as.data.frame(micro.ncep$metout) # above ground microclimatic conditions, min shade
    soil <- as.data.frame(micro.ncep$soil) # soil temperatures, minimum shade
    soilmoist <- as.data.frame(micro.ncep$soilmoist) # soil water content, minimum shade
    rainfall <- as.data.frame(micro.ncep$RAINFALL)
    dates <- micro.ncep$dates
    dates2 <- micro.ncep$dates2
  }else{
    metout <- as.data.frame(micro.aust$metout) # above ground microclimatic conditions, min shade
    soil <- as.data.frame(micro.aust$soil) # soil temperatures, minimum shade
    soilmoist <- as.data.frame(micro.aust$soilmoist) # soil water content, minimum shade
    rainfall <- as.data.frame(micro.aust$RAINFALL)
    dates <- micro.aust$dates
    dates2 <- micro.aust$dates2
  }
  soil <- cbind(dates, soil)
  soilmoist <- cbind(dates, soilmoist)
  metout <- cbind(dates, metout)
  rainfall <- as.data.frame(cbind(dates2, rainfall))
  colnames(rainfall) <- c("dates", "rainfall")

  # subset prediction to years with observations
  plotsoilmoist <- subset(soilmoist, format(soilmoist$dates, "%Y") %in% years)
  plotsoil <- subset(soil, format(soil$dates, "%Y") %in% years)

  TEMPDATES <- plotsoil$dates
  TEMP <- plotsoil[, 4:13] # ten columns of soil temperature for ten depth nodes

  # aggregate prediction to daily
  plotsoilmoist_day<-aggregate(plotsoilmoist, by = list(format(plotsoilmoist$dates, "%d/%m/%Y")), FUN = mean, na.rm = TRUE, na.action = NULL)
  plotsoilmoist_day$Group.1 <- as.POSIXct(plotsoilmoist_day$Group.1,format = "%d/%m/%Y", tz = tzone)
  plotsoilmoist_day <- plotsoilmoist_day[order(plotsoilmoist_day$Group.1), ]

  SMDATES <- plotsoilmoist$dates
  SMDATES_day <- plotsoilmoist_day$dates
  SM <- plotsoilmoist[, 4:13] * 100 # soil moisture predictions
  SM_day <- plotsoilmoist_day[, 4:13] * 100 # soil moisture predictions
  OSMDATES <- as.POSIXct(with(XLdat1, paste(DATE, TIME, sep = " ")), format = "%Y-%m-%d %H:%M")
  OSMDATES_day <- as.POSIXct(XLdat1_day$DATE, format = "%Y-%m-%d")
  DATE_OBS_PRED <- NULL # initialise date, observation, predict matrix
  DATE_OBS_PRED_day <- NULL # initialise date, observation, predict matrix


  if(ncep == 1){
    pdf(paste0("output/",site2do,"_micro_ncep_moist.pdf"))
  }else{
    pdf(paste0("output/",site2do,"_micro_aust_moist.pdf"))
  }
  par(mfrow=c(4, 1))
  par(mar = c(1, 1, 1, 1) + 0.1) # margin spacing stuff
  par(mar = c(4, 4, 3, 4) + 0.1) # margin spacing stuff

  for(depth in obs_depth_soil){
    inode <- which(DEP == depth) # which node represents observation depth
    plot(SMDATES_day, SM_day[, inode + 1], type = 'l', ylim = c(0, 50), ylab = paste(depth, 'cm' ,'moisture (% vol)'), xlab = 'date')
    OSMcol <- 3 + length(obs_depth_temp) + which(obs_depth_soil == depth) # column of soil moisture observations
    XLdat1_day[XLdat1_day[, OSMcol] < 0 & !is.na(XLdat1_day[,OSMcol]), OSMcol] <- NA #get rid of missing data
    lines(OSMDATES_day, XLdat1_day[,OSMcol], col = addTrans("red", 150))
    # append date, observation and prediction - hourly
    x_pred <- approx((SMDATES),  SM[,inode], OSMDATES, method = "linear")
    date_obs_pred <- cbind((OSMDATES), XLdat1[, OSMcol], x_pred$y, rep(inode, length(x_pred$y)))
    # get rid of missing data (moisture < -99 )
    date_obs_pred1 <- subset(date_obs_pred, date_obs_pred[, 2] >= 0)
    # get rid of NAs
    date_obs_pred1 <- subset(date_obs_pred1, date_obs_pred1[, 3] != "NA")
    # append to other site data
    DATE_OBS_PRED <- rbind(DATE_OBS_PRED, date_obs_pred1)

    # append date, observation and prediction - daily
    x_pred_day <- approx((SMDATES_day),  SM_day[, inode + 1], OSMDATES_day, method = "linear")
    date_obs_pred_day <- cbind((OSMDATES_day), XLdat1_day[,OSMcol], x_pred_day$y, rep(inode, length(x_pred_day$y)))
    # get rid of missing data (moisture < -99 )
    date_obs_pred1_day <- subset(date_obs_pred_day, date_obs_pred_day[, 2] >= 0)
    # get rid of NAs
    date_obs_pred1_day <- subset(date_obs_pred1_day, date_obs_pred1_day[,3] != "NA")
    # append to other site data
    DATE_OBS_PRED_day <- rbind(DATE_OBS_PRED_day, date_obs_pred1_day)

    ## end loop for all sites
    mtext(paste0("oznet site ",OzNetsite), side = 3, line = -2, outer = TRUE, font = 4)
  }

  x <- 0:50
  DATE_OBS_PRED_NODE_day <- DATE_OBS_PRED_day
  for(inode in 1:length(DEP)){
    DATE_OBS_PRED_day2 <- subset(DATE_OBS_PRED_NODE_day, inode == DATE_OBS_PRED_NODE_day[, 4])
    if(nrow(DATE_OBS_PRED_day2) > 0){
      r <- cor(DATE_OBS_PRED_day2[, 2], DATE_OBS_PRED_day2[, 3])
      rmsd <- sqrt(mean(((DATE_OBS_PRED_day2[, 2] - DATE_OBS_PRED_day2[, 3]) ^ 2),na.rm = TRUE))
      plot(x, x, type = 'l', main = sprintf('model fit for node %1.0f at %1.0f cm', inode, DEP[inode]), ylab = 'predicted', xlab = 'observed')
      points(DATE_OBS_PRED_day2[, 2], DATE_OBS_PRED_day2[,3], col=addTrans("black",50), pch = 16)
      text(11.5, 44, "       r       rmsd")
      text(10, 36, paste0('NicheMapR ', round(r, 3), '   ', round(rmsd, 2)))
      results <- as.data.frame(cbind(as.character(OzNetsite), "SLGA", DEP[inode], r, rmsd))
      colnames(results) <- c("site", "soil", "depth", "r_NMR", "rmsd_NMR")
      if(ncep == 1){
        filename <- "output/micro_ncep_moist.csv"
      }else{
        filename <- "output/micro_aust_moist.csv"
      }
      if(file.exists(filename) == TRUE){
        write.table(results, filename, append = TRUE, sep = ",", col.names = FALSE,row.names = FALSE)
      }else{
        write.table(results, filename, append = TRUE, sep = ",", col.names = TRUE,row.names = FALSE)
      }
    }
  }
  dev.off()
}
