plot_temp <- function(ncep){
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
  DATE_OBS_PRED_soiltemp <- NULL # initialise date, observation, predict matrix

  if(ncep == 1){
    pdf(paste0("output/",site2do,"_micro_ncep_temp.pdf"))
  }else{
    pdf(paste0("output/",site2do,"_micro_aust_temp.pdf"))
  }

  par(mfrow=c(4,1))
  par(mar = c(1,1,1,1) + 0.1) # margin spacing stuff
  par(mar = c(4,4,3,4) + 0.1) # margin spacing stuff


  for(depth in obs_depth_temp){
    inode<-which(DEP==depth) # which node represents observation depth
    plot(TEMPDATES, TEMP[,inode], type = 'l', ylim = c(-10,70), ylab = bquote(paste(.(depth), ' cm', ' temperature (', ~degree~C,')')), xlab ='date')
    OTEMPcol <- 2+which(obs_depth_temp==depth) # column of soil moisture observations
    XLdat1[XLdat1[,OTEMPcol] < -20 & !is.na(XLdat1[,OTEMPcol]),OTEMPcol] <- NA #get rid of missing data
    lines(OTEMPDATES, XLdat1[,OTEMPcol],col=addTrans("red",150))
    mtext(paste("oznet site ",OzNetsite,sep=""), side = 3, line = -2, outer = TRUE, font = 4)
    # append date, observation and prediction - hourly
    x_pred_stemp<-approx((TEMPDATES),  TEMP[,inode], OTEMPDATES, method = "linear")
    date_obs_pred <- cbind((OTEMPDATES), XLdat1[,OTEMPcol], x_pred_stemp$y, rep(inode, length(x_pred_stemp$y)))
    # get rid of missing data (temp < -99 )
    date_obs_pred1<- subset(date_obs_pred, date_obs_pred[,2]>=-99)
    # get rid of NAs
    date_obs_pred1<- subset(date_obs_pred1, date_obs_pred1[,3]!="NA")
    # append to other site data
    DATE_OBS_PRED_soiltemp <- rbind(DATE_OBS_PRED_soiltemp, date_obs_pred1)
  }


  par(mfrow=c(4,1))
  par(mar = c(1,1,1,1) + 0.1) # margin spacing stuff
  par(mar = c(4,4,3,4) + 0.1) # margin spacing stuff
  x = 0:60
  DATE_OBS_PRED_NODE <- DATE_OBS_PRED_soiltemp

  for(inode in 1:length(DEP)){
    DATE_OBS_PRED2 <- subset(DATE_OBS_PRED_NODE, inode == DATE_OBS_PRED_NODE[,4])
    if(nrow(DATE_OBS_PRED2)>0){
      r<-cor(DATE_OBS_PRED2[,2],DATE_OBS_PRED2[,3])
      rmsd = sqrt(mean(((DATE_OBS_PRED2[,2]-DATE_OBS_PRED2[,3])^2),na.rm=TRUE))

      plot(x,x, type = 'l',
           main=sprintf('model fit for node %1.0f at %1.0f cm',inode, DEP[inode]),
           ylab='predicted', xlab = 'observed')
      points(DATE_OBS_PRED2[,2],DATE_OBS_PRED2[,3], col=addTrans("black",50), pch = 16)

      text(4.5+5,56, "         r     rmsd")
      text(3+5,48, paste0('NicheMapR ',round(r,3),'   ',round(rmsd,2)))

      results<-as.data.frame(cbind(as.character(OzNetsite),"SLGA",DEP[inode],r,rmsd))
      colnames(results)<-c("site","soil","depth","r_NMR","rmsd_NMR")
      if(ncep == 1){
        filename <- "output/micro_ncep_temp.csv"
      }else{
        filename <- "output/micro_aust_temp.csv"
      }
      if(file.exists(filename)==TRUE){
        write.table(results, filename, append=TRUE, sep=",",col.names =FALSE,row.names =FALSE)
      }else{
        write.table(results, filename, append=TRUE,sep=",",col.names =TRUE,row.names =FALSE)
      }
    }
  }

  dev.off()
}
