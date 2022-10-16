compare <- function(data1, data2, lab1, lab2, bad){

# label first column
colnames(data1)[2]<-"test"
colnames(data2)[2]<-"test"

# combine data sets to one table
tests<-rbind(data1, data2)
#bad<-c("k1275","k1245","a445","y34")
tests<-subset(tests, !paste0(tests$site, tests$depth)%in%bad)
tests2<-c(lab1, lab2)
test.types<-rep(c(paste0('1',lab1),paste0('2',lab2)),4) # add numbers for sorting
test.types<-test.types[order(test.types)] # sort
test.types<-substring(test.types, 2) # get rid of number 
test.types<-c(test.types, lab1, lab2)

# create an empty matrix for summary stats per depth and test
stats<-matrix(data = 0, nrow = 10, ncol = 4)
stats<-as.data.frame(stats)
stats<-cbind(test.types,stats, stringsAsFactors=FALSE)
depths<-c(rep(c(4,15,45,75),2), 'overall', 'overall')
stats<-cbind(depths,stats)
colnames(stats)<-c('depth','scenario','r mean','r stdev','RMSE mean','RMSE stdev')

# compute mean r and RMSE and put in summary stats table
for(i in 1:2){
 # means
stats[4*(i-1)+1,3]<-with(subset(tests, depth <= 4 & test == tests2[i]), mean(r_NMR, na.rm=TRUE))
stats[4*(i-1)+2,3]<-with(subset(tests, depth == 15 & test == tests2[i]), mean(r_NMR, na.rm=TRUE))
stats[4*(i-1)+3,3]<-with(subset(tests, depth >= 42 & depth <= 45 & test == tests2[i]), mean(r_NMR, na.rm=TRUE))
stats[4*(i-1)+4,3]<-with(subset(tests, depth > 45 & test == tests2[i]), mean(r_NMR, na.rm=TRUE))
stats[4*(i-1)+1,5]<-with(subset(tests, depth <= 4 & test == tests2[i]), mean(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+2,5]<-with(subset(tests, depth == 15 & test == tests2[i]), mean(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+3,5]<-with(subset(tests, depth >= 42 & depth <= 45 & test == tests2[i]), mean(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+4,5]<-with(subset(tests, depth > 45 & test == tests2[i]), mean(rmsd_NMR, na.rm=TRUE))
 # standard deviatoins
stats[4*(i-1)+1,4]<-with(subset(tests, depth <= 4 & test == tests2[i]), sd(r_NMR, na.rm=TRUE))
stats[4*(i-1)+2,4]<-with(subset(tests, depth == 15 & test == tests2[i]), sd(r_NMR, na.rm=TRUE))
stats[4*(i-1)+3,4]<-with(subset(tests, depth >= 42 & depth <= 45 & test == tests2[i]), sd(r_NMR, na.rm=TRUE))
stats[4*(i-1)+4,4]<-with(subset(tests, depth > 45 & test == tests2[i]), sd(r_NMR, na.rm=TRUE))
stats[4*(i-1)+1,6]<-with(subset(tests, depth <= 4 & test == tests2[i]), sd(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+2,6]<-with(subset(tests, depth == 15 & test == tests2[i]), sd(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+3,6]<-with(subset(tests, depth >= 42 & depth <= 45 & test == tests2[i]), sd(rmsd_NMR, na.rm=TRUE))
stats[4*(i-1)+4,6]<-with(subset(tests, depth > 45 & test == tests2[i]), sd(rmsd_NMR, na.rm=TRUE))
}

stats[9,3] <- with(subset(tests, test == tests2[1]), mean(r_NMR, na.rm=TRUE))
stats[9,5] <- with(subset(tests, test == tests2[1]), mean(rmsd_NMR, na.rm=TRUE))
stats[10,3] <- with(subset(tests, test == tests2[2]), mean(r_NMR, na.rm=TRUE))
stats[10,5] <- with(subset(tests, test == tests2[2]), mean(rmsd_NMR, na.rm=TRUE))

stats[9,4] <- with(subset(tests, test == tests2[1]), sd(r_NMR, na.rm=TRUE))
stats[9,6] <- with(subset(tests, test == tests2[1]), sd(rmsd_NMR, na.rm=TRUE))
stats[10,4] <- with(subset(tests, test == tests2[2]), sd(r_NMR, na.rm=TRUE))
stats[10,6] <- with(subset(tests, test == tests2[2]), sd(rmsd_NMR, na.rm=TRUE))

ttests <- matrix(data = 0, nrow = 4, ncol = 4)
ttests<-as.data.frame(ttests)
depths<-c(4,15,45,75)#, 'overall')
ttests<-cbind(depths,ttests)
colnames(ttests)<-c('depth','r Pval', 'r Diff', 'RMSE Pval', 'RMSE Diff')

r_test1 <- t.test(subset(tests, depth <= 4 & test == lab1)$r_NMR, subset(tests, depth <= 4 & test == lab2)$r_NMR, paired = TRUE)
r_test2 <- t.test(subset(tests, depth == 15 & test == lab1)$r_NMR, subset(tests, depth == 15 & test == lab2)$r_NMR, paired = TRUE)
r_test3 <- t.test(subset(tests, depth >= 42 & depth <= 45 & test == lab1)$r_NMR, subset(tests, depth >= 42 & depth <= 45 & test == lab2)$r_NMR, paired = TRUE)
r_test4 <- t.test(subset(tests, depth > 45 & test == lab1)$r_NMR, subset(tests, depth > 45 & test == lab2)$r_NMR, paired = TRUE)
#r_test5 <- t.test(subset(tests, test == lab1)$r_NMR, subset(tests, test == lab2)$r_NMR, paired = TRUE)

rmsd_test1 <- t.test(subset(tests, depth <= 4 & test == lab1)$rmsd_NMR, subset(tests, depth <= 4 & test == lab2)$rmsd_NMR, paired = TRUE)
rmsd_test2 <- t.test(subset(tests, depth == 15 & test == lab1)$rmsd_NMR, subset(tests, depth == 15 & test == lab2)$rmsd_NMR, paired = TRUE)
rmsd_test3 <- t.test(subset(tests, depth >= 42 & depth <= 45 & test == lab1)$rmsd_NMR, subset(tests, depth >= 42 & depth <= 45 & test == lab2)$rmsd_NMR, paired = TRUE)
rmsd_test4 <- t.test(subset(tests, depth > 45 & test == lab1)$rmsd_NMR, subset(tests, depth > 45 & test == lab2)$rmsd_NMR, paired = TRUE)
#rmsd_test5 <- t.test(subset(tests, test == lab1)$rmsd_NMR, subset(tests, test == lab2)$rmsd_NMR, paired = TRUE)

ttests[1, 2] <- r_test1$p.value
ttests[2, 2] <- r_test2$p.value 
ttests[3, 2] <- r_test3$p.value
ttests[4, 2] <- r_test4$p.value
#ttests[5, 2] <- r_test5$p.value

ttests[1, 4] <- rmsd_test1$p.value
ttests[2, 4] <- rmsd_test2$p.value 
ttests[3, 4] <- rmsd_test3$p.value
ttests[4, 4] <- rmsd_test4$p.value
#ttests[5, 4] <- rmsd_test5$p.value
  
ttests[1, 3] <- r_test1$estimate
ttests[2, 3] <- r_test2$estimate 
ttests[3, 3] <- r_test3$estimate
ttests[4, 3] <- r_test4$estimate
#ttests[5, 3] <- r_test5$estimate

ttests[1, 5] <- rmsd_test1$estimate
ttests[2, 5] <- rmsd_test2$estimate 
ttests[3, 5] <- rmsd_test3$estimate
ttests[4, 5] <- rmsd_test4$estimate
#ttests[5, 5] <- rmsd_test5$estimate

return(list(stats = stats, ttests = ttests))
}