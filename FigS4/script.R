library(survival)

AAb.data <-read.table("data/MN_AAb.csv", header=T, sep=",")
BIde.data <-read.table("data/MN_BIde.csv", header=T, sep=",")
BOa.data <-read.table("data/MN_BOa.csv", header=T, sep=",")
FRab.data <-read.table("data/MN_FRab.csv", header=T, sep=",")
HVb.data <-read.table("data/MN_HVb.csv", header=T, sep=",")
KOa.data <-read.table("data/MN_KOa.csv", header=T, sep=",")
KVc.data <-read.table("data/MN_KVc.csv", header=T, sep=",")
TUb.data <-read.table("data/MN_TUb.csv", header=T, sep=",")
USs.data <-read.table("data/MN_USs.csv", header=T, sep=",")
MCalifornica.data <-read.table("data/Mcalifornica.csv", header=T, sep=",")
MEligulata.data <-read.table("data/Meligulata.csv", header=T, sep=",")
MImperfecta.data <-read.table("data/Mimperfecta.csv", header=T, sep=",")
MJacquemontii.data <-read.table("data/Mjacquemontii.csv", header=T, sep=",")
Mciliata.31675.data <-read.table("data/31675.csv", header=T, sep=",")
Mciliata.636632.data <-read.table("data/636632.csv", header=T, sep=",")
Mciliata.516979.data <-read.table("data/516979.csv", header=T, sep=",")


all.data <- list(AAb.data, BIde.data, BOa.data, FRab.data, 
                 HVb.data, KOa.data, KVc.data, TUb.data, USs.data,
                 MCalifornica.data, MEligulata.data,
                 MImperfecta.data, MJacquemontii.data,
                 Mciliata.516979.data, Mciliata.636632.data, 
                 Mciliata.31675.data
)
titles <- c("Mnutans, AAb", "Mnutans, BIde", "Mnutans, BOa",
            "Mnutans, FRab", "Mnutans, HVb", "Mnutans, KOa",
            "Mnutans, KVc", "Mnutans, TUb", "Mnutans, USs", 
            "Mcalifornica", "Meligulata", "Mimperfecta", 
            "Mjacquemontii", "Mciliata, 516979", 
            "Mciliata, 636632", "Mciliata, 31675"
)


# creating and saving subfigures individually
# merging them into a single figure and labeling is done afterwards
for (i in seq_along(all.data)){
  
  data <- all.data[[i]]
  plant.DTH <- Surv(data$DTH, data$LastRecordedFlowered)
  ft.fh <- survfit(plant.DTH ~ 1, conf.type="log-log", type="fh")
  if (i > 9){
    levels = c("8w","6w","4w","2w","0w")
    Vernalization.Period <- factor(
      data$Treatment, 
      levels = levels
    ) 
  } else {
    levels = c("12w","9w","6w","3w","0w")
    Vernalization.Period <- factor(
      data$Treatment, 
      levels = levels)
  }
  
  ft.gh.fh <- survfit(plant.DTH ~ Vernalization.Period, type="fh")
  
  
  group <- rep(seq_along(ft.gh.fh$strata), times = ft.gh.fh$strata)
  cols = c("salmon4","salmon1","#80B918","#1E6091", "skyblue2")
  pchv <- 21:25
  
  fname <- gsub(", ", "_", titles[i])
  pdf(paste("./", fname, ".pdf", sep=""), width = 5, height = 4)
  
  par(mar=c(5, 5, 2, 2) + 0.2)
  plot(ft.gh.fh, fun="event", 
       col=cols,
       xlab="Days", ylab="Probability of Flowering", 
       main=titles[i], 
       cex.lab=2, cex.axis=1.5, bty="n", lwd=3, yaxt="n")
  
  axis(2, at=seq(0, 1, 0.2), labels=seq(0, 1, 0.2), 
       cex.axis=1.5, las=1)
  
  points(ft.gh.fh$time, 1 - ft.gh.fh$surv, 
         bg = cols[group],
         pch = pchv[group], 
         col = cols[group], cex=1.75)
  
  dev.off()
  
}
