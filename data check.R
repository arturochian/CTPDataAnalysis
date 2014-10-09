library(data.table)
library(reshape2)
library(dplyr)

setwd("~/Workspaces/9.27 ctp data")

dt <- fread("dt.csv/dt.csv", header=TRUE)
dt <- dt[, HMS := as.ITime(V2)]
dt <- dt[, HM  := as.ITime(V2, format="%H:%M")]

View( head(dt, 100) )
dim(dt)


# contract
contract.name <- unique(dt$V4)
contract.num <- length(contract.name)
contract.num

contract.length <- table(dt[, V4])
View(contract.length)
head(contract.length)
write.csv(contract.length, "report/contract.length.csv")

# HM volume
HM.volume <- table(dt[, HM])
View(HM.volume)
write.csv(HM.volume, "report/HM.volume.csv")

png("report/HM.volume.png", width=1200, height=600)
barplot(HM.volume, border=FALSE, col=rainbow(10)[5])
dev.off()

summary(data.frame(HM.volume)[, 2])

# HMS volume
HMS.volume <- table(dt[, HMS])
write.csv(HMS.volume, "report/HM.volume.csv")

png("report/HMS.volume.png", width=1200, height=600)
barplot(HMS.volume, border=rainbow(10)[8], col=rainbow(10)[8])
dev.off()

summary(data.frame(HMS.volume)[, 2])

# Omit Time
dt.time <- as.ITime(sort( unique( as.integer( dt[, HMS] ) ) ),
                    origin=as.ITime("16:00:00"))
abs.time <- as.ITime( c( as.ITime("9:00:00"):as.ITime("10:15:00"), 
                         as.ITime("10:30:00"):as.ITime("11:30:00"),
               as.ITime("13:30:00"):as.ITime("15:00:00") ), 
               origin=as.ITime("16:00:00") )
is.in.time <- abs.time %in% dt.time

head(abs.time)
head(omit.time)
omit.time <- abs.time[!is.in.time]
plot(omit.time)

View(head(dt, 100))

View( filter(dt, HMS >= as.ITime("10:00:00"), HMS <= as.ITime() ))

# Time Omit
dt.time[ diff(dt.time) != as.ITime("00:00:01") ]


# 500 ms
ms <- dcast.data.table(dt, V4~V3, length)
write.csv(ms, "report/500 ms.csv")

# CF 411
dt.cf <- filter(dt, V4 == "CF411")

png("report/CF411.png", width=1200, height=600)
par(mfrow=c(2,2))
plot(dt.cf$HMS, dt.cf$V5, pch=20)
plot(dt.cf$HMS, dt.cf$V6, pch=20)
plot(dt.cf$HMS, dt.cf$V7, pch=20)
plot(dt.cf$HMS, dt.cf$V8, pch=20)
par(mfrow=c(1,1))
dev.off()

View(dt.cf)

# completeness

dt.ncomplete <- filter(dt, !complete.cases(dt))
head(dt.ncomplete)
nrow(dt.ncomplete)
View(head(dt.ncomplete, 100))

head(ms)
ms.true <- ms$V4[ data.frame(ms)[, 3] != 0 ]
ms.false <- ms$V4[ data.frame(ms)[, 3] == 0 ]

dt.ncom.sh <- filter(dt.ncomplete, V4 %in% ms.true)
dt.ncom.nsh <- filter(dt.ncomplete, V4 %in% ms.false )

nrow(dt.ncom.sh)
nrow(dt.ncom.nsh)

View( head(dt.ncom.sh, 100) )
View( head(dt.ncom.nsh, 100) )
View( filter(dt.ncom.sh, is.na(V9)) )


apply(dt.ncom.sh, MARGIN=2, FUN=function(var) sum(is.na(var)))
apply(dt.ncom.nsh, MARGIN=2, FUN=function(var) sum(is.na(var)))
