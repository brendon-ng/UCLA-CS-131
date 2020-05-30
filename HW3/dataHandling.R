lnxsrv09 <- read.csv("~/UCLA-CS-131/HW3/lnxsrv09.txt", sep="")
lnxsrv10 <- read.csv("~/UCLA-CS-131/HW3/lnxsrv10.txt", sep="")


array100.09 <-lnxsrv09[lnxsrv09$Array.Size == 100,] 
array100.09$Times <-  paste(array100.09$Real.Time.s., array100.09$CPU.Time.s.,sep="/")
data09 <- data.frame(T1=array100.09[array100.09$Threads==1, "Times"],T8=array100.09[array100.09$Threads==8, "Times"],T20=array100.09[array100.09$Threads==20, "Times"],T40=array100.09[array100.09$Threads==40, "Times"])
rownames(data09) = c("Null", "Synchronized", "UnSynchronized", "AcmeSafe")
names(data09) = c("1", "8", "20", "40")
data09

array100.10 <-lnxsrv10[lnxsrv10$Array.Size == 100,] 
array100.10$Times <-  paste(array100.10$Real.Time.s., array100.10$CPU.Time.s.,sep="/")
data10 <- data.frame(T1=array100.10[array100.10$Threads==1, "Times"],T8=array100.10[array100.10$Threads==8, "Times"],T20=array100.10[array100.10$Threads==20, "Times"],T40=array100.10[array100.10$Threads==40, "Times"])
rownames(data10) = c("Null", "Synchronized", "UnSynchronized", "AcmeSafe")
names(data10) = c("1", "8", "20", "40")
data10



cpu10 <- data.frame(T1=array100.10[array100.10$Threads==1, "CPU.Time.s."],T8=array100.10[array100.10$Threads==8, "CPU.Time.s."],T20=array100.10[array100.10$Threads==20, "CPU.Time.s."],T40=array100.10[array100.10$Threads==40, "CPU.Time.s."])
rownames(cpu10) = c("Null", "Synchronized", "UnSynchronized", "AcmeSafe")
names(cpu10) = c("1", "8", "20", "40")
cpu10


cpu09 <- data.frame(T1=array100.09[array100.09$Threads==1, "CPU.Time.s."],T8=array100.09[array100.09$Threads==8, "CPU.Time.s."],T20=array100.09[array100.09$Threads==20, "CPU.Time.s."],T40=array100.09[array100.09$Threads==40, "CPU.Time.s."])
rownames(cpu09) = c("Null", "Synchronized", "UnSynchronized", "AcmeSafe")
names(cpu09) = c("1", "8", "20", "40")
cpu09

dCPU09 <-aggregate(formula=array100.09$CPU.Time.s.~array100.09$Threads+array100.09$Class, FUN=mean)
names(dCPU09) <- c("Threads", "Class", "CPU.Time")
dCPU10 <- aggregate(formula=array100.10$CPU.Time.s.~array100.10$Threads+array100.10$Class, FUN=mean)
names(dCPU10) <- c("Threads", "Class", "CPU.Time")
dReal09 <-aggregate(formula=array100.09$Real.Time.s.~array100.09$Threads+array100.09$Class, FUN=mean)
names(dReal09) <- c("Threads", "Class", "Real.Time")
dReal10 <- aggregate(formula=array100.10$Real.Time.s.~array100.10$Threads+array100.10$Class, FUN=mean)
names(dReal10) <- c("Threads", "Class", "Real.Time")

library(ggplot2)
par(mfrow=c(2,2))
real09 <- ggplot(data=dReal09) + geom_line(aes(x=Threads, y=Real.Time, color=Class)) + ggtitle("Real Time on lnxsrv09") + scale_x_continuous(name="Threads", breaks=c(1,8,20,40))  + ylim(0,27) + theme(legend.title = element_blank(),legend.position = "none") 
real10 <- ggplot(data=dReal10) + geom_line(aes(x=Threads, y=Real.Time, color=Class)) + ggtitle("Real Time on lnxsrv10") + scale_x_continuous(name="Threads", breaks=c(1,8,20,40))  + ylim(0,27) +theme(legend.title = element_blank(),legend.position = "none") 
cpu09 <-ggplot(data=dCPU09) + geom_line(aes(x=Threads, y=CPU.Time, color=Class)) + ggtitle("CPU Time on lnxsrv09")      + scale_x_continuous(name="Threads", breaks=c(1,8,20,40))  + ylim(0,133)+theme(legend.title = element_blank(),legend.position = "none") 
cpu10 <- ggplot(data=dCPU10) + geom_line(aes(x=Threads, y=CPU.Time, color=Class)) + ggtitle("CPU Time on lnxsrv10")     + scale_x_continuous(name="Threads", breaks=c(1,8,20,40)) + ylim(0,133)+theme(legend.title = element_blank(),legend.position = "none") 
library(gridExtra)
grid.arrange(real09, real10, cpu09, cpu10, nrow=2)

