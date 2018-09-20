library(ggplot2)
library(ggfortify)
library(dplyr)
library(plyr)
library(reshape2)
library(ggthemes)
library(lattice)
library(gplots)
library(tidyr)

d1 <- read.csv("~/Documents/College/Comps/Data/R_M-FINALDATA_1.csv")
d1 <- select(d1, emp = starts_with("Employment"), unemp = starts_with("Unemploy"), semp = starts_with("Self"), sempr = sempr, date=Date3)
d1$date <- as.Date(paste(d1$date," 01",sep=""), format = "%m %Y %d")

b1 <- read.csv("~/Documents/College/Comps/Excel/Fig3_output_X.csv", stringsAsFactors = FALSE)
b1$date <- as.Date(b1$X, format = "%Y-%m-%d")

b4 <- read.csv("~/Documents/College/Comps/Excel/Fig4_IRF-US_output_X.csv", stringsAsFactors = FALSE)
b4 <- b4[c(3:6)]
b4_1 <- melt(b4, id="yr")

b5 <- read.csv("~/Documents/College/Comps/Excel/Fig4_IRF-SU_output_X.csv", stringsAsFactors = FALSE)
b5 <- b5[c(2:5)]
b5_1 <- melt(b5, id="yr")


limits <- c(as.Date("1944-01-01"),NA)
#labels1 <- as.Date(seq(as.Date('1940-01-01'), as.Date('2020-01-01'), by = "5 years"))

#Figure 1 
#UNEMPLOYMENT
ggplot(d1, aes(date, unemp)) + 
  theme_tufte() +
  geom_line() + 
  scale_x_date(date_breaks= "5 years", date_labels="%Y", limits=limits) +
  scale_y_continuous(limits=c(0,12), breaks=c(0,5,10)) +
  xlab("Year") +
  ylab("Unemployment Rate (%)") +
  ggtitle("Unemployment Rate") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.3),
    axis.ticks = element_line(colour = "lightgrey", size = 0.3),
    plot.title = element_text(hjust=0.54))


#Figure 2
#SELF-EMPLOYMENT
ggplot(d1, aes(date, sempr)) + 
  theme_tufte() +
  geom_line() + 
  scale_x_date(date_breaks= "5 years", date_labels="%Y", limits=limits) +
  scale_y_continuous(limits=c(0,0.2)) +
  xlab("Year") +
  ylab("Self-Employment Rate (%)") +
  ggtitle("Self-Employment Rate") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.3),
    axis.ticks = element_line(colour = "lightgrey", size = 0.3),
    plot.title = element_text(hjust=0.54))

summary(b1$e)

#Figure 3
#Recovered Residuals
ggplot(b1, aes(date, e)) + 
  theme_tufte() +
  geom_line() + 
  scale_x_date(date_breaks= "5 years", date_labels="%Y", limits=limits) +
  scale_y_continuous() +
  xlab("Year") +
  ylab("Self-Employment Rate (%)") +
  ggtitle("Self-Employment Rate") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.3),
    axis.ticks = element_line(colour = "lightgrey", size = 0.3),
    plot.title = element_text(hjust=0.54))


#Figure 4
#IRF - US
ggplot(b4, aes(yr, oirf)) + 
  theme_tufte() +
  geom_ribbon(aes(x=yr, ymin=lower, ymax=upper), alpha=0.3) +
  geom_line(aes(x=yr, y=oirf)) + 
  scale_x_continuous() +
  scale_y_continuous() +
  xlab("Years After Shock") +
  ylab("Impulse Response of Self-Employment") +
  ggtitle("IRF: Response of Self-Employment to Unemployment Shock") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.3),
    axis.ticks = element_line(colour = "lightgrey", size = 0.3),
    plot.title = element_text(hjust=0.54))



#Figure 5
#IRF - SU
ggplot(b5, aes(yr, oirf)) + 
  theme_tufte() +
  geom_ribbon(aes(x=yr, ymin=Lower, ymax=Upper), alpha=0.3) +
  geom_line(aes(x=yr, y=oirf)) + 
  scale_x_continuous() +
  scale_y_continuous() +
  xlab("Years After Shock") +
  ylab("Impulse Response of Unemployment") +
  ggtitle("IRF: Response of Unemployment to Self-Employment Shock") +
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.3),
    axis.ticks = element_line(colour = "lightgrey", size = 0.3),
    plot.title = element_text(hjust=0.54))






##############################
#Model Selection Visualization
#http://stackoverflow.com/questions/18663159/conditional-coloring-of-cells-in-table
ms1 <- read.csv("~/Documents/College/Comps/Excel/ModelSelect_Vis1.csv")
msirf <- ms1[which(ms1$hypo=="IRFUS" | ms1$hypo=="IRFSU_I"| ms1$hypo=="IRFSU_D"),]
msirf2 <- melt(msirf, id.vars = c("hypo", "gran"))
msirf2$lags <- ifelse(msirf2$variable=="L1",1, ifelse(msirf2$variable=="L2", 2, ifelse(msirf2$variable=="L4", 4,  ifelse(msirf2$variable=="L6", 6, ifelse(msirf2$variable=="L8", 8, ifelse(msirf2$variable=="L10", 10, ifelse(msirf2$variable=="L12", 12, ifelse(msirf2$variable=="L14", 14, "!!!"))))))))
msirf2$gran <- ifelse(msirf2$gran=="M", "Month",
                      ifelse(msirf2$gran=="Q", "Quarter",
                             ifelse(msirf2$gran=="H", "Biannual",
                                    ifelse(msirf2$gran=="Y", "Annual","!!!"))))
msirf2$lagN <- as.numeric(msirf2$lags)
msirf2$bin <- ifelse(msirf2$value=="Y",1,0)
msirf2$lag <- as.numeric(msirf2$variable)
# need data as matrix
hm1 <- msirf2[,c(1,2,4,6)]
hm1$value <- ifelse(hm1$value == "Y", 1, ifelse(hm1$value == "U", 0, ifelse(hm1$value == "N", -1, "!!!")))



msgc <- ms1[which(ms1$hypo=="GCSU" | ms1$hypo=="GCUS"),]
msgc2 <- melt(msgc, id.vars = c("hypo", "gran"))
msgc2$lags <- ifelse(msgc2$variable=="L1", 1, ifelse(msgc2$variable=="L2", 2, ifelse(msgc2$variable=="L4", 4, ifelse(msgc2$variable=="L6", 6, ifelse(msgc2$variable=="L8", 8, ifelse(msgc2$variable=="L10", 10, ifelse(msgc2$variable=="L12", 12, ifelse(msgc2$variable=="L14", 14, "!!!"))))))))
msgc2$gran <- ifelse(msgc2$gran=="M", "Month", ifelse(msgc2$gran=="Q", "Quarter", ifelse(msgc2$gran=="H", "Biannual", ifelse(msgc2$gran=="Y", "Annual","!!!"))))
msgc2$lagN <- as.numeric(msgc2$lags)
msgc2$value <- ifelse(msgc2$value == "0.001", 5, ifelse(msgc2$value == "0.005", 4,ifelse(msgc2$value == "0.05", 3,ifelse(msgc2$value == "0.01", 2, ifelse(msgc2$value == "0.1", 1,ifelse(msgc2$value == "U", 0,ifelse(is.na(msgc2$value), NA , "!!!")))))))
msgc2$value <- as.numeric(msgc2$value)
# need data as matrix
hmgc1 <- msgc2[,c(1,2,4,6)]
hmgc1$hypo <- as.character(hmgc1$hypo)


#IRF_US
hm_irfus <- hm1[which(hm1$hypo=="IRFUS"),c(2,3,4)]
hm_irfus <- spread(hm_irfus, key=lagN, value=value)
rownames(hm_irfus) <- hm_irfus$gran
hm_irfus <- data.matrix(hm_irfus)
hm_irfus <- hm_irfus[c(3,4,2,1),-1]


#IRF_SU_I
hm_irfsui <- hm1[which(hm1$hypo=="IRFSU_I"),c(2,3,4)]
hm_irfsui <- spread(hm_irfsui, key=lagN, value=value)
rownames(hm_irfsui) <- hm_irfsui$gran
hm_irfsui <- data.matrix(hm_irfsui)
hm_irfsui <- hm_irfsui[c(3,4,2,1),-1]

#IRF_SU_D
hm_irfsud <- hm1[which(hm1$hypo=="IRFSU_D"),c(2,3,4)]
hm_irfsud <- spread(hm_irfsud, key=lagN, value=value)
rownames(hm_irfsud) <- hm_irfsud$gran
hm_irfsud <- data.matrix(hm_irfsud)
hm_irfsud <- hm_irfsud[c(3,4,2,1),-1]

#GC_SU#####################
hm_gcsu <- hmgc1[which(hmgc1$hypo=="GCSU"),c(2,3,4)]
hm_gcsu <- spread(hm_gcsu, key=lagN, value=value)
rownames(hm_gcsu) <- hm_gcsu$gran
hm_gcsu <- data.matrix(hm_gcsu)
hm_gcsu <- hm_gcsu[c(3,4,2,1),-1]

#GC_US#####################
hm_gcus <- hmgc1[which(hmgc1$hypo=="GCUS"),c(2,3,4)]
hm_gcus <- spread(hm_gcus, key=lagN, value=value)
rownames(hm_gcus) <- hm_gcus$gran
hm_gcus <- data.matrix(hm_gcus)
hm_gcus <- hm_gcus[c(3,4,2,1),-1]


#HEAT MAPS
#IRF US
heatmap.2(x = hm_irfus, Rowv=FALSE, Colv=FALSE, dendrogram = "none", 
          notecol = "black", notecex = 2, trace = "none", margins = c(5, 8), 
          na.color="lightgrey", col=c('#fc8d59','#ffffbf','#91cf60'),
          key = FALSE, cexRow = 1, cexCol = 1,
          main="IRF, Unemployment on Self-Employment: \n Positive Short-Term Response",
          ylab="Granularity",
          xlab="Lag-Order Length \n(Number of Years Covered by Lags)")

heatmap.2(x = hm_irfsud, Rowv=FALSE, Colv=FALSE, dendrogram = "none", 
          notecol = "black", notecex = 2, trace = "none", margins = c(5, 8), 
          na.color="lightgrey", col=c('#fc8d59','#ffffbf','#91cf60'),
          key = FALSE, cexRow = 1, cexCol = 1,
          main="IRF, Self-Employment on Unemployment: \n Negative Long-Term Response",
          ylab="Granularity",
          xlab="Lag-Order Length \n(Number of Years Covered by Lags)")

heatmap.2(x = hm_irfsui, Rowv=FALSE, Colv=FALSE, dendrogram = "none", 
          notecol = "black", notecex = 2, trace = "none", margins = c(5, 8), 
          na.color="lightgrey", col=c('#fc8d59','#ffffbf','#91cf60'),
          key = FALSE, cexRow = 1, cexCol = 1,
          main="IRF, Self-Employment on Unemployment: \n Positive Short-Term Response",
          ylab="Granularity",
          xlab="Lag-Order Length \n(Number of Years Covered by Lags)")



#GC SU
heatmap.2(x = hm_gcus, Rowv=FALSE, Colv=FALSE, dendrogram = "none", 
          notecol = "black", notecex = 2, trace = "none", margins = c(5, 8), 
          na.color="lightgrey", col=c('#d73027','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
          key = FALSE, cexRow = 1, cexCol = 1,
        main="Granger-Causation: \n Self-Employment on Unemployment",
          ylab="Granularity",
          xlab="Lag-Order Length \n(Number of Years Covered by Lags)")


#GC US
heatmap.2(x = hm_gcsu, Rowv=FALSE, Colv=FALSE, dendrogram = "none", 
          notecol = "black", notecex = 2, trace = "none", margins = c(5, 8), 
          na.color="lightgrey", col=c('#d73027','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
          key = FALSE, cexRow = 1, cexCol = 1,
          main="Granger-Causation: \n Unemployment on Self-Employment",
          ylab="Granularity",
          xlab="Lag-Order Length \n(Number of Years Covered by Lags)")







############################
#Summary Visualization







