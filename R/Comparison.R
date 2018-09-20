library(ggplot2)
library(car)
library(lattice)
library(methods)
library(dplyr)
library(reshape2)
library(tidyr)



##################################################################
#ONLY RUN THIS AFTER RUNNING ALL SETUP FOR SPECTRA AND LUCCHI.R
##################################################################




##################################################################
##Import & Setup

#Summary
lsdtest <- l1summ[which(l1summ$force==25),]
lsdtest$woodnum <- paste0(toupper(lsdtest$woodtype),toupper(lsdtest$woodnum))
##REQUIRES sdsum from Spectra2.R
sdsumimport <- sdsum
sdsumimport$woodnum <- as.character(sdsumimport$woodnum)
comparison <- left_join(sdsumimport, lsdtest, by="woodnum")
comparison <- comparison[,-c(2,3,9,11)]
comparison <- rename(comparison, schmidt=mean, l_sd=standev, lucchi=measure, s_sd=sd,s_se=se,s_ci=ci)

comparison$woodpiece <- paste0(as.character(toupper(comparison$woodtype)),as.character(toupper(comparison$woodpiece)))
comparison$woodpiece <- as.factor(comparison$woodpiece)
comp2 <- melt(comparison, measure=c("lucchi","schmidt","l_sd","s_sd","s_se","s_ci"))
comp3_all <- comp2[,-c(3,4,6)]
comp3 <- comp2[which(comp2$variable=="lucchi" | comp2$variable=="schmidt"),-6]

#All?
lcomp <- l1
lcomp$woodnum <- paste0(toupper(lcomp$woodtype),toupper(lcomp$woodnum))
lcomp$woodpiece <- paste0(toupper(lcomp$woodtype),toupper(lcomp$woodpiece))
lcomp$woodnum <-as.character(lcomp$woodnum)
lcomp$woodtype <- toupper(as.character(lcomp$woodtype))
lcomp2 <- lcomp[,-c(4,7)]
lcomp2 <- rename(lcomp2, l_force=force, lucchi=measure)


scomp <-maxres2
scomp2 <- scomp[,-c(1,3,5,7)]
scomp2$woodnum <-as.character(scomp2$woodnum)
scomp2 <- rename(scomp2, s_tap=tap, s_mic=mic, s_specnum=specnum, s_freq=freq, s_id=id, schmidt=speed)

bcomp <- left_join(scomp2,lcomp2,by="woodnum")

bcomp2 <- melt(bcomp, measure=c("schmidt","lucchi"))
####
#NOTE: THIS DID NOT WORK PREFECTLY_____ THERE ARE REPEATS BC OF USING SPECNUM & TAP & FORCE AS LABELS FOR SOME BUT NOT ALL

##########################################################################################
##exploration
ggplot(bcomp2, aes(x=woodnum, y=value, color=variable)) +
	geom_boxplot() +
#	ggtitle("Speed of Sound: Comparison of Measurement Techniques") +
	xlab("Wood Sample") + ylab("Speed of Sound (m/s)") +
	scale_color_discrete(name="Method of \n Measurement", labels=c("Schmidt", "Lucchi")) +
	facet_wrap(~woodpiece,nrow=1,scales="free_x")

bcompTEST <- melt(bcomp[which(bcomp$schmidt >3800),], measure=c("schmidt","lucchi"))
bcomp_f2025_sbm <- bcompTEST[which((bcompTEST$s_tap=="B" & bcompTEST$s_mic=="M" & bcompTEST$variable=="schmidt") | (bcompTEST$l_force==25 & bcompTEST$variable=="lucchi")),]
ggplot(bcomp_f2025_sbm, aes(x=woodnum, y=value, color=variable)) +
	geom_boxplot() +
#	ggtitle("Speed of Sound: Comparison of Measurement Techniques") +
	xlab("Wood Sample") + ylab("Speed of Sound (m/s)") +
	scale_color_discrete(name="Method of \n Measurement", labels=c("Schmidt","Lucchi")) +
	facet_wrap(~woodpiece,nrow=1,scales="free_x")




##########################################################################################
##exploration
bcompTEST <- melt(bcomp[which(bcomp$schmidt >3800),], measure=c("schmidt","lucchi"))
bcomptest <- bcompTEST[which(bcompTEST$s_tap=="B" & bcompTEST$s_mic=="M" & bcompTEST$l_force==25),]
ggplot(bcomptest, aes(x=schmidt, y=lucchi, color=woodnum)) +
	geom_point() +
	geom_abline(slope=1, intercept = 0) +
	stat_summary(fun.y="mean", geom="point", aes(size=2)) +

 
vars

"M15A" "M15B" "M47A" "M47B" "S67A" "S67B" "S68A" "S68B"
schmidt <- c(4823, )

	
	

prezs <- scomp2[which(scomp2$s_tap=="B" & scomp2$s_mic=="M"),]


	
