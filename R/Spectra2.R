############################################################################
#Setup
library(reshape2)
library(dplyr)
library(plyr)
library(ggplot2)
library(data.table)
filenames <- list.files(path = "./Spec", recursive = TRUE)
filenames <- paste0("./Spec/",filenames)
read_csv_filename <- function(filename){
    ret <- read.csv(filename, sep="\t")
    ret$Source <- filename #EDIT
    ret
}

import.list <- llply(filenames, read_csv_filename)
combined <- do.call("rbind", import.list)
write.csv(combined, file="test.csv")

combined$freq <- combined$Frequency..Hz.
combined$hertz <- combined$Level..dB.
combined <- combined[,c(3:6)]

testfunction1 <- combined[c(1:512),]
z <- 0
for(i in 1:182580) {
	if (combined[i,1] < 87) {
		z <- z + 1
	}
	combined$group[i] <- z
}

#how many do we expect?
8*3*3*10 #720~=716

write.csv(combined, file="Rexp.csv")
##Excel edits
s1 <- read.csv("~/Documents/College/AppliedMathConsulting/R/Analysis/spectra2excel1.csv", row.names=1, stringsAsFactors = TRUE)


# tapl <- data.frame(tapl=c("A","B","C"), stringsAsFactors = FALSE)
# micl <- data.frame(micl=c("F","M","T"), stringsAsFactors = FALSE)
# for (i in 1:3) {
# 	for (j in 1:3) {
# 	ggplot(s1, aes(x=freq, y=hertz)) +
# 		geom_line() +
# 		ggtitle(paste("Tap ",tapl[i,], "and Mic ", micl[j,])) +
# 		facet_wrap(~specnum)
# 	}
# }



############################################################################
#Find Max Hertz
maxtest <- s1[which(s1$freq > 3000),-1]
maxres <- maxtest %>% group_by(woodnum, tap, mic, specnum) %>% filter(hertz == max(hertz))



############################################################################
#Adding Actual Found Speed of Sound
woodnum <- c("S67A","M47A","S67B", "M47B", "S68A", "M15A", "S68B", "M15B")
la <- c(17+(13.5/16), 16+(10/16), 17+(13.5/16), 16+(10.5/16), 17+(13/16), 16+(7.5/16), 17+(13/16), 16+(8/16))
lb <- c(17+(9.5/16), 16+(11/16), 17+(13.5/16), 16+(11/16), 17+(13/16), 16+(8/16), 17+(12.5/16), 16+(8/16))
lengths <- data.frame(woodnum,la,lb)
maxres2 <- left_join(maxres, lengths, by="woodnum")
maxres2$speed <- 2*(((maxres2$la*0.025) + (maxres2$lb*0.025))/2)*maxres2$freq
maxres3 <- unique(maxres2)


############################################################################
#Find SD for b-m (EXCLUDE those under 3000)
sdtest <- s1[which(s1$tap=="B" & s1$mic=="M" & s1$freq > 3000),-1]
sdres <- sdtest %>% group_by(woodnum, tap, mic, specnum) %>% filter(hertz == max(hertz))
sdres2 <- left_join(sdres, lengths, by="woodnum")
sdres2$speed <- 2*(((sdres2$la*0.025) + (sdres2$lb*0.025))/2)*sdres2$freq
sdsum <- sdres2 %>% 
	dplyr::group_by(woodnum, tap, mic) %>% 
	dplyr::summarize(standev = sd(speed), mean=mean(speed)) 
write.csv(sdsum, file="sdsum.csv")




############################################################################
#Plotting Spectra for all woods B-M>3000

sdcheckgg <- s1[which(s1$freq > 3000 & s1$tap=="B" & s1$mic=="M"),-1]

vars <- as.character(sdsum$woodnum)
plots <- list()
xintbb <- sdcheckgg %>% group_by(woodnum, tap, mic, specnum) %>% filter(hertz == max(hertz))
for (var in vars) {
  	ggplot(sdcheckgg[which(sdcheckgg$woodnum==var),], aes(x=freq, y=hertz)) +
  		geom_line(size=0.2) +
			geom_vline(data=xintbb[which(xintbb$woodnum==var),], aes(xintercept=freq, alpha=0.3, color="red"), show.legend = FALSE) +
			geom_text(data=xintbb[which(xintbb$woodnum==var),], mapping=aes(x=freq, y=0, label=paste("Peak: ", round(freq, digits=0), "dB")), size=2, angle=90, vjust=-0.5, hjust=2.88) +
		ggtitle(paste("Frequency Spectrum Plots for ", var, " by Trial Number \n (Middle Tap and Middle Microphone)")) +
			xlab("Frequency") +
			ylab("Hertz (dB)") +
  		facet_wrap(~specnum, nrow=2) +
			theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), plot.title = element_text(hjust = 0.5))
		ggsave(filename=paste("spectrum_",var,".png", sep=""), width = 10, height = 5, units = c("in"))
}



##########################

testeroo <- sdcheckgg %>% group_by(woodnum, tap, mic, specnum) %>% filter(ifelse(diff(sign(diff(freq)))==-2,TRUE,FALSE))
testeroo <- sdcheckgg[which(diff(sign(diff(sdcheckgg$freq)))==-2)+1,]



#tt <- c(1,2,3,2,1, 1, 2, 1)
#which(diff(sign(diff(sdcheckgg)))==-2)+1
############################################################################
#Plotting Distribution of freqs for B-M>3000
denstest <- maxres2[which(maxres2$tap=="B" & maxres2$mic=="M" & (maxres2$woodnum=="M15A" | maxres2$woodnum=="S68B" | maxres2$woodnum=="S67A" | maxres2$woodnum=="M15B")),]

ggplot(denstest, aes(x=speed)) +
	geom_histogram(bins=30) +
	xlab("Measured Speed") + ylab("Count") +
	facet_wrap(~woodnum)




############################################################################
#Some Plots to investigate variation by taps and mics
ggplot(maxres, aes(x=freq)) +
	geom_histogram() +
	ggtitle("Speed of Sound by Wood Piece \n (All Tap and Mic Combinations)")+
	xlab("Frequency (Hz)") + ylab("Count") +
	facet_wrap(~woodnum, scales="free_y", nrow=2)

ggplot(maxres, aes(x=freq)) +
	geom_histogram() +
	ggtitle("Speed of Sound by Wood Number \n (All Tap and Mic Combinations)")+
	xlab("Frequency (Hz)") + ylab("Count") +
	facet_wrap(~woodpiece, scales="free_y", nrow=2)
	
#
ggplot(maxres, aes(x=tap, y=speed)) +
	geom_boxplot() +
	facet_wrap(~mic)
ggplot(maxres, aes(x=mic, y=speed)) +
	geom_boxplot() +
	facet_wrap(~tap)

#
ggplot(maxres2[which(maxres2$mic=="M" & maxres2$woodpiece=="M15"),], aes(x=speed, color=tap)) +
	geom_density()
ggplot(maxres2[which(maxres2$mic=="M" & maxres2$woodpiece=="M47"),], aes(x=speed, color=tap)) +
	geom_density()
ggplot(maxres2[which(maxres2$mic=="M" & maxres2$woodpiece=="S68"),], aes(x=speed, color=tap)) +
	geom_density()
ggplot(maxres2[which(maxres2$mic=="M" & maxres2$woodpiece=="S67"),], aes(x=speed, color=woodnum)) +
	geom_density()





############################################################################
#Initial attempt at model to account for systematic variation
test.lm1 <- lm(data=maxres2, speed ~ woodpiece + woodnum)
summary(test.lm1)

test.lm1 <- lm(data=maxres2, speed ~ woodtype + mic + tap + mic*tap)
summary(test.lm1)

test.lm2 <- lm(data=maxres2, speed ~ woodpiece + mic + tap + mic*tap)
summary(test.lm2)




  	ggplot(sdcheckgg[which(sdcheckgg$woodnum=="M15A" & sdcheckgg$specnum < 4),], aes(x=freq, y=hertz)) +
  		geom_line(size=0.2) +
			geom_vline(data=xintbb[which(xintbb$woodnum=="M15A" & xintbb$specnum < 4),], aes(xintercept=freq, alpha=0.3, color="red"), show.legend = FALSE) +
			geom_text(data=xintbb[which(xintbb$woodnum=="M15A" & xintbb$specnum < 4),], mapping=aes(x=freq, y=0, label=paste("Peak: ", round(freq, digits=0), "dB")), size=2, angle=90, vjust=-0.5, hjust=2.88) +
		#ggtitle(paste("Frequency Spectrum Plots for ", var, " by Trial Number \n (Middle Tap and Middle Microphone)")) +
			xlab("Frequency") +
			ylab("Hertz (dB)") +
  		facet_wrap(~specnum, nrow=2) +
			theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), plot.title = element_text(hjust = 0.5))
		


############################################################################
############ Plotting Spectra for all woods B-M > 3000
###WITH max @

sdcheckgg <- s1[which(s1$freq > 3000 & s1$tap=="B" & s1$mic=="M"),-1]

vars <- as.character(sdsum$woodnum)
plots <- list()
xinttt <- sdcheckgg %>% group_by(woodnum, tap, mic, specnum) %>% filter(hertz == max(hertz))
for (var in vars) {
  	ggplot(sdcheckgg[which(sdcheckgg$woodnum==var),], aes(x=freq, y=hertz)) +
  		geom_line(size=0.2) +
			geom_vline(data=xintbb[which(xintbb$woodnum==var),], aes(xintercept=freq, alpha=0.3, color="red"), show.legend = FALSE) +
			geom_text(data=xintbb[which(xintbb$woodnum==var),], mapping=aes(x=freq, y=0, label=paste("Peak: ", round(freq, digits=0), "dB")), size=2, angle=90, vjust=-0.5, hjust=2.88) +
		ggtitle(paste("Frequency Spectrum Plots for ", var, " by Trial Number \n (Middle Tap and Middle Microphone)")) +
			xlab("Frequency") +
			ylab("Hertz (dB)") +
  		facet_wrap(~specnum, nrow=2) +
			theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), plot.title = element_text(hjust = 0.5))
		ggsave(filename=paste("spectrum_",var,".png", sep=""), width = 10, height = 5, units = c("in"))
}



