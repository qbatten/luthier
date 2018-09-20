library(reshape2)
library(dplyr)
library(ggplot2)

file_list <- list.files(path="./Spectra", all.files=FALSE)
file_list2 <- paste0("./Spectra/",file_list)
testo <-  do.call(rbind, lapply(file_list2, FUN=read.csv, sep=",", header=F)) 
write.csv(testo, "./testo.csv")

#################
#DATA PREP

stest <- read.csv("~/Documents/College/AppliedMathConsulting/R/Analysis/Spectra_Exported_DeDupe_ed2-1.csv", row.names=1)
stest <- stest[,c(1:7)]
stest <- stest[which(!is.na(stest$file)),]
write.csv(stest, file="Spectra_R_ed1.csv")
stest$woodnumf <- 
			ifelse(stest$woodnum=="15a","a1", 
			ifelse(stest$woodnum=="15b","a2", 
			ifelse(stest$woodnum=="47a","b1", 
			ifelse(stest$woodnum=="47b","b2",
			ifelse(stest$woodnum=="67a","c1", 
			ifelse(stest$woodnum=="67b","c2",
			ifelse(stest$woodnum=="68a","d1", 
			ifelse(stest$woodnum=="68b","d2", "!!!"))))))))

stest$woodpiecef <- 
			ifelse(stest$woodnum=="15a","a", 
			ifelse(stest$woodnum=="15b","a", 
			ifelse(stest$woodnum=="47a","b", 
			ifelse(stest$woodnum=="47b","b",
			ifelse(stest$woodnum=="67a","c", 
			ifelse(stest$woodnum=="67b","c",
			ifelse(stest$woodnum=="68a","d", 
			ifelse(stest$woodnum=="68b","d", "!!!"))))))))

######################################################
#PLOT ALL SPECTRA

ggplot(stest[which(stest$tap=="a" & stest$mic=="t"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap A and Mic T") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="b" & stest$mic=="t"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap B and Mic T") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="c" & stest$mic=="t"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap C and Mic T") +
	facet_wrap(~woodnumf)

ggplot(stest[which(stest$tap=="a" & stest$mic=="m"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap A and Mic M") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="b" & stest$mic=="m"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap B and Mic M") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="c" & stest$mic=="m"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap C and Mic M") +
	facet_wrap(~woodnumf)

ggplot(stest[which(stest$tap=="a" & stest$mic=="f"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap A and Mic F") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="b" & stest$mic=="f"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap B and Mic F") +
	facet_wrap(~woodnumf)
ggplot(stest[which(stest$tap=="c" & stest$mic=="f"),], aes(x=freq, y=hertz, color=mic)) +
	geom_line() +
	ggtitle("Tap C and Mic F") +
	facet_wrap(~woodnumf)



###############################################################
#Test Simple alg--MAX only

maxtest <- stest[which(stest$freq > 3000),c(1,3:9)]
maxres <- maxtest %>% group_by(woodnum, tap, mic) %>% filter(hertz == max(hertz))


#Investigate Reliability of TAP v MIC
ggplot(maxres, aes(x=freq, y=hertz, color=woodpiecef)) +
	geom_point() +
	facet_wrap(~mic)
ggplot(maxres, aes(x=freq, y=hertz, color=woodpiecef)) +
	geom_point() +
	facet_wrap(~tap)
##Mic F & Tap B ????
maxresFB <- maxres[which(maxres$mic=="f" & maxres$tap=="b"),]
ggplot(maxresFB, aes(x=freq, color=woodpiecef)) +
	geom_histogram() 

ggplot(maxres, aes(x=woodpiecef, y=freq, color=mic)) +
	geom_boxplot() +
	facet_wrap(~tap)
#Tap=B, Mic=F,M

