library(ggplot2)
library(car)
library(lattice)
library(methods)
library(dplyr)
library(reshape2)
library(RColorBrewer)
#function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
    detach(plyr)
}



##################################################################
## Prep data
l1 <- read.csv("~/Documents/College/AppliedMathConsulting/Lucchi2/lucchi2.csv")
l1 <- l1[c(1:463),c(1:6)]
l1$woodnumf <- as.factor(l1$woodnum)
l1summ <- summarySE(l1, measurevar="measure", groupvars=c("woodtype","woodnum","woodpiece","side","force","woodnumf"))
l1summ$forcef <- as.factor(l1summ$force)
l1summ$woodpiecef <- as.factor(l1summ$woodpiece)

#test samples for each force level
test <- l1summ %>%
	dplyr::group_by(woodnum, forcef,N) %>%
	dplyr::summarize(sampsize = sum(N))
#20 is tinytinytiny, drop it.
l1 <- l1[which(l1$force!=20),]
l1summ <-  l1summ[which(l1summ$force!=20),]




##################################################################



##################################################################
## Investigating 

# ggplot(l1[which(l1$woodtype=="s"),], aes(x=force, y=measure)) +
# 	geom_point(aes(color=woodnum)) +
# 	geom_smooth()+
# 	#stat_smooth(formula = y ~ I(1/x), method=glm) +
# 	scale_x_continuous(name="Force Applied (Newtons)") +
# 	scale_y_continuous(name="Lucchi Value")

ggplot(l1summ, aes(x=force, y=measure, color=woodnum)) +
	geom_line() +
	geom_errorbar(aes(ymin=(measure-se), ymax=(measure+se)), width=0.1) +
	xlab("Force Applied (Newtons)") +
	ylab("Lucchi Value") +
	scale_color_discrete(name="Wood\nSample", labels=c("M15A","M15B","M47A","M47B","S67A","S67B","S68A","S68B")) +
	facet_wrap(~woodpiece)


palette1 <- c(brewer.pal(7,"Blues")[c(4:7)], brewer.pal(7,"Reds")[c(4:7)])
ggplot(l1summ[which(l1summ$force>2),], aes(x=force, y=se, color=woodnum)) +
	geom_line() +
	scale_color_manual(values = palette1, name="Wood\nSample", labels=c("M15A","M15B","M47A","M47B","S67A","S67B","S68A","S68B")) +
	xlab("Force Applied to Sensors (Newtons)") + ylab("Standard Error of Speed Measurement (m/s)")

ggplot(l1summ, aes(x=forcef, y=se)) +
	geom_boxplot() +
	xlab("Force Applied to Sensors (Newtons)") + ylab("Standard Error of Speed Measurement (m/s), by Wood Sample")





##################################################################
## Comparison Overall
ggplot(comparison, aes(x=woodnumf, y=)) +
	geom_boxplot() +
	ggtitle("Standard Error in Lucchi Measurements by Force Applied \n(All Wood Samples)") +
	xlab("Force Applied to Sensors (Newtons)") + ylab("Standard Error for Wood Sample")










