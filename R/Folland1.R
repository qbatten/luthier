#Setup
library(dplyr)
library(ggplot2)
library(rmarkdown)

FollandData_ed2 <- read.csv("~/Documents/College/AppliedMathConsulting/FollandData_ed2.csv")
View(FollandData_ed2)


ggplot(FollandData_ed2, aes(x=ogs, color=wtype)) + 
  geom_density() +
	ggtitle("Old Sound")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Dens_Old.pdf')
ggplot(FollandData_ed2, aes(x=schmidt, color=wtype)) + 
  geom_density() +
	ggtitle("Schmidt")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Dens_Schmidt.pdf')
ggplot(FollandData_ed2, aes(x=lucl, color=wtype)) + 
  geom_density() +
ggtitle("Lucchimeter - Length")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Dens_LucL.pdf')
ggplot(FollandData_ed2, aes(x=lucw, color=wtype)) + 
  geom_density() +
	ggtitle("Lucchimeter - Width")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Dens_LucW.pdf')
ggplot(FollandData_ed2, aes(x=density, color=wtype)) + 
  geom_density() +
	ggtitle("Density")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Dens_Dens.pdf')

##First Model, exploring importance of different variables
v.lm1<-lm(ll ~ wtype + schmidt + density + ogs, data=FollandData_ed2)
summary(v.lm)
anova(v.lm)
# Looks like ogs and density are not very important


## Graphs of interactions
ggplot(FollandData_ed2, aes(x=ogs, y=lucl, color=wtype)) + 
  geom_point() +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("Old vs. Lucchi")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Sctr_Old_LucL.pdf')
ggplot(FollandData_ed2, aes(x=schmidt, y=lucl, color=wtype)) + 
  geom_point() +
	geom_smooth(method=lm) +
	ggtitle("Schmidt vs. Lucchi")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Sctr_Schmidt-LucL.pdf')
ggplot(FollandData_ed2, aes(x=density, y=lucl, color=wtype)) + 
  geom_point() +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("Density vs. Lucchi")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Sctr_Dens-LucL.pdf')
ggplot(FollandData_ed2, aes(x=lucw, y=lucl, color=wtype)) + 
  geom_point() +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("LucchiW vs. LucchiL")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Sctr_LucW-LucL.pdf')


##Looking at measurements for same piece of wood
abm_a <- subset(FollandData_ed2, wtype=="M" & side=="A")
abm_b <- subset(FollandData_ed2, wtype=="M" & side=="B")
abs_a <- subset(FollandData_ed2, wtype=="S" & side=="A")
abs_b <- subset(FollandData_ed2, wtype=="S" & side=="B")
abs <- left_join(abm_a,abm_b,by="tnum")


ggplot(abs, aes(x=ogs.x, y=ogs.y)) + 
  geom_point() +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("Side comparison: Old Speed")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Side_Old.pdf')
ggplot(abs, aes(x=schmidt.x, y=schmidt.y)) + 
  geom_point()  +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("Side comparison: Schmidt")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Side_Schmidt.pdf')
ggplot(abs, aes(x=lucl.x, y=lucl.y)) + 
  geom_point()  +
	geom_smooth(method=lm, se=FALSE) +
	ggtitle("Side comparison: Lucchi")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/Side_LucL.pdf')

#Predicting Lucchi of one side via vars of the other
side.lm <- lm(lucl.y ~ lucl.x + ogs.x + schmidt.x + density.x, data=abs)
summary(side.lm)
anova(side.lm)
side.lm1 <- lm(lucl.y ~ lucl.x + schmidt.x + ogs.x + density.x, data=abs)
summary(side.lm1)
anova(side.lm1)
side.lm2 <- lm(lucl.y ~ lucl.x + schmidt.x, data=abs)
summary(side.lm2)
summary(abs$lucl.x)
qplot(geom="density", data=abs, schmidt.x)
anova(side.lm2)

#Lucchi by opposite Lucchi
lside.lm <- lm(lucl.y ~ lucl.x, data=abs)
summary(lside.lm)

#Density by opposite Density
dside.lm <- lm(density.y ~ density.x, data=abs)
summary(dside.lm)
predict(dside.lm, data.frame(density.x=0.5803))
summary(FollandData_ed2$density[which(FollandData_ed2$wtype=="M")])
#lucl.x predicts lucl.y pretty well!
coef(side.lm2)
predict1 <- predict(side.lm2, data.frame(lucl.x=3550:5000,schmidt.x=4700))
0.2801144*4375
1225.5-312.47
ggplot(abs, aes(x=lucl.x, y=lucl.y)) + 
  geom_point()  +
	geom_abline(intercept=913.03, slope= 0.7996) +
	ggtitle("Predicted Lucchi-L \n (side.lm2, lucl-opposite + avg(schmidt-opposite))")
dev.print(pdf, '~/Documents/College/AppliedMathConsulting/Graphs/Week3_1/pred-lucl-sidelm2.pdf')


#Schmidt-Lucchi Corr
sl.lm <-lm(lucl ~ schmidt, data=FollandData_ed2)
summary(sl.lm)


##Finding the (error + interhalf diff) in Lucchi
luclerr.lm1 <- lm(lucl.x ~ lucl.y, data=abs)
summary(luclerr.lm1)
luclerr_resid <- data.frame(err=resid(luclerr.lm1))
ggplot(luclerr_resid, aes(x=err)) + 
  geom_density() +
	ggtitle("Density vs. Lucchi")
sd(luclerr_resid$err) #SD = 166.6782
hist(luclerr_resid$err, breaks=50)

##Finding the (error + interhalf diff) in Schmidt
schmidterr.lm1 <- lm(schmidt.x ~ schmidt.y, data=abs)
summary(schmidterr.lm1)
schmidterr_resid <- data.frame(err=resid(schmidterr.lm1))
ggplot(schmidterr_resid, aes(x=err)) + 
  geom_density() +
	ggtitle("Density vs. Lucchi")
sd(luclerr_resid$err) #SD = 166.6782
hist(luclerr_resid$err, breaks=50)



