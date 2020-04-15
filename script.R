# Code for thesis - "What else do plants talk about?" 
# Andrew Rowley 17/3/2020
#Acknowledgments Professor Jump, Dr Thomas, Dr.Bussiere, & Dr.Duthie 

#To make text into paragraphs then go to 'Tools/Global Options/Code/Soft-wrap R source files'

# Brief experimental explanation
#The experiment consisted of 90 pots, each containing 2 plants with the aim of investigating whether communication about the ammount of light they were each recieving occured between them. To do this I implemented a full factorial experiment with 2 treatment types each with 3 levels. The 2 treatment types were Shade (Full shade; Half shade; No shade) and Barrier (Full barrier; Half barrier; No barrier). Plants were randomly assigned left or right position within the pot apart from plants within the 'Half shade' treatment where the shaded plants were all assinged as the left hand plant. At the end of the experiment, the total height of the plant were recorded & the newest and second newest leaves were harvested and had their chlorophyll level recorded and their stomatal density measured. I looked for changes in stomatal density within the 'Half shade' treatment plants to assess whether plants were communicating about their irradiance levels. 



#open list below by clicking arrow to the left of the line below.
#-------------------------Height--------------------------------------------


install.packages('readr')
library(readr)

#Read in data set
a <- read_csv("~/A Fungal Affair/Complete analysis/all data.csv")
View(a)


str(a)

#changing variables to factors 
a$shade <- as.factor(a$shade)
a$pot <- as.factor(a$pot)
a$side <- as.factor(a$side)
a$age <- as.factor(a$age)
a$potnumber <- as.factor(a$potnumber)
a$plantnumber <- as.factor(a$plantnumber)
a$remarks <- as.factor(a$remarks)
a$shadepot <- as.factor(a$shadepot)
a$shadepotword <- as.factor(a$shadepotword)
a$potshadeword <- as.factor(a$potshadeword)
a$withintreatmentnumber <- as.factor(a$withintreatmentnumber)


#Data Description

#code: indivual sample ID that identifies a sample from its combination of treatments, potnumber within the treatment, side of the pot and age of the sample.(The age gets removed for the height analysis as both the newest and second oldest leaf come from a plant of the same height.)

#shade: The shade treatment (Full shade= both plants shaded, Half shade= left hand plant shaded, No shade= neither plant shaded)

#barrier: The barrier treatment (Full barrier= No communication allowed between plants, Half barrier= Mycorrhizal communication allowed, No barrier= Both Root and Mycorrhizal communication allowed)

#side: the side of the pot that the plant was collected from, this was randomly assigned for all pots apart from the Half shade treatment where the shaded plant was designated the left hand plant.

#age:two samples were taken from each plant, one from the newest leaf and one from the second newest leaf (Newest leaf designated as N & second newest leaf designated as O)

#potnumber: there were 90 pots overall and accounting for it allows for random effects to be calculated

#plantnumber: there were two samples taken from each plant and so accounting for it allows for random effects to be calculated (Not included for the height analysis as only 1 sample per plant was given and so the model wouldnt have been able to run)

#remarks: some plants were too small to harvest to the planned leaf and so the 3 Newest leaf was recorded. This is handled by removing them from the analysis to see if they cause an effect and then if they dont they are kept in the data but are mentioned.

#height: height of plant in cm

#spad1-5: individual spad meter (measures chlorophyll amount) recordings from leaves on harvest day (5 were taken per leaf)

#spadav: average of these readings 

#sd1-5:individual stomatal counts from leaves (5 were taken per leaf)

#sdav: average of individual stomatal counts

#shadepot:shorthand of treatment combination

#shadepotword:shorthand of treatment combination

#potshadeword:shorthand of treatment combination

#withintreatmentnumber: There were 90 pots with a total of 9 treatments (3 shade x 3 barrier) and so there were 10 pots per treatment. This labels each pot within the treatment. 



#The main data set contains more data than we need and has the height value doubled as two samples were taken per plant, this can be thinnned down before analysis. 

#seperating out required data/removing duplicates 

ab <- data.frame(a$shade,a$pot,a$side,a$age,a$potnumber,a$plantnumber,a$height,a$shadepot,a$shadepotword,a$potshadeword,a$withintreatmentnumber)

View(ab)

ab <- ab[-seq(2, NROW(a), by = 2),]


#Graphics

install.packages("ggplot2")
library(ggplot2)

install.packages("checkmate")
library(checkmate)

install.packages("Hmisc")
library(Hmisc)

install.packages("xfun")
library(xfun)

install.packages("stringi")
library(stringi)

install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")


#Violin plot of shade/barrier treatment against height
ggplot(ab, aes(x=a.shadepotword, y=a.height, color = a.side)) + 
  geom_violin(trim=TRUE) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.05), alpha=0.5) +
  stat_summary(fun=mean, geom="point", shape=15, size=2,position=position_dodge(0.9)) +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  scale_x_discrete(labels = abbreviate) +
  labs(color = "Mean from left and right plant") +
  theme(legend.position="top") +
  labs(x="Shade/Barrier Treatment", y = "Height (cm)")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))


#descriptions
#shaded plants etiolated and were taller than the non-shaded plants
#plant height also varied with barrier type with increasing plant height as barrier lessened from full to no barrier. #Is barrier type height increase due to etiolation or increased plant growth as increased soil substrate?


#stats
install.packages("lme4")
library(lme4)

install.packages("MuMIn")
library(MuMIn)

install.packages("lmerTest")
library(lmerTest)

#mixed model with 3 way interaction shows that shade does have an effect on height and that barrier also has an effect during an interaction with shade. A model comparison was completed with a model that did not include barrier type to see if it was still important.

mixed<- lmer(a.height~a.shade*a.pot*a.side+(1|a.potnumber),data=ab)

mixed1<- lmer(a.height~a.shade*a.side+(1|a.potnumber),data=ab)


#Diagnostics

#checking for linearity with plot of residuals against observed values, no pattern so assume linear and homogeneity of variance 

plot(mixed, xlab="Fitted values", ylab="Oberserved residuals")

ggplot(mixed)

#This residual plot does not indicate any deviations from a linear form. It also shows relatively constant variance across the fitted range. The slight reduction in apparent variance on the right and left of the graph are likely a result of there being fewer observation in these predicted areas.



#coeffients 
summary(mixed)
summary(mixed1)

#confidence intervals
confint(mixed)

#model comparison
AICc(mixed,mixed1)

#mixed: 1072.188
#mixed1: 1115.041

#Model comparison reveals that the more complex model improves the AICc score 


install.packages("merTools")
library(merTools)
predictInterval(mixed)   # for various model predictions, possibly with new data
REsim(mixed)             # mean, median and sd of the random effect estimates
plotREsim(REsim(mixed))  # plot the interval estimates
#Add pot to xaxis and coeffcien to y axis

##plot of the estimated random effects for each pot and their interval estimate. It can be seen that the majority include 0 and so have a small-negligabale effect. with a small number having an effect shown in bold.

#exporting coeeficients to table

install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)

tab_model(mixed)




#-------------------------Chlorophyll content------------------------------------
install.packages('readr')
library(readr)
a <- read_csv("~/A Fungal Affair/Complete analysis/all data.csv")
View(a)


str(a)

a$shade <- as.factor(a$shade)
a$pot <- as.factor(a$pot)
a$side <- as.factor(a$side)
a$age <- as.factor(a$age)
a$potnumber <- as.factor(a$potnumber)
a$plantnumber <- as.factor(a$plantnumber)
a$remarks <- as.factor(a$remarks)
a$shadepot <- as.factor(a$shadepot)
a$shadepotword <- as.factor(a$shadepotword)
a$potshadeword <- as.factor(a$potshadeword)
a$withintreatmentnumber <- as.factor(a$withintreatmentnumber)


#Data description

#code: indivual sample ID that identifies a sample from its combination of treatments, potnumber within the treatment, side of the pot and age of the sample.

#shade: The shade treatment (Full shade= both plants shaded, Half shade= left hand plant shaded, No shade= neither plant shaded).

#barrier: The barrier treatment (Full barrier= No communication allowed between plants, Half barrier= Mycorrhizal communication allowed, No barrier= Both Root and Mycorrhizal communication allowed).

#side: the side of the pot that the plant was collected from, this was randomly assigned for all pots apart from the Half shade treatment where the shaded plant was designated the left hand plant.

#age: two samples were taken from each plant, one from the newest leaf and one from the second newest leaf (Newest leaf designated as N & second newest leaf designated as O).

#potnumber: there were 90 pots overall and accounting for it allows for random effects to be calculated.

#plantnumber: there were two samples taken from each plant and so accounting for it allows for random effects to be calculated.

#remarks: some plants were too small to harvest to the planned leaf and so the 3 Newest leaf was recorded. This is handled by removing them from the analysis to see if they cause an effect and then if they dont they are kept in the data but are mentioned.

#height: height of plant in cm.

#spad1-5: individual spad meter (measures chlorophyll amount) recordings from leaves on harvest day (5 were taken per leaf).

#spadav: average of these readings.

#sd1-5: individual stomatal counts from leaves (5 were taken per leaf).

#sdav: average of individual stomatal counts.

#shadepot: shorthand of treatment combination.

#shadepotword: shorthand of treatment combination.

#potshadeword:shorthand of treatment combination.

#withintreatmentnumber: There were 90 pots with a total of 9 treatments (3 shade x 3 barrier) and so there were 10 pots per treatment. This labels each pot within the treatment. 


#select data we need
ac <- data.frame(a$shade,a$pot,a$side,a$age,a$potnumber,a$plantnumber,a$spadav,a$shadepot,a$shadepotword,a$potshadeword,a$withintreatmentnumber,a$remarks)

View(ac)

#Graphics

install.packages("ggplot2")
library(ggplot2)

install.packages("checkmate")
library(checkmate)

install.packages("Hmisc")
library(Hmisc)

install.packages("xfun")
library(xfun)

install.packages("stringi")
library(stringi)

View(a)

#to remove all leaves that didnt meet criteria (developed post day 35)
ac2 <- ac[ac$a.remarks=="none",]

#violin plot of shade/barrier treatment against chlorophyll model 

ggplot(ac, aes(x=a.shadepotword, y=a.spadav, color = a.side)) + 
  geom_violin(trim=TRUE) +
  geom_point(position = position_jitterdodge(dodge.width = 0.9, jitter.width = 0.05), alpha=0.5) +
  stat_summary(fun=mean, geom="point", shape=15, size=2,position=position_dodge(0.9)) +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  scale_x_discrete(labels = abbreviate) +
  labs(color = "Means from left and right plant") +
  theme(legend.position="top") +
  labs(x="Shade/Barrier Treatment", y = "Spad meter readings (units)")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))



#code for times new roman in plots

library(ggplot2)
install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")

#descriptions
#shaded plants had lower chloropyll levels
#shade had an effect on chlorophyll levels 
#barrier had no effect on chlorophyll levels
  
#increasing volume of substrate increased plant hieght however did not effect chlorophyll levels which were soley affected by shade. However effect of shade in chlorophyll not larger than variation found within chlorophyll measurements so not signif despite p-values


#stats
install.packages("lme4")
library(lme4)

install.packages("MuMIn")
library(MuMIn)

install.packages("lmerTest")
library(lmerTest)

#mixed model with 3 way interaction shows that age does have an effect on chlorophyll levels and that shade & barrier also has an effect during an interactions. Side also has an effect in the Half shade treatments.A model comparison was completed with a model that did not include each fixed effect to see if they are still important.

#if need reference change
ac$a.shade <- factor(ac$a.shade, levels = c("No shade","Half shade","Full shade"))

mixed<- lmer(a.spadav~a.shade*a.pot*a.side+a.age+(1|a.potnumber)+(1|a.plantnumber),data=ac)

summary(mixed)

mixed1<- lmer(a.spadav~a.pot*a.side+a.age+(1|a.potnumber)+(1|a.plantnumber),data=ac)

mixed2<- lmer(a.spadav~a.shade*a.side+a.age+(1|a.potnumber)+(1|a.plantnumber),data=ac)

AICc(mixed,mixed1,mixed2)


#Model comparison reveals that the more complex model improves the AICc score

#Diagnostics

#checking for linearity with plot of residuals against observed values, no pattern so assume linear and homogeneity of variance 

plot(mixed, xlab="Fitted values", ylab="Oberserved residuals")

#This residual plot does not indicate any deviations from a linear form. It also shows relatively constant variance across the fitted range. The slight reduction in apparent variance on the right and left of the graph are likely a result of there being fewer observation in these predicted areas.

#coeffients 
summary(mixed)

#confidence intervals
confint(mixed)


#random effects code


install.packages("merTools")
library(merTools)
predictInterval(mixed)   # for various model predictions, possibly with new data
REsim(mixed)             # mean, median and sd of the random effect estimates
plotREsim(REsim(mixed))  # plot the interval estimates


##plot of the estimated random effects for each pot and their interval estimate. It can be seen that the majority include 0 and so have a small-negligabale effect. with a small number having an effect shown in bold.


#The below excersize in removing the leaves that didnt meet criteria, showed that we should remove them and they are removed in the above code before formal anaylsis. 

#during to complications during the experiment, some of the plants did not reach the desired maturity and so the 3rd newest leaf was harvested. These are marked as ts (too small) within the remarks column. I will now remove them to see if they influence the analysis
#360 with ts
#285 without ts

View(a)

a <- a[a$remarks=="none",]

ad <- data.frame(a$shade,a$pot,a$side,a$age,a$potnumber,a$plantnumber,a$spadav,a$shadepot,a$shadepotword,a$potshadeword,a$withintreatmentnumber,a$remarks)

View(ad)

mixed<- lmer(a.spadav~a.shade*a.pot*a.side+a.age+(1|a.potnumber)+(1|a.plantnumber),data=ad)

summary(mixed)



#table output

install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)

tab_model(mixed)


#-------------------------Stomatal Density-------------------------------------

install.packages("readr")
library(readr)
a<- read_csv("~/A Fungal Affair/Complete analysis/LR.csv")


a$age <- as.factor(a$age)
a$potnumber <- as.factor(a$potnumber)
a$shade <- as.factor(a$shade)
a$barrier <- as.factor(a$barrier)
a$shadebarrier <- as.factor(a$shadebarrier)
a$shadebarrierword <- as.factor(a$shadebarrierword)
a$remark <- as.factor(a$remark)

View(a)
str(a)


#Data

#Stomatal Density Average R: Average Stomatal density of right hand plant within pot

#Stomatal Density Average L: Average Stomatal density of left hand plant within pot

#Difference between L & R: Difference in average stomatal density between left and right hand plant within pot

#Difference between L & R mm2: The same difference ^, just converted into standardised units (mm2)

#age: two leaves were harvested from each plant (newest = N and second newest = O)

#potnumber: Allows for the random effects of pot to be accounted for

#shade: The shade treatment (Full shade= both plants shaded, Half shade= left hand plant shaded, No shade= neither plant shaded)

#barrier: The barrier treatment (Full barrier= No communication allowed between plants, Half barrier= Mycorrhizal communication allowed, No barrier= Both Root and Mycorrhizal communication allowed)

#shadebarrier: shorthand coding of above shade and barrier treatments that aid with graphics... where (Fullshade=1,Half shade=2, No shade=3 & Full barrier= A, Half barrier=B, No barrier=C)

#shadebarrierword: short hand coding for above treatments that aid with graphics... where (Fullshade=Fs,Half shade=Hs, No shade=Ns & Full barrier= Fb, Half barrier=Hb, No barrier=Nb)


#Due to complications (silicoln sealent used to secure barrier to pots poisoned the plants stunting growth) during the experiment, some of the plants did not reach the desired maturity and so the 3rd newest leaf was harvested. These are marked as ts (too small) within the remarks column. I will now remove them to see if they influence the analysis
#180 with ts
#124 without ts


View(a)

b <- a[a$remark=="none",]





#Graphical identification of outliers 

boxplot(b$`Difference between L & R mm2`~b$shadebarrier)

# 3 outliers detected however they did not affect analysis upon removal and so were left in.





#graphics
install.packages("xfun")
library(xfun)

install.packages("checkmate")
library(checkmate)

install.packages("Hmisc")
library(Hmisc)

install.packages("stringi")
library(stringi)

install.packages("digest")
library(digest)

install.packages("ggplot2")
library(ggplot2)




#violin plot of shade/barrier treatment against stomatal density
ggplot(b, aes(x=shadebarrierword, y=`Difference between L & R mm2`, colour = shade)) + 
  geom_violin(trim=TRUE, size=1) +
  geom_jitter(shape=16, position=position_jitter(0.075), colour ="#333333") +
  stat_summary(fun=mean, geom="point", shape=15, size=2,position=position_dodge(0.9), color= "red") +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar", color="black", width=0.2, size=0.75)+
  scale_x_discrete(labels = abbreviate) +
  labs(color = "Shade Treatment") +
  labs(x="Shade/Barrier Treatment", y = "Difference in Stomatal density (mm2)")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))





library(ggplot2)
install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")

#fancy plot with 
#geom_point() +
#  facet_wrap(~ cyl)

#which direction is change when moving from full barrier to half barrier within halfshade treatments? around about -25 difference
#plot showing left and right plants from full barrier treatment and half barrier treatment

c <- read.csv("~/A Fungal Affair/Complete analysis/compareeffectchangelightshade.csv")

View(c)

c2 <- c[c$remark=="none",]

View(c)

#boxplot checking how the left and right hand plants changed between full barrier and no barrier within half shade treatment 
ggplot(c, aes(x=treatment, y=averagels,colour=side)) + 
  geom_boxplot()+
  labs(x="Treatment", y = "Stomatal density")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top")


#violin plot checking how the left and right hand plants changed between full barrier and no barrier within half shade treatment  
ggplot(c2, aes(x=treatment, y=averagels, colour = side)) + 
  geom_violin(trim=TRUE, size=1) +
  geom_point(position=position_jitterdodge(jitter.width=0.15,dodge.width=0.9))+
  labs(x="Shade/Barrier Treatment", y = "Stomatal density (mm2)")+
  labs(color = "Side") +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  stat_summary(fun=mean, geom="point",position=position_dodge(0.9), shape=15, size=4 )+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))



#Checking how the left and right hand plants changed between full barrier and no barrier within full light and full shade differences 

light <- read.csv("~/A Fungal Affair/Complete analysis/compareeffectchangelight.csv")
shade <- read.csv("~/A Fungal Affair/Complete analysis/compareeffectchangeshadee.csv")


light2 <- light[light$remark=="none",]

shade2 <- shade[shade$remark=="none",]

View(shade2)

ggplot(light2, aes(x=treatment, y=averages, colour = side)) + 
  geom_violin(trim=TRUE, size=1) +
  geom_point(position=position_jitterdodge(jitter.width=0.15,dodge.width=0.9))+
  labs(x="Shade/Barrier Treatment", y = "Stomatal density (mm2)")+
  labs(color = "Side") +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  stat_summary(fun=mean, geom="point",position=position_dodge(0.9), shape=15, size=4 )+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="top",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))


library(ggplot2)
install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")





#Mixed model 

install.packages("lme4")
library(lme4)

install.packages("MuMIn")
library(MuMIn)

install.packages("lmerTest")
library(lmerTest)

#Here I aim to find the effect on stomatal density when moving from a Half shade/Full barrier to Half shade/ Half barrier & Half shade/ No barrier. 
#When grown in the light, Tomato plants have stomatal densities of ~ 120 mm2. This can drop by ~40 mm2 when grown in the shade (O'Carrigan et al.2014). 
#I expect to see that the difference in stomatal density between the plants within Half shade/Full barrier is similar to the other controls. I expect this as there is no communication allowed by the barrier type despite their being one plant in the light and the other in the shade
#I expect that the difference in stomatal density between the plants within Half shade/ Half barrier & Half shade/ No barrier will differ from Half shade/Full barrier. I expect this as communication is allowed. 

#The default reference level is Full shade/Full barrier, however, this does not allow me to make a comparison between Half shade/ Full barrier and Half shade/ Half barrier & Half shade/ No barrier

#get correct reference level 
b$shade <- factor(b$shade, levels = c("Halfshade","Fullshade","Noshade"))

mixed<- lmer(`Difference between L & R mm2`~shade*barrier+age+(1|potnumber),data=b)

#Assumptions
plot(resid(mixed),ab$a.height)

plot(mixed, xlab="Fitted values", ylab="Oberserved residuals")
#coeffients 
summary(mixed)

#confidence intervals
confint(mixed)

#There is a large effect (19.289) of moving from Full barrier to Half barrier within the Half shade treatment. However the CI's are wide (1.936046-36.641732) and so I cannot exclude that the effect may be much weaker. 


#There is a negligable effect (2.385) of moving from Full barrier to No barrier within the Half shade treatment. The CI's are also wide (-15.070965-19.844363) and so I cannot assume with much certainty.
#Over half of the variation within the random effects was caused by potnumber between the new and old leaves?

#table output

install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)

tab_model(mixed)

#random effects
install.packages("merTools")
library(merTools)
predictInterval(mixed)   # for various model predictions, possibly with new data
REsim(mixed)             # mean, median and sd of the random effect estimates
plotREsim(REsim(mixed))  # plot the interval estimates
#Add pot to xaxis and coeffcien to y axis

#plot of the estimated random effects for each pot and their interval estimate. It can be seen that the majority include 0 and so have a small-negligabale effect. with a small number having an effect shown in bold.

#Finally, one critique of including a full factorial experiment within a mixed model and then 'extorting' it for a few particular values instead of general effect is that those results are artifacts of this particular data analaysis and the relationship may not exist if they were investigated in isolation. Below we investigate just the Half shade treatment and look for effects within barrier type. This is not a formal analysis however some may see it neccessary to assure presence of effect when accounting for a complex experiment within the model. 
#Below is a model of just the Half shade data across the three treatments

c <- read.csv("~/A Fungal Affair/Complete analysis/comparesimpleandcomplex.csv")

c <- c[c$remark=="none",]

test <- lmer(`Difference.between.L...R.mm2`~barrier+age+(1|potnumber),data=c)
summary(test)
confint(test)

#The effect size is still -19.556 (-47.82 - 8.67)so I find the more complex model to be held up. rediduals variation within random effects increases. 




#References
# O'Carrigan, A., Hinde, E., Lu, N., Xu, X., Duan, H., Huang, G., Mak, M., Bellotti, B. and Chen, Z. (2014) Effects of light irradiance on stomatal regulation and growth of tomato. Environmental and Experimental Botany, 98, pp. 65-73. 10.1016/j.envexpbot.2013.10.007.



#Possible code for nice random effect plots, if i have time later on

model_output <- readRDS("model_output.rds")

model_output %>%
  tidy(effects = "fixed", component = "cond", conf.int = T, scales = "NA") %>%
  select(-effect, -component, -std.error) %>%
  select(term, estimate, conf.low, conf.high, everything()) %>%
  mutate(
    term = c("Acari (Intercept)", "Coleoptera", "Diptera", "Hemiptera", "Hymenoptera", "Lepidoptera", "Other"),
    estimate = plogis(estimate),
    conf.low = plogis(conf.low),
    conf.high = plogis(conf.high)
  ) %>%
  ggplot(aes(y = term, x = estimate, colour = 1 - p.value)) +
  geom_vline(xintercept = 0, size = 1.5, colour = "#efefef") +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = .5
  ) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none")

model_output %>%
  tidy(effects = "ran_vals", conf.int = T) %>%
  mutate(
    level = factor(c("Start hr: 10:00", "Start hr: 11:00", "Start hr: 13:00", "Start hr: 14:00", "Start hr: 18:00", " Start hr: 19:00", "Day: one", "Day: two", "Distance: 000 m", "Distance: 025 m", "Distance: 050 m", "Distance: 075 m", "Distance: 125 m", "Observer: A", "Observer: B", "Observer: C", "Observer: D")),
    estimate = plogis(estimate),
    std.error = plogis(std.error),
    conf.low = plogis(conf.low),
    conf.high = plogis(conf.high)
  ) %>%
  ggplot(aes(y = level, x = estimate, color = factor(group))) +
  geom_vline(xintercept = 0, size = 1.5, colour = "#efefef") +
  #facet_grid(~grpvar) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = .5
  ) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "top")




#-------------------------Experiment 2-------------------------------




#Experiment 2

library(readr)
a <- read.csv("~/A Fungal Affair/data/exp2with0.csv")

a$number <- as.factor(a$number)

str(a)

View(a)


install.packages("ggplot2")
library(ggplot2)

install.packages("checkmate")
library(checkmate)

install.packages("Hmisc")
library(Hmisc)

install.packages("xfun")
library(xfun)

install.packages("stringi")
library(stringi)

install.packages("extrafont")
library(extrafont)
loadfonts(device = "win")


#plot damage to petioles 
ggplot(a, aes(x=treatment, y=pertiole.damage)) + 
  geom_violin(trim=TRUE) +
  geom_jitter(width = 0.03, shape = 1)+
  stat_summary(fun=mean, geom="point", shape=15, size=2,position=position_dodge(0.9)) +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  labs(x="Sealant Treatment", y = "Damage (%)")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="none",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))

View(a)
# is this bad boy normal? 
hist(a$pertiole.damage)

m1 <- lm(a$pertiole.damage~a$treatment)

plot(m1)

# This bad boy is definetly not normal...

#will use randomisation technique (v. happy i get to use this, my favourite statistical method!!)

View(a)


#Petiole damage

# randomising the treatment labels and comparing find the difference between treatments. Repeat 9999 times and make distribution out of difference. Compare real difference between treatments to this distribution & calculate p-values from how many are equal to or above the actual difference/10000. 


iter    <- 9999;    
diff    <- NULL;          
N       <- dim(a)[1];
for(i in 1:iter){   
  a_samp   <- sample(x = a[,5], size = N, replace = FALSE);
  samp_sealant  <- which(a_samp == "Sealant");
  samp_nsealant   <- which(a_samp == "No Sealant");
  mn_samp_s   <- mean(a[samp_sealant, 3]);
  mn_samp_ns   <- mean(a[samp_nsealant, 3]);
  diff[i]     <- mn_samp_s - mn_samp_ns;
}

View(diff)

hist(diff)

#adding arrow

obs_pink <- which(a[,5] == "Sealant");
obs_yellow  <- which(a[,5] == "No Sealant");
obs_diff  <- mean(a[obs_pink,3]) - mean(a[obs_yellow,3]); 
arrows(x0 = obs_diff, x1 = obs_diff, y0 = 500, y1 = 10, lwd = 3, length = 0.1);


greater_or_equal_obs <- sum(abs(diff) >= abs(obs_diff)) + 1;
total_generated   <- length(diff) + 1;
new_p_value       <- greater_or_equal_obs / total_generated;

print(new_p_value)
#[1] 3e-04











#plot height
ggplot(a, aes(x=treatment, y=height)) + 
  geom_violin(trim=TRUE) +
  geom_jitter(width = 0.05, shape = 1)+
  stat_summary(fun=mean, geom="point", shape=15, size=2,position=position_dodge(0.9)) +
  stat_summary(fun.data = "mean_cl_normal", geom="errorbar",position=position_dodge(0.9), width=0.2, size=0.75 )+
  labs(x="Sealant Treatment", y = "Height (cm)")+
  theme_light()+
  theme(axis.line.x.bottom = element_line(color = "black"),
        axis.line.y.left = element_line(color = "black"),
        legend.position="none",
        axis.text = element_text(size = 15, colour="black", family="serif"),
        axis.title = element_text(size = 15, colour = "black", family="serif"),
        legend.text = element_text(size = 15, family="serif"),
        legend.title = element_text(size = 15, family="serif"))






#Plant height

iter    <- 9999;    
diff    <- NULL;          
N       <- dim(a)[1];
for(i in 1:iter){   
  a_samp   <- sample(x = a[,5], size = N, replace = FALSE);
  samp_sealant  <- which(a_samp == "Sealant");
  samp_nsealant   <- which(a_samp == "No Sealant");
  mn_samp_s   <- mean(a[samp_sealant, 2]);
  mn_samp_ns   <- mean(a[samp_nsealant, 2]);
  diff[i]     <- mn_samp_s - mn_samp_ns;
}

View(diff)

hist(diff)

#adding arrow

obs_sealant <- which(a[,5] == "Sealant");
obs_nsealant  <- which(a[,5] == "No Sealant");
obs_diff  <- mean(a[obs_sealant,2]) - mean(a[obs_nsealant,2]); 
arrows(x0 = obs_diff, x1 = obs_diff, y0 = 500, y1 = 10, lwd = 3, length = 0.1);


greater_or_equal_obs <- sum(abs(diff) >= abs(obs_diff)) + 1;
total_generated   <- length(diff) + 1;
new_p_value       <- greater_or_equal_obs / total_generated;

print(new_p_value)
#1e-04







