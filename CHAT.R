# Case Study: Childhood Adenotonsillectomy Trial (CHAT) Study

# import data
chatdata<-read.csv("H:\\Case Studies\\Case Study 1\\chatsibs.csv")
# chatdata<-read.csv("~/Dropbox/Case Study 1/chatsibs.csv")

# specify factors
chatdata$agec<-factor(chatdata$agec, labels=c("5-7", "8-10"))
chatdata$arm<-factor(chatdata$arm, labels=c("eAT", "WWSC"))
chatdata$race_c<-factor(chatdata$race_c, labels=c("African American", "Non-African American"))
chatdata$race_cat<-factor(chatdata$race_cat, labels=c("African American", "Non-African American"))
chatdata$site<-factor(chatdata$site, labels=c("10:CHOP", "20:CINN", "30:CLD", "50:SLU", "60:MNYC", "70:BOST"))
chatdata$Overweight_BL<-factor(chatdata$Overweight_BL, labels=c("no", "yes"))
chatdata$Overweight_FLP<-factor(chatdata$Overweight_FLP, labels=c("no", "yes"))

# BMI Diff
chatdata$bmi_diff<-chatdata$bmi_flp-chatdata$bmi_bl

# Score Diff
chatdata$scorediff<-chatdata$conn_gitot_care_flp-chatdata$conn_gitot_care_bl

# Score Improvement
chatdata$improv<-factor(NA,levels=c("yes","no"))
chatdata$improv[chatdata$scorediff<0]<-"yes"; chatdata$improv[chatdata$scorediff>=0]<-"no"

attach(chatdata)
# detach(chatdata)

#view data
View(chatdata)
str(chatdata)
summary(chatdata)

# outcome variables
chatdata$conn_gitot_care_bl
chatdata$conn_gitot_care_flp

# summary outcome variables
summary(chatdata$conn_gitot_care_bl)
summary(chatdata$conn_gitot_care_flp)

#########################################
# SIMPLE LINEAR REGRESSION
#########################################
# BMI Diff
cor(scorediff, bmi_diff, use = "complete.obs")
slr0<-lm(scorediff ~ bmi_diff); summary(slr0) #scorediff ~ bmi_diff
slr1<-glm(scorediff ~ bmi_diff); summary(slr1) #scorediff ~ bmi_diff
plot(predict(slr1),rstandard(slr1))

# Baseline Systolic BP
slr2<-glm(scorediff ~ avgsysbp_bl); summary(slr2) #scorediff ~ avgdysbp_bl
plot(predict(slr2),rstandard(slr2))


# Baseline Dyastolic BP
slr3<-glm(scorediff ~ avgdysbp_bl); summary(slr3) #scorediff ~ avgsysbp_bl
plot(predict(slr3),rstandard(slr3))

# Baseline CRS ~ Baseline BMI
slr4<-glm(conn_gitot_care_bl ~ bmi_bl); summary(slr4) #conn_gitot_care_bl ~ bmi_bl
plot(predict(slr4),rstandard(slr4))

# Followup CRS ~ Followup BMI
slr5<-glm(conn_gitot_care_flp ~ bmi_flp); summary(slr5) #conn_gitot_care_flp ~ bmi_flp
plot(predict(slr5),rstandard(slr5))

#########################################
# MULTIPLE LINEAR REGRESSION
#########################################
# Baseline BMI, Baseline Sys BP, Baseline Dys BP
mlr1<-glm(scorediff ~ bmi_bl + avgdysbp_bl + avgsysbp_bl); summary(mlr1) #scorediff ~ bmi_bl + avgdysbp_bl + avgsysbp_bl
plot(predict(mlr1),rstandard(mlr1))
pairs(~scorediff +bmi_bl + avgdysbp_bl + avgsysbp_bl)

#BMI Diff, Race, Age, Tx Arm
mlr2<-glm(scorediff ~ bmi_diff + race_cat + agec + arm); summary(mlr2) #scorediff ~ bmi_diff + race_cat + agec + arm
drop1(mlr2)
plot(predict(mlr2),rstandard(mlr2))
pairs(~ scorediff+bmi_diff + race_cat + agec + arm)

#BMI Diff, Race, Tx Arm
mlr3<-glm(scorediff ~ bmi_diff + race_cat + arm); summary(mlr3)
plot(predict(mlr3),rstandard(mlr3))
pairs(~ scorediff+bmi_diff + race_cat + arm)


# ~SUPER MODEL~
super<-glm(scorediff ~ bmi_bl + avgdysbp_bl + avgsysbp_bl + race_cat + agec + arm + site); summary(super)
drop1(super)
super2<-glm(scorediff ~ bmi_bl + avgdysbp_bl + avgsysbp_bl + race_cat + arm + site); summary(super2)
drop1(super2)
super3<-glm(scorediff ~ avgdysbp_bl + avgsysbp_bl + race_cat + arm + site); summary(super3)
drop1(super3)
super4<-glm(scorediff ~ avgdysbp_bl + race_cat + arm + site); summary(super4)
drop1(super4)
super5<-glm(scorediff ~ race_cat + arm + site); summary(super5)
drop1(super5)
plot(predict(super5),rstandard(super5))

#interaction
int1<-glm(scorediff ~ bmi_bl + race_cat + bmi_bl:race_cat); summary(int1)
drop1(int1)

int2<-glm(scorediff ~ race_cat + site + race_cat:site); summary(int2)
drop1(int2)



#########################################
# LOGISTIC REGRESSION
#########################################
# Score Improvement ~ Overweight Baseline
lr1<-glm(improv ~ Overweight_BL, family="binomial"); summary(lr1) #improv ~ Overweight_BL
exp(coef(lr1))


RSTable<-tapply(race_c,site,FUN=table); RSTable
chisq.test(race_cat,site)
