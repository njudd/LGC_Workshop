wisc <- read.csv("~/Downloads/wisc.csv")

#assignment 1: install lavaan, ggplot

library(lavaan)
library(ggplot2)


#assignment 2: load data


#assignment 3: select variable to visualize
wisc_verbal<-data.frame(wisc$ID,wisc$V6,wisc$V7,wisc$V9,wisc$V11)
colnames(wisc_verbal)<-c('ID','T1_verbal','T2_verbal','T3_verbal','T4_verbal')

wisc_verbal_long<-wisc_verbal %>%pivot_longer(!ID, names_to = "wave", values_to = "verbal")
ggplot(wisc_verbal_long,aes(wave,verbal,group=ID,fill=ID))+geom_point(col='dodgerblue')+geom_line(col='dodgerblue')

#assignment 4: fit an linear growth curve model

linear_growth_model <- ' i =~ 1*T1_verbal + 1*T2_verbal + 1*T3_verbal + 1*T4_verbal
                         s =~ 0*T1_verbal + 1*T2_verbal + 2*T3_verbal + 3*T4_verbal '
fit_linear_growth_model <- growth(linear_growth_model, data=wisc_verbal,missing='fiml')
summary(fit_linear_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#questions:
#how is model fit?
#what is the average verbal score at baseline?
#what is the average change per wave?

#Do people differ at baseline? what is the range in SD's?
#Do people differ in how much they change?

#what does the starting value (intercept) tell you about how much they change (slopes)?

###################################
######extra assignment#############
###################################
#refit the model for other variables in the datafile

#assignment 5: fit a nonlinear (basis) model

basis_growth_model <- ' i =~ 1*T1_verbal + 1*T2_verbal + 1*T3_verbal + 1*T4_verbal
                         s =~ 0*T1_verbal + T2_verbal + T3_verbal + 1*T4_verbal '
fit_basis_growth_model <- growth(basis_growth_model, data=wisc_verbal,missing='fiml')
summary(fit_basis_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#questions
#how is model fit?
#what is the average verbal score at baseline?
#what is the shape of change over time?

#which model fits better - linear or nonlinear?

anova(fit_basis_growth_model,fit_linear_growth_model)

#assignment 6: what predicts change?


basis_growth_model_cov_moe <- ' i =~ 1*V6 + 1*V7 + 1*V9 + 1*V11
                         s =~ 0*V6 + V7 + V9 + 1*V11 
s~moedu
i~moedu
i~~s
'
fit_basis_growth_model_covs <- growth(basis_growth_model_covs, data=wisc,missing='fiml')
summary(fit_basis_growth_model_covs, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



basis_growth_model__cov_P <- ' i =~ 1*V6 + 1*V7 + 1*V9 + 1*V11
                         s =~ 0*V6 + V7 + V9 + 1*V11 
s~P6
i~P6
i~~s
'
fit_basis_growth_model__cov_P <- growth(basis_growth_model__cov_P, data=wisc,missing='fiml')
summary(fit_basis_growth_model__cov_P, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)




#extra assignment: Simulations

#Simulate data for a four wave LGM

sim_lgm_4waves<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 

i~100*1         ##<## this specifies the intercept, or the mean at T1
i~~15*i         ##<## this specifies the intercept variance at T1

s~5*1           ##<## this specifies the slope, or change per wave
s~~10*s         ##<## this specifies the slope variance 

i~~-3*s         ##<## this specifies the intercept/slope covariance 

'
sim_lgm_4waves_dat <- simulateData(sim_lgm_4waves, sample.nobs = 500)

#Sanity check of the column means. As expected the scores increase ~ 5 points per wave
colMeans(sim_lgm_4waves_dat)


#Plotting. Now you can start tweaking parameters to see what the data looks like
sim_lgm_4waves_dat$id<-as.factor(1:nrow(sim_lgm_4waves_dat))
theme_set(theme_grey(base_size = 18)) #increase text size
plotdat<-melt(sim_lgm_4waves_dat)
ggplot(plotdat,aes(variable,value,group=id))+geom_point(col='dodgerblue')+geom_line(col='dodgerblue')




#Now let's see if we can recover the model parameters!

fit_lgm_4waves<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 
i~~s

'
fit_fit_lgm_4waves <- growth(fit_lgm_4waves, data=sim_lgm_4waves_dat,missing='fiml')

#Model fits well
summary(fit_fit_lgm_4waves, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#Parameters are recovered nicely
parTable(fit_fit_lgm_4waves)




#Simulate data for a quadratic model

sim_lgm_5waves_quad<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4+1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4+4*t5
sq =~ 0*t1 + 1*t2 + 4*t3 + 9*t4+16*t5

i~100*1         ##<## this specifies the intercept, or the mean at T1
i~~15*i         ##<## this specifies the intercept variance at T1

s~5*1           ##<## this specifies the slope, or change per wave
s~~15*s         ##<## this specifies the slope variance 

sq~5*1         ##<## this specifies the quadratic slope
sq~~1*sq         ##<## this specifies the quadratic slope variance


i~~3*s         ##<## this specifies the intercept/slope covariance 


'
sim_lgm_5waves_dat_quad <- simulateData(sim_lgm_5waves_quad, sample.nobs = 500)
colMeans(sim_lgm_5waves_dat_quad)

#Plotting. Now you can start tweaking parameters to see what the data looks like
sim_lgm_5waves_dat_quad$id<-as.factor(1:nrow(sim_lgm_5waves_dat_quad))
theme_set(theme_grey(base_size = 18)) #increase text size
plotdat<-melt(sim_lgm_5waves_dat_quad)
ggplot(plotdat,aes(variable,value,group=id))+geom_point(col='#FF4C4C')+geom_line(col='#FF4C4C')


#Estimate freely for 5 waves
fit_lgm_5waves_toquad<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4+1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4+4*t5
sq =~ 0*t1 + 1*t2 + 4*t3 + 9*t4 +16*t5

t1~~ervar1*t1
t2~~ervar2*t2
t3~~ervar3*t3
t4~~ervar4*t4
t5~~ervar5*t5
'
fit_fit_lgm_5waves_toquad <- growth(fit_lgm_5waves_toquad, data=sim_lgm_5waves_dat_quad)

#Model fits well
summary(fit_fit_lgm_5waves_toquad, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)




#Simulate data for a multigroup four wave LGM

sim_lgm_4waves_MG<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 

i~c(100,90,80)*1         ##<## this specifies the intercept, or the mean at T1
i~~c(15,15,2)*i         ##<## this specifies the intercept variance at T1

s~c(4,0,10)*1           ##<## this specifies the slope, or change per wave
s~~c(10,10,5)*s         ##<## this specifies the slope variance 

i~~c(3,3,3)*s         ##<## this specifies the intercept/slope covariance 


'
sim_sim_lgm_4waves_MG <- simulateData(sim_lgm_4waves_MG, sample.nobs = c(100,100,20))



fit_lgm_4waves_MG<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 

'
fit_sim_sim_lgm_4waves_MG <- growth(fit_lgm_4waves_MG, data=sim_sim_lgm_4waves_MG,group='group')

summary(fit_sim_sim_lgm_4waves_MG, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)



#Plotting. Now you can start tweaking parameters to see what the data looks like
sim_sim_lgm_4waves_MG$id<-as.factor(1:nrow(sim_sim_lgm_4waves_MG))
sim_sim_lgm_4waves_MG$group<-as.factor(sim_sim_lgm_4waves_MG$group)
theme_set(theme_grey(base_size = 18)) #increase text size
plotdat<-melt(sim_sim_lgm_4waves_MG,id=c("id","group"))
ggplot(plotdat,aes(variable,value,group=id,col=group))+geom_point()+geom_line(alpha=.6)+facet_wrap(~group)



fit_lgm_4waves_MG_cons<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
i~c(int1,int1)*1

'
fitfit_lgm_4waves_MG_cons <- growth(fit_lgm_4waves_MG_cons, data=sim_sim_lgm_4waves_MG,group='group')

summary(fitfit_lgm_4waves_MG_cons, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

anova(fitfit_lgm_4waves_MG_cons,fit_sim_sim_lgm_4waves_MG)





###############   MODEL COMPARISONS LGM
###############


#Now let's specify a reduced model, where we constrain the intercept/slope covariance to 0
fit_lgm_4waves_cons<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 
i~~0*s

'
fit_fit_lgm_4waves_cons <- growth(fit_lgm_4waves_cons, data=sim_lgm_4waves_dat)
summary(fit_fit_lgm_4waves_cons, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#Model fits poorly
summary(fit_fit_lgm_4waves, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#Now let's compare the models formally. This procedure is the same for any model comparison

#Likelihood ratio test: Simple model is preferred (more complex model not significantly better)
anova(fit_fit_lgm_4waves_cons,fit_fit_lgm_4waves)

#Model comparison using AIC

#Simplest: just look up the AIC. Lower is better

AIC(fit_fit_lgm_4waves_cons)
AIC(fit_fit_lgm_4waves)


#Compute akaike weights according to Wagenmakers and Farrell https://www.researchgate.net/profile/Simon_Farrell/publication/8588301_AIC_model_selection_using_Akaike_weights/links/53fc51540cf22f21c2f3c376.pdf

aicvecall<-c(AIC(fit_fit_lgm_4waves),AIC(fit_fit_lgm_4waves_cons))
aicvaluesweights<-akaike.weights(aicvecall)$weights

#Normalized
aicvaluesweights[2]/(aicvaluesweights[1]+aicvaluesweights[2])

#Relative evidence ratio
aicvaluesweights[2]/aicvaluesweights[1]+aicvaluesweights[2]


#How about fitting linear to quadratic


fit_lgm_5waves_linear<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4+1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4+4*t5

'
fit_fit_lgm_5waves_linear <- growth(fit_lgm_5waves_linear, data=sim_lgm_5waves_dat_quad)
summary(fit_fit_lgm_5waves_linear, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)


fit_lgm_5waves_toquad<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4+1*t5
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4+4*t5
sq =~ 0*t1 + 1*t2 + 4*t3 + 9*t4 +16*t5

t1~~ervar1*t1
t2~~ervar1*t2
t3~~ervar1*t3
t4~~ervar1*t4
t5~~ervar1*t5
'
fit_fit_lgm_5waves_toquad <- growth(fit_lgm_5waves_toquad, data=sim_lgm_5waves_dat_quad)
summary(fit_fit_lgm_5waves_toquad, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

anova(fit_fit_lgm_5waves_toquad,fit_fit_lgm_5waves_linear)



#####pandemie######

sim_lgm_4waves_pandemy<- '
i =~ 1*t1 + 1*t2 + 1*t3 # intercept
s =~ 0*t1 + 1*t2 + 2*t3 # development
s_pand =~ 0*t1 + 0*t2 + 1*t3 # development

i~100*1         ##<## this specifies the intercept, or the mean at T1
i~~15*i         ##<## this specifies the intercept variance at T1

s~10*1           ##<## this specifies the slope, or change per wave
s~~2*s         ##<## this specifies the slope variance 

s_pand~-5*1           ##<## this specifies the slope, or change per wave
s_pand~~1*s         ##<## this specifies the slope variance 





'
sim_lgm_4waves_pandemy <- simulateData(sim_lgm_4waves_pandemy, sample.nobs = 500)

colMeans(sim_lgm_4waves_pandemy)


#Plotting. Now you can start tweaking parameters to see what the data looks like
sim_lgm_4waves_pandemy$id<-as.factor(1:nrow(sim_lgm_4waves_pandemy))
theme_set(theme_grey(base_size = 18)) #increase text size
plotdat<-melt(sim_lgm_4waves_pandemy)
ggplot(plotdat,aes(variable,value,group=id))+geom_point(col='dodgerblue')+geom_line(col='dodgerblue')




fit_linear<- '
i =~ 1*t1 + 1*t2 + 1*t3 
s =~ 0*t1 + 1*t2 + 2*t3 

t1~~ervar*t1
t2~~ervar*t2
t3~~ervar*t3

'
fit_fit_linear <- growth(fit_linear, data=sim_lgm_4waves_pandemy,missing='fiml')

fit_pandemie<- '
i =~ 1*t1 + 1*t2 + 1*t3 
s =~ 0*t1 + 1*t2 + 2*t3 
s_pand =~ 0*t1 + 0*t2 + 1*t3

t1~~ervar*t1
t2~~ervar*t2
t3~~ervar*t3
s~~0*s_pand

'
fit_fit_pandemie <- growth(fit_pandemie, data=sim_lgm_4waves_pandemy,missing='fiml')

anova(fit_fit_linear,fit_fit_pandemie)



fit_basis<- '
i =~ 1*t1 + 1*t2 + 1*t3 
s =~ 0*t1 + t2 + 1*t3 
t1~~ervar*t1
t2~~ervar*t2
t3~~ervar*t3

'
fit_fit_basis <- growth(fit_basis, data=sim_lgm_4waves_pandemy,missing='fiml')

summary(fit_fit_basis, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

anova(fit_fit_basis,fit_fit_pandemie)





