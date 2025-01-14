# RMD to R 
# Dr. Nicholas Judd - 14-01-2025
# njudd.com
# title: "Workshop on Latent Growth Modeling in Lavaan"
# author: "Lifespan Cognitive Dynamics Lab"


if(!require("pacman")){install.packages("pacman",repos = "http://cran.us.r-project.org")}

pacman::p_load(lavaan, tidyverse, here, reshape2)

# just put the correct path
wisc <- read.csv("~/projects/LGC_Workshop/wisc.csv")[,-1]

head(wisc)         #first 6 rows
colnames(wisc)     #names of columns
dim(wisc)          #number of rows and columns
sapply(wisc,class) #number of each column


wisc_verbal <- wisc[,c("ID","Verbal_T6","Verbal_T7","Verbal_T9","Verbal_T11")]

#Reshape the data subset from wide to long format.**</font>

wisc_verbal_long <- wisc_verbal %>% 
  pivot_longer(!ID, names_to = "wave", values_to = "verbal") #test

# Now that we have prepared the data we want to model, let's plot them!

wisc_verbal_long$wave = factor(wisc_verbal_long$wave, levels=c("Verbal_T6","Verbal_T7","Verbal_T9","Verbal_T11"))

ggplot(wisc_verbal_long, aes(wave, verbal, group=ID, fill=ID, color=ID)) +
  geom_point() + 
  geom_line() +
  theme_classic(base_size = 15) + # adding a classic theme; https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.position = "none") + # getting rid of legend
  labs(x = "Wave", y = "Score on Verbal Subtest")

# Create LGM
linear_growth_model <- '
  i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s =~ 0*Verbal_T6 + 1*Verbal_T7 + 2*Verbal_T9 + 3*Verbal_T11'




# Fit LGM
fit_linear_growth_model <- growth(linear_growth_model, data=wisc_verbal,missing='fiml')
# Output results
summary(fit_linear_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



### Different Shapes of Growth

# Create quadratic growth model
quad_growth_model <- 'i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
                      s =~ 0*Verbal_T6 + 1*Verbal_T7 + 2*Verbal_T9 + 3*Verbal_T11
                      q =~ 0*Verbal_T6 + 1*Verbal_T7 + 4*Verbal_T9 + 9*Verbal_T11'
# Fit model
fit_quad_growth_model <- growth(quad_growth_model, data=wisc_verbal,missing='fiml')
# Output results
summary(fit_quad_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
#



# Create non-linear growth model
basis_growth_model <- 'i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
                       s =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11'
# Fit model
fit_basis_growth_model <- growth(basis_growth_model, data=wisc_verbal,missing='fiml')
# Output results
summary(fit_basis_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# Compare model fit
anova(fit_basis_growth_model,fit_linear_growth_model)
#Here it looks like the basis model fits better

### Predictors and outcomes
# Specify model
basis_growth_model_cov <- ' 
  i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  s~mo_edu
  i~mo_edu
  '
# Fit model
fit_basis_growth_model_cov <- growth(basis_growth_model_cov, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_cov, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# Specify model
basis_growth_model_covO <- ' 
  i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  Pspeed_T11~s
  Pspeed_T11~1
'
# Fit model
fit_basis_growth_model_covO <- growth(basis_growth_model_covO, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_covO, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



# Specify model
basis_growth_model_covP <- ' 
  i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  s~Pspeed_T6
  i~Pspeed_T6
  i~~s
'
# Fit model
fit_basis_growth_model_covP <- growth(basis_growth_model_covP, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_covP, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# Specify model
basis_growth_model_tvp <- ' 
  i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  Verbal_T6~Pspeed_T6
  Verbal_T7~Pspeed_T7
  Verbal_T9~Pspeed_T9
  Verbal_T11~Pspeed_T11
  '
# Fit LGM
fit_basis_growth_model_tvp <- growth(basis_growth_model_tvp, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_tvp, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

### Uneven time intervals

# Specify linear model
linear_growth_model_uneven <- ' i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
                                s =~ 6*Verbal_T6 + 7*Verbal_T7 + 9*Verbal_T9 + 11*Verbal_T11'

# Fit LGM
fit_linear_growth_model_uneven <- growth(linear_growth_model_uneven, data=wisc_verbal,missing='fiml')
# Output results
summary(fit_linear_growth_model_uneven, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

# Specify quadratic model
quad_growth_model_uneven <- ' i =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
                                s =~ 6*Verbal_T6 + 7*Verbal_T7 + 9*Verbal_T9 + 11*Verbal_T11
                                q =~ 36*Verbal_T6 + 49*Verbal_T7 + 81*Verbal_T9 + 121*Verbal_T11'

# Fit LGM
fit_quad_growth_model_uneven <- growth(quad_growth_model_uneven, data=wisc_verbal,missing='fiml')
# Output results
summary(fit_quad_growth_model_uneven, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#You should get the warning message "In lav_object_post_check(object) : lavaan WARNING: some estimated lv variances are negative" 

# Compare linear model fit between the model not taking into account the uneven intervals and the one taking into account the uneven intervals
anova(fit_linear_growth_model_uneven, fit_linear_growth_model)
#You cannot compare these models

# Compare model fit between linear trajectory with uneven intervals and the basis model
anova(fit_linear_growth_model_uneven,fit_basis_growth_model)
#Here it looks like the basis model is preferred. 

### Intercept and slopes relations

# Specify model
basis_growth_model_cor_ver_pro <- ' 
  i_verbal =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s_verbal =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  i_processpeed =~ 1*Pspeed_T6 + 1*Pspeed_T7 + 1*Pspeed_T9 + 1*Pspeed_T11
  s_processpeed =~ 0*Pspeed_T6 + Pspeed_T7 + Pspeed_T9 + 1*Pspeed_T11 
  s_verbal ~~ s_processpeed'

# Fit LGM
fit_basis_growth_model_cor_ver_pro <- growth(basis_growth_model_cor_ver_pro, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_cor_ver_pro, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

# Specify model
basis_growth_model_pred_ver_pro <- ' 
  i_verbal =~ 1*Verbal_T6 + 1*Verbal_T7 + 1*Verbal_T9 + 1*Verbal_T11
  s_verbal =~ 0*Verbal_T6 + Verbal_T7 + Verbal_T9 + 1*Verbal_T11 
  i_processpeed =~ 1*Pspeed_T6 + 1*Pspeed_T7 + 1*Pspeed_T9 + 1*Pspeed_T11
  s_processpeed =~ 0*Pspeed_T6 + Pspeed_T7 + Pspeed_T9 + 1*Pspeed_T11 
  s_verbal ~ i_processpeed
  s_processpeed ~ i_verbal'

# Fit LGM
fit_basis_growth_model_pred_ver_pro <- growth(basis_growth_model_pred_ver_pro, data=wisc,missing='fiml')
# Output results
summary(fit_basis_growth_model_pred_ver_pro, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

### LEAP data [full deleted]

### Simulate your data [extra]

# Specify linear model
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

ggplot(plotdat, aes(variable,value,group=id, fill=id, color=id)) +
  geom_point() + 
  geom_line() +
  theme_classic(base_size = 15) + # adding a classic theme; https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.position = "none") + # getting rid of legend
  labs(x = "Wave", y = "Score")


# Specify linear model
fit_lgm_4waves<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 
i~~s
'

# Fit LGM
fit_fit_lgm_4waves <- growth(fit_lgm_4waves, data=sim_lgm_4waves_dat,missing='fiml')
# Output results
summary(fit_fit_lgm_4waves, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

parTable(fit_fit_lgm_4waves)   #Parameters are recovered nicely


# You might want to simulate different means for the intercepts and slopes of different groups (e.g., female and male). You can simulate those groups at the same time by using a list: c().


# Specify linear model
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

#Sanity check of the column means. 
colMeans(sim_sim_lgm_4waves_MG)


# Specify linear model
fit_lgm_4waves_MG<- '
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 
'

# Fit LGM
fit_sim_sim_lgm_4waves_MG <- growth(fit_lgm_4waves_MG, data=sim_sim_lgm_4waves_MG,group='group')

# Output results
summary(fit_sim_sim_lgm_4waves_MG, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE,ci=T)

#Plotting. Now you can start tweaking parameters to see what the data looks like
sim_sim_lgm_4waves_MG$id<-as.factor(1:nrow(sim_sim_lgm_4waves_MG))
sim_sim_lgm_4waves_MG$group<-as.factor(sim_sim_lgm_4waves_MG$group)
theme_set(theme_grey(base_size = 18)) #increase text size
plotdat<-melt(sim_sim_lgm_4waves_MG,id=c("id","group"))

ggplot(plotdat, aes(variable,value,group=id)) +
  geom_point() + 
  geom_line() +
  theme_classic(base_size = 15) + # adding a classic theme; https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.position = "none") + # getting rid of legend
  labs(x = "Wave", y = "Score") +
  facet_wrap(~group)



# Simulating data can also be used for power analysis to test what effect size would be detected with your sample size and your model. 


#General specifications
nsims<-100                               #Define the number of iterations
samplesize<- 400                         #Define the sample size 
modelcomp<-data.frame(matrix(NA,nsims,7)) #create empty object to store logicals for whether the correct model is preferred
colnames(modelcomp)<-c('effect size','#LRT delta chi square','LRT delta df','LRT pv-value','delta AIC','AIC logical')


# Create a matrix for the effect size tested
effectsize <- c(0,0.05,0.1,0.15,0.2,0.25,0.3)

power_effectsize<-data.frame(matrix(NA,length(effectsize),4)) 
colnames(power_effectsize)<-c('effect_size','powerLRT','powerAIC','powerAICthres')

# Display the resulting data frame
print(effectsize)
a=0
for (j in 1:length(effectsize)) {
  effectsize[j]
  
for (i in 1:nsims) #initiate loop
{
  
  model_test<-NA   #change model output back to NA
  model_test_constraint<-NA   #change model output back to NA
  
  #Specify the true model
  #Simulate data for a bivariate Latent Change Score model
  model_sim_lgm_4waves<-
   paste('i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
          s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 

          i~100*1         ##<## this specifies the intercept, or the mean at T1
          i~~15*i         ##<## this specifies the intercept variance at T1

          s~5*1           ##<## this specifies the slope, or change per wave
          s~~10*s         ##<## this specifies the slope variance 

          i~~-3*s         ##<## this specifies the intercept/slope covariance 

          pred~10*1         ##<## this specifies the pred, or the mean at T1
          pred~~5*pred         ##<## this specifies the pred variance at T1

          s~ ',effectsize[j],'*pred
         
         ')

#Simulate data 
simdat_lgm_4waves<-simulateData(model_sim_lgm_4waves,sample.nobs = samplesize,meanstructure = T,empirical=TRUE)       

#check if the parameters make sense with the number you provided 
colMeans(simdat_lgm_4waves)
  
  model_test <- '
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
    i~~s
    s~ a*pred
'
  fit_model_test <- growth(model_test, data=simdat_lgm_4waves, estimator='mlr',missing='fiml')
  
  model_test_constraint <- '
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
    i~~s
    s~ 0*pred
'
  fit_model_test_constraint <- growth(model_test_constraint, data=simdat_lgm_4waves, estimator='mlr',missing='fiml')
    
  #Likelihood ratio test
  LRTcomp<-anova(fit_model_test,fit_model_test_constraint)
  modelaAIC<-AIC(fit_model_test) #grab AIC for the true model fit
  modelbAIC<-AIC(fit_model_test_constraint) #grab AIC for the competitor model fit
  
  modelcomp[i+a,1]<-effectsize[j]
  modelcomp[i+a,2]<-LRTcomp[2,5] #LRT chi square
  modelcomp[i+a,3]<-LRTcomp[2,6] #LRT delta df
  modelcomp[i+a,4]<-LRTcomp[2,7] #LRT p val
  modelcomp[i+a,5]<-modelbAIC-modelaAIC #delta AIC: positive means A preferred over B
  modelcomp[i+a,6]<-modelaAIC<modelbAIC #logical, does the alternative model have a
}
  power_effectsize[j,1]<- effectsize[j]
  powerLRT<-(sum(modelcomp[c((a+1):(a+100)),4]<0.05)/nsims)*100 #sum logical: divide by nsims to get percentage power
  power_effectsize[j,2]<- powerLRT
  powerAIC<-(sum(as.numeric(modelcomp[c((a+1):(a+100)),6]))/nsims)*100
  power_effectsize[j,3]<- powerAIC
  AICthres<-10
  powerAICthres<-sum(modelcomp[c((a+1):(a+100)),5]>10)/nsims*100
  power_effectsize[j,4]<- powerAICthres
  a=a+100
}

ggplot(power_effectsize,aes(effect_size,powerLRT))+
  geom_point()




