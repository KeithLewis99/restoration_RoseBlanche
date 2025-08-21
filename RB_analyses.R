# Summary:
# This file is a comparison of density and biomass of BT and AS in Rose Blanche.  The data are from 2000-2002 and 2015.  So far, there has been no success in locating the "Before data" from 1999.  There is no sign of it on the archive or on anyone's computer at DFO and Jim McCarthy of Wood (formerly AMEC) has not responded with these data.  

# The data are analyzed using a zero-inflated gamma model with random effects for Year.  The residuals are checked for normality, homogeneity of variance, temporal autocorrelation, and spatial autocorrelation.  The residuals are then used to create a spatial dataset that is used to check for spatial autocorrelation.  The results are then summarized and compared to the results of Scruton et al. 2005.  The results are then plotted - see RB_figs.R.

## abun.stand and bio.stand area #/area*100
# source ----
# source("Pam_abun_bio.R")
source("RB_data_new.R")
source("RB_fun.R")


# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
library(glmmTMB)
library(DHARMa)
library(ggplot2)
# library(cowplot)

# check for zeros
df_a |>
  group_by(Year, Species) |>
  summarise(a = sum(abun != 0), 
            a0 = sum(abun == 0))


# Density ----
## BT ----
### data ----
str(df_a, give.attr=FALSE)
df_aBT <- df_a[df_a$Species == "BT",]
plot(density(df_aBT$abun.stand, na.rm = T))
summary(df_aBT$abun.stand)
with(df_aBT, table(abun, Year))
with(df_aBT, table(abun, Station, Year))
ggplot(df_aBT, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### I ran both models with a ziformula = ~ 1. bt.glmm2 was the better model but residuals were worse.  I noticed some zeros and experimented with modifying the ziformula and found that ~ time was best.  This also seemed to help the resids but nothing is significant which seems to match the BACI plots.  The main question is whether there are hidden zeros, i.e., Stations with no zeros because these don't seem to get entered in the datasets.
bt.glmm1 <- glmmTMB(
  abun.stand ~ type + (1 | Year), #+ (1 | Station),
  #abun.stand ~ type*time + as.numeric(Year) + (1 | Year),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ time,
  REML = TRUE,
  data = df_aBT
)

summary(bt.glmm1)
# str(bt.glmm1)


# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt.glmm2 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), 
  #+ (1 | Station),
  #  abun.stand ~ type*time + as.numeric(Year) + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ time, # see notes below
  REML = TRUE,
  data = df_aBT
)
summary(bt.glmm2)

# str(bt.glmm2)
anova(bt.glmm1, bt.glmm2) # this suggests that model bt.glmm2 without dispersion is better than with (bt.glmm1)


### diagnostics ----
bt.glmm2_simres <- simulateResiduals(bt.glmm2, plot = T)
# str(bt.glmm1_simres,1)
residuals(bt.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - these look great
testUniformity(bt.glmm2_simres)
testQuantiles(bt.glmm2_simres)

# time did not work well with ziformula = ~1 but OK with ziformula = ~time
plotResiduals(bt.glmm2_simres, form = df_aBT$time)
plotResiduals(bt.glmm2_simres, form = df_aBT$type)
plotResiduals(bt.glmm2_simres, form = df_aBT$int)
plotResiduals(bt.glmm2_simres, form = df_aBT$Year) # not part of the model and Dave wasn't concerned about this, especially since not much that can be done

# redundant with above
# testCategorical(bt.glmm2_simres, catPred = df_aBT$time)
# testCategorical(bt.glmm2_simres, catPred = df_aBT$type)


# dispersion/zeroinflation - red line should be in the middle of the histogram - looks great
testDispersion(bt.glmm2_simres)
testZeroInflation(bt.glmm2_simres)


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt.glmm2_simres_recalc <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Year)

testTemporalAutocorrelation(bt.glmm2_simres_recalc, time = unique(df_aBT$Year))

# autocorrelations is fine but definitely a trend with Year resids.  Dave thinks that this isn't a problem - its not part of the model and not much to do about it. Conclude no temproal issues


### spatial independence - so few data here that its a bit of a meaningless exercise
# recalculate resids with stations as the grouping variable
bt.glmm2_simres_recalcSpace <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Station)
unique(df_aBT$Station) 
#str(bt.glmm2_simres_recalcSpace)
length(bt.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt.glmm2_simres_recalcSpace)   


bt.biomass.all <- spatialData_join(df_sumBT, bt.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(bt.biomass.all)


# Diagnostics had a few issues but largely fixed or not a big deal

### summary ----
summary(bt.glmm2)
mean_by_site(df_sumBT, "d")
baci.plot(df_baciBT, "d")
ggsave(paste0("output/BT_density.png"), width=10, height=8, units="in")

confint(bt.glmm2)
tab.ci(bt.glmm2, "bt_den") 





## BTYOY ----
### data ----
str(df_a, give.attr=FALSE)
df_aBTYOY <- df_a[df_a$Species == "BTYOY",]
df_aBTYOY$int <- interaction(df_aBTYOY$type, df_aBTYOY$time)
plot(density(df_aBTYOY$abun.stand, na.rm = T))
summary(df_aBTYOY$abun.stand)
with(df_aBTYOY, table(abun, Year))
with(df_aBTYOY, table(abun, Station, Year))
ggplot(df_aBTYOY, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### 

btyoy.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBTYOY
)

summary(btyoy.glmm1)



# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
btyoy.glmm2 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), 
  family=ziGamma(link="log"), 
  ziformula = ~ 1, 
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy.glmm2)

# str(bt.glmm2)
anova(btyoy.glmm1, btyoy.glmm2) # this suggests that model btyoy.glmm2 without dispersion is slightly better than with (btyoy.glmm1)


### diagnostics ----
btyoy.glmm2_simres <- simulateResiduals(btyoy.glmm2, plot = T)
# str(bt.glmm1_simres,1)
residuals(btyoy.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - these look great
testUniformity(btyoy.glmm2_simres)
testQuantiles(btyoy.glmm2_simres)

# these are also great except for Year but not part of model
plotResiduals(btyoy.glmm2_simres, form = df_aBTYOY$time)
plotResiduals(btyoy.glmm2_simres, form = df_aBTYOY$type)
plotResiduals(btyoy.glmm2_simres, form = df_aBTYOY$int)
plotResiduals(btyoy.glmm2_simres, form = df_aBTYOY$Year) # not part of the model and Dave wasn't concerned about this, especially since not much that can be done


# dispersion/zeroinflation - red line should be in the middle of the histogram - looks great
testDispersion(btyoy.glmm2_simres)
testZeroInflation(btyoy.glmm2_simres)


### temporal independence
#### this seems fine
btyoy.glmm2_simres_recalc <- recalculateResiduals(btyoy.glmm2_simres, group = df_aBTYOY$Year)

testTemporalAutocorrelation(btyoy.glmm2_simres_recalc, time = unique(df_aBTYOY$Year))

# autocorrelations is fine but definitely a trend with Year resids.  Dave thinks that this isn't a problem - its not part of the model and not much to do about it. Conclude no temproal issues


### spatial independence - so few data here that its a bit of a meaningless exercise
# recalculate resids with stations as the grouping variable
btyoy.glmm2_simres_recalcSpace <- recalculateResiduals(btyoy.glmm2_simres, group = df_aBTYOY$Station)

testSpatialAutocorrelation(btyoy.glmm2_simres_recalcSpace, x = unique(df_aBTYOY$west), y = unique(df_aBTYOY$north))

spatialAutoCorrBase_fun(df_aBTYOY, btyoy.glmm2_simres_recalcSpace)   


btyoy.biomass.all <- spatialData_join(df_sumBT, btyoy.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(btyoy.biomass.all)


# Diagnostics are fine - proceed

### summary ----
summary(btyoy.glmm2)
mean_by_site(df_sumBTYOY, "d")
baci.plot(df_baciBTYOY, "d")
ggsave(paste0("output/BTYOY_density.png"), width=10, height=8, units="in")

confint(btyoy.glmm2)
tab.ci(btyoy.glmm2, "btyoy_den") 




## AS ----
### data ----
df_aAS <- df_a[df_a$Species == "AS",]
df_aAS$int <- interaction(df_aAS$type, df_aAS$time)
with(df_aAS, table(abun, Year))
plot(density(df_aAS$abun.stand, na.rm = T))
ggplot(df_aAS, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)



### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### I ran both models with a ziformula = ~ 1. as.glmm1 was the better model but the model had some issues with homogeneity of type and autocorrelation.  I experimented with modifying the dispformula and ziformula and found that the below is best for diagnostics
as.glmm1 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ type + time,
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aAS
)

summary(as.glmm1)
# str(bt.glmm1)


# Fifield advised the following
## Compre the results of:
### glmmTMB to glm.  The estimates are virtually identical but the Std. Errors are much smaller for glmmTMB.
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as.glmm2 <- glmmTMB(
  abun.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ type + time,
  REML = TRUE,
  data = df_aAS
)
summary(as.glmm2)


anova(as.glmm1, as.glmm2)
# str(as.glmm2)
# as.glmm1 is the better model with ziformula = ~ 1 and when type + time - significant difference


### diagnostics ----
as.glmm1_simres <- simulateResiduals(as.glmm1, plot = T)
# str(as.glmm1_simres,1)
residuals(as.glmm1_simres) 
# these are scaled residuals
# The normality/overdispersion are great but the  homogeneity of variance is bad.  

# plots by themselves or by group
testUniformity(as.glmm1_simres)
testQuantiles(as.glmm1_simres)

plotResiduals(as.glmm1_simres, form = df_aAS$time)
plotResiduals(as.glmm1_simres, form = df_aAS$type) # this fails first model run but not bad with the second model
plotResiduals(as.glmm1_simres, form = df_aAS$int)
plotResiduals(as.glmm1_simres, form = df_aAS$Year)
# these suggest that there may be an interaction


# dispersion/zeroinflation
testDispersion(as.glmm1_simres)
testZeroInflation(as.glmm1_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as.glmm1_simres_recalc <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as.glmm1_simres_recalc, time = unique(df_aAS$Year))

# resids were not good with the initial model;  


### spatial independence
# recalculate resids with stations as the grouping variable
as.glmm1_simres_recalcSpace <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as.glmm1_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as.glmm1_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as.glmm1_simres_recalcSpace)   


as.biomass.all <- spatialData_join(df_sumAS, as.glmm1_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(as.biomass.all)


# Diagnostics look fantastic for this model.  Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
test <- summary(as.glmm1)
test$vcov$cond
mean_by_site(df_sumAS, "d")
baci.plot(df_baciAS, "d")
ggsave(paste0("output/AS_density.png"), width=10, height=8, units="in")

confint(as.glmm1)[1:4, ]
#confint(as.glmm2)[1:4, ]
confint(as.glmm1)
tab.ci(as.glmm1, "as_den") 

tmp <- confint(as.glmm1)
tmp[1:5, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100


## ASYOY ----
### data ----
str(df_a, give.attr=FALSE)
df_aASYOY <- df_a[df_a$Species == "ASYOY",]
df_aASYOY <- df_a[df_a$Species == "ASYOY" & df_a$Year != "1992",]
df_aASYOY$int <- interaction(df_aASYOY$type, df_aASYOY$time)
plot(density(df_aASYOY$abun.stand, na.rm = T))



### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### ### Get the following: Error in fitTMB(TMBStruc) : negative log-likelihood is NaN at starting parameter values.  I Googled this and there is a problem with the Matrix package which I uninstalled and not chang.e  
### I also tried to manipulate the ziformula and used the control code when things don't converge - nothing worked.
### I then tried to simplify the model which worked.  Tweedie "works" but I get a model with no error estimates.  There are an awful lot of zeros in this AND, there is the 1992 stocking.  
###  Tried removing 1992 as this was the stocking year
### Tried many different starting values to no effect - start=list(beta=c(1, 1, 1, 1)),
### The only thing that worked was simplifying the model for asyoy.glmm2, therefore, I think that the only thing to do is to switch to a binomial approach.
### I tried this and it didn't work.  The reason is that there is no values for before:below:abun>1 and therefore, its inappropriate to use an interaction term in the model.  This explains why it works for the other species-age groups. See scratch_pad_ASYOY.R for details and proofs
#### So, we'll just have to go with the main effects - all we can do

asyoy.glmm1 <- glmmTMB(
  abun.stand ~ type + time + (1 | Year),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ time,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = df_aASYOY
)

summary(asyoy.glmm1)



# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
asyoy.glmm2 <- glmmTMB(
  abun.stand ~ type + time + (1 | Year), 
  family=ziGamma(link="log"), 
  ziformula = ~ time, 
  REML = TRUE,
  data = df_aASYOY
)
summary(asyoy.glmm2)


# asyoy.glmm1 does not converge so use asyoy.glmm2.  But diagnostics aren't good for homogeneity.  Re-ran with dispersion formula for time but diagnostics still bad.  Then, modified the ziformula since lots of zeros and that worked
anova(asyoy.glmm1, asyoy.glmm2)

### diagnostics ----
asyoy.glmm2_simres <- simulateResiduals(asyoy.glmm2, plot = T)
# str(as.glmm1_simres,1)
residuals(asyoy.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - these look great
testUniformity(asyoy.glmm2_simres)
testQuantiles(asyoy.glmm2_simres)

# these are also great except for Year but not part of model
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$time)
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$type)
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$int)
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$Year) # not part of the model and Dave wasn't concerned about this, especially since not much that can be done


# dispersion/zeroinflation - red line should be in the middle of the histogram - looks great
testDispersion(asyoy.glmm2_simres)
testZeroInflation(asyoy.glmm2_simres)


### temporal independence
#### this seems fine
asyoy.glmm2_simres_recalc <- recalculateResiduals(asyoy.glmm2_simres, group = df_aASYOY$Year)

testTemporalAutocorrelation(asyoy.glmm2_simres_recalc, time = unique(df_aASYOY$Year))

# autocorrelations is fine but definitely a trend with Year resids.  Dave thinks that this isn't a problem - its not part of the model and not much to do about it. Conclude no temproal issues


### spatial independence - so few data here that its a bit of a meaningless exercise
# recalculate resids with stations as the grouping variable
asyoy.glmm2_simres_recalcSpace <- recalculateResiduals(asyoy.glmm2_simres, group = df_aASYOY$Station)

testSpatialAutocorrelation(asyoy.glmm2_simres_recalcSpace, x = unique(df_aASYOY$west), y = unique(df_aASYOY$north))

spatialAutoCorrBase_fun(df_aASYOY, asyoy.glmm2_simres_recalcSpace)   


asyoy.biomass.all <- spatialData_join(df_sumAS, asyoy.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(asyoy.biomass.all)


# Diagnostics are fine - proceed

### summary ----
summary(asyoy.glmm2)
mean_by_site(df_sumASYOY, "d")
baci.plot(df_baciASYOY, "d")
ggsave(paste0("output/ASYOY_density.png"), width=10, height=8, units="in")

confint(asyoy.glmm2)
tab.ci(asyoy.glmm2, "asyoy_den") 



# Biomass ----
## BT ----
### data ----


plot(density(df_aBT$bio.stand, na.rm = T))
summary(df_aBT$bio.stand)
with(df_aBT, table(bio, Year))
with(df_aBT, table(bio, Station, Year))
ggplot(df_aBT, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros

bt_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBT
)

summary(bt_bio.glmm1)


bt_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ time,
  REML = TRUE,
  data = df_aBT
)
summary(bt_bio.glmm2)

anova(bt_bio.glmm1, bt_bio.glmm2)

# bt_bio.glmm2 is the better model but see how diagnostics look 


### diagnostics ----
bt_bio.glmm2_simres <- simulateResiduals(bt_bio.glmm2, plot = T)
# str(as_bio.glmm1_simres,1)
residuals(bt_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion s great and  homogeneity of variance is good.  

# plots by themselves or by group
testUniformity(bt_bio.glmm2_simres)
testQuantiles(bt_bio.glmm2_simres)

plotResiduals(bt_bio.glmm2_simres, form = df_aBT$time)
plotResiduals(bt_bio.glmm2_simres, form = df_aBT$type)
plotResiduals(bt_bio.glmm2_simres, form = df_aBT$int)
# these all look great


# dispersion/zeroinflation
testDispersion(bt_bio.glmm2_simres)
testZeroInflation(bt_bio.glmm2_simres) # these look great


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt_bio.glmm2_simres_recalc <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(bt_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# not autocorrelatoin but resids v. time has trend.  See above and conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
bt_bio.glmm2_simres_recalcSpace <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aBT$Station) 
#str(as.glmm2_simres_recalcSpace)
length(bt_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt_bio.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt_bio.glmm2_simres_recalcSpace)   


bt_bio.biomass.all <- spatialData_join(df_sumBT, bt_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(bt_bio.biomass.all)


# Diagnostics look fine for this model.  Proceed with this model (bt_bio.glmm2).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(bt_bio.glmm2)
mean_by_site(df_bio_sumBT, "b")
baci.plot(df_bio_baciBT, "b")
ggsave(paste0("output/BT_biomass.png"), width=10, height=8, units="in")


confint(bt_bio.glmm2)
confint(bt_bio.glmm2)[1:4, ]
tab.ci(bt_bio.glmm2, "bt_bio") 



## BTYOY ----
### data ----


plot(density(df_aBTYOY$bio.stand, na.rm = T))
summary(df_aBTYOY$bio.stand)
with(df_aBTYOY, table(bio, Year))
with(df_aBTYOY, table(bio, Station, Year))
ggplot(df_aBTYOY, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros

btyoy_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBTYOY
)

summary(btyoy_bio.glmm1)


btyoy_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy_bio.glmm2)

anova(btyoy_bio.glmm1, btyoy_bio.glmm2)

# btyoy_bio.glmm2 is a marginally better model but see how diagnostics look 


### diagnostics ----
btyoy_bio.glmm2_simres <- simulateResiduals(btyoy_bio.glmm2, plot = T)
# str(as_bio.glmm1_simres,1)
residuals(btyoy_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion s great and  homogeneity of variance is good.  

# plots by themselves or by group
testUniformity(btyoy_bio.glmm2_simres)
testQuantiles(btyoy_bio.glmm2_simres)

plotResiduals(btyoy_bio.glmm2_simres, form = df_aBTYOY$time)
plotResiduals(btyoy_bio.glmm2_simres, form = df_aBTYOY$type)
plotResiduals(btyoy_bio.glmm2_simres, form = df_aBTYOY$int)
# these all look great


# dispersion/zeroinflation
testDispersion(btyoy_bio.glmm2_simres)
testZeroInflation(btyoy_bio.glmm2_simres) # these look great


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoy_bio.glmm2_simres_recalc <- recalculateResiduals(btyoy_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(btyoy_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# no autocorrelatoin but resids v. time has trend.  See above and conclude no temproal issues


### spatial independence
# recalculate resids with stations as the grouping variable
btyoy_bio.glmm2_simres_recalcSpace <- recalculateResiduals(btyoy_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aBTYOY$Station) 
#str(as.glmm2_simres_recalcSpace)
length(btyoy_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(btyoy_bio.glmm2_simres_recalcSpace, x = unique(df_aBTYOY$west), y = unique(df_aBTYOY$north))

spatialAutoCorrBase_fun(df_aBTYOY, btyoy_bio.glmm2_simres_recalcSpace)   


btyoy_bio.biomass.all <- spatialData_join(df_sumBTYOY, btyoy_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(btyoy_bio.biomass.all)


# Diagnostics look fine for this model.  Proceed with this model (btyoy_bio.glmm2).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(btyoy_bio.glmm2)
mean_by_site(df_sumBTYOY, "b")
baci.plot(df_bio_baciBTYOY, "b")
ggsave(paste0("output/BTYOY_biomass.png"), width=10, height=8, units="in")

confint(btyoy_bio.glmm2)[1:4, ]
#confint(btyoy_bio.glmm1)[1:4, ]
tab.ci(btyoy_bio.glmm2, "btyoy_bio") 




## AS ----
### data ----

plot(density(df_aAS$bio.stand, na.rm = T))
summary(df_aAS$bio.stand)
with(df_aAS, table(bio, Year))
with(df_aAS, table(bio, Station, Year))
ggplot(df_aAS, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)



### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
### all looks good with first model except temp autocorr

as_bio.glmm1 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  #  bio.stand ~ type*time + Year +  (1 | Year), #+ (1 | Station),
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ time, #~1,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = df_aAS
)

summary(as_bio.glmm1)


as_bio.glmm2 <- glmmTMB(
  bio.stand ~ type*time + (1 | Year), #+ (1 | Station),
  #  bio.stand ~ type*time + Year + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ time, #~1,
  REML = TRUE,
  data = df_aAS
)
summary(as_bio.glmm2)

anova(as_bio.glmm1, as_bio.glmm2)
# str(as_bio.glmm2)
# as_bio.glmm2 is a slightly better model, for both original and ziformula ~ time


### diagnostics ----
as_bio.glmm2_simres <- simulateResiduals(as_bio.glmm2, plot = T)


# str(as_bio.glmm1_simres,1)
residuals(as_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion isn't great and definite pattern in the homogeneity of variance but not awful

# plots by themselves or by group
testUniformity(as_bio.glmm2_simres)
testQuantiles(as_bio.glmm2_simres)

plotResiduals(as_bio.glmm2_simres, form = df_aAS$time) # not great
plotResiduals(as_bio.glmm2_simres, form = df_aAS$type)
plotResiduals(as_bio.glmm2_simres, form = df_aAS$int)


# dispersion/zeroinflation
testDispersion(as_bio.glmm2_simres)
testZeroInflation(as_bio.glmm2_simres) # looks fine


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as_bio.glmm2_simres_recalc <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# Temporal resids are awful with first model but OK with the second


### spatial independence
# recalculate resids with stations as the grouping variable
as_bio.glmm2_simres_recalcSpace <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as_bio.glmm2_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as_bio.glmm2_simres_recalcSpace)   


as_bio.biomass.all <- spatialData_join(df_sumAS, as_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(as_bio.biomass.all)


# Diagnostics were fine for first iteration except for temp autocorrelation.  Tweaked the ziformula and now it all looks fantastic Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(as_bio.glmm2)
mean_by_site(df_bio_sumAS, "b")
baci.plot(df_bio_baciAS, "b")
ggsave(paste0("output/AS_biomass.png"), width=10, height=8, units="in")


confint(as_bio.glmm2)
confint(as_bio.glmm2)[1:4, c(3, 1, 2)]
tab.ci(as_bio.glmm2, "as_bio") 

tmp <- confint(as_bio.glmm2)
tmp[1:5, c(3, 1:2)]
# percent increase - main effect
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

# interaction - this helped me to arrive at the conclusion below.  
model.matrix.lm(as_bio.glmm2)
# 1 - this is the difference of interaction (before:below) from the intecept (after:above) - meaningless in this context
((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100 

# 2 - this is the difference between the after:below and before:below
((exp(tmp[1,3] + tmp[2,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]))/exp(tmp[1,3]+ tmp[2,3])*100 

# 3 - or is this - I think that this is right because its the full interaction (before:below) compared to the after:below:after and its consistent with the design matrix while the above is not

((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]))/exp(tmp[1,3]+ tmp[2,3])*100 


# 4 - this is difference between the interaction and the main effects
((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]+ tmp[3,3]))/exp(tmp[1,3]+ tmp[2,3] + tmp[3,3])*100

# however, 2 and 4 give the same result which is scary


## ASYOY ----
### data ----

plot(density(df_aASYOY$bio.stand, na.rm = T))
summary(df_aASYOY$bio.stand)
with(df_aASYOY, table(bio, Year))
with(df_aASYOY, table(bio, Station, Year))
ggplot(df_aASYOY, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)



### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### see issues with ASYOY Density

asyoy_bio.glmm1 <- glmmTMB(
  bio.stand ~ type + time + (1 | Year), 
  dispformula = ~ int,
  family=ziGamma(link="log"), 
  ziformula = ~ 1, #~time,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = df_aASYOY
)

summary(asyoy_bio.glmm1)


asyoy_bio.glmm2 <- glmmTMB(
  bio.stand ~ type + time + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ time, #~1,
  REML = TRUE,
  data = df_aASYOY
)
summary(asyoy_bio.glmm2)


# asyoy_bio.glmm1 does not converage - so use asyoy_bio.glmm2.  As with density, the diagnostics were pretty bad so changed ziformula to ~time

### diagnostics ----
asyoy_bio.glmm2_simres <- simulateResiduals(asyoy_bio.glmm2, plot = T)


# str(asyoy_bio.glmm1_simres,1)
residuals(asyoy_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion fine; definite trend in the homogeneity of variance but not awful

# plots by themselves or by group
testUniformity(asyoy_bio.glmm2_simres)
testQuantiles(asyoy_bio.glmm2_simres)

# none of these are great but not awful
plotResiduals(asyoy_bio.glmm2_simres, form = df_aASYOY$time)
plotResiduals(asyoy_bio.glmm2_simres, form = df_aASYOY$type)
plotResiduals(asyoy_bio.glmm2_simres, form = df_aASYOY$int)


# dispersion/zeroinflation
testDispersion(asyoy_bio.glmm2_simres)
testZeroInflation(asyoy_bio.glmm2_simres) # looks fine


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoy_bio.glmm2_simres_recalc <- recalculateResiduals(asyoy_bio.glmm2_simres, group = df_aASYOY$Year)

testTemporalAutocorrelation(asyoy_bio.glmm2_simres_recalc, time = unique(df_aASYOY$Year))

# Temporal resids look fine except for trend in years


### spatial independence
# recalculate resids with stations as the grouping variable
asyoy_bio.glmm2_simres_recalcSpace <- recalculateResiduals(asyoy_bio.glmm2_simres, group = df_aASYOY$Station)
unique(df_aASYOY$Station) 
#str(asyoy.glmm2_simres_recalcSpace)
length(asyoy_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(asyoy_bio.glmm2_simres_recalcSpace, x = unique(df_aASYOY$west), y = unique(df_aASYOY$north))

spatialAutoCorrBase_fun(df_aASYOY, asyoy_bio.glmm2_simres_recalcSpace)   


asyoy_bio.biomass.all <- spatialData_join(df_sumASYOY, asyoy_bio.glmm2_simres_recalcSpace, df_loc[-c(2:3, 5:6, 9), ])


spatialAutoCorrGG_fun(asyoy_bio.biomass.all)


# Diagnostics were fine for first iteration except for temp autocorrelation.  Tweaked the ziformula and now it all looks fantastic Proceed with this model (as.glmm1).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(asyoy_bio.glmm2)
mean_by_site(df_bio_sumASYOY, "b")
baci.plot(df_bio_baciASYOY, "b")
ggsave(paste0("output/ASYOY_biomass.png"), width=10, height=8, units="in")


confint(asyoy_bio.glmm2)
confint(asyoy_bio.glmm2)[1:4, c(3, 1, 2)]
tab.ci(asyoy_bio.glmm2, "asyoy_bio") 


# END ----