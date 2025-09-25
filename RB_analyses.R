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
library(emmeans)
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
ggplot(df_aBT, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_grid(type~Year)


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### I ran both models with a ziformula = ~ 1. bt.glmm2 was the better model but residuals were worse.  I noticed some zeros and experimented with modifying the ziformula and found that ~ time was best.  This also seemed to help the resids but nothing is significant which seems to match the BACI plots.  The main question is whether there are hidden zeros, i.e., Stations with no zeros because these don't seem to get entered in the datasets.

df_aBT$Year1 <- df_aBT$Year-min(df_aBT$Year)
df_aBT$Year2 <- scale(df_aBT$Year)

bt.glmm1 <- glmmTMB(
  # abun.stand ~ type + (1 | Year), # Year resid bad, spatial unacceptable 
  # abun.stand ~ type + north + (1 | Year),# lowest AIC, variance issue, year trend
  # abun.stand ~ type + Year1 + (1 | Year), # all good but spatial bad
  abun.stand ~ type + Year1 + north + (1 | Year), # variance issue, Year trend
  # abun.stand ~ type + I(Year1^2) + north + (1 | Year), # all good but spatial bad
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ type, # started with type but Year may be more appropriate
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = df_aBT
)
summary(bt.glmm1)
# str(bt.glmm1)


# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
bt.glmm2 <- glmmTMB(
  # abun.stand ~ type + (1 | Year), 
  # abun.stand ~ type + north + (1 | Year),
  # abun.stand ~ type + Year1 + (1 | Year),
   abun.stand ~ type + Year1 + north + (1 | Year),
  # abun.stand ~ type + I(Year1^2) + north + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ Year1, # see notes below
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aBT
)
summary(bt.glmm2)

# str(bt.glmm2)
anova(bt.glmm1, bt.glmm2) # this suggests that model bt.glmm2 without dispersion is better than with (bt.glmm1)

# Get the model with the lowest AIC
tmp <- anova(bt.glmm1, bt.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
bt.glmm2_simres <- simulateResiduals(best_model, plot = T)
# str(bt.glmm1_simres,1)
residuals(bt.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - some issues with variance
testUniformity(bt.glmm2_simres)
testQuantiles(bt.glmm2_simres)

# time did not work well with ziformula = ~1 but OK with ziformula = ~time
plotResiduals(bt.glmm2_simres, form = df_aBT$type)
plotResiduals(bt.glmm2_simres, form = df_aBT$Year) # not part of the model and Dave wasn't concerned about this, especially since not much that can be done


# dispersion/zeroinflation - red line should be in the middle of the histogram - looks great
testDispersion(bt.glmm2_simres)
testZeroInflation(bt.glmm2_simres)


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt.glmm2_simres_recalc <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Year)

testTemporalAutocorrelation(bt.glmm2_simres_recalc, time = unique(df_aBT$Year))

# autocorrelations is fine; a trend with Year resids but resolved it.  


### spatial independence - so few data here that its a bit of a meaningless exercise
# recalculate resids with stations as the grouping variable
bt.glmm2_simres_recalcSpace <- recalculateResiduals(bt.glmm2_simres, group = df_aBT$Station)
unique(df_aBT$Station) 
#str(bt.glmm2_simres_recalcSpace)
length(bt.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt.glmm2_simres_recalcSpace)   


bt.biomass.all <- spatialData_join(df_sumBT, bt.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(bt.biomass.all)

# Spatial is great but had to extend the model from just ~type. Diagnostics had a few issues but minor and p is far from alpha.

### summary ----
summary(best_model)
mean_by_site(df_sumBT, "d")
ggsave(paste0("output/BT_density.png"), width=10, height=8, units="in")

confint(best_model)
tab.ci(best_model, "bt_den") 

## emmeans
emmeans(best_model, ~ type, component = "cond")
emm.bt.den <- as.data.frame(emmeans(best_model, ~ type,       component = "response"))
((emm.bt.den[2,2]-emm.bt.den[1,2])/
    emm.bt.den[1,2])*100

#### blup ---- 
btd_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(btd_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/BT_density_blups.png"), width=8, height=6, units="in")

#### zi ----
zi <- summary(best_model)$coefficients$zi
plogis(zi[1,1]) # 58% chance of zeros on Control
plogis(zi[1,1] + zi[2,1]) # 11% chance of zeros on  Treatment

  
## BTYOY ----
### data ----
df_aBTYOY <- df_a[df_a$Species == "BTYOY",]
plot(density(df_aBTYOY$abun.stand, na.rm = T))
summary(df_aBTYOY$abun.stand)
with(df_aBTYOY, table(abun, Year))
with(df_aBTYOY, table(abun, Station, Year))
ggplot(df_aBTYOY, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)


### analyses ----
df_aBTYOY$Year1 <- df_aBTYOY$Year-min(df_aBTYOY$Year)
df_aBTYOY$Year2 <- scale(df_aBTYOY$Year)

btyoy.glmm1 <- glmmTMB(
  abun.stand ~ type + (1 | Year),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ Year1,
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy.glmm1)


# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
btyoy.glmm2 <- glmmTMB(
  abun.stand ~ type + (1 | Year), 
  family=ziGamma(link="log"), 
  ziformula = ~ Year1, 
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy.glmm2)

# str(bt.glmm2)
anova(btyoy.glmm1, btyoy.glmm2) # this suggests that model btyoy.glmm1 with dispersion is better 

# Get the model with the lowest AIC
tmp <- anova(btyoy.glmm1, btyoy.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
btyoy.glmm2_simres <- simulateResiduals(best_model, plot = T)
# str(bt.glmm1_simres,1)
residuals(btyoy.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - these look great
testUniformity(btyoy.glmm2_simres)
testQuantiles(btyoy.glmm2_simres)

# these are also great except for Year but not part of model
plotResiduals(btyoy.glmm2_simres, form = df_aBTYOY$type)
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


btyoy.biomass.all <- spatialData_join(df_sumBT, btyoy.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(btyoy.biomass.all)


# Diagnostics are fine.  Small issue with trend in years - proceed

### summary ----
summary(best_model)
mean_by_site(df_sumBTYOY, "d")
ggsave(paste0("output/BTYOY_density.png"), width=10, height=8, units="in")

confint(best_model)
tab.ci(best_model, "btyoy_den") 

## emmeans
emmeans(best_model, ~ type, component = "cond")
emm.bty.den <- as.data.frame(emmeans(best_model, ~ type,       component = "response"))
((emm.bty.den[2,2]-emm.bty.den[1,2])/
    emm.bty.den[1,2])*100

#### blup ---- 
btyd_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(btyd_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/BTY_density_blups.png"), width=8, height=6, units="in")

#### zi ----
# non-sig Year effect


## AS ----
### data ----
df_aAS <- df_a[df_a$Species == "AS",]
with(df_aAS, table(abun, Year))
plot(density(df_aAS$abun.stand, na.rm = T))
ggplot(df_aAS, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)



### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
#### I ran both models with a ziformula = ~ 1. as.glmm1 was the better model but the model had some issues with homogeneity of type and autocorrelation.  I experimented with modifying the dispformula and ziformula and found that the below is best for diagnostics
as.glmm1 <- glmmTMB(
  abun.stand ~ type + (1 | Year), # Year trend
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aAS
)
summary(as.glmm1)
# str(bt.glmm1)


### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
as.glmm2 <- glmmTMB(
  abun.stand ~ type + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aAS
)
summary(as.glmm2)

anova(as.glmm1, as.glmm2)
# str(as.glmm2)
# as.glmm2 is the better model 

# Get the model with the lowest AIC
tmp <- anova(as.glmm1, as.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
as.glmm1_simres <- simulateResiduals(best_model, plot = T)
# str(as.glmm1_simres,1)
residuals(as.glmm1_simres) 
# these are scaled residuals
# The normality/overdispersion and homogeneity of varinace are great.  

# plots by themselves or by group
testUniformity(as.glmm1_simres)
testQuantiles(as.glmm1_simres)


plotResiduals(as.glmm1_simres, form = df_aAS$type) # fine 
plotResiduals(as.glmm1_simres, form = df_aAS$Year) # all lines trend upwards



# dispersion/zeroinflation
testDispersion(as.glmm1_simres)
testZeroInflation(as.glmm1_simres) # this gives a bogus result but clearly, from the inflation model, there is no zero-inflation.


### temporal independence
as.glmm1_simres_recalc <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as.glmm1_simres_recalc, time = unique(df_aAS$Year))

# autocorrelation is fine but definite trend in resids


### spatial independence
# recalculate resids with stations as the grouping variable
as.glmm1_simres_recalcSpace <- recalculateResiduals(as.glmm1_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as.glmm1_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as.glmm1_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as.glmm1_simres_recalcSpace)   


as.biomass.all <- spatialData_join(df_sumAS, as.glmm1_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(as.biomass.all)


# Diagnostics look fantastic for this model except for a trend in the Years.  Proceed with this model (as.glmm2).  


### summary ----
test <- summary(best_model)
test$vcov$cond
mean_by_site(df_sumAS, "d")
ggsave(paste0("output/AS_density.png"), width=10, height=8, units="in")

confint(best_model)[1:4, ]
#confint(as.glmm2)[1:4, ]
confint(best_model)
tab.ci(best_model, "as_den") 

tmp <- confint(best_model)
tmp[1:5, c(3, 1:2)]
# percent increase
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

#### blup ---- 
asd_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(asd_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/AS_density_blups.png"), width=8, height=6, units="in")

#### zi ----
# non-sig Year effect

## ASYOY ----
### data ----
df_aASYOY <- df_a[df_a$Species == "ASYOY",]
plot(density(df_aASYOY$abun.stand, na.rm = T))
ggplot(df_aASYOY, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_wrap(~Year)
ggplot(df_aASYOY, aes(x = Station, y = abun.stand)) + geom_boxplot() + facet_grid(type~Year)


### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros
df_aASYOY$Year1 <- df_aASYOY$Year-min(df_aASYOY$Year)
df_aASYOY$Year2 <- scale(df_aASYOY$Year)

asyoy.glmm1 <- glmmTMB(
  # abun.stand ~ type + (1 | Year),
  abun.stand ~ type + (1 | Year),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ type,
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aASYOY
)
summary(asyoy.glmm1)


# Fifield advised the following
### PLOTS/RESULTS WITH AND WITHOUT DISPERSION FOR THE GLMMTMB.  
asyoy.glmm2 <- glmmTMB(
  abun.stand ~ type + (1 | Year), 
  family=ziGamma(link="log"), 
  ziformula = ~ type, 
  REML = TRUE,
  data = df_aASYOY
)
summary(asyoy.glmm2)

anova(asyoy.glmm1, asyoy.glmm2)

# Get the model with the lowest AIC
tmp <- anova(asyoy.glmm1, asyoy.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
asyoy.glmm2_simres <- simulateResiduals(best_model, plot = T)
# str(as.glmm1_simres,1)
residuals(asyoy.glmm2_simres) 
# these are scaled residuals


# nomality and over dispersion - these look great
testUniformity(asyoy.glmm2_simres)
testQuantiles(asyoy.glmm2_simres)

# these are also great except for Year but not part of model
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$type)
plotResiduals(asyoy.glmm2_simres, form = df_aASYOY$Year) # not part of the model and Dave wasn't concerned about this, especially since not much that can be done


# dispersion/zeroinflation - red line should be in the middle of the histogram - looks great
testDispersion(asyoy.glmm2_simres)
testZeroInflation(asyoy.glmm2_simres)


### temporal independence
#### this seems fine
asyoy.glmm2_simres_recalc <- recalculateResiduals(asyoy.glmm2_simres, group = df_aASYOY$Year)

testTemporalAutocorrelation(asyoy.glmm2_simres_recalc, time = unique(df_aASYOY$Year))



### spatial independence - so few data here that its a bit of a meaningless exercise
# recalculate resids with stations as the grouping variable
asyoy.glmm2_simres_recalcSpace <- recalculateResiduals(asyoy.glmm2_simres, group = df_aASYOY$Station)

testSpatialAutocorrelation(asyoy.glmm2_simres_recalcSpace, x = unique(df_aASYOY$west), y = unique(df_aASYOY$north))

spatialAutoCorrBase_fun(df_aASYOY, asyoy.glmm2_simres_recalcSpace)   


asyoy.biomass.all <- spatialData_join(df_sumAS, asyoy.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(asyoy.biomass.all)


# Diagnostics are fine - proceed

### summary ----
summary(best_model)
mean_by_site(df_sumASYOY, "d")
ggsave(paste0("output/ASYOY_density.png"), width=10, height=8, units="in")

confint(best_model)
tab.ci(best_model, "asyoy_den") 

### emmeans ----
## no sig effects
#### blup ---- 
asyd_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(asyd_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/ASY_density_blups.png"), width=8, height=6, units="in")

#### zi ----
# no zi



# Biomass ----
## BT ----
### data ----

plot(density(df_aBT$bio.stand, na.rm = T))
summary(df_aBT$bio.stand)
with(df_aBT, table(bio, Year))
with(df_aBT, table(bio, Station, Year))
ggplot(df_aBT, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)
ggplot(df_aBT, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_grid(type~Year)

### analyses ----
### Need glmmTMB to have random effects and a non-normal distribution
#### Need Gamma with ziformula because there are zeros

bt_bio.glmm1 <- glmmTMB(
  # bio.stand ~ type + (1 | Year), #+ (1 | Station),
  bio.stand ~ type + north + Year1  + (1 | Year),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBT
)
summary(bt_bio.glmm1)


bt_bio.glmm2 <- glmmTMB(
  # bio.stand ~ type + (1 | Year), #+ (1 | Station),
  bio.stand ~ type + north + Year1 + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ type,
  REML = TRUE,
  data = df_aBT
)
summary(bt_bio.glmm2)

anova(bt_bio.glmm1, bt_bio.glmm2)

# bt_bio.glmm2 is the better model


# Get the model with the lowest AIC
tmp <- anova(bt_bio.glmm1, bt_bio.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
bt_bio.glmm2_simres <- simulateResiduals(best_model, plot = T)
# str(as_bio.glmm1_simres,1)
residuals(bt_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion s and  homogeneity of variance is good.  

# plots by themselves or by group
testUniformity(bt_bio.glmm2_simres)
testQuantiles(bt_bio.glmm2_simres)

plotResiduals(bt_bio.glmm2_simres, form = df_aBT$type) # great
plotResiduals(bt_bio.glmm2_simres, form = df_aBT$Year) # some trend

# dispersion/zeroinflation
testDispersion(bt_bio.glmm2_simres)
testZeroInflation(bt_bio.glmm2_simres) # these look great


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
bt_bio.glmm2_simres_recalc <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(bt_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# no autocorrelatoin;  resids v. time is OK.  


### spatial independence
# recalculate resids with stations as the grouping variable
bt_bio.glmm2_simres_recalcSpace <- recalculateResiduals(bt_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aBT$Station) 
#str(as.glmm2_simres_recalcSpace)
length(bt_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(bt_bio.glmm2_simres_recalcSpace, x = unique(df_aBT$west), y = unique(df_aBT$north))

spatialAutoCorrBase_fun(df_aBT, bt_bio.glmm2_simres_recalcSpace)   


bt_bio.biomass.all <- spatialData_join(df_sumBT, bt_bio.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(bt_bio.biomass.all)
# spatial autocorrelation is great

# Diagnostics look fine for this model.  Proceed with this model (bt_bio.glmm2).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(best_model)
mean_by_site(df_bio_sumBT, "b")
ggsave(paste0("output/BT_biomass.png"), width=10, height=8, units="in")


confint(best_model)
confint(best_model)[1:4, ]
tab.ci(best_model, "bt_bio") 

## emmeans ----
emmeans(best_model, ~ type, component = "cond")
emmeans(best_model, ~ type, component = "response")
emm.bt.bio <- as.data.frame(emmeans(best_model, ~ type,       component = "response"))
((emm.bt.bio[2,2]-emm.bt.bio[1,2])/
    emm.bt.bio[1,2])*100

#### blup ---- 
btb_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(btb_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/BT_biomass_blups.png"), width=8, height=6, units="in")


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
  bio.stand ~ type + (1 | Year), #+ (1 | Station),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy_bio.glmm1)


btyoy_bio.glmm2 <- glmmTMB(
  bio.stand ~ type + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  data = df_aBTYOY
)
summary(btyoy_bio.glmm2)

anova(btyoy_bio.glmm1, btyoy_bio.glmm2)
# btyoy_bio.glmm2 is a marginally better model

# Get the model with the lowest AIC
tmp <- anova(btyoy_bio.glmm1, btyoy_bio.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
btyoy_bio.glmm2_simres <- simulateResiduals(best_model, plot = T)
# str(as_bio.glmm1_simres,1)
residuals(btyoy_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion s great and  homogeneity of variance is good.  

# plots by themselves or by group
testUniformity(btyoy_bio.glmm2_simres)
testQuantiles(btyoy_bio.glmm2_simres)

plotResiduals(btyoy_bio.glmm2_simres, form = df_aBTYOY$type) # great
plotResiduals(btyoy_bio.glmm2_simres, form = df_aBTYOY$Year) # temporal trend - ignore


# dispersion/zeroinflation
testDispersion(btyoy_bio.glmm2_simres)
testZeroInflation(btyoy_bio.glmm2_simres) # these look great


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
btyoy_bio.glmm2_simres_recalc <- recalculateResiduals(btyoy_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(btyoy_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))
# autocorrelation looks great

### spatial independence
# recalculate resids with stations as the grouping variable
btyoy_bio.glmm2_simres_recalcSpace <- recalculateResiduals(btyoy_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aBTYOY$Station) 
#str(as.glmm2_simres_recalcSpace)
length(btyoy_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(btyoy_bio.glmm2_simres_recalcSpace, x = unique(df_aBTYOY$west), y = unique(df_aBTYOY$north))

spatialAutoCorrBase_fun(df_aBTYOY, btyoy_bio.glmm2_simres_recalcSpace)   


btyoy_bio.biomass.all <- spatialData_join(df_sumBTYOY, btyoy_bio.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(btyoy_bio.biomass.all)
# spatial autocorrelation is fine

# Diagnostics look fine for this model.  Proceed with this model (btyoy_bio.glmm2).  See glmm_anova for above but without REML which will allow for the BACI.

### summary ----
summary(best_model)
mean_by_site(df_bio_sumBTYOY, "b")
ggsave(paste0("output/BTYOY_biomass.png"), width=10, height=8, units="in")

confint(best_model)[1:4, ]
#confint(btyoy_bio.glmm1)[1:4, ]
tab.ci(best_model, "btyoy_bio") 

## emmeans ----
emmeans(best_model, ~ type, component = "cond")
emmeans(best_model, ~ type, component = "response")
emm.bty.bio <- as.data.frame(emmeans(best_model, ~ type,       component = "response"))
((emm.bty.bio[2,2]-emm.bty.bio[1,2])/
    emm.bty.bio[1,2])*100

#### blup ---- 
btyb_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(btyb_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/BTY_biomass_blups.png"), width=8, height=6, units="in")


## AS ----
### data ----
plot(density(df_aAS$bio.stand, na.rm = T))
summary(df_aAS$bio.stand)
with(df_aAS, table(bio, Year))
with(df_aAS, table(bio, Station, Year))
ggplot(df_aAS, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)


### analyses ----
as_bio.glmm1 <- glmmTMB(
  bio.stand ~ type + (1 | Year), #+ (1 | Station),
  #  bio.stand ~ type*time + Year +  (1 | Year), #+ (1 | Station),
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ 1, #~1,
  REML = TRUE,
  control = glmmTMBControl(
    optimizer = optim,
    optArgs=list(method = "BFGS")),
  data = df_aAS
)
summary(as_bio.glmm1)


as_bio.glmm2 <- glmmTMB(
  bio.stand ~ type + (1 | Year), #+ (1 | Station),
  #  bio.stand ~ type*time + Year + (1 | Year), #+ (1 | Station),
  family=ziGamma(link="log"), 
  ziformula = ~1, #~1,
  REML = TRUE,
  data = df_aAS
)
summary(as_bio.glmm2)

anova(as_bio.glmm1, as_bio.glmm2)
# str(as_bio.glmm2)
# as_bio.glmm2 is a slightly better model, for both original and ziformula ~ time


# Get the model with the lowest AIC
tmp <- anova(as_bio.glmm1, as_bio.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)



### diagnostics ----
as_bio.glmm2_simres <- simulateResiduals(best_model, plot = T)


# str(as_bio.glmm1_simres,1)
residuals(as_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion and the homogeneity of variance are great

# plots by themselves or by group
testUniformity(as_bio.glmm2_simres)
testQuantiles(as_bio.glmm2_simres)

plotResiduals(as_bio.glmm2_simres, form = df_aAS$type) #great
plotResiduals(as_bio.glmm2_simres, form = df_aAS$Year) # slight trend but OK


# dispersion/zeroinflation
testDispersion(as_bio.glmm2_simres)
testZeroInflation(as_bio.glmm2_simres) # looks fine


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
as_bio.glmm2_simres_recalc <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Year)

testTemporalAutocorrelation(as_bio.glmm2_simres_recalc, time = unique(df_aAS$Year))

# Temporal resids are fine 


### spatial independence
# recalculate resids with stations as the grouping variable
as_bio.glmm2_simres_recalcSpace <- recalculateResiduals(as_bio.glmm2_simres, group = df_aAS$Station)
unique(df_aAS$Station) 
#str(as.glmm2_simres_recalcSpace)
length(as_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(as_bio.glmm2_simres_recalcSpace, x = unique(df_aAS$west), y = unique(df_aAS$north))

spatialAutoCorrBase_fun(df_aAS, as_bio.glmm2_simres_recalcSpace)   


as_bio.biomass.all <- spatialData_join(df_sumAS, as_bio.glmm2_simres_recalcSpace, df_loc[, c(2:4)])

spatialAutoCorrGG_fun(as_bio.biomass.all)
# spatial autocorrelation is fine

### summary ----
summary(best_model)
mean_by_site(df_bio_sumAS, "b")
ggsave(paste0("output/AS_biomass.png"), width=10, height=8, units="in")


confint(best_model)
confint(best_model)[1:4, c(3, 1, 2)]
tab.ci(best_model, "as_bio") 

tmp <- confint(best_model)
tmp[1:4, c(3, 1:2)]
# percent increase - main effect
((exp(tmp[1,3] + tmp[2,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100

# # interaction - this helped me to arrive at the conclusion below.  
# model.matrix.lm(best_model)
# # 1 - this is the difference of interaction (before:below) from the intecept (after:above) - meaningless in this context
# ((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3]))/exp(tmp[1,3])*100 
# 
# # 2 - this is the difference between the after:below and before:below
# ((exp(tmp[1,3] + tmp[2,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]))/exp(tmp[1,3]+ tmp[2,3])*100 
# 
# # 3 - or is this - I think that this is right because its the full interaction (before:below) compared to the after:below:after and its consistent with the design matrix while the above is not
# 
# ((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]))/exp(tmp[1,3]+ tmp[2,3])*100 
# 
# 
# # 4 - this is difference between the interaction and the main effects
# ((exp(tmp[1,3] + tmp[2,3] + tmp[3,3] + tmp[4,3]))-exp(tmp[1,3] + tmp[2,3]+ tmp[3,3]))/exp(tmp[1,3]+ tmp[2,3] + tmp[3,3])*100
# 
# # however, 2 and 4 give the same result which is scary
#### blup ---- 
asb_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(asb_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/AS_biomass_blups.png"), width=8, height=6, units="in")


## ASYOY ----
### data ----

plot(density(df_aASYOY$bio.stand, na.rm = T))
summary(df_aASYOY$bio.stand)
with(df_aASYOY, table(bio, Year))
with(df_aASYOY, table(bio, Station, Year))
ggplot(df_aASYOY, aes(x = Station, y = bio.stand)) + geom_boxplot() + facet_wrap(~Year)



### analyses ----
asyoy_bio.glmm1 <- glmmTMB(
  bio.stand ~ type + (1 | Year), 
  dispformula = ~ type,
  family=ziGamma(link="log"), 
  ziformula = ~ 1,
  REML = TRUE,
  # control = glmmTMBControl(
  #   optimizer = optim,
  #   optArgs=list(method = "BFGS")),
  data = df_aASYOY
)
summary(asyoy_bio.glmm1)

asyoy_bio.glmm2 <- glmmTMB(
  bio.stand ~ type + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ 1, 
  REML = TRUE,
  data = df_aASYOY
)
summary(asyoy_bio.glmm2)

anova(asyoy_bio.glmm1, asyoy_bio.glmm2) # glmm2 is a smidge better - really no difference

# Get the model with the lowest AIC
tmp <- anova(asyoy_bio.glmm1, asyoy_bio.glmm2)
best_model_name <- rownames(tmp)[which.min(tmp$AIC)]

# Assign the best model to a new object
best_model <- get(best_model_name)


### diagnostics ----
asyoy_bio.glmm2_simres <- simulateResiduals(best_model, plot = T)


# str(asyoy_bio.glmm1_simres,1)
residuals(asyoy_bio.glmm2_simres) 
# these are scaled residuals
# The normality/overdispersion fine; small trend in the homogeneity of variance but not awful

# plots by themselves or by group
testUniformity(asyoy_bio.glmm2_simres)
testQuantiles(asyoy_bio.glmm2_simres)

# none of these are great but not awful
plotResiduals(asyoy_bio.glmm2_simres, form = df_aASYOY$type) # fine
plotResiduals(asyoy_bio.glmm2_simres, form = df_aASYOY$Year) # an upward trend - proceed


# dispersion/zeroinflation
testDispersion(asyoy_bio.glmm2_simres)
testZeroInflation(asyoy_bio.glmm2_simres) # looks fine


### temporal independence
# But, to look at temporal autocorrelation, we need to recalculate them with Year as a grouping variable
asyoy_bio.glmm2_simres_recalc <- recalculateResiduals(asyoy_bio.glmm2_simres, group = df_aASYOY$Year)

testTemporalAutocorrelation(asyoy_bio.glmm2_simres_recalc, time = unique(df_aASYOY$Year))

# Temporal resids look fine


### spatial independence
# recalculate resids with stations as the grouping variable
asyoy_bio.glmm2_simres_recalcSpace <- recalculateResiduals(asyoy_bio.glmm2_simres, group = df_aASYOY$Station)
unique(df_aASYOY$Station) 
#str(asyoy.glmm2_simres_recalcSpace)
length(asyoy_bio.glmm2_simres_recalcSpace$scaledResiduals)

testSpatialAutocorrelation(asyoy_bio.glmm2_simres_recalcSpace, x = unique(df_aASYOY$west), y = unique(df_aASYOY$north))

spatialAutoCorrBase_fun(df_aASYOY, asyoy_bio.glmm2_simres_recalcSpace)   


asyoy_bio.biomass.all <- spatialData_join(df_sumASYOY, asyoy_bio.glmm2_simres_recalcSpace, df_loc[, c(2:4)])


spatialAutoCorrGG_fun(asyoy_bio.biomass.all)
# spatial resids are OK.

# Diagnostics look fine.

### summary ----
summary(best_model)
mean_by_site(df_bio_sumASYOY, "b")
ggsave(paste0("output/ASYOY_biomass.png"), width=10, height=8, units="in")


confint(best_model)
confint(best_model)[1:4, c(3, 1, 2)]
tab.ci(best_model, "asyoy_bio") 

#### blup ---- 
asyb_rdm <- modelbased::estimate_grouplevel(best_model)
p <- plot(asyb_rdm) # this seems to work now
p + labs(y = "BLUP - 95% CI")
ggsave(paste0("output/ASY_biomass_blups.png"), width=8, height=6, units="in")

# END ----