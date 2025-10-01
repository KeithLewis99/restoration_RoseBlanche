bt.glmm3 <- glmmTMB(
  abun.stand ~ type + (1 | Year),
  family=ziGamma(link="log"), 
  ziformula = ~ Year1, # see notes below
  REML = TRUE,
  data = df_aBT
)
summary(bt.glmm3)
mm_cond <- model.matrix(bt.glmm3, component = "cond")
mm_rand <- getME(bt.glmm3, "Z")
mm_cond_zi <- model.matrix(bt.glmm3, component = "zi")

mm_cond_all <- cbind(mm_cond, mm_rand)
cond_BT <- summary(bt.glmm3)$coefficients$cond
mm <- as.matrix(mm_cond)%*%as.matrix(cond_BT[,1])
cond_[1,1]
mean(mm[c(1:7, 11:17, 21:27, 31:37)]) + mean(mm[c(8:10, 18:20, 28:30, 38:40)])


# with real model
mm_cond <- model.matrix(bt.glmm2, component = "cond")
mm_rand <- getME(bt.glmm2, "Z")
mm_cond_zi <- model.matrix(bt.glmm2, component = "zi")
ranef(bt.glmm2)

mm_cond_all <- cbind(mm_cond, mm_rand)
cond_BT <- summary(bt.glmm2)$coefficients$cond
fef_BT <- fixef(bt.glmm2)$cond
mm <- as.matrix(mm_cond)%*%as.matrix(cond_BT[,1])
mm <- as.matrix(mm_cond)%*%as.matrix(fef_BT)
tail(mm)
tail(mm_cond)
# trying to relate this to the output from mm
cond_BT[1,1] + cond_BT[2,1]*0 + cond_BT[3,1]*15 + cond_BT[4,1]*47.64081

mm_cond[40,1]*fef_BT[1] + 
  mm_cond[40,2]*fef_BT[2] + 
  mm_cond[40,3]*fef_BT[3] + 
  mm_cond[40,4]*fef_BT[4] # OK - so this equals the above, the problem was that I rounded the Year term to 47.6. 
mean(mm_cond[mm_cond[,2] == 0,] %*% fef_BT)

# trying to relate this to the emmeans output
mean(mm[c(1:7, 11:17, 21:27, 31:37)]) + mean(mm[c(8:10, 18:20, 28:30, 38:40)])
emmeans(bt.glmm2, ~ type, component = "cond")





# Salamanders
library(glmmTMB)

m <- glmmTMB(
  count ~ spp + (1 | site),
  ziformula = ~ spp,
  family = poisson,
  data = Salamanders
)

# Fixed-effect design matrix (conditional model)
X_cond <- model.matrix(m, component = "cond")

# Random-effect design matrix
Z_cond <- getME(m, "Z")


# Combine fixed and random effects
full_cond_matrix <- cbind(X_cond, as.matrix(Z_cond))


X_zi <- model.matrix(m, component = "zi")


X_cond <- model.matrix(m, component = "cond")
head(X_cond)
beta   <- fixef(m)$cond   # fixed-effect coefficients
eta_fixed <- X_cond %*% beta
head(eta_fixed)
