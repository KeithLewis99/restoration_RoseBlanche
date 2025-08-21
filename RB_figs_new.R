# source ----
# source("Pam_abun_bio.R")
source("RB_data_new.R")
source("RB_fun.R")


# See Seal Cove code for original work and thoughts on this approach
# See ReadMe for thoughts on Pamehac. 

# library ----
library(ggplot2)
library(cowplot)

# Scruton figs ----
# use this to compare to Scruton et al. 1998
## but I think Kristin's figures are better - use them

# density (Fig X in Scruton)
# df_a |>  
#   group_by(Year, Species, type) |> 
#   summarise(mean_abun = mean(abun.stand)) |>
#   ggplot(aes(x = Year, y = mean_abun, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
#   facet_wrap(~Species) + 
#   theme_bw() + 
#   ylab("Density Estimate  (#/100 sq. meters)") 
# ggsave(paste0("output/all_density.png"), width=10, height=8, units="in")
# 
# # biomass (Fig X in Scruton)
# df_a |>  
#   group_by(Year, Species, type) |> 
#   summarise(mean_bio = mean(bio.stand)) |>
#   ggplot(aes(x = Year, y = mean_bio, group = type, fill = type)) + geom_col(position = position_dodge(width = 0.9)) +
#   facet_wrap(~Species) + 
#   theme_bw() + 
#   ylab("Biomass Estimate  (grams/100 sq. meters)")
# ggsave(paste0("output/all_biomass.png"), width=10, height=8, units="in")


# paper figs ----

# this is just to see that a raw calculation makes sense with fitted values below
View(df_a |>  
       filter(Species == "BT") |>
       group_by(Year,type) |> 
       summarise(n = n(),
                 mean_abun = mean(abun.stand),
                 se_abun = sd(abun.stand)/n))

## control-impact ----
# create a df with variables, data, and predicted values
btd <- cbind(df_aBT[,c("Year", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(bt.glmm2, se.fit = T))
)
btb <- cbind(df_aBT[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(bt_bio.glmm2, se.fit = T))
)
btyd <- cbind(df_aBTYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
              as.data.frame(predict(btyoy.glmm2, se.fit = T))
)
btyb <- cbind(df_aBTYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
              as.data.frame(predict(btyoy_bio.glmm2, se.fit = T))
)
asd <- cbind(df_aAS[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(as.glmm1, se.fit = T))
)
asb <- cbind(df_aAS[,c("Year", "time", "type", "abun.stand", "bio.stand")],
             as.data.frame(predict(as_bio.glmm2, se.fit = T))
)
asyd <- cbind(df_aASYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
              as.data.frame(predict(asyoy.glmm2, se.fit = T))
)
asyb <- cbind(df_aASYOY[,c("Year", "time", "type", "abun.stand", "bio.stand")],
              as.data.frame(predict(asyoy_bio.glmm2, se.fit = T))
)


## create plots ----
# this plots converts the fitted value and se to CIs taking zeros and the link into account.  CI's are not symetrical and don't overlap zero!

p1 <- above_below_year(btd, "d", "n")
p2 <- above_below_year(btyd, "n", "y")
p3 <- above_below_year(asd, "d", "n")
p4 <- above_below_year(asyd, "n", "n")

p5 <- above_below_year(btb, "b", "n")
p6 <- above_below_year(btyb, "n", "y")
p7 <- above_below_year(asb, "b", "n")
p8 <- above_below_year(asyb, "n", "n")

plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), nrow = 2)
ggsave("output/all_density_new.png", width=10, height=8, units="in")

plot_grid(p5, p6, p7, p8, labels = c('A', 'B', 'C', 'D'), nrow = 2)
ggsave("output/all_biomass_new.png", width=10, height=8, units="in")

# all salmonids ----
# note that these are NOT based on modeloutputs
#https://dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/#sub_sup

df_a |>  
  group_by(Year,type) |> 
  summarise(n = n(),
            mean_abun = mean(abun.stand),
            se_abun = sd(abun.stand)/n) |>
  ggplot(aes(x = as.factor(Year), y = mean_abun, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_abun+se_abun*1.96, ymin = mean_abun-se_abun*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")



df_a |>  
  group_by(Year,type) |> 
  summarise(n = n(),
            mean_bio = mean(bio.stand),
            se_bio = sd(bio.stand)/n) |>
  ggplot(aes(x = as.factor(Year), y = mean_bio, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  #facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  #  ylab("Biomass Estimate (g/100 sq. m)") +
  ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_bio+se_bio*1.96, ymin = mean_bio-se_bio*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_biomass_new.png", width=10, height=8, units="in")


# by species ----
df_a |>  
  group_by(Year,Species, type) |> 
  summarise(n = n(),
            mean_abun = mean(abun.stand),
            se_abun = sd(abun.stand)/n) |>
  ggplot(aes(x = as.factor(Year), y = mean_abun, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = mean_abun+se_abun*1.96, ymin = mean_abun-se_abun*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
  geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
  geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
  #theme(legend.title=element_blank()) +
  #theme(legend.position=c(.85, .88)) +
  scale_fill_discrete(name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("above", "below"),
                      labels=c("Above", "Below")) 
ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")

# END ----