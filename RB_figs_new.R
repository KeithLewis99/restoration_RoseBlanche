# Creat the figures - not sure if we're keeping the Scruton recreations etc......

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
# the below are the graphs that I created for Pamehac to Scruton et al. 1998 but Scruton 2005 used different graphs and Fig 2 is for all salmonids; Fig 3 is for BT and BTY - the below don't match these well. Comment the below out as i'm not using them.
## but I think Kristin's figures are better - I have modified these a little for the bootstrap estimates (see below).  

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
# View(df_a |>  
#        filter(Species == "BT") |>
#        group_by(Year,type) |> 
#        summarise(n = n(),
#                  mean_abun = mean(abun.stand),
#                  se_abun = sd(abun.stand)/n))
# 
## predicted values ----
## create a df with variables, data, and predicted values; 
### this is a graphical presentation of the parameter estimates.  These are on log scale with the se's, not CIs.
### Deprecated: see ReadMe.Rmd
# btd <- cbind(df_aBT[,c("Year", "type", "abun.stand", "bio.stand")],
#              as.data.frame(predict(bt.glmm1, se.fit = T))
# )
# btb <- cbind(df_aBT[,c("Year", "type", "abun.stand", "bio.stand")],
#              as.data.frame(predict(bt_bio.glmm2, se.fit = T))
# )
# btyd <- cbind(df_aBTYOY[,c("Year", "type", "abun.stand", "bio.stand")],
#               as.data.frame(predict(btyoy.glmm1, se.fit = T))
# )
# btyb <- cbind(df_aBTYOY[,c("Year", "type", "abun.stand", "bio.stand")],
#               as.data.frame(predict(btyoy_bio.glmm2, se.fit = T))
# )
# asd <- cbind(df_aAS[,c("Year", "type", "abun.stand", "bio.stand")],
#              as.data.frame(predict(as.glmm2, se.fit = T))
# )
# asb <- cbind(df_aAS[,c("Year", "type", "abun.stand", "bio.stand")],
#              as.data.frame(predict(as_bio.glmm2, se.fit = T))
# )
# asyd <- cbind(df_aASYOY[,c("Year", "type", "abun.stand", "bio.stand")],
#               as.data.frame(predict(asyoy.glmm2, se.fit = T))
# )
# asyb <- cbind(df_aASYOY[,c("Year", "type", "abun.stand", "bio.stand")],
#               as.data.frame(predict(asyoy_bio.glmm2, se.fit = T))


### create plots ----
# # this plots converts the fitted value and se to CIs taking zeros and the link into account.  CI's are not symetrical and don't overlap zero!
# See ReadMe for reason that these are deprecated; however, it is not a bad way to see the predicted values for each site - the control_impact_year function works.
# 
# p1 <- control_impact_year(btd, "d", "n")
# p2 <- above_below_year(btyd, "n", "y")
# p3 <- above_below_year(asd, "d", "n")
# p4 <- above_below_year(asyd, "n", "n")
# 
# p5 <- above_below_year(btb, "b", "n")
# p6 <- above_below_year(btyb, "n", "y")
# p7 <- above_below_year(asb, "b", "n")
# p8 <- above_below_year(asyb, "n", "n")
# 
# plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), nrow = 2)
# ggsave("output/all_density_new.png", width=10, height=8, units="in")
# 
# plot_grid(p5, p6, p7, p8, labels = c('A', 'B', 'C', 'D'), nrow = 2)
# ggsave("output/all_biomass_new.png", width=10, height=8, units="in")
 
# # all salmonids ----
## Naive estimates - DEPRECATED
# # note that these are NOT based on modeloutputs
# #https://dataanalytics.org.uk/axis-labels-in-r-plots-using-expression/#sub_sup
# df_a |>
#   group_by(Year,type) |>
#   summarise(n = n(),
#             mean_abun = mean(abun.stand),
#             se_abun = sd(abun.stand)/n) |>
#   ggplot(aes(x = as.factor(Year), y = mean_abun, fill = type, colour = type)) +
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   #facet_wrap(~Species) +
#   theme_bw(base_size = 20) +
#   ylab(expression("Density Estimate (#/100 m" ^2*")")) +
#   xlab("Year") +
#   geom_errorbar(aes(ymax = mean_abun+se_abun*1.96, ymin = mean_abun-se_abun*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
#   geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
#   geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
#   #theme(legend.title=element_blank()) +
#   #theme(legend.position=c(.85, .88)) +
#   scale_fill_discrete(name="",
#                       breaks=c("above", "below"),
#                       labels=c("Above", "Below")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("above", "below"),
#                       labels=c("Above", "Below"))
# ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")
# 
# 
# 
# df_a |>  
#   group_by(Year,type) |> 
#   summarise(n = n(),
#             mean_bio = mean(bio.stand),
#             se_bio = sd(bio.stand)/n) |>
#   ggplot(aes(x = as.factor(Year), y = mean_bio, fill = type, colour = type)) + 
#   geom_point(position = position_dodge(width = 0.5), size = 3) +
#   #facet_wrap(~Species) + 
#   theme_bw(base_size = 20) + 
#   #  ylab("Biomass Estimate (g/100 sq. m)") +
#   ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
#   xlab("Year") +
#   geom_errorbar(aes(ymax = mean_bio+se_bio*1.96, ymin = mean_bio-se_bio*1.96), linewidth=1, width=0.15, position=position_dodge(0.5)) +
#   geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
#   geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
#   geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
#   #theme(legend.title=element_blank()) +
#   #theme(legend.position=c(.85, .88)) +
#   scale_fill_discrete(name="",
#                       breaks=c("above", "below"),
#                       labels=c("Above", "Below")) +
#   scale_colour_manual(values=c("black", "dark grey"),
#                       name="",
#                       breaks=c("above", "below"),
#                       labels=c("Above", "Below")) 
# ggsave("output/salmonid_biomass_new.png", width=10, height=8, units="in")


# bootstrap ----
## density ----
## bootstrap estimates for density and biomass by species and year from RB_tables.R
den_ci <- read.csv("data_derived/density_ci.csv")

ggplot(den_ci, aes(x = as.factor(Year), y = mean, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  geom_errorbar(aes(ymax = ul, ymin = ll), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  scale_fill_discrete(name="",
                      breaks=c("con", "trt"),
                      labels=c("Control", "Treatment")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("con", "trt"),
                      labels=c("Control", "Treatment"))
ggsave("output/salmonid_density_new.png", width=10, height=8, units="in")


## biomass ----
bio_ci <- read.csv("data_derived/biomass_ci.csv")

ggplot(bio_ci, aes(x = as.factor(Year), y = mean, fill = type, colour = type)) + 
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~Species) + 
  theme_bw(base_size = 20) + 
  ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
  xlab("Year") +
  geom_errorbar(aes(ymax = ul, ymin = ll), linewidth=1, width=0.15, position=position_dodge(0.5)) +
  scale_fill_discrete(name="",
                      breaks=c("con", "trt"),
                      labels=c("Control", "Treatment")) +
  scale_colour_manual(values=c("black", "dark grey"),
                      name="",
                      breaks=c("con", "trt"),
                      labels=c("Control", "Treatment"))
ggsave("output/salmonid_biomass_new.png", width=10, height=8, units="in")


# density - publicatoin -----
source("RB_fun.R")
den_ci_bt <- den_ci |> filter(Species == "BT") 
den_ci_bty <- den_ci |> filter(Species == "BTYOY") 
den_ci_as <- den_ci |> filter(Species == "AS") 
den_ci_asy <- den_ci |> filter(Species == "ASYOY") 

p1 <- control_impact_year1(den_ci_bt, "d", "n")
p2 <- control_impact_year1(den_ci_bty, "n", "y")
p3 <- control_impact_year1(den_ci_as, "d", "n")
p4 <- control_impact_year1(den_ci_asy, "n", "n")

### combine ----
p1_clean <- p1 + theme(axis.title = element_blank())
p2_clean <- p2 + theme(axis.title = element_blank())
p3_clean <- p3 + theme(axis.title = element_blank())
p4_clean <- p4 + theme(axis.title = element_blank())
#plot_grid(p1, p2, p3, p4, labels = c('A', 'B', 'C', 'D'), nrow = 2) # this is OK but labels look lame and need to be species names

ggsave("output/all_density_boot_new.png", width=10, height=8, units="in")
grid_den <- plot_grid(p3_clean, 
                      p4_clean,
                      p1_clean, 
                      p2_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      labels = c("AS", "ASY","BT", "BTY"),
                      label_x = 0.1, label_y = 1, 
                      hjust = 0,
                      #hjust = -2, 1
                      vjust = 1.25)

# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Density Estimate (#/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1, fontface = "bold", size = 14)
final_plot_den

save_plot("figs/species_den_ci.png", 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")

## biomass ----
bio_ci_bt <- bio_ci |> filter(Species == "BT") 
bio_ci_bty <- bio_ci |> filter(Species == "BTYOY") 
bio_ci_as <- bio_ci |> filter(Species == "AS") 
bio_ci_asy <- bio_ci |> filter(Species == "ASYOY") 

p5 <- control_impact_year1(bio_ci_bt, "b", "n")
p6 <- control_impact_year1(bio_ci_bty, "n", "y")
p7 <- control_impact_year1(bio_ci_as, "b", "n")
p8 <- control_impact_year1(bio_ci_asy, "n", "n")

### combine ----
p5_clean <- p5 + theme(axis.title = element_blank())
p6_clean <- p6 + theme(axis.title = element_blank())
p7_clean <- p7 + theme(axis.title = element_blank())
p8_clean <- p8 + theme(axis.title = element_blank())

# plot_grid(p5, p6, p7, p8, labels = c('A', 'B', 'C', 'D'), nrow = 2)

grid_plot <- plot_grid(p7_clean, 
                       p8_clean,
                       p5_clean, 
                       p6_clean, 
                       ncol = 2, 
                       align = "hv", 
                       axis = "tblr",
                       scale = 0.9,
                       labels = c("AS", "ASY","BT", "BTY"),
                       label_x = 0.1, label_y = 1, 
                       hjust = 0,                       # hjust = -3, 
                       vjust = 1.25
                       )

# Add shared axis labels
final_plot_bio <- ggdraw(grid_plot) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Biomass Estimate (g/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1, fontface = "bold", size = 14)

final_plot_bio
save_plot("figs/species_bio_ci.png", 
          final_plot_bio, 
          base_height = 6, 
          base_width = 10,
          bg = "white")

ggsave("output/all_biomass_boot_new.png", width=10, height=8, units="in")

# replacement -----

library(ggplot2)
source("RB_data.R")

sal_den_ci <- read.csv("data_derived/sal_density_ci.csv")
sal_bio_ci <- read.csv("data_derived/sal_biomass_ci.csv")
str(sal_bio_ci)

# 
## density ----
# for bootstrap values

ggplot(sal_den_ci, 
       aes(as.factor(Year), mean)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_errorbar(aes(ymax= ll, ymin=ul, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  geom_hline(yintercept = RB_meansalmonidsbysite[1,6], colour = "red") +
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

ggsave("figs/salmonids_density_boot_by_trt.png", width=10, height=8, units="in")


# density unscaled
ggplot(sal_den_ci, 
       aes(as.factor(Year), mean*100)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_errorbar(aes(ymax= ll*100, ymin=ul*100, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
  ylab("Abundance") +
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  geom_hline(yintercept = RB_meansalmonidsbysite[1,6]*100, colour = "red") +
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

ggsave("figs/salmonids_abundance_boot_by_trt.png", width=10, height=8, units="in")

 
## biomass ----
# for bootstrap values

ggplot(sal_bio_ci, 
       aes(as.factor(Year), mean)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_errorbar(aes(ymax= ll, ymin=ul, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
  ylab(expression("Biomass Estimate (g/100 m" ^2*")")) + 
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  geom_hline(yintercept = RB_MeanBioComp[1,3], colour = "red") +
  # geom_hline(yintercept = 42, colour = "red", linetype = "dashed") + # density of dewatered area
  # geom_hline(yintercept = 42*5.7, colour = "red", linetype = "dashed") + # 
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

ggsave("figs/salmonids_biomass_boot_by_trt.png", width=10, height=8, units="in")

# just biomass
ggplot(sal_bio_ci, 
       aes(as.factor(Year), mean*100)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_errorbar(aes(ymax= ll*100, ymin=ul*100, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
  ylab("Biomass Estimate (g)") + 
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  geom_hline(yintercept = RB_MeanBioComp[1,3]*100, colour = "red") +
  # geom_hline(yintercept = 42, colour = "red", linetype = "dashed") + # density of dewatered area
  # geom_hline(yintercept = 42*5.7, colour = "red", linetype = "dashed") + # 
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

ggsave("figs/salmonids_biomass_unscaled_boot_by_trt.png", width=10, height=8, units="in")


# Scruton 2005 Table III ----
tab <- sal_bio_ci |>
  filter(trt != "con") |>
  select(Year, mean) |>
  mutate(Total_biomass = mean*100)

tmp <- tab[1, ]
tmp[1,] <- c(1998, 42, 42*570)

tab <- bind_rows(tmp, tab)


# large fish ----
unique(df_all$Species)
df_all <- df_all |> 
  filter(Sweep <= 3 & Species == "BT"| Species == "AS") |>
  mutate(large = ifelse(Length.mm >= 150, "Y", "N"))

tmp1 <- df_all |>
  group_by(Year, Species, Station) |>
  summarise(count = n())

tmp2 <- df_all |>
  filter(large == "Y") |>
  group_by(Year, Species, Station) |>
  summarise(count_large = n())

df_percent <- left_join(tmp1, tmp2, by = c("Year", "Species", "Station"))

df_percent$count_large[is.na(df_percent$count_large)] <- 0
df_percent <- df_percent |> 
  mutate(prob_large = count_large/count,
         trt = ifelse(Station <=7, "trt", "con"))

df_percent_sum <- df_percent |>
  group_by(Year, Species, trt) |>
  summarise(mean = mean(prob_large),
            sd = sd(prob_large),
            count = sum(count)) 

library(Hmisc)
df_percent_boot <- df_percent |>
group_by(Year, Species, trt) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$prob_large)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
df_percent_boot

df_percent_boot <- left_join(df_percent_boot, df_percent_sum[, c(1:3, 6)], by = c("Year", "Species", "trt"))

df_percent_boot$label <- ifelse(
  !is.na(df_percent_boot$ul), 
  df_percent_boot$ul + 0.05,
  df_percent_boot$mean + 0.05)

### fig ----
ggplot(df_percent_boot, 
       aes(x = as.factor(Year), y = mean)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
  geom_text(aes(label = count, y = label, group = as.factor(trt)), position=position_dodge(0.5), check_overlap = FALSE) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  geom_errorbar(aes(ymax= ll, ymin=ul, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
  ylab(expression("Percent of age-1+ fish" >= "150 mm")) +
  xlab("Year") +
  facet_grid(~Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.60, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))

#ggsave("figs/large_fish_percent_by_trt.png", width=10, height=8, units="in")

### purrr -----
df_per_boot_split <- df_percent_boot |> 
  split(df_percent_boot$Species)


library(purrr)
plot_per <- map(names(df_per_boot_split), function(Species) {
  df <- df_per_boot_split[[Species]]  
  legend_BT <- if(any(df$Species == "BT"))theme(
    legend.position=c(0.45, 0.88),
    legend.background = element_rect(fill = "transparent", color = NA), legend.title=element_blank(),
  legend.key.size = unit(0.4, "cm")
  )
  legend_notBT <- if(any(df$Species != "BT"))
    theme(legend.position= "none")
  ggplot(df, 
         aes(as.factor(Year), mean)) + 
    theme_bw(base_size = 20) + 
    geom_point(aes(colour=as.factor(trt), shape=as.factor(trt)), position=position_dodge(0.5), size = 3) + 
    legend_notBT +
    legend_BT +
    ylim(0, 1.1) +
    geom_vline(xintercept = 3.5, linetype = "dashed") +
    geom_errorbar(aes(ymax= ll, ymin=ul, colour = as.factor(trt)), linewidth=0.6, width=0.30, position=position_dodge(.5)) +
    geom_text(aes(label = count, y = label, group = as.factor(trt)), position=position_dodge(0.5)) +
    ylab(expression("Percent of age-1+ fish" >= "150 mm")) +
    xlab("Year") +
    # theme(legend.title=element_blank()) +
    # theme(legend.position = "inside", legend.position.inside = c(.60, .85)) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    scale_colour_manual(
      breaks = c("con", "trt"),
      labels = c("Control", "Treatment"),
      values=c("grey", "black")) +
    scale_shape_manual(
      breaks = c("con", "trt"), 
      labels = c("Control", "Treatment"),
      values=c(16, 16))
  
})  


names(plot_per) <- paste0(names(df_per_boot_split))
list2env(plot_per, envir = .GlobalEnv)
p1 <- plot_per$AS
p2 <- plot_per$BT


### combine ----
p1_clean <- p1 + theme(axis.title = element_blank())
p2_clean <- p2 + theme(axis.title = element_blank())

grid_plot <- plot_grid(p1_clean, 
                       p2_clean,
                       ncol = 2, 
                       align = "hv", 
                       axis = "tblr",
                       scale = 0.9,
                       labels = c("AS", "BT"),
                       hjust = -3, 
                       vjust = 1.25)

# Add shared axis labels
final_plot_bio <- ggdraw(grid_plot) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 20) +
  draw_label(expression("Percent of age-1+ fish" >= "150 mm"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 20)

final_plot_bio
save_plot("figs/large_fish_percent_boot_by_trt.png", 
          final_plot_bio, 
          base_height = 6, 
          base_width = 10,
          bg = "white")

  
  # END ----