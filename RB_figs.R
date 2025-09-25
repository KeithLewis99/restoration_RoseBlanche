# recreate Kristin's figures; these are figures that were in the folders and the manuscript she sent but no corresponding code.  Also the error bars here do not 

# I think that Fig 4 is redundant wtih 5.  Therefore, I have modified 5 accordingly.

library(ggplot2)
source("RB_data.R")

# Fig 4 ----

ggplot(RB_MeanBioComp[3:6,], aes(as.factor(year), meanbiomass)) + 
  geom_point(position=position_dodge(0.5)) +
  theme_bw() +  
  # theme(axis.text.x  = element_text(vjust=0.2, size=10, angle=45)) +
  # theme(axis.text.y  = element_text(vjust=0.2, size=10)) +
  geom_errorbar(aes(ymin=meanbiomass-se, ymax= meanbiomass + se), width=0.5, position=position_dodge(0.5)) +
  ylab("Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  geom_hline(yintercept = RB_MeanBioComp[1,3], colour = "red")
#  annotate("label", x = 2.5, y = 225, 
       #    label = "Biomass Estimate of Destroyed Habitat", colour = "red")
# Kristin had a label but I think that this should just go in teh caption
  


# Fig 5 ---- 

  RB_Biomass_MainandComp_se <- 
    ggplot(RB_meansalmonidsbysite, 
           aes(as.factor(Year), MeanBiomass)) + 
    theme_bw(base_size = 20) + 
    geom_point(aes(colour=as.factor(Habitat), shape=as.factor(Habitat)), position=position_dodge(0.9), size = 3) + 
    geom_vline(xintercept = 4.5, linetype = "dashed") +
    geom_errorbar(aes(ymax= MeanBiomass + MB_se, ymin=MeanBiomass - MB_se, colour = as.factor(Habitat)), linewidth=0.6, width=0.30, position=position_dodge(.9)) +
#    scale_colour_manual(breaks = c("Destroyed", "Compensation", "Main Stem"), values=c("red", "black", "blue")) +
    # theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
    # theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
    #ylab("Biomass Estimate (grams/100 sq. meters)") + 
    ylab(expression("Biomass Estimate (g/m" ^2*")")) + 
    xlab("Year") +
    theme(legend.title=element_blank()) +
    theme(legend.position = "inside", legend.position.inside = c(.20, .85)) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    geom_hline(yintercept = RB_MeanBioComp[1,3], colour = "red") +
  scale_colour_manual(
    breaks = c("Destroyed", "Compensation", "Main Stem"), 
    values=c("black", "black", "grey")) +
  scale_shape_manual(
    breaks = c("Destroyed", "Compensation", "Main Stem"), 
    values=c(0, 16, 16))
RB_Biomass_MainandComp_se
ggsave("figs/salmonids_biomass_by_trt.png", width=10, height=8, units="in")

# Fig 6 ----  
RB_Density_MainandComp_se <- 
  ggplot(RB_meansalmonidsbysite, 
         aes(as.factor(Year), MeanDensity)) + 
  theme_bw(base_size = 20) + 
  geom_point(aes(colour=as.factor(Habitat), shape=as.factor(Habitat)), position=position_dodge(0.9), size = 3) + 
  geom_errorbar(aes(ymax= MeanDensity + MD_se, ymin=MeanDensity - MD_se, colour = as.factor(Habitat)), linewidth=0.6, width=0.30, position=position_dodge(.9)) +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  geom_hline(yintercept = RB_meansalmonidsbysite[1,6], colour = "red") +
#  scale_colour_manual(
    # breaks = c("Destroyed", "Compensation", "Main Stem"), values=c("red", "black", "blue")) +
  scale_colour_manual(
    breaks = c("Destroyed", "Compensation", "Main Stem"), 
    values=c("black", "black", "grey")) +
    scale_shape_manual(
      breaks = c("Destroyed", "Compensation", "Main Stem"), 
      values=c(0, 16, 16)) +
  # theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  # theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  # theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  # theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  # ylab("Biomass Estimate (grams/100 sq. meters)") + 
  ylab(expression("Density Estimate (#/100 m" ^2*")")) +
  xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.2, .2)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  
RB_Density_MainandComp_se
ggsave("figs/salmonids_density_by_trt.png", width=10, height=8, units="in")

# Fig 7 ----
# so not only are the error bars off, between this and Kristin's plot, there is a discrepancy between AS:Main:2002 and BT:Main:2000
jpeg("RB_bothsppbio_point.jpg", width=6.5, height=4, units='in', res=600)

RB_bothsppbio_point <-
  ggplot(RB_spp_maincomp, aes(x = Year, y = biomass_100m)) + 
  geom_point() + 
  #geom_errorbar(aes(x = Year, ymax=biomass_ucl, ymin=biomass_lcl), linewidth=1, width=0.25) +
  facet_grid(Station~Species) + 
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  ylim(-20,300) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())  

RB_bothsppbio_point
dev.off()
par(mfrow=c(1,1))


# Fig 8 ----
jpeg("RB_bothsppabun_point.jpg", width=6.5, height=4, units='in', res=600)

RB_bothsppabund_point <-
  ggplot(RB_spp_maincomp, aes(x = Year, y = abundance_100m)) + 
  geom_point() + 
  geom_errorbar(aes(x = Year, ymax=abundance_ucl, ymin=abundance_lcl), linewidth=1, width=0.25) +
  facet_grid(Station~Species) + 
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  ylim(0,15) +
  theme_bw() +
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())  

RB_bothsppabund_point
dev.off()
par(mfrow=c(1,1))

# END ----  
 #AAAAAAAAAAA