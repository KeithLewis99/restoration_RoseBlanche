# recreate Kristin's figures; these are figures that were in the folders she sent but no corresponding code.


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
  
  
  RB_Biomass_MainandComp_se <- 
    ggplot(RB_meansalmonidsbysite, 
           aes(as.factor(Year), MeanBiomass)) + 
    theme_bw() + 
    geom_point(aes(colour=as.factor(Habitat)), position=position_dodge(0.9), size = 3) + 
    # theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
    # theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
    ylab("Biomass Estimate (grams/100 sq. meters)") + 
    xlab("Year") +
    theme(legend.title=element_blank()) +
    theme(legend.position = "inside", legend.position.inside = c(.25, .85)) +
    geom_errorbar(aes(ymax= MeanBiomass + MB_se, ymin=MeanBiomass - MB_se, colour = as.factor(Habitat)), size=1, width=0.30, position=position_dodge(.9)) +
    scale_colour_manual(values=c("red", "black", "blue")) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  
  RB_Biomass_MainandComp_se
  
  
  RB_Density_MainandComp_se <- 
    ggplot(RB_meansalmonidsbysite, 
           aes(as.factor(Year), MeanDensity)) + 
    theme_bw() + 
    geom_point(aes(colour=as.factor(Habitat)), position=position_dodge(0.9), size = 3) + 
    # theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
    # theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
    # theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
    ylab("Biomass Estimate (grams/100 sq. meters)") + 
    xlab("Year") +
    theme(legend.title=element_blank()) +
    theme(legend.position = "inside", legend.position.inside = c(.2, .2)) +
    geom_errorbar(aes(ymax= MeanDensity + MD_se, ymin=MeanDensity - MD_se, colour = as.factor(Habitat)), size=1, width=0.30, position=position_dodge(.9)) +
    scale_colour_manual(values=c("red", "black", "blue")) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
  
  RB_Density_MainandComp_se

# END ----  