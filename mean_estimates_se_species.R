

library(ggplot2)
source("RB_data.R")

####Biomass by species and Type Plot


jpeg("RB_Spp_Biomass_cs.jpg", width=6.5, height=4, units='in', res=300)

ggplot(RB_Spp_Biomass_cs, aes(as.factor(Year), mean)) + 
  geom_point(position=position_dodge(0.5)) +
  theme_bw() +  
  facet_grid(Type~Species) +
  theme(axis.text.x  = element_text(vjust=0.2, size=10, angle=45)) +
  theme(axis.text.y  = element_text(vjust=0.2, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax= mean + se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Biomass Estimate (g/100 sq. m)") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

dev.off()
  

#######DENSITY

jpeg("RB_Spp_density_cs.jpg", width=6.5, height=4, units='in', res=300)

ggplot(RB_Spp_Density, aes(as.factor(Year), mean)) + 
  geom_point(position=position_dodge(0.5)) +
  theme_bw() +  
  facet_grid(Type~Species) +
  theme(axis.text.x  = element_text(vjust=0.2, size=10, angle=45)) +
  theme(axis.text.y  = element_text(vjust=0.2, size=10)) +
  geom_errorbar(aes(ymin=mean-se, ymax= mean + se), width=0.5, position=position_dodge(0.5)) +
  ylab("Mean Density Estimate (#/100 sq. m)") +
  xlab("Year") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

dev.off()


  