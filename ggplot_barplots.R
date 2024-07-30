# This is just my attempt to consolidate Kristin Loughlin's code in a way that I understand it.  This file is for the barplots as indicated in the title.

library(ggplot2)

source("RB_data.R")

# all species ----
## biomass ----
#####Plot of biomass for each species each year in Compensation Stream
jpeg("RB_spp_plot.jpg", width=6.5, height=4, units='in', res=300)


allspp_plot<-ggplot(allspp_comp, aes(Year, biomass_100m)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), linewidth=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


allspp_plot
dev.off()
par(mfrow=c(1,1))


## density ----
#####Plot of abundance estimate for each species each year in Compensation Stream
jpeg("RB_sppabun_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_sppabun_plot <- 
  ggplot(allspp_comp, aes(Year, abundance_100m)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
  facet_grid(~Species) +
  geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())


RB_sppabun_plot
dev.off()
par(mfrow=c(1,1))


# both streams ----
## abundance ----
#####Plot of abundance estimate for each species each year in both Main Stem and Compensation Habitats

jpeg("RB_bothsppabun_plot.jpg", width=6.5, height=4, units='in', res=600)


RB_bothsppabun_plot <-
  ggplot(RB_spp_maincomp, aes(Year, abundance_100m)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Population Estimate (Number/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

RB_bothsppabun_plot
dev.off()
par(mfrow=c(1,1))



## biomass ----
#####Plot of biomass estimate for each species each year in both Main Stem and Compensation Habitats

jpeg("RB_bothsppbio_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_bothsppbio_plot <-
  ggplot(RB_spp_maincomp, aes(Year, biomass_100m)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity") + 
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=8)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  facet_grid(Station~Species) +
  geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.25) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

RB_bothsppbio_plot
dev.off()
par(mfrow=c(1,1))

# salmonids ----
## biomass ----
####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats

jpeg("RB_salmonidsbio_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_salmonidsbio_plot <- 
  ggplot(RB_salmonids_maincomp, aes(Year, biomass_100m, fill=Station)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Biomass Estimate (grams/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.8, .85)) +
  geom_errorbar(aes(ymax=biomass_ucl, ymin=biomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  scale_fill_manual(values=c("dark grey", "light grey")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

RB_salmonidsbio_plot

dev.off()
par(mfrow=c(1,1))


## abundance ----
####Plot of biomass estimate for all salmonids combined each year in both Main Stem and Compensation Habitats

jpeg("RB_salmonidsabun_plot.jpg", width=6.5, height=4, units='in', res=600)

RB_salmonidsabun_plot <- 
  ggplot(RB_salmonids_maincomp, aes(Year, abundance_100m, fill=Station)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Population Estimate (number/100 sq. meters)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.8, .85)) +
  geom_errorbar(aes(ymax=abundance_ucl, ymin=abundance_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  scale_fill_manual(values=c("dark grey", "light grey")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

RB_salmonidsabun_plot

dev.off()
par(mfrow=c(1,1))


#####Plot of Total biomass for compensation stream compared to biomass destroyed

jpeg("RB_nonetloss_plot.jpg", width=7, height=5, units='in', res=600)

RB_nonetloss_plot <-
  ggplot(RB_nonetloss, aes(Year, total_biomass, fill=Station)) + 
  theme_bw() + 
  geom_bar(position=position_dodge(), stat="identity", colour="black") + 
  theme(axis.text.x  = element_text(vjust=0.4, size=12)) +
  theme(axis.text.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.y  = element_text(vjust=0.4, size=12)) +
  theme(axis.title.x  = element_text(vjust=0.4, size=12)) +
  ylab("Total Estimated Biomass (grams)") + xlab("Year") +
  theme(legend.title=element_blank()) +
  #theme(legend.position = "inside", legend.position.inside = c(.8, .85)) +
  geom_errorbar(aes(ymax=tbiomass_ucl, ymin=tbiomass_lcl), size=1, width=0.30, position=position_dodge(.9)) +
  geom_hline(yintercept = 23940, colour="red", linetype="dashed") +
  annotate("text", x = 2.6, y = 25000, label = "'No Net Loss'", colour="red") +
  scale_fill_manual(labels = ~ stringr::str_wrap(.x, width = 15), values=c("dark grey", "red")) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

RB_nonetloss_plot

dev.off()
par(mfrow=c(1,1))



