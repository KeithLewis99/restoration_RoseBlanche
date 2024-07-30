# This is just my attempt to consolidate Kristin Loughlin's code in a way that I understand it.  This file is for importing data and manipulating it for the various graphs.  I can now create all or almost all of Kristin's plots for Rose Blanche.

## Rose Blance

# libraries ----
library(plyr)


# read data ----
##
RB_carlestrub_output_each_spp_by_site <- read.csv("../Carle Strub Estimates/RB_carlestrub_output_each_spp_by_site.csv")

##
RB_spp_maincomp <- read.csv("../Carle Strub Estimates/RB_spp_maincomp.csv")
RB_spp_maincomp$Year <- as.factor(RB_spp_maincomp$Year)
allspp_comp <- subset(RB_spp_maincomp, RB_spp_maincomp$Station=="Compensation")

##
RB_salmonids_maincomp <- read.csv("../Carle Strub Estimates/RB_salmonids_maincomp.csv")
RB_salmonids_maincomp$Year <- as.factor(RB_salmonids_maincomp$Year)

##
RB_nonetloss <- read.csv("../Carle Strub Estimates/RB_nonetloss.csv")
RB_nonetloss$Year <- as.factor(RB_nonetloss$Year)

##
RB_MeanBioComp <- read.csv("../Carle Strub Estimates/RB_MeanBioComp.csv")

##
RB_meansalmonidsbysite <- 
  read.csv("../Carle Strub Estimates/RB_meansalmonidsbysite.csv")

# data manipulation ----

##
RB_Spp_Biomass_cs <- ddply(RB_carlestrub_output_each_spp_by_site, c("Year", "Type", "Species"), summarise,
                           N    = length(biomass_cs),
                           mean = mean(biomass_cs),
                           sd   = sd(biomass_cs),
                           se   = sd / sqrt(N)
)

RB_Spp_Biomass_cs

##
RB_Spp_Density <- ddply(RB_carlestrub_output_each_spp_by_site, c("Year", "Type", "Species"), summarise,
                        N    = length(density),
                        mean = mean(density),
                        sd   = sd(density),
                        se   = sd / sqrt(N)
)

RB_Spp_Density


# END ----