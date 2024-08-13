# This is just my attempt to consolidate Kristin Loughlin's code in a way that I understand it.  This file is for importing data and manipulating it for the various graphs.  I can now create all or almost all of Kristin's plots for Rose Blanche.

## Rose Blance

# libraries ----
library(plyr)


# read data ----
## 2000-2015 with averages of species by year and site (Type: Compensation v main stream)
### site in this file is just RB, Station is Site 1-7 are Compensation and 8-10 are Mainstream. #### THIS IS THE "RAW" DATA, i.e., its what is distilled from the AMEC code.  The true raw data is the number of fish detected in each electrofishing sweep which the AMEC code converts into.  BUT - THIS HAS no CIs.    

RB_carlestrub_output_each_spp_by_site <- read.csv("../Carle Strub Estimates/RB_carlestrub_output_each_spp_by_site.csv")

RB_carlestrub_output_each_spp_by_site_all <- read.csv("../Carle Strub Estimates/RB_carlestrub_output_each_spp_by_site_all.csv")


str(RB_carlestrub_output_each_spp_by_site_all)

## 2000-2015 with averages of species by year and site with the CI
## this is the same values as in file RB_carlestrub_output_each_spp_maincomp.xlsx for stand.species.biomass.contr
RB_spp_maincomp <- read.csv("../Carle Strub Estimates/RB_spp_maincomp.csv")
RB_spp_maincomp$Year <- as.factor(RB_spp_maincomp$Year)
allspp_comp <- subset(RB_spp_maincomp, RB_spp_maincomp$Station=="Compensation")

## 2000-2015 - averages for salmonids by site with CI
RB_salmonids_maincomp <- read.csv("../Carle Strub Estimates/RB_salmonids_maincomp.csv")
RB_salmonids_maincomp$Year <- as.factor(RB_salmonids_maincomp$Year)

## 1998-2015 - averages for salmonids by year with CI (Destroyed v Compensated)
RB_nonetloss <- read.csv("../Carle Strub Estimates/RB_nonetloss.csv")
RB_nonetloss$Year <- as.factor(RB_nonetloss$Year)

## mean biomass by year with sd
RB_MeanBioComp <- read.csv("../Carle Strub Estimates/RB_MeanBioComp.csv")

## 1998-2015 - averages for salmonids by year with CI (Destroyed v Compensated)
RB_meansalmonidsbysite <- 
  read.csv("../Carle Strub Estimates/RB_meansalmonidsbysite.csv")



# data manipulation ----
##
RB_Spp_Biomass_cs <- ddply(RB_carlestrub_output_each_spp_by_site, c("Year", "Type", "Species"), summarise,
                           N    = length(biomass_cs),
                           mean = mean(biomass_cs),
                           sd   = sd(biomass_cs),
                           se   = sd / sqrt(N)  # this is not the right standard error; see scratch_pad.R and ReadMe for explanation
)

RB_Spp_Biomass_cs

##
RB_Spp_Density <- ddply(RB_carlestrub_output_each_spp_by_site, c("Year", "Type", "Species"), summarise,
                        N    = length(density),
                        mean = mean(density),
                        sd   = sd(density),
                        se   = sd / sqrt(N) # this is not the right standard error; see scratch_pad.R and ReadMe for explanation
)

RB_Spp_Density



# END ----