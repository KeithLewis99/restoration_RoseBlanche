# This was supposed to JUST create a table for the parameter estimates much like what I did in Seal Cove.  However, after the decision to just compare GC to other sites, I added tables that summarize the density/biomass by species per year. 

## Param CIs and Bootstrapped CIs
### Density

# Param Estimates ----
## load files ----
# create pattern
temp1 = list.files(path = "output", pattern=".*_ci.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
  name = gsub(".*output/", "", x)
  name = gsub("_ci.csv", "", paste0(name, "_ci"))
  return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_sc_ci1 = (lapply(temp1, read.csv))
names(ls_sc_ci1) <- name_files(temp1)
list2env(ls_sc_ci1, envir = .GlobalEnv)

bt_den_ci$species <- "BT"
btyoy_den_ci$species <- "BTYOY"
as_den_ci$species <- "AS"
asyoy_den_ci$species <- "ASYOY"

# bind CIs from param estimates
tab_den <- rbind(bt_den_ci[1:6,-1], 
                 btyoy_den_ci[1:2,-1], 
                 as_den_ci[1:2, -1], 
                 asyoy_den_ci[1:2, -1])

bt_bio_ci$species <- "BT"
btyoy_bio_ci$species <- "BTYOY"
as_bio_ci$species <- "AS"
asyoy_bio_ci$species <- "ASYOY"

tab_bio <- rbind(bt_bio_ci[1:6,-1], 
                 btyoy_bio_ci[1:2,-1], 
                 as_bio_ci[1:2, -1], 
                 asyoy_bio_ci[1:2, -1])

# change the parameter names
tab <- cbind(tab_den, tab_bio[, 2:4])
tab$parm[tab$parm == "cond.(Intercept)"] <- "Int"
tab$parm[tab$parm == "cond.typetrt"] <- "Type"
tab$parm[tab$parm == "cond.north"] <- "North"
tab$parm[tab$parm == "zi.(Intercept)"] <- "zi:Int"

## table: params ----
### table of density and biomass estimates from GLMMs
library(kableExtra)
kbl(tab[, c(5, 1, 4, 2:3, 8, 6:7)], 
    col.names = c('spp', 'parm', 'Estimate', '2.5%', '97.5%',
                  'Estimate', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Density" = 3, "Biomass" = 3)) |>
  add_header_above(header = c(" " = 2, "Summer" = 6)) |>
  kable_paper()


tabC <- tab[, c(5, 1, 4, 2:3, 8, 6:7)]
tabC$ci_den <- paste0(round(tabC$Estimate, 2), 
                     " (", 
                     round(tabC$X2.5., 2), 
                     ", ",
                     round(tabC$X97.5., 2), 
                     ")")
tabC$ci_bio <- paste0(round(tabC$Estimate.1, 2), 
                     " (", 
                     round(tabC$X2.5..1, 2), 
                     ", ",
                     round(tabC$X97.5..1, 2), 
                     ")")

# the above table but with CIs in parantheses
kbl(tabC[, c(1:2, 9,10)], 
    col.names = c('Species-age', 'Parameter', 'Density',
                  'Biomass'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Density" = 1, "Biomass" = 1)) |>
  #add_header_above(header = c(" " = 2, "Summer" = 6)) |>
  kable_paper()


# Bootstrap estimates ----
# The below is tables and bootstrapped values to get CIs around density and biomass estimates that don't overlap zero.  
# the corresponding figures are in RB_figs_new under bootstrap
## using kable() and kableExtra() - see website: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Grouped_Columns__Rows

## import data ----
df_a <- read.csv("data_derived/df_a3.csv")


## density - CI ----

# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)
library(dplyr)
library(kableExtra)
library(tidyr)

spp_den.ci <- df_a |>
  group_by(Species, Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_den.ci$ci <- paste0("(", round(spp_den.ci$ll, 1), ", ", round(spp_den.ci$ul, 1), ")")
spp_den.ci


### total density ----
spp_den_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_den_tot.ci$ci <- paste0("(", round(spp_den_tot.ci$ll, 1), ", ", round(spp_den_tot.ci$ul, 1), ")")
spp_den_tot.ci



### all density ----
all_den_ci <- rbind(spp_den.ci, spp_den_tot.ci)
all_den_ci$Year <- ifelse(is.na(all_den_ci$Year), "Total", all_den_ci$Year)
all_den_ci$mci <- paste(round(all_den_ci$mean, 2), "",  all_den_ci$ci)

write.csv(all_den_ci, "data_derived/density_ci.csv")


# combine mean and ci
all_den_ci_tabC <- pivot_wider(all_den_ci,
                               id_cols = c(Species, type),
                               names_from = c(Year),
                               values_from = c(mci)
)

all_den_ci_tabC$Species <- factor(all_den_ci_tabC$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
all_den_ci_tabC <- all_den_ci_tabC[order(all_den_ci_tabC$Species),]
str(all_den_ci_tabC, give.attr = F)
write.csv(all_den_ci_tabC, "data_derived/density_ci_tabC.csv")

# make table
kbl(all_den_ci_tabC,
    col.names = c('spp', 'type',
                  '2000',
                  '2001',
                  '2002',
                  '2016',
                  'total'),
    align = 'c', caption = "Density CIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()


## density by year
#### I'm not sure if this is really needed, i.e., this does not match Kristin's figures which are for the whole year; but there is a reason that you can't match.  I could so a sum of all fish or biomass in a year but then that is what I would be stuck with - a number.  This is not awful but there would be no confidence intervals around it.  Also, i'm not sure what the point of it is.  A yearly trend in fish and biomass.  Largely driven by ASY.

# year_den_tot.ci <- df_b |>
#   filter(Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Year, trt, type) |>
#   #need to get a total here
#   do(data.frame(rbind(Hmisc::smean.cl.boot(.$T.stand)))) |>
#   rename(mean = Mean, ll = Lower, ul = Upper)
# 
# year_den_tot.ci$ci <- paste0("(", round(year_den_tot.ci$ll, 1), ", ", round(year_den_tot.ci$ul, 1), ")")
# year_den_tot.ci
# 
# write.csv(year_den_tot.ci, "data_derived/density_all_year.csv")
# 



## biomass - CI ----
spp_bio.ci <- df_a |>
  group_by(Species, Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio.ci$ci <- paste0("(", round(spp_bio.ci$ll, 1), ", ", round(spp_bio.ci$ul, 1), ")")
spp_bio.ci
# spp_bio.ci$mci <- paste(round(spp_bio.ci$mean, 2), "",  spp_bio.ci$ci)


### total biomass ----
spp_bio_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio_tot.ci$ci <- paste0("(", round(spp_bio_tot.ci$ll, 1), ", ", round(spp_bio_tot.ci$ul, 1), ")")
spp_bio_tot.ci
# spp_bio_tot.ci$mci <- paste(round(spp_bio_tot.ci$mean, 2), "",  spp_bio_tot.ci$ci)



### all biomass ----
all_bio_ci <- rbind(spp_bio.ci, spp_bio_tot.ci)
all_bio_ci$Year <- ifelse(is.na(all_bio_ci$Year), "Total", all_bio_ci$Year)
all_bio_ci$mci <- paste(round(all_bio_ci$mean, 2), "",  all_bio_ci$ci)
write.csv(all_bio_ci, "data_derived/biomass_ci.csv")


# combine mean and ci
all_bio_ci_tabC <- pivot_wider(all_bio_ci,
                               id_cols = c(Species,  type),
                               names_from = c(Year),
                               values_from = c(mci)
)
#View(all_den_ci_tab)
all_bio_ci_tabC$Species <- factor(all_bio_ci_tabC$Species, levels = c("BT", "BTYOY", "AS", "ASYOY"))
all_bio_ci_tabC <- all_bio_ci_tabC[order(all_bio_ci_tabC$Species),]

write.csv(all_bio_ci_tabC, "data_derived/biomass_ci_tabC.csv")

# make table
kbl(all_bio_ci_tabC,
    col.names = c('spp', 'type',
                  '2000',
                  '2001',
                  '2002',
                  '2015',
                  'total'),
    align = 'c', caption = "Biomass CIs", digits = 2 ) |>
  collapse_rows(valign = "top",
                latex_hline = "major") |>
  add_header_above(header = c(" " = 2, "Year" = 5)) |>
  kable_paper()


# see note above in commented out section "densit by year"
## biomass by year
# year_bio_tot.ci <- df_b |>
#   filter(Year != 2006 & trt != "con" & Month != "Sept") |>
#   group_by(Year, trt, type) |>
#   
#   do(data.frame(rbind(Hmisc::smean.cl.boot(.$B.stand)))) |>
#   rename(mean = Mean, ll = Lower, ul = Upper)
# 
# year_bio_tot.ci$ci <- paste0("(", round(year_bio_tot.ci$ll, 1), ", ", round(year_bio_tot.ci$ul, 1), ")")
# year_bio_tot.ci
# 
# 
# write.csv(year_bio_tot.ci, "data_derived/biomass_all_year.csv")

## percent change ----
all_bio_ci |> filter(Species == "AS")
((all_bio_ci[8,4] - all_bio_ci[6,4])/all_bio_ci[6,4])*100
all_bio_ci |> filter(Species == "ASYOY")
((all_bio_ci[16,4] - all_bio_ci[14,4])/all_bio_ci[14,4])*100

# all salmonids ----
### Bootstrap


## import data ----
df_a <- read.csv("data_derived/df_a3.csv")


## density - CI ----

# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)
library(dplyr)
library(kableExtra)
library(tidyr)

# add up density or biomass by station, then scale by area, then bootstrap
tmp <- df_a |>
  group_by(Year, Station, Area) |> 
  summarise(sum_den = sum(abun)) 

tmp$abun.stand <- tmp$sum_den/tmp$Area*100
tmp$trt <- ifelse(tmp$Station <= 7, "trt", "con")

sal_den_stn.ci <- tmp |>
  group_by(Year, trt) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

write.csv(sal_den_stn.ci, "data_derived/sal_density_ci.csv")


## biomass - CI ----
tmp <- df_a |>
  group_by(Year, Station, Area) |> 
  summarise(sum_bio = sum(bio)) 

tmp$bio.stand <- tmp$sum_bio/tmp$Area*100
tmp$trt <- ifelse(tmp$Station <= 7, "trt", "con")

sal_bio_stn.ci <- tmp |>
  group_by(Year, trt) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
str(sal_bio_stn.ci)

write.csv(sal_bio_stn.ci, "data_derived/sal_biomass_ci.csv")

# 2000
bio2000 <- tmp |> filter(Year == 2000 & trt == "trt")
bio2000_boot <- Hmisc::smean.cl.boot(bio2000$bio.stand,reps = T)
str(tmp3)
bio2000_boot_vals <- attr(bio2000_boot, "reps") 
plot(density(bio2000_boot_vals))
length(bio2000_boot_vals)
length(bio2000_boot_vals[bio2000_boot_vals > 239])

# 2001
bio2001 <- tmp |> filter(Year == 2001 & trt == "trt")
bio2001_boot <- Hmisc::smean.cl.boot(bio2001$bio.stand,reps = T)
bio2001_boot_vals <- attr(bio2001_boot, "reps") 
plot(density(bio2001_boot_vals))
length(bio2001_boot_vals)
length(bio2001_boot_vals[bio2001_boot_vals > 239])

# 2002
bio2002 <- tmp |> filter(Year == 2002 & trt == "trt")
bio2002_boot <- Hmisc::smean.cl.boot(bio2002$bio.stand,reps = T)
str(tmp3)
bio2002_boot_vals <- attr(bio2002_boot, "reps") 
plot(density(bio2002_boot_vals))
length(bio2002_boot_vals)
length(bio2002_boot_vals[bio2002_boot_vals > 239])

length(bio2000_boot_vals[bio2000_boot_vals > 239])/1000*100
length(bio2001_boot_vals[bio2001_boot_vals > 239])/1000*100
length(bio2002_boot_vals[bio2002_boot_vals > 239])/1000*100

# END ----