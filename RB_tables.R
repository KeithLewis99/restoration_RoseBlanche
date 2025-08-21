# The below is tables and bootstrapped values to get CIs around density and biomass estimates that don't overlap zero.  

## using kable() and kableExtra() - see website: https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Grouped_Columns__Rows

df_a <- read.csv("data_derived/df_a3.csv")
df_a |> filter(Year == 2001) 
# density - CI ----

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


## total density ----
spp_den_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)
# this works.  The means are the same as if calculated in dplyr and the CI's make sense relative to the means.

spp_den_tot.ci$ci <- paste0("(", round(spp_den_tot.ci$ll, 1), ", ", round(spp_den_tot.ci$ul, 1), ")")
spp_den_tot.ci



## all density ----
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


## density by year -----
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



# biomass - CI ----
spp_bio.ci <- df_a |>
  group_by(Species, Year, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio.ci$ci <- paste0("(", round(spp_bio.ci$ll, 1), ", ", round(spp_bio.ci$ul, 1), ")")
spp_bio.ci
# spp_bio.ci$mci <- paste(round(spp_bio.ci$mean, 2), "",  spp_bio.ci$ci)


## total biomass ----
spp_bio_tot.ci <- df_a |>
  group_by(Species, type) |>
  do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
  rename(mean = Mean, ll = Lower, ul = Upper)

spp_bio_tot.ci$ci <- paste0("(", round(spp_bio_tot.ci$ll, 1), ", ", round(spp_bio_tot.ci$ul, 1), ")")
spp_bio_tot.ci
# spp_bio_tot.ci$mci <- paste(round(spp_bio_tot.ci$mean, 2), "",  spp_bio_tot.ci$ci)



## all biomass ----
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


## biomass by year -----
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

# END ----