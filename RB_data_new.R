# the purpose of this file is to take the data for Rose Blanche and make it available for general use for Carle STrub, unmarked, and Bayesian analyses


# Set up a project - see the below link for directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - Version Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("data_derived"))dir.create("data_derived")
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report

# Source ----
source("RB_fun.R")

# library ----
library(tidyr)
library(dplyr)
library(ggplot2)


# import ----
## import files in catch and convert to proper format
## create a pattern and bind directory to pattern
temp = list.files(path = "../data/RB_year/", pattern="RoseBlanche.*bysite.csv$", full.names = T)

## import files as a list
ls_rb = (lapply(temp, read.csv))
str(ls_rb)
str(ls_rb,1)
str(ls_rb[1])

unique(ls_rb[1]$`2000`$Species)
# standardize names
names(ls_rb) <- c("2000", "2001", "2002", "2015")

## make all classes the same for each variable across lists
ls_rb[["2000"]]$Fish.ID <- as.integer(ls_rb[["2000"]]$Fish.ID)
ls_rb[["2000"]]$Weight.g <- as.numeric(ls_rb[["2000"]]$Weight.g)
ls_rb[['2001']] <- ls_rb[["2001"]][,1:8]

ls_rb[["2015"]]$Station <- as.numeric(regmatches(ls_rb[["2015"]]$Station, regexpr("\\d+", ls_rb[["2015"]]$Station)))
str(ls_rb)

## create either a large dataframe and then do some summaries
## as above, create summaries for FSA
df_all <- bind_rows(ls_rb)
unique(df_all$Site)
str(df_all)

## note that there is no area for 2001 - this gets added below in "area"
## 1599 records
# write.csv(df_all, "data_derived/df_all.csv")

# standardize data across years
## remove EEL
df_all <- df_all |>
  filter(Species != "EEL")


# sum catch ----
## first, create a table for T
df_sum <- df_all |>
  group_by(Year, Species, Station, Sweep) |>
  summarise(bio.sum = sum(Weight.g), 
            abun = n(), 
            ) 
str(df_sum, give.attr = F)

# 313 records
# write.csv(df_sum, "data_derived/df_sum.csv")

# grid ----
# create dataset with all possible combinations of the following variables
year <- as.character(unique(df_sum$Year))
station <- unique(df_sum$Station)
species <- c("AS", "ASYOY", "BT", "BTYOY")
sweep <- c(1:max(df_sum$Sweep))

# make grid
df_grid <- expand.grid(Year = year, 
                       Species = species,
                       Station = station,
                       Sweep = sweep) |> 
  arrange(Year, Species, Station, Sweep)
#str(df_grid)

# write.csv(df_grid, "data_derived/df_grid.csv")

# edit grid ----

# get max by Year and Station
df_sweep <- df_sum |> 
  group_by(Year, Station) |>
  summarise(max_sweep = max(Sweep)) |> 
  pivot_wider(id_cols = Year,
              names_from = Station,
              values_from = max_sweep)


# edit grid
# remove the structural zeros, i.e., sites that weren't fished; assume that 5 sweeps were conducted in 2000 but only 4 in later years.  Anything below the max for a year is a real zero, not a structural one.
df_grid1 <- df_grid |>
  filter(!(Year == 2001 & Sweep == 5) &
           !(Year == 2002 & Sweep == 5) &
         !(Year == 2015 & Sweep == 5)
  )

#str(df_grid1, give.attr = F)
df_grid1$Year <- as.integer(as.character(df_grid1$Year))
df_grid1$Sweep <- as.integer(df_grid1$Sweep)
# write.csv(df_grid1, "data_derived/df_grid1.csv")
#str(df_grid1, give.attr = F) # 680 records


# join grid and summary ----
# now, join the two dataframes - sites with no fish caught are NA; 680 records
df_all1 <- full_join(df_grid1, df_sum[, 1:6], by = c("Year", "Species", "Station", "Sweep")) |>
  arrange(Year, Species, Station, Sweep)
#str(df_all1, give.attr = F)
nrow(df_all1 |> filter(Sweep > 3)) # 202 rows with Sweep > 3 

# write.csv(df_all1, "data_derived/df_all1.csv")

# sum previous catch----
## get max Sweep ----
# this is for below where I remove the extra sweeps
## its really a redundant step - not needed but not changing it just to make sure i'm not pulling out a critical thread needed somewhere else.
df_stn_tag <- df_all1 |>
  group_by(Year) |>
  filter(!is.na(abun)) |>
  summarise(tag = max(Sweep)) |>
  print(n = Inf)

# # this may be redundant with above
# df_stn_tag_all <- df_all1 |>
#   group_by(Year, Station) |>
#   filter(!is.na(abun)) |>
#   summarise(tag = max(Sweep)) |>
#   print(n = Inf)
# plot(density(df_stn_tag_all$tag))
# 
# 
# # join all and tags
df_all2 <- full_join(df_all1, df_stn_tag, by = c("Year")) |>
  arrange(Year, Species, Station, Sweep) |>
  filter( Sweep <= tag) #!is.na(abun) &
# str(df_all2, give.attr = F)
# df_all2$Area[is.na(df_all2$Area)]


## NA to zero ----
df_all2 <- df_all2 |>
  replace_na(list(bio.sum = 0, abun = 0)) 

df_all2 <- df_all1 |>
  replace_na(list(bio.sum = 0, abun = 0)) 

# write.csv(df_all2, "data_derived/df_all2.csv") # 680 records


## spc ----
# calculate sum of previous catch
df_all2$spc <- NA

# calculate spc and flag sites without a Sweep == 1; more negative means more Sweeps before fish is found
df_all2 <- df_all2 |>
  group_by(Year, Species, Station) |>
  #summarise(min = min(Sweep))
  # case_when is vectorized if-else: so when Sweep ==1, spc is 0, when Sweep ==2 & there is a Sweep ==1, abundance, else -1, when Sweep ==3, if there is a Sweep ==1, sum Sweep 1 & 2, else -2, etc.
  mutate(spc = case_when(
    Sweep == 1 ~ 0,
    Sweep == 2 ~ ifelse(any(Sweep == 1), abun[Sweep ==1], -1),
    Sweep == 3 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2])), -2),
    Sweep == 4 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3])), -3),
    Sweep == 5 ~ ifelse(any(Sweep == 1), sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3 | Sweep == 4])), -4)
  ))

#View(arrange(df_all2, Year, Station,  Sweep, Species))
# str(df_all2, give.attr = F)


## spc plot ----
### year by spp

p <- ggplot(
  #df_all2 |> filter(Species == "AS"|Species == "ASYOY"),
  df_all2 |> filter(Species == "BT"|Species == "BTYOY"),
  aes(x = spc, y = abun, 
      group = Station, fill = Station,
      text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
  )) +
  geom_point() +
  geom_path() +
  facet_grid(Year ~ Species)

p
plotly:: ggplotly(p, tooltip = "text")


### subset
p <- ggplot(
  df_all2[df_all2$Species == "BTYOY",], 
  # df_all2[df_all2$Species == "BTYOY" & df_all2$Year == 2000,], 
  aes(x = spc, y = abun, 
      group = Station, fill = Station,
      text = paste("SPC: ", spc, "\n",
                   "Abund: ", abun, "\n",
                   "Stn: ", Station, "\n",
                   "Sweep: ", Sweep,
                   sep = "")
  )) +
  geom_point() +
  geom_path()
p

# write.csv(df_all2[df_all2$Species == "AS" & df_all2$Year == 2015,], "data_derived/spc_example.csv") # 40 records
plotly::ggplotly(p, tooltip = "text")


# df for analysis ----
df_a <- df_all2 |> 
  filter(Sweep <= 3) |>
  group_by(Year, Station, Species) |>
  summarise(abun = sum(abun),
            bio = sum(bio.sum))

# write.csv(df_a, "data_derived/df_a.csv")


## variables ----
df_a$type <- NA

df_a <- df_a |>
  mutate(type = if_else(Station >= 8, "con", "trt"))
df_a$type <- as.factor(df_a$type)
str(df_a, give.attr=FALSE)  


## area ----
station <- as.character(1:10)
library(readxl)
area_2001 <- read_excel("../data/Rose Blanche - EF Site Dimensions.xls", 
           sheet = "August 2001", 
           range = "A3:D13") |>
  rename(Station = ...1, Area = area)
area_2001$Year <- 2001

df_area <- df_all |>
  group_by(Year, Station) |>
  summarise(Area = first(Area)) |>
  filter( Year != 2001)

df_area <- rbind(df_area, area_2001[, c(5, 1, 4)]) |>
  arrange(Year)
df_area |> print(n = Inf)

df_a <- left_join(df_a, df_area, by = c("Year", "Station"))

df_a <- df_a |>
  group_by(Year, Species, Station) |>
  mutate(abun.stand = abun/Area*100, bio.stand = bio/Area*100)


## lat-long ----
df_loc <- read.csv("../data/waypoints_RB.csv")
df_loc <- df_loc |>
  filter(!(Site == "rb8pp" |
             Site == "Rbcsixtpp" |
             Site == "Site 3"  |
             Site == "Site rbc4" |
             Site == "Rbmaim9tp")
         )
df_a <- left_join(df_a, df_loc[, 2:4], by = c("Station" = "sites")) 

str(df_a, give.attr=F)
df_a$Station <- as.factor(df_a$Station)
df_loc$sites <- as.factor(df_loc$sites)
# write.csv(df_a, "data_derived/df_a3.csv")



# summary stats ----
df_sumBT <- tab_type(df_a, "BT", abun.stand)
df_bio_sumBT <- tab_type(df_a, "BT", bio.stand)

df_sumAS <- tab_type(df_a, "AS", abun.stand)
df_bio_sumAS <- tab_type(df_a, "AS", bio.stand)

df_sumBTYOY <- tab_type(df_a, "BTYOY", abun.stand)
df_bio_sumBTYOY <- tab_type(df_a, "BTYOY", bio.stand)

df_sumASYOY <- tab_type(df_a, "ASYOY", abun.stand)
df_bio_sumASYOY <- tab_type(df_a, "ASYOY", bio.stand)



## totals ----
length(unique(df_sum$Year))
length(unique(df_sum$Station))

# total year:spp:site:catch
nrow(df_sum)
nrow(df_a)

# total year:spp:site
df_sum |> group_by(Year, Species, Station) |> 
  summarise (catch_num = n()) |> 
  ungroup() |>
  summarise(tot = n())

# zeros - 31 of these
df_a |>
  filter(abun == 0)


## tables ----
## number of sweeps per station
df_sum |>
  group_by(Year, Species, Station) |>
  summarize(Sweeps = length(Sweep)) |>
  pivot_wider(id_cols = c(Year, Species), 
              names_from = Station, values_from = Sweeps)

# 4-5 passes
df_all2$pass_no <-NA

# summarize just 4-5-pass sites and had fish
df_4_5pass <- df_all2 |>
  group_by(Year, Species, Station) |>
#  mutate(pass_no = ifelse(max(Sweep )<=3, 3, 5)) |>
  mutate(pass_no = ifelse(Sweep <=3, 3, 5)) |>
  ungroup() |>
  filter(pass_no == 5) |> 
  group_by(Year, Species, Station, Sweep) |>
#  summarize(count = n()) |>
  # pivot_wider(names_from = Sweep, values_from = count, values_fill = 0)
  filter(abun > 0) |>
  pivot_wider(names_from = Sweep, values_from = abun, values_fill = 0) 
# |>
#   relocate(`1`, .after = Station) |>
#   relocate(`4`, .after = `3`)

df_4_5pass |> print(n = Inf)

str(df_4_5pass, give.attr = F)

## The below is used in RB_abun_bio.R for the FSA Carle-Strub estimates.
# NOTE: WHEN USING:
### filter(length(Sweep) > 1 & Sweep <= 3) - this give sites where there were at least 2 sweeps but excludes 4 and 5 - this is inappropriate for any analysis involving a catchability estimate.  
### filter(Sweep <= 3)would be appropriate for using T
### filter(!(is.na(`2`) & is.na(`3`))) - without this, you still get one catch value

df_tab1 <- df_all2 |>
  group_by(Year, Species, Station) |>
  #filter(length(Sweep) > 1 & Sweep <= 3) |>
  filter(Sweep <= 3) |>
  ungroup() |>
  pivot_wider(id_cols = c(Year, Species, Station),
              names_from = Sweep, values_from = c(abun, bio.sum)) |> 
  filter(!(is.na(`abun_2`) & is.na(`abun_3`))) 
df_tab1[df_tab1$Species == "BT" & df_tab1$Year == 2001,]

df_tab1 |> print(n = Inf)

# write.csv(df_tab1, "data_derived/df_tab1.csv")

# temp is same as df_tab1 but without the last filter
## the query above does not get rid of stations with captures on Sweep 1 (or 2 or 3) & 4 or 5 (all three have captures on sweep 3)
anti_join(df_4_5pass, df_tab1,  by = c('Year', 'Species', 'Station'))


# sum by year and species
df_tab2 <- df_all2 |>
  group_by(Year, Species, Station) |>
  pivot_wider(id_cols = c(Year, Species, Station), 
              names_from = Sweep, values_from = abun) #bio.sum abun
str(df_tab2, give.attr = F)
#View(df_tab2)
# write.csv(df_tab2, "data_derived/df_tab2.csv")

 
df_aj2 <- anti_join(df_tab2, df_tab1, by = c('Year', 'Species', 'Station'))


# sum catches by year and species
df_tab2 |>
  group_by(Year, Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
  )

# sum catches by species for 3 passes - both of this and the below sum to XXXX
df_tab2 |>
  group_by(Species) |>
  summarise(sum_c1 = sum(`1`, na.rm = T),
            sum_c2 = sum(`2`, na.rm = T),
            sum_c3 = sum(`3`, na.rm = T)
  )


df_sum |> group_by(Species) |> filter(Sweep ==4 | Sweep ==5) |> summarise(sum = sum(abun))

df_tab2 |> filter(`1` == 0)
df_tab1 |> filter(abun_1 == 0)


# sum by catch for Year and Species 3 passes - this sum so XXXX which matches Excel
df_tab_T <- df_sum |>
  group_by(Year, Species) |>
  filter(Sweep <= 3) |>
  summarize(T = sum(abun)) |>
  pivot_wider(id_cols = c(Year), 
              names_from = Species, 
              values_from = T) 
df_tab_T # this is right????

# as above but summation of total sites and catches by year and species - but this is only for the site where fish were caught
df_tab_T <- df_sum |>
  group_by(Year, Species) |>
  filter(Sweep <= 3) |>
  summarize(T = sum(abun),
            n = n_distinct(Station)) |>
  #  ungroup() |>
  pivot_wider(id_cols = c(Year), 
              names_from = Species, 
              values_from = c(T, n)) 
df_tab_T
str(df_tab_T, give.attr = F)
# write.csv(df_tab_T[, c(1, 6, 2, 7, 3, 8, 4, 9, 5)], "data_derived/df_tab_T.csv")

### DEPRECATE
# this summary is for all sites that were fished irregardless of whether they had fish or not.  The assumption here is that there were 5 passes in 2000, and 4 in 2001, 2002, and 2015.  
#### this needs to be updated
# df_tab3 <- 
#   df_tab2 |>
#   mutate_at(c(5, 6), ~replace_na(.,0)) |>
#   mutate(`4` = case_when(
#     Year == 1990 | Year == 1991 | Year == 1996 
#     ~ ifelse(is.na(`4`), 0 , `4`))) |>
#   mutate(`5` = case_when(
#     Year == 1990 | Year == 1991 | Year == 1996 
#     ~ ifelse(is.na(`5`), 0 , `5`))) |> 
#   print(n = 140)
# #View(test)
# str(df_tab3, give.attr = F)
# nrow(df_tab3)
# #View(df_tab3 |> filter(!is.na(`1`)) |> count(`1`))
# sum(rowSums(!is.na(df_tab3[,4:8])))
# write.csv(df_tab3, "data_derived/df_tab3.csv")

# pool ----
# pool by Year and Station - this is for the Cote method
# df_tab_pool <- df_tab2 |>
#   group_by(Year, Station) |>
#   summarise(`1` = sum(`1`, na.rm = T), 
#             `2` = sum(`2`, na.rm = T), 
#             `3` = sum(`3`, na.rm = T),
#             `4` = sum(`3`, na.rm = T),
#             `5` = sum(`3`, na.rm = T))
# df_tab_pool |> print(n = Inf)
# 
# df_tab_pool_spc <- df_sum |>
#   group_by(Year, Station, Sweep) |>
#   summarise(bio.sum = sum(bio.sum), abun = sum(abun)) |>
#   mutate(spc = case_when(
#     Sweep == 1 ~ 0,
#     Sweep == 2 ~ abun[Sweep ==1],
#     Sweep == 3 ~ sum(c(abun[Sweep == 1 | Sweep == 2])),
#     Sweep == 4 ~ sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3])),
#     Sweep == 5 ~ sum(c(abun[Sweep == 1 | Sweep == 2 | Sweep == 3 | Sweep == 4]))
#   ))
# df_tab_pool_spc
# 
# p <- ggplot(
#   df_tab_pool_spc,
#   aes(x = spc, y = abun, 
#       group = Station, fill = Station,
#       text = paste("SPC: ", spc, "\n",
#                    "Abund: ", abun, "\n",
#                    "Stn: ", Station, "\n",
#                    "Sweep: ", Sweep,
#                    sep = "")
#   )) +
#   geom_point() +
#   geom_path() +
#   facet_grid(Year ~ Station)
# 
# p
# 
# 
# 
# # for analysis
# tmp1 <- 
#   df_tab1 |>
#   group_by(Year, Species, Station) |>
#   pivot_longer(
#     cols = starts_with("abun"),
#     values_to = "abun") |>
#   summarize(abun = sum(abun, na.rm = T))
# 
# tmp2 <- 
#   df_tab1 |>
#   group_by(Year, Species, Station) |>
#   pivot_longer(
#     cols = starts_with("bio"),
#     values_to = "bio") |>
#   summarize(bio = sum(bio, na.rm = T))
# 
# # df_a for analysis
# df_b <- full_join(tmp1, tmp2, by = c("Year", "Species", "Station"))
# df_b |> print(n = Inf)
# str(df_b, give.attr=F)
# # df_a$time <- NA
# # df_a$type <- NA
# # 
# # df_a <- df_a |>
# #   mutate(time = if_else(Year == 1990, "before", "after")) |>
# #   mutate(type = if_else(Station == "6"|Station == "7", "above", "below"))
# 
# # from RoseBlancheAug2001bysite.xlsx - these are in order by station so Station 1 == 221, and Station 10 == 413
# station <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
# area <- c(221, 216, 432, 361, 340, 239, 237, 252, 311, 413)
# 
# df_area <- data.frame(station, area)
# str(df_area)
# df_area$station <- as.numeric(df_area$station)
# 
# # join dataframes
# df_a <- left_join(df_a, df_area, by = c("Station" = "station"))
# df_a <- df_a |>
#   group_by(Year, Species, Station) |>
#   mutate(abun.stand = abun/area, bio.stand = bio/area)
# 
# # END ----
