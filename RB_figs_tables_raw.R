# REVISION - RAW ----
## After a great deal of thought, I decided that bootstraping is not the way to go.  See MMM_data in archive electrofishing for an explanation.  Briefly, bootstrapping makes no sense when only a few samples are available and the data are not normally distributed.  In this case, we have 3-4 samples per year and treatment and the data are not normally distributed.  So, I am going to just report the raw means and standard deviations for each year and treatment.  I will also report the total mean and standard error across all years for each treatment.  I will make tables for both density and biomass.  I will also make a table that summarizes the parameter estimates from the models.

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(cowplot)
df_b <- read.csv("data_derived/df_a3.csv")
str(df_b)

## split data ----
df_b_split <- df_b |> 
  filter(Year != "Total") |>
  split(df_b$Species)
str(df_b_split)

### density ----
plot_den <- map(names(df_b_split), function(Species) {
  df <- df_b_split[[Species]]  
  ggplot(df, 
         aes(x = as.factor(Year), y = abun.stand, fill = type, colour = type, shape = type)) + 
    geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.3), size = 3) +
    theme_bw(base_size = 20) + 
    theme_bw(base_size = 20) + 
    ylab(expression("Density Estimate (#/100 m" ^2*")")) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position= "none") +
    scale_fill_discrete(name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment"))
})

names(plot_den) <- paste0(names(df_b_split))
list2env(plot_den, envir = .GlobalEnv)
p1 <- plot_den$AS
p2 <- plot_den$BT
p3 <- plot_den$BTYOY
p4 <- plot_den$ASYOY

# save iteratively
plots <- list(
  AS = p1,
  BT = p2,
  BTYOY = p3,
  ASYOY = p4
)

for (nm in names(plots)) {
  ggsave(
    filename = paste0("figs/raw_species", nm, "_density_age.png"),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank())
p2_clean <- p2 + theme(axis.title = element_blank())
p3_clean <- p3 + theme(axis.title = element_blank())
p4_clean <- p4 + theme(axis.title = element_blank())

grid_den <- plot_grid(p1_clean, 
                      p4_clean,
                      p2_clean, 
                      p3_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("Atlantic salmon age-1+",
                                 "Atlantic salmon YOY",
                                 "Brook trout age -1+",
                                 "Brook trout YOY"),
                      # hjust = -3, 
                      hjust = -0.5, 
                      vjust = 1.25)

# Add shared axis labels
final_plot_den <- ggdraw(grid_den) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Density Estimate (#/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14)
final_plot_den

save_plot("figs/raw_species_den.png", 
          final_plot_den, 
          base_height = 6, 
          base_width = 10,
          bg = "white")



### biomass ----
plot_bio <- map(names(df_b_split), function(Species) {
  df <- df_b_split[[Species]]  
  ggplot(df, 
         aes(x = as.factor(Year), y = bio.stand, fill = type, colour = type, shape = type)) + 
    geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.3), size = 3) +
    theme_bw(base_size = 20) + 
    theme_bw(base_size = 20) + 
    ylab(expression("Biomass Estimate (g/100 m" ^2*")")) +
    xlab("Year") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position= "none") +
    scale_fill_discrete(name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment"))
})

names(plot_bio) <- paste0(names(df_b_split))
list2env(plot_bio, envir = .GlobalEnv)
p1 <- plot_bio$AS
p2 <- plot_bio$BT
p3 <- plot_bio$BTYOY
p4 <- plot_bio$ASYOY

# save iteratively
plots <- list(
  AS = p1,
  BT = p2,
  BTYOY = p3,
  ASYOY = p4
)

for (nm in names(plots)) {
  ggsave(
    filename = paste0("figs/raw_species", nm, "_biomass_age.png"),
    plot = plots[[nm]],
    width = 6, height = 4, dpi = 300
  )
}

#### combine ----
p1_clean <- p1 + theme(axis.title = element_blank())
p2_clean <- p2 + theme(axis.title = element_blank())
p3_clean <- p3 + theme(axis.title = element_blank())
p4_clean <- p4 + theme(axis.title = element_blank())

grid_bio <- plot_grid(p1_clean, 
                      p4_clean,
                      p2_clean, 
                      p3_clean, 
                      ncol = 2, align = "hv", axis = "tblr",
                      scale = 0.9,
                      #labels = c("AS", "ASY","BT", "BTY"),
                      labels = c("Atlantic salmon age-1+",
                                 "Atlantic salmon YOY",
                                 "Brook trout age -1+",
                                 "Brook trout YOY"),
                      # hjust = -3, 
                      hjust = -0.5, 
                      vjust = 1.25)

# Add shared axis labels
final_plot_bio <- ggdraw(grid_bio) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 14) +
  draw_label(expression("Biomass Estimate (g/100 m" ^2*")"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 14)
final_plot_bio

save_plot("figs/raw_species_bio.png", 
          final_plot_bio, 
          base_height = 6, 
          base_width = 10,
          bg = "white")


# summaries ----
# get df_b in archival format
head(df_b)
df_b$study_area <- "Rose Blanche"
df_b_arch <- df_b |>
  rename(species = Species, year = Year, site = Station, trt = type, mean_den = abun.stand, mean_bio = bio.stand) |>
  select(study_area, species, year, site, trt, mean_den, mean_bio)

df_b_arch$age_new <- if_else(df_b_arch$species %in% c("AS", "BT"), "age-1+", "YOY")
df_b_arch$species <- if_else(df_b_arch$species == "ASYOY", "AS", df_b_arch$species)
df_b_arch$species <- if_else(df_b_arch$species == "BTYOY", "BT", df_b_arch$species)

write.csv(df_b_arch, "../../archival_data/archive_electrofish/data_derived/MMM/RB_age_2000_2015.csv", , row.names = F)

RB_site <- df_b_arch |>
  group_by(study_area, species, age_new, year, trt) |>
  summarise(n = n(),
            min_den = min(mean_den), 
            max_den = max(mean_den),
            mean_den = mean(mean_den),
            sd_den = sd(mean_den, na.rm = T),
            min_bio = min(mean_bio),
            max_bio = max(mean_bio),
            mean_bio = mean(mean_bio),
            sd_bio = sd(mean_bio, na.rm = T)
  )
write.csv(RB_site, "../../archival_data/archive_electrofish/data_derived/MMM/RB_site_2000_2015.csv", , row.names = F)


RB_year <- df_b_arch |>
  group_by(study_area, species, age_new, trt) |>
  summarise(n = n(),
            min_den = min(mean_den), 
            max_den = max(mean_den),
            mean_den = mean(mean_den),
            sd_den = sd(mean_den, na.rm = T),
            min_bio = min(mean_bio),
            max_bio = max(mean_bio),
            mean_bio = mean(mean_bio),
            sd_bio = sd(mean_bio, na.rm = T)
  )
write.csv(RB_year, "../../archival_data/archive_electrofish/data_derived/MMM/RB_yr_2000_2015.csv", , row.names = F)
