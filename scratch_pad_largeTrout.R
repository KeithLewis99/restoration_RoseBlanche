# these are dot plots

# percent ----
ggplot(df_percent, 
       aes(x = as.factor(Year), y = prob_large)) + 
  theme_bw(base_size = 20) + 
  geom_dotplot(binaxis='y', stackdir='center', aes(fill = as.factor(trt), colour=as.factor(trt)), position=position_dodge(0.75), dotsize = 0.75) +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
               geom="crossbar", width=0.5, aes(colour=as.factor(trt)), position=position_dodge(0.75)) + # bards are 2xstandard deviation
  # geom_text(aes(label = count, y = prob_large, fill = as.factor(trt), colour=as.factor(trt)),
  #           colour = "red",
  #           position=position_dodge(width = 0.5),
  #           size = 4) +
  geom_text(df_percent_boot, mapping = aes(x = as.factor(Year), label = count, y = 1.1, group = as.factor(trt)), position=position_dodge(0.75)) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  ylab(expression("Percent of age-1+ fish" >= "150 mm")) +
  xlab("Year") +
  facet_grid(~ Species) +
  theme(legend.title=element_blank()) +
  theme(legend.position = "inside", legend.position.inside = c(.60, .85)) +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  scale_colour_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_fill_manual(
    breaks = c("con", "trt"),
    labels = c("Control", "Treatment"),
    values=c("grey", "black")) +
  scale_shape_manual(
    breaks = c("con", "trt"), 
    labels = c("Control", "Treatment"),
    values=c(16, 16))


df_percent |> print(n = Inf)

ggsave("figs/large_fish_dot_by_trt.png", width=10, height=8, units="in")

### purrr -----
df_per_split <- df_percent |> 
  split(df_percent$Species)


library(purrr)
plot_per <- map(names(df_per_split), function(Species) {
  df <- df_per_split[[Species]]  
  legend_BT <- if(any(df$Species == "BT"))theme(
    legend.position=c(0.45, 0.70),
    legend.background = element_rect(fill = "transparent", color = NA), legend.title=element_blank(),
    legend.key.size = unit(0.4, "cm")
  )
  legend_notBT <- if(any(df$Species != "BT"))
    theme(legend.position= "none")
  ggplot(df, 
         aes(x = as.factor(Year), y = prob_large)) + 
    theme_bw(base_size = 20) + 
    geom_dotplot(binaxis='y', stackdir='center', aes(fill = as.factor(trt), colour=as.factor(trt)), position=position_dodge(0.75), dotsize = 0.75) +
    stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
                 geom="crossbar", width=0.5, aes(colour=as.factor(trt)), position=position_dodge(0.75)) + 
    legend_notBT +
    legend_BT +
    ylim(-0.2, 1.1) +# bards are 2xstandard deviation
    # geom_text(aes(label = count, y = prob_large, fill = as.factor(trt), colour=as.factor(trt)),
    #           colour = "red",
    #           position=position_dodge(width = 0.5),
    #           size = 4) +
    # geom_text(df_percent_boot, mapping = aes(x = as.factor(Year), label = count, y = 1.1, group = as.factor(trt)), position=position_dodge(0.75)) +
    geom_vline(xintercept = 3.5, linetype = "dashed") +
    ylab(expression("Percent of age-1+ fish" >= "150 mm")) +
    xlab("Year") +
    # theme(legend.title=element_blank()) +
    # theme(legend.position = "inside", legend.position.inside = c(.60, .85)) +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
    scale_colour_manual(
      breaks = c("con", "trt"),
      labels = c("Control", "Treatment"),
      values=c("grey", "black")) +
    scale_fill_manual(
      breaks = c("con", "trt"),
      labels = c("Control", "Treatment"),
      values=c("grey", "black")) +
    scale_shape_manual(
      breaks = c("con", "trt"), 
      labels = c("Control", "Treatment"),
      values=c(16, 16))
})  


names(plot_per) <- paste0(names(df_per_split))
list2env(plot_per, envir = .GlobalEnv)
p1 <- plot_per$AS
p2 <- plot_per$BT


### combine ----
p1_clean <- p1 + theme(axis.title = element_blank()) +
  geom_text(df_percent_boot |> filter(Species == "AS"), mapping = aes(x = as.factor(Year), label = count, y = 1.1, group = as.factor(trt)), position=position_dodge(0.75))
p2_clean <- p2 + theme(axis.title = element_blank()) +
  geom_text(df_percent_boot |> filter(Species == "BT"), mapping = aes(x = as.factor(Year), label = count, y = 1.1, group = as.factor(trt)), position=position_dodge(0.75))

grid_plot <- plot_grid(p1_clean, 
                       p2_clean,
                       ncol = 2, 
                       align = "hv", 
                       axis = "tblr",
                       scale = 0.9,
                       labels = c("AS", "BT"),
                       hjust = -3, 
                       vjust = 1.25)

# Add shared axis labels
final_plot_bio <- ggdraw(grid_plot) +
  draw_label("Year", x = 0.5, y = 0, vjust = -0.5, fontface = "bold", size = 20) +
  draw_label(expression("Percent of age-1+ fish" >= "150 mm"), x = 0, y = 0.5, angle = 90, vjust = 1.5, fontface = "bold", size = 20)

final_plot_bio
save_plot("figs/large_fish_percent_dot_by_trt.png", 
          final_plot_bio, 
          base_height = 6, 
          base_width = 10,
          bg = "white")
