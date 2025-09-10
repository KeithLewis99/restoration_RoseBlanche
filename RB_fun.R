# Functions for Rose Blanche - note that I haven't done ROxygen for all of these nor have all been converted to RB.
str2 <- function(object, ...) {
  utils::str(object, give.attr = FALSE, ...)
}

tab_type <- function(df, species, metric){
  #browser()
  df_sum <- df |>
    filter(Species == {{species}}) |>
    group_by(Station, type) 
  df_test <- df_sum |>
    summarise(N = n(),
              mean = mean({{ metric }}, na.rm = T),
              sd = sd({{metric}}, na.rm = T)
    )
  return(df_test)    
}


#' spatialAutoCorrBase_fun
#'
#' @param x - Seal Cove data set
#' @param y - residuals calculated in DHARMa for a glmmTMB object
#' @param xtextRes - a graphical term to move the residual value away from the point
#' @param xtextSite - a graphical term to move the residual value away from the point
#'
#' @return a base plot of lat v long for Seal Cove sites.  alpha numerics to the right are the Seal Cove station and to the left are the scaled residuals (see below)
#' Scaled residuals  
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# To interpret the residuals, remember that a scaled residual value of 0.5 means that half of the simulated data are higher than the observed value (blue), and half of them lower (red).
#' @export
#'
#' @examples spatialAutoCorrBase_fun(bt.np, bt.glmm1_simres_recalcSpace)
spatialAutoCorrBase_fun <- function(x, y, xtextRes = -0.00025, xtextSite = 0.00025){
  col = colorRamp(c("red", "white", "blue"))(y$scaledResiduals)
  plot(x$west, x$north, type = 'n') # , xlim=c(345448.6, 345970.3)
  points(x = unique(x$west), y = unique(x$north), col = rgb(col, maxColorValue = 255))
  text(unique(x$west)+xtextRes, unique(x$north), labels = unique(x$Station), cex = 0.5)
  text(unique(x$west)+xtextSite, unique(x$north), labels = y$scaledResiduals, cex = 0.5)
}

spatialAutoCorrBase_fun1 <- function(x, y, xtextRes = -0.00025, xtextSite = 0.00025){
  col = colorRamp(c("red", "white", "blue"))(y$scaledResiduals)
  plot(x$west, x$north, type = 'n') # , xlim=c(345448.6, 345970.3)
  points(x = unique(x$west), y = unique(x$north), col = rgb(col, maxColorValue = 255))
  text(unique(x$west)+xtextRes, unique(x$north), labels = unique(x$Station), cex = 0.5)
  text(unique(x$west)+xtextSite, unique(x$north), labels = y$scaledResiduals, cex = 0.5)
}


# temp1 <- btyoy.glmm4_new_simres_recalcSpace$scaledResiduals # get resids
# group1 <- unique(btyoy.glmm4_new_simres_recalcSpace$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
# temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station = V1, scResid = temp1)

#' spatialData_join
#'
#' @param x data set with N, mean, sd, and se for Seal Cove sites
#' @param y - residuals calculated in DHARMa for a glmmTMB object
#' @param z - the coordinates (lat/long) for the Seal Cove sites
#'
#' @return a dataframe with the above dataframes properly bound together
#' @export
#'
#' @examples bt.np.biomass.all <- spatialData_join(bt.np.biomass.station[-4,], bt.glmm1_simres_recalcSpace, coords.np)
#' 
spatialData_join <- function(x, y, z){
  #browser()
  #z <- as.data.frame(matrix(NA, nrow(x), 2)) %>% rename(Station = V1, scResid = temp1)
  temp1 <- y$scaledResiduals # get resids
  group1 <- unique(y$group) # establish the order of resids - see scratch_pad and https://github.com/florianhartig/DHARMa/issues/359 
  temp2 <- as.data.frame(cbind(as.character(group1),temp1)) %>% rename(Station = V1, scResid = temp1)
  #temp2$Station <- as.numeric(temp2$Station)
  z1 <- left_join(temp2, x, by = "Station")
  z2 <- left_join(z1, z, by = c("Station" = "sites"))
}



#' spatialAutoCorrGG_fun
#'
#' @param x - a dataframe produced by spatialData_join
#' @param xtextRes - a graphical term to move the residual value away from the point
#' @param xtextSite - a graphical term to move the residual value away from the point
#'
#' @return a ggplot of lat v long for Seal Cove sites.  alpha numerics to the right are the Seal Cove station and to the left are the scaled residuals (see spatialAutoCorr_fun above for an explanation).  
#' @export
#'
#' @examples spatialAutoCorrGG_fun(bt.np.biomass.all)
#' 
spatialAutoCorrGG_fun <- function(x, xtextRes = -0.0003, xtextSite = 0.0003) {
  ggplot(x, aes(x = west, y = north, size = mean)) +
    geom_point() +
    #xlim(345498.6, 345920.3) +
    geom_text(aes(label = Station), check_overlap = T, nudge_x = xtextSite, size = 3) +
    geom_text(aes(label = scResid), nudge_x = xtextRes, size = 3)
}


#' Title
#'
#' @param x 
#' @param y filter variable: pools or non-pools - pool == "yes", non-pool == "no, LUNKER == "lunker"
#' @param z filter variable: density or biomass, density == "d", biomass == "b"
#'
#' @return a ggplot with Seal Cove station on the x-axis and density/biomass on the y-axis with the means and standard deviations by Control and Impact for non-pools, Before-After for Pools, and Lunker/no Lunker for the pools on Impact sites.
#' @export
#'
#' @examples mean_by_site(bt.np.biomass.station, "no", "b")
mean_by_site <- function(x, z){
  #browser()
  ggplot(x, aes(as.factor(Station), mean)) +
    geom_point(size=4, position=position_dodge(1)) +
    theme_bw() +
    theme(axis.text.x  = element_text(angle=90, vjust=0.4, size=10)) +
    facet_grid(~factor(type, levels = c("con", "trt"))) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.5, position=position_dodge(1)) +
    {if (z == "b"){
      ylab(expression("Biomass Estimate (g/100 m" ^2*")"))
    } else if (z == "d"){
      ylab(expression("Density Estimate (#/100 m" ^2*")"))
    }
    } +
    xlab("Station") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
}


#' Confidence Interval table
#'
#' @param df dataframe
#' @param name name of the file to be exported, e.g. bt_den for brook trout density or bt for brook trout biomass
#'
#' @return a csv file with the confidence intervals for the glmmTMB object to be used in Overview.Rmd
#' @export
#'
#' @examples
tab.ci <- function(df, name){
  #browser()
  df_ciout <- as.data.frame(confint(df))
  df_ciout <- cbind(parm = rownames(df_ciout), data.frame(df_ciout, row.names = NULL)) 
  colnames(df_ciout)[2] <- "2.5%"
  colnames(df_ciout)[3] <- "97.5%"
  write.csv(df_ciout, paste0("output/", name, "_ci.csv"))
}

#' control_impact_year
#'
#' @param df dataframe of year, type, time, abundance
#' @param z filter variable: response variable - density or biomass, density == "d", biomass == "b"
#'
#' @return
#' @export
#'
#' @examples
control_impact_year <- function(df, z, leg){
  # browser()
  p1 <- ggplot(df, aes(x = as.factor(Year), y = exp(fit), fill = type, colour = type)) + 
    # geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_point(position = position_jitterdodge(
      jitter.width = 0.8,
      dodge.width = 1, 
      seed = 123),
      size = 2) +
    
    #facet_wrap(~Species) + 
    theme_bw() + 
    {if (z == "b"){
      ylab(expression("Biomass Estimate (g/m" ^2*")"))
    } else if (z == "d"){
      ylab(expression("Density Estimate (#/100 m" ^2*")"))
    } else if (z == "n"){
      theme(axis.title.y = element_blank())
    }
    } +
    xlab("Year") +
    # geom_errorbar(aes(ymax = exp(fit+se.fit*1.96), ymin = exp(fit-se.fit*1.96)), linewidth=1, width=0.15, position=position_dodge(0.5)) +
    geom_pointrange(aes(ymax = exp(fit+se.fit*1.96),
                      ymin = exp(fit-se.fit*1.96)),
                  linewidth=1, width=0,
                  position = position_jitterdodge(
                    jitter.width = 0.8,
                    dodge.width = 1,
                    seed = 123)
                  ) +
    # geom_vline(xintercept = 1.5, linetype="solid", linewidth=0.5) +
    # geom_vline(xintercept = 3.5, linetype="dashed", linewidth=0.5) +
    # geom_vline(xintercept = 4.5, linetype="dashed", linewidth=0.5) +
    #theme(legend.title=element_blank()) +
    {if(leg == "y") {
      theme(legend.position=c(.9, .85))
    } else if(leg == "n"){
      theme(legend.position="none")
    }
    } +
    scale_fill_discrete(name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) 
  #theme(legend.position=c(.85, .88))
  return(p1)
}


#' control_impact_year - differs from above bc these are for CIs
#'
#' @param df dataframe of year, type, time, abundance
#' @param z filter variable: response variable - density or biomass, density == "d", biomass == "b"
#'
#' @return
#' @export
#'
#' @examples
control_impact_year1 <- function(df, z, leg){
  # browser()
  p1 <- ggplot(df, aes(x = as.factor(Year), y = mean, fill = type, colour = type)) + 
    geom_point(position = position_dodge(width = 0.45),
               size = 2) +
    theme_bw() + 
    {if (z == "b"){
      ylab(expression("Biomass Estimate (g/m" ^2*")"))
    } else if (z == "d"){
      ylab(expression("Density Estimate (#/100 m" ^2*")"))
    } else if (z == "n"){
      theme(axis.title.y = element_blank())
    }
    } +
    xlab("Year") +
    geom_pointrange(aes(ymax = ll,
                        ymin = ul),
                    linewidth=1, 
                    position = position_dodge(width = 0.45)
    ) +
    {if(leg == "y") {
      theme(legend.position=c(0.85, 0.9), 
            legend.background = element_rect(fill = "transparent")
  #          legend.box.background = element_rect(fill = "transparent")
            )
    } else if(leg == "n"){
      theme(legend.position="none")
    }
    } +
    scale_fill_discrete(name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) +
    scale_colour_manual(values=c("black", "dark grey"),
                        name="",
                        breaks=c("con", "trt"),
                        labels=c("Control", "Treatment")) 
  return(p1)
}


