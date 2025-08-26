# this is to do the Carle Strub estimates and get T (total Catch) as well as diagnostics.  Basically, it shows that using FSA is pretty suspect for these data.
source("RB_data_new.R")

### CS ----
library(FSA)
#res_list <- apply(df_tab1[c(1:2, 4:10), c(4:6)], MARGIN=1, FUN = removal, method = "CarleStrub") # 
# use df_tab1 bc it has abundance in the right format and 160 rows (right) and has been filtered for less than 3 sweeps
res_list <- apply(df_tab1[, c(4:6)], MARGIN=1, FUN = removal, method = "CarleStrub") # takes NA's


# this works but only when three catches so maybe that is fine - filter above on this.
out <- as.data.frame(matrix(NA, length(res_list), 11))
colnames(out) <- c("c1",  "c2", "c3","k",  "T", "X", 
                   "No",  "No.se", "No.LCI", 
                   "No.UCI", "p"
)


# format res_list so that its readable
for(i in seq_along(res_list)){
  if(length(res_list[[i]]$catch) ==3){
    out[i,] <- round(c(res_list[[i]]$catch, 
                       res_list[[i]]$int, 
                       res_list[[i]]$est[1:5]), 2)
    
  } else if (names(res_list[[i]]$catch[2]) == 2 & 
             length(res_list[[i]]$catch) == 2){
    out[i,c(1:2, 4:11)] <- round(c(res_list[[i]]$catch, 
                                   res_list[[i]]$int, 
                                   res_list[[i]]$est[1:5]), 2)
  } else if (names(res_list[[i]]$catch[2]) == 3 & 
             length(res_list[[i]]$catch) == 2){
    out[i,c(1, 3:11)] <- round(c(res_list[[i]]$catch, 
                                 res_list[[i]]$int, 
                                 res_list[[i]]$est[1:5]), 2)
  }
}

# bind year, species and station to res_list
out <- cbind(year = df_tab1$Year, 
             spp = df_tab1$Species, 
             sta = df_tab1$Station, 
             out)

#View(out)
head(out)
#View(out)

### GF calc ----
out$GF <- with(out, round(
    (c1 - (No*p))^2/No*p +
    (c2 - No*(1-p)*p)^2/(No*(1-p)*p) +
    (c3 - (No*(1-p)^2*p))^2/(No*(1-p)^2*p)
  ,4)
)
str(out)
# write.csv(out, "data_derived/FSA_output.csv")
dchisq(1.2813, 2) ## What is the likelihood of this value
pchisq(1.2813, 2) # probability of this value or less - cumulative density
1-pchisq(1.2813, 2) #- this is the pvalue
qchisq(0.95, 1) #- gives the critical test 3.84 - this is right - it should be one df because k-2: 2 df are lost because N is estimated - see Locwood2000



# # filter out sites with only 1 catch or where c2 & c3 == NA
# nrow(out)
# out |> filter(GF > qchisq(0.95, 1)) # 10 sites don't make GF with T > 30 on 5 sites 
# nrow(out |> filter(GF > qchisq(0.95, 1)))
# out |> filter(T < 30)
nrow(out) # 160
nrow(out |> filter(T < 30)) # 150 of 160
nrow(out |> filter(T < 20)) # 135 of 160
nrow(out |> filter(T < 10)) # 109 of 160
nrow(out |> filter(T < 5)) # 79 of 160

# density of total catch
plot(density(out$T))

# T v No
p <- ggplot(out, aes(x = T, y = No, group = as.factor(spp), colour = spp)) +
  geom_point()
p
nrow(out) # 160
nrow(out |> filter(GF < qchisq(0.95, 1))) # 90 of 160 (56%)
nrow(out |> filter(GF >= qchisq(0.95, 1))) # 17 of 160 (11%)
nrow(out |> filter(is.na(GF))) # 53 of 160 (33%)



# tabulations ----
table(out$year, out$spp, out$T)



# END ----