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
