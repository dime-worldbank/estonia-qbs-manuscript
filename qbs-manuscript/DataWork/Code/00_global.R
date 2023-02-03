
### SET-UP --- 

#
# Packages ----
#
# rm(list=ls())

if( !is.element("pacman", installed.packages() )){
  install.packages("pacman", dep= T)
}

pacman::p_load(tidyverse, haven, stringr, xlsx, svMisc,
               janitor, data.table, ggplot2, stringi, dplyr,
               foreign, labelled, fastDummies,
               lubridate, scales, purrr, plotly, openxlsx, 
               stargazer,  ggpubr, paletteer, grid, quantreg,
               gridExtra, patchwork,cowplot, foreach, raster,
               doBy, stringi, expss, Rmpfr, foreign, plm, AER,
               fixest, lmtest, multiwayvcov, knitr, beepr, estimatr,
               update = F)

#
# Custom functions ----
#

mean_miss <- function(x){mean(x, na.rm = TRUE)}
sum_miss  <- function(x){sum(x, na.rm = TRUE)}
min_miss  <- function(x){min(x, na.rm = TRUE)}
max_miss  <- function(x){max(x, na.rm = TRUE)}
sf        <- function(x){return(summary(factor(x)))}
pr        <- function(x){return(prop.table(table(x, useNA = "no"))*100)}

# Set path 
System <- Sys.getenv(x = NULL, unset = "")


if(Sys.info()[["user"]] == "wb539995"){
  project_path  <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/ECM uptake/ECM_itt"
  raw_2009_2019      <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2009_2019"
  raw_2019_2021      <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2019_2021"
  raw_2021      <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2020"
  raw_2022      <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2022_v2"
  raw_2022_2      <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2022"
  raw_2022_nov  <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/raw_2022_Nov"
  output        <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/ECM uptake/ECM_itt/output"
  data          <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/ECM uptake/ECM_itt/data"
  
}else if(Sys.info()[["user"]] == "ASUS"){
  project_path  <- "~/World_Bank/Locker/Estonia/Health/Billing"
  raw_2021      <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/Raw/raw_2020"
  raw_2022      <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/Raw/raw_2022_v2"
  raw_2022_2      <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/Raw/raw_2022"
  raw_2022_nov  <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/Raw/raw_2022_Nov"
  output        <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/ECM_uptake/Clean"
  data          <- "~/World_Bank/Locker/Estonia/Health/Billing/Data/ECM_uptake/Raw"
  scripts          <- "~/World_Bank/Locker/Estonia/Health/Billing/code"
  figures          <- "~/World_Bank/Locker/Estonia/Health/Billing/Figures"
  
}


theme_set(theme_bw()+
            theme(
              panel.border = element_blank(),
              legend.position = 'top',
              legend.direction = 'horizontal',
              legend.title = element_text(size = 30, face = 'plain'),
              legend.text = element_text(size = 23),
              legend.box = "vertical",
              axis.line = element_line(),
              axis.title =element_text(face = 'bold', color = 'black', size = 30),
              axis.text.y  = element_text(face = 'plain', color = 'black', size = 26),
              axis.text.x  = element_text(face = 'plain', color = 'black', size = 15),
              #plot.background = element_rect(fill='white', color=NA), #transparent plot bg
              plot.title = element_text(face = 'bold', color = 'black', size = 35, hjust = 0.5),
              plot.subtitle = element_text(face = 'bold', color = 'grey30', size = 26, hjust = 0.5),
              plot.caption= element_text(face = 'plain', color = 'grey40', size = 13, hjust = 0),
            ))

#
# END OF CODE ----
#
