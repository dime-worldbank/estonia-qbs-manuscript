
### SET-UP ----------------------------------------------------------------------------------------------------------


#
# Packages ----------------------------------------------------------------------------------------------------------
#

if( !is.element("pacman", installed.packages() )){install.packages("pacman", dep= T)}

pacman::p_load(tidyverse, haven, stringr, svMisc, janitor, data.table, ggplot2, stringi, dplyr,
               foreign, labelled, fastDummies, car, arrow, lubridate, scales, purrr, plotly,stargazer,  ggpubr, paletteer, grid, quantreg,
               gridExtra, patchwork,cowplot, foreach, raster, doBy, stringi, expss, Rmpfr, foreign, plm, AER, tidylog,
               fixest, lmtest, multiwayvcov, knitr, beepr, estimatr, DescTools,
               ggtext, ggridges, psych, NLP, tm, extrafont, ggridges,  collapse, knitr, tableone,
               survival, zoo, pdftools, rdd, locfit, KernSmooth, ggpubr, digest, EventStudy,data.table,Gmisc,glue,htmlTable,grid,magrittr,
               update = F)

### Load fonts
loadfonts(quiet = T)
fonts()

# Set path 
System <- Sys.getenv(x = NULL, unset = "")

if(Sys.info()[["user"]] == "wb539995"){
  project_path  <- "C:/Users/WB539995/WBG/Daniel Rogger - Data/ECM uptake/ECM_itt"
}else if(Sys.info()[["user"]] == "jonas"){
  # project_path  <- "~/World_Bank/Locker/Estonia/Health/ECM (code review)"
  project_path  <- "/Users/jonas/Downloads/Code Review (2023 Q2)"
}


#
# Custom functions ----
#

mean_miss <- function(x){mean(x, na.rm = TRUE)}
median_miss <- function(x){median(x, na.rm = TRUE)}
sum_miss  <- function(x){sum(x, na.rm = TRUE)}
sd_miss   <- function(x){sd(x, na.rm = TRUE)}
lower_ci  <- function(x){mean_miss(x) - 1.96 * (sd_miss(x) / sqrt(length(x)))}
upper_ci  <- function(x){mean_miss(x) + 1.96 * (sd_miss(x) / sqrt(length(x)))}
min_miss  <- function(x){min(x, na.rm = TRUE)}
max_miss  <- function(x){max(x, na.rm = TRUE)}
sf        <- function(x){return(summary(factor(x)))}
pr        <- function(x){return(prop.table(table(x, useNA = "no"))*100)}
pr_na        <- function(x){return(prop.table(table(x, useNA = "ifany"))*100)}


x = c(1.01, .3412)
round_flex = function(x){
  if(abs(x) < 1){
    x = sprintf("%.3f",round(x, 3))
  }else if(abs(x) >= 1  & abs(x) < 10){
    x = sprintf("%.2f",round(x, 2))
  }else if(abs(x) >= 10 & abs(x) < 100){
    x = sprintf("%.1f",round(x, 1))
  }else if(abs(x) >= 100){
    x= sprintf("%.0f",round(x, 0))
  }
  
  return(x)
}


# Function to get significance stars
sig_stars = function(var){
  
  var = ifelse(var == '<0.001', round(0.00, 2), round(as.numeric(var), 3))
  
  if(is.numeric(var)){
    #var = ifelse(test = var < 0.10 & var >= 0.05, yes  = paste0(var, '$^{\\dotr}$'), no = var)
    var = ifelse(test = var < 0.10 & var >= 0.05, yes  = paste0(var, '$^{*}$'), no = var)
    var = ifelse(test = var < 0.05  & var >= 0.01, yes  = paste0(var, '$^{**}$'), no = var)
    var = ifelse(test = var < 0.01  & var >= 0.001, yes  = paste0(var, '$^{***}$'), no = var)
    #var = ifelse(test = var < 0.001, yes  = paste0(var, '$^{***}$'), no = var)  
  }
  
  return(var)
}

# Function to summarize all columns in the dataset
sf_dataset = function(data){
  for(var1 in names(data)){
    print(var1)
    data = data %>% mutate(value = !!rlang::ensym(var1))
    print(sf(data$value))
  }
}


# Function to find observations above X st. dev. from the mean
outlier_sd = function(data, var, x){
  lower = mean_miss(data[,var]) - x*sd_miss(data[, var])
  upper = mean_miss(data[,var]) + x*sd_miss(data[, var])
  return(which(data[,var] < lower | 
                 data[,var] > upper))
}
#outlier_sd(dta, 'covid_vaccine', 3)


# Extract coefficients from the model into a (semi-)clean LaTeX row
extract_coeftest = function(m1, length1){
  
  ### Extract coefficients
  
  # If OLS
  if(class(m1) %in% c('lm')){
    temp = summary(m1)$coefficients
    temp = data.frame(beta = temp[,1], se = temp[,2], p_value = temp[,4])
  }
  if(class(m1) %in% c('coeftest')){
    temp = temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  } 
  if(class(m1) %in% c('iv_robust')){
    temp = data.frame(beta = m1$coefficients, se = m1$std.error, p_value = m1$p.value)
  }
  
  if(class(m1) %in% c('coxph')){
    m1 = tidy(m1)
    temp = data.frame(beta = m1$estimate, se = m1$std.error, p_value = m1$p.value)
    rownames(temp) = m1$term
  }
  
  # length1 = 2
  # m1 = lm(var ~  class_code * ecm_include_patient, data = dta_reg)
  # m1 = coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T))
  # temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  
  temp = add_column(temp, .before = 1, 'var' = rownames(temp))
  temp = temp[(nrow(temp)-length1):nrow(temp),]
  
  ### Round numbers
  temp$beta = sapply(temp$beta, round_flex)
  temp$se   = str_trim(paste0('(', sapply(temp$se, round_flex), ')'))
  
  ### Add significance stars
  #temp$beta = ifelse(test = temp$p_value < 0.10 & temp$p_value >= 0.05, yes  = paste0(temp$beta, '$^{\\dotr}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.10 & temp$p_value >= 0.05, yes  = paste0(temp$beta, '$^{*}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.05  & temp$p_value >= 0.01, yes  = paste0(temp$beta, '$^{**}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.01, yes  = paste0(temp$beta, '$^{***}$'), no = temp$beta)
  #temp$beta = ifelse(test = temp$p_value < 0.001, yes  = paste0(temp$beta, '$^{***}$'), no = temp$beta)
  
  ### To long
  temp$var_clean = temp$var
  temp = gather(temp, key, beta, -c(var, var_clean, p_value, se))
  temp = temp[order(temp$var), ]
  
  ### Clean variable (row) names
  #temp = temp %>% mutate(across(c(var_clean), ~ paste0('\\multirow{2}{*}{', .x, '}')))
  #temp$var_clean[seq(2,nrow(temp), 2)] = ''
  
  ###  Put minuses in $ signs, as otherwise they won't print correctly in LaTeX
  temp$beta = gsub('-', '$-$', temp$beta, fixed=T)
  
  ###  Put all columns in one dataframe column with LaTeX table separators
  # temp$cell1 = paste0(apply(temp[,c('var_clean', 'beta')], 1, paste, collapse = "&"), '\\')
  
  ### Final selection of columns
  temp = temp %>% dplyr::select(c(var, beta))
  
  ###  Return
  return(temp)
}

### Mode
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){x = x[!is.na(x)]}
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}



#
# DICTIONARY ----------------------------------------------------------------------------------------------------------
#

#dict_outcomes = xlsx::read.xlsx(file.path(project_path, 'Data', 'Clean', 'Other', 'Outcome_dict.xlsx'), sheetName = 'outcomes')

#
# END OF CODE ----------------------------------------------------------------------------------------------------------
#
