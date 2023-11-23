
### Source the '01_clean_diag.R' script with required packages and functions and code cleaning

# if(Sys.info()[["user"]] == "wb539995"){
#   # source('~/path/to/r_script/00_global.R'
# }else if(Sys.info()[["user"]] == "ASUS"){
#   source(file.path('~/World_Bank/Locker/Estonia/Health/Billing/R Scripts/Clean/01_clean_diag.R'))
# }


### Source the '00_global.R' script with required packages and functions

if(Sys.info()[["user"]] == "wb539995"){
  # source('~/path/to/r_script/00_global.R'
}else if(Sys.info()[["user"]] == "ASUS"){
  source(file.path('~/World_Bank/Locker/Estonia/Health/Billing/R Scripts/Clean/00_global.R'))
}


#
# READ AND CHECK DATA ----
#
hospitalization = fread(file.path(project_path, 'Data/ECM_uptake/Raw', "patient_dta_reg.csv"))

### Count any visit due to one of these 5 conditions
hospitalization <- add_column(hospitalization, .after = 'i10_diag',
                                'hosp_combined' = ifelse(rowSums(hospitalization[, c('asthma', 'copd', 'e11_diag', 'i50_diag', 'i10_diag')]) > 0, 1,0))

### For ambulance visits  ts 5.002% for ECM and 7.09% for non-ECM is the correct value
tapply(hospitalization$ambulance_emergency, hospitalization$treat_ecm, pr)

### Add strata variable
hospitalization <- add_column(hospitalization, .after = 'patient_treatment', 'strata' = paste(hospitalization$list_id, hospitalization$class_code))

hospitalization$treat_ecm2  <- ifelse(hospitalization$treat_ecm == 'Enrolled', 1,0)

#
# REGRESSION LOOP (DUMMIES) ----
#


names(hospitalization)
var1 = 'price_procedure_times_sum'

# If we want to use logit model for dummy variable, please specify logit = 'yes'
logit = 'no'

### For each outcome column...
count = 0

for(var1 in names(hospitalization)[which(names(hospitalization) == 'asthma'):which(names(hospitalization) == 'readmission_short')]){
 
  count = count+1
  print(paste0(count, ") ", var1)) # To control the progress
  
  hospitalization_reg = hospitalization %>% mutate(value = !!rlang::ensym(var1)) # Iteratively define variable used as outcome
  
  # For re-admission we want to focus only on the patients who were admitted at least once
  if(grepl('readmission_short', var1)){
    hospitalization_reg = hospitalization_reg[hospitalization_reg$n > 0,]
  }

  # Fit the model
  model1 = lm(value ~  strata + treat_ecm, data = hospitalization_reg)
  

  if(logit == 'yes'){
      model1 = glm(value ~  strata + treat_ecm, data = hospitalization_reg, family = binomial(link = 'logit'))
  }

  len1 = length(coeftest(model1, cluster.vcov(model1, hospitalization_reg$list_id, df_correction = T))) # Correct SE for clustering
  p1=coeftest(model1, cluster.vcov(model1, hospitalization_reg$list_id, df_correction = T))[len1] # Extract p-value
  
  # Extract relevant numbers to the dataframe
  temp = data.frame(var = var1,
                    'ECM_total' = length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Enrolled']),
                    'ECM_n' = sum(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Enrolled'] > 0, na.rm=T),
                    'ECM_per' = 100*sum(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Enrolled'] > 0, na.rm=T)/length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Enrolled']),
                    'non_ECM_total' = length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Not Enrolled']),
                    'non_ECM_n' = sum(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Not Enrolled'] > 0, na.rm=T),
                    'non_ECM_per' = 100*sum(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Not Enrolled'] > 0, na.rm=T)/length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Not Enrolled']))
  
  # Create additional columns based on the ones created (differences) and add p-value
  temp[1, c('diff_n', 'diff_per', 'p_value')] <- c(temp$ECM_n - temp$non_ECM_n, temp$ECM_per - temp$non_ECM_per, p1)
  
  temp <- temp %>% mutate(across(c(ECM_per, non_ECM_per, diff_per), round, 2)) # Round the numbers
  temp <- temp %>% mutate(across(c(p_value), round, 3)) # Round the numbers
  
  print(temp)
  
  # If first iteration, create new table object, if not, add rows to the existing one
  if(count == 1){
    hospitalization_table = temp
  }else{
    hospitalization_table = rbind(hospitalization_table, temp)
  }
}
count=0

kable(hospitalization_table, "latex", booktabs = TRUE,  digits=3)






#
# REGRESSION LOOP (CONTINUOUS) ----
#
names(hospitalization)

### As in the loop above, but using continuous dependent variables (they need different summary statistics, that's why a distinct loop)


var1 = 'price_procedure_times_sum'

library(truncreg)
library(survival)
library(AER)

quantile(hospitalization$price_procedure_times_sum, .95)

sample1 = 'full'

count = 0

for(sample1 in c('full', 'hospital')){

  # Loop over different possible specifications of the costs model
  for(type1 in c('all', 'logs+', 'logs>0', '<99th percentile', '<95th percentile', '<90th percentile', 'tobit', 'quantile')){
    
    # For different variable costs
    #for(var1 in names(hospitalization)[grepl('price_', names(hospitalization))]){
    for(var1 in c('price_procedure_times_sum')){
      count = count + 1
      
      print(paste0(sample1, '. ', type1, ': ', var1))
      
      hospitalization_reg = hospitalization %>% mutate(value = !!rlang::ensym(var1)) # Iteratively define variable used as outcome
      
      # If only hospitalized sample is of interest, remove patients with 0 costs from the dataset
      if(sample1 == 'hospital'){
        hospitalization_reg = hospitalization_reg[hospitalization_reg$value > 0,]
      }
      
      if(type1 == 'all'){
        model1 = lm(value ~ strata + treat_ecm, data = hospitalization_reg)
        summary(model1)
      }
      if(type1 == 'logs+'){
        model1 = lm(log(value+.1) ~ strata + treat_ecm, data = hospitalization_reg)
      }
      if(type1 == 'logs>0'){
        hospitalization_reg = hospitalization_reg[hospitalization_reg$value > 0,]
        model1 = lm(log(value) ~ strata + treat_ecm, 
                    data = hospitalization_reg[hospitalization_reg$value > 0,])
      }
      if(type1 == 'tobit'){
        model1 = tobit(value ~ treat_ecm, left = 0, right = quantile(hospitalization_reg$value, 0.9),
                        data = hospitalization_reg)
        
        hospitalization_reg$value =  ifelse(test = hospitalization_reg$value > quantile(hospitalization_reg$value, 0.9), 
                                               yes = quantile(hospitalization_reg$value, 0.9), 
                                               no = hospitalization_reg$value)
                                      
      }
      if(type1 == '<99th percentile'){
        hospitalization_reg = hospitalization_reg[hospitalization_reg$value < quantile(hospitalization_reg$value, 0.99),]
        model1 = lm(value ~ strata + treat_ecm, 
                    data = hospitalization_reg)      
      }
      if(type1 == '<95th percentile'){
        hospitalization_reg = hospitalization_reg[hospitalization_reg$value < quantile(hospitalization_reg$value, 0.95),]
        model1 = lm(value ~ strata + treat_ecm, 
                    data = hospitalization_reg)     
      }
      if(type1 == '<90th percentile'){
        hospitalization_reg = hospitalization_reg[hospitalization_reg$value < quantile(hospitalization_reg$value, 0.90),]
        model1 = lm(value ~ strata + treat_ecm, 
                    data = hospitalization_reg)       
      }
    }
    if(type1 == 'quantile'){
      model2=rq(value ~ treat_ecm2 + strata, data=hospitalization_reg)
      
      p1 = c()
      set.seed(123)
      for(i in 1:5){
        print(i)
        summary.rq(model2, se = 'rank')
        ?summary.rq
        coefficients[2,4]
        p1 = c(p1, summary.rq(model2, se = 'nid')$coefficients[2,4])
        }
      p1 = mean_miss(p1)
    }

    if(type1 == 'tobit'){
      p1=summary(model1)$coefficients[2,4]
      #summary(model1)$coefficients[2,1]
    }else if(type1 == 'quantile'){
      p1 = p1
    }else{
      len1 = length(coeftest(model1, cluster.vcov(model1, hospitalization_reg$list_id, df_correction = T)))
      p1=coeftest(model1, cluster.vcov(model1, hospitalization_reg$list_id, df_correction = T))[len1]
    }
  
    
    temp = data.frame('Sample' = sample1,
                      'Model' = type1,
                      'ECM_total' = length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Enrolled']),
                      'ECM_mean' = mean(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Enrolled'], na.rm=T),
                      'non_ECM_total' = length(hospitalization_reg$list_id[hospitalization_reg$treat_ecm == 'Not Enrolled']),
                      'non_ECM_mean' = mean(hospitalization_reg$value[hospitalization_reg$treat_ecm == 'Not Enrolled'], na.rm=T))
    
    temp[1, c('diff_mean', 'p_value')] <- c(temp$ECM_mean - temp$non_ECM_mean, p1)
    
    temp <- temp %>% mutate(across(c(ECM_mean, non_ECM_mean, diff_mean), round, 1))
    temp <- temp %>% mutate(across(c(p_value), round, 3))
    
    if(count == 1){
      hosp_table2 = temp
    }else{
      hosp_table2 = rbind(hosp_table2, temp)
    }
  }
}

hosp_table2

kable(hosp_table2, "latex", booktabs = TRUE,  digits=3)


#
# VARIATION COSTS ----
#
dim(hospitalization[hospitalization$price_procedure_times_mean > 0,])

g1=ggplot(data = hospitalization[hospitalization$price_procedure_times_mean > 0,],
       aes(x = log(price_procedure_times_sum),
          group = treat_ecm, col = treat_ecm))+
  geom_density(size = 2, key_glyph = draw_key_path)+
  labs(x = '\nTotal procedures costs per patient [log(€)]\n', y = 'Density\n',
       title = 'Distribution of treatment costs by ECM status\n',
       caption = 'Only patients with non-zero costs included.')+
  scale_color_manual(name = 'ECM group', values = c('darkorange', 'royalblue4'))+
  scale_x_continuous(breaks = seq(0, 12, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1),
                                        add = c(0, 0.02)))+
  guides(color  = guide_legend(override.aes = list(size = 5), title.position = 'top', title.hjust = 0.5))
  

ggsave(file.path(figures, paste0('Distribution of treatment costs', ".png")),
       plot = g1, width = 40, height = 28, unit = "cm")

#
# END OF CODE -----
#