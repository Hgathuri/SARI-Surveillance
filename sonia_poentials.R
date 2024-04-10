#Potential SONIA participants
sonia_potentials<- retro %>% filter(is_minimum==0,c('hosp_id','is_minimum'))

sp<- sonia_potentials[ , c('record_id','hosp_id', 'cough','fever','diff_breathing','suspected_covid','dx_adm___1','date_adm','dateofentry')]

write.csv(sp, "C:/Users/hgathuri/hsuApps/weekly_report/sonia_poentials2.csv")

unique(retro$is_minimum)
