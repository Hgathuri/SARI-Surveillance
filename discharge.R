dim(real_time)

dim(DS.surveillance)

names(DS.surveillance)

dim(fds_all)

dim(fds_weekly)

dim(fds_today)

dim(fds.summary.retro)

dim(fds.summary.realtime)
unique(fds_weekly[c("hosp_id")])

length(aa)


real_time[is.na(real_time$hosp_id),'clerk_id']

real_time[is.na(real_time$hosp_id),'record_id']

clerk_id

discharge_missing <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge) & los > 5,0,1),
                                            date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
                                            diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1))

write.csv(discharge_missing, "Discharge_doc_missing.csv", row.names = F)


discharge_missing2 <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge),0,1),
                                                date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
                                                diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1))
  
write.csv(discharge_missing2, "Discharge_doc_missing2.csv", row.names = F)



discharge_missing3 <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge),0,1),
                                             date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) ,0,1),
                                             diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) ,0,1))

write.csv(discharge_missing3, "Discharge_doc_missing3.csv", row.names = F)


#discharge information output ammendment
discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse((is.na(outcome_at_discharge) & !is.na(date_of_discharge) & !is.na(date_of_death)),0,1),
                                                 diagnosis_score=ifelse((is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) &
                                                                         is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & !is.na(date_of_discharge) & !is.na(date_of_death)
                                                                         los > 5),0,1)),
                                              date_discharge_death_score = ifelse((is.na(date_of_discharge) & is.na(date_of_death) & diagnosis_score = 1 & outcome_score = 1 & los > 5),0,1),%>%
  group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),
                                         outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T)) %>% filter(!is.na(hosp_id))


##2
#discharge information output ammendment
  discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse((is.na(outcome_at_discharge) & !is.na(date_of_discharge) & !is.na(date_of_death)),0,1),
                                                  diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3) & is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & !is.na(date_of_discharge) & !is.na(date_of_death)),0,1),
                                                  date_discharge_death_score = ifelse((is.na(date_of_discharge) & is.na(date_of_death) & diagnosis_score = 1 & outcome_score = 1),0,1),%>% 
    group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T), outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T)) %>% filter(!is.na(hosp_id))





  discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge) & !is.na(date_of_discharge) & !is.na(date_of_death)),0,1),
                                                  date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & diagnosis_score = 1 & outcome_score = 1 & los > 5,0,1),
                                                  diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & !is.na(date_of_discharge) & !is.na(date_of_death) & los > 5,0,1) %>% 
                                                    group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T))
  
                                                  
discharge_information4 <- clinical.ds %>% mutate(status_adm_score= ifelse((is.na(health_status_adm),0,1),outcome_score = ifelse(is.na(outcome_at_discharge) & !is.na(date_of_discharge) & !is.na(date_of_death)),0,1),
                                                date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & diagnosis_score = 1 & outcome_score = 1 & los > 5,0,1),
                                                diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & !is.na(date_of_discharge) & !is.na(date_of_death) & los > 5,0,1) %>% 
                                                  group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T))
                                                  




                                                discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1),
                                                                                                outcome_score = ifelse(is.na(outcome_at_discharge) & (!is.na(date_of_discharge) | !is.na(date_of_death)),0,1),
                                                                                                date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & !is.na(outcome_at_discharge),0,1),
                                                                                                diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & (!is.na(date_of_discharge) | !is.na(date_of_death) | !is.na(outcome_at_discharge)),0,1)) %>%
                                                  group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T))
                                                


is.na(real_time$hosp_id)






