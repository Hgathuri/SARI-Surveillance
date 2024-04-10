discharge_missing <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge) & los > 5,0,1),
                                                date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
                                                diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1))

write.csv(discharge_missing, "Discharge_doc_missing.csv", row.names = F)

#Discharge information per Hospital below 100%
discharge_df <-  discharge_missing %>% filter((date_discharge_death_score == 1) && (diagnosis_score == 0 | outcome_score == 0))
write.csv(invest_df, "investigations_doc_missing.csv", row.names = F)

discharge_df22 <-  discharge_missing %>% filter((diagnosis_score < 1 | date_discharge_death_score < 1) && (outcome_score == 1 ))

discharge_df2 <-  discharge_missing %>% filter(diagnosis_score == 0 && outcome_score == 0 && date_discharge_death_score == 1)

