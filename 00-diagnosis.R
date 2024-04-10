clinical_progress <- c("date_adm","date_of_discharge","date_of_death","health_status_adm",grep("dx_discharge", names(fds_weekly),value = T),"other_dx_discharge","other_dx_discharge_2","other_dx_discharge_3","outcome_at_discharge")
 clinical.ds <- select(fds_weekly,c(keys,clinical_progress))
 # clinical.ds <- select(BDS,c(keys,clinical_progress))
 clinical.ds[clinical.ds=="No"] <- NA
 clinical.ds[clinical.ds==""] <- NA
 clinical.ds[clinical.ds== "Empty"] <- NA
 clinical.ds[clinical.ds== "1913-01-01"] <- NA
 ###Add the length of stay to the dataset
 clinical.ds <- mutate(clinical.ds, los = Sys.Date()-as.Date(date_adm))
 
 ###Filter length of stay > 5 days
 los5days <- filter(clinical.ds, los > 5 & is.na(date_of_discharge) & is.na(date_of_death)) %>% select("record_id","hosp_id","date_adm","los")
 write.csv(los5days, "los.csv", row.names = F)
 ##Health status and outcome at discharge
 
 discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge),0,1),
                                                 date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
                                                 diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1)) %>%
   group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T))
 
 
#Amended section 
# discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1),
#                                                 outcome_score = ifelse(is.na(outcome_at_discharge) & (!is.na(date_of_discharge) | !is.na(date_of_death)),0,1),
#                                                 date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & !is.na(outcome_at_discharge),0,1),
#                                                 diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & (!is.na(date_of_discharge) | !is.na(date_of_death) | !is.na(outcome_at_discharge)),0,1)) %>%
#     group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T))
 
 
 
 
 
 
 # clinical_co <- filter(clinical.ds,!is.na(hosp_id)) %>%dplyr::group_by(hosp_id,outcome_at_discharge) %>%dplyr::summarise(total= length(record_id))  
 # levels(discharge_information$hosp_id) <- aa
 
 ##Reshape the data
 dx.final <- melt(as.data.frame(discharge_information), id="hosp_id")
 
# clinic_p <- ggplot(dx.final, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color ="black")+
#   geom_text(aes(label=round(value,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
#   theme_classic()+ scale_fill_brewer(type = "seq")+
#   ylab("Completeness score")+xlab("Hospitals")+ggtitle("Discharge information per hospital(All)", subtitle = "Status at time of admission,discharge diagnosis & outcome at discharge")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
# ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
# ppt <- ph_with(x = ppt, value = clinic_p,
#                location = ph_location_fullsize() ) 

 ###Retrospective data 
 # clinical.ds <- select(Retro,c(keys,clinical_progress))
 # 
 # clinical.ds[clinical.ds=="No"] <- NA
 # clinical.ds[clinical.ds==""] <- NA
 # clinical.ds[clinical.ds== "Empty"] <- NA
 # clinical.ds[clinical.ds== "1913-01-01"] <- NA
 # ###Add the length of stay to the dataset
 # clinical.ds <- mutate(clinical.ds, los = Sys.Date()-as.Date(date_adm))
 # 
 # ###Filter length of stay > 5 days
 # los5days <- filter(clinical.ds, los > 5 & is.na(date_of_discharge) & is.na(date_of_death)) %>% select("record_id","hosp_id","date_adm","los")
 # # write.csv(los5days, "los.csv", row.names = F)
 # ##Health status and outcome at discharge
 # 
 # discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge),0,1),
 #                                                 date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
 #                                                 diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1)) %>%
 #    group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T)) %>% filter(!is.na(hosp_id))
 # 
 # # clinical_co <- filter(clinical.ds,!is.na(hosp_id)) %>%dplyr::group_by(hosp_id,outcome_at_discharge) %>%dplyr::summarise(total= length(record_id))  
 # # levels(discharge_information$hosp_id) <- aa
 # 
 # ##Reshape the data
 # dx.final <- melt(as.data.frame(discharge_information), id="hosp_id")
 # 
 # clinic_Rp <- ggplot(dx.final, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color ="black")+
 #    geom_text(aes(label=round(value,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
 #    theme_classic()+ scale_fill_brewer(type = "seq")+
 #    ylab("Completeness score")+xlab("Hospitals")+ggtitle("Discharge information per hospital(Retrospective data)", subtitle = "Status at time of admission,discharge diagnosis & outcome at discharge")+
 #    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
 # ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
 # ppt <- ph_with(x = ppt, value = clinic_Rp,
 #                location = ph_location_fullsize() ) 
 
 ###Realtime data
 clinical.ds <- select(real_time,c(keys,clinical_progress))
 
 clinical.ds[clinical.ds=="No"] <- NA
 clinical.ds[clinical.ds==""] <- NA
 clinical.ds[clinical.ds== "Empty"] <- NA
 clinical.ds[clinical.ds== "1913-01-01"] <- NA
 ###Add the length of stay to the dataset
 clinical.ds <- mutate(clinical.ds, los = Sys.Date()-as.Date(date_adm))
 
 ###Filter length of stay > 5 days
 los5days <- filter(clinical.ds, los > 5 & is.na(date_of_discharge) & is.na(date_of_death)) %>% select("record_id","hosp_id","date_adm","los")
 # write.csv(los5days, "los.csv", row.names = F)
 ##Health status and outcome at discharge
 
# discharge_information <- clinical.ds %>% mutate(status_adm_score= ifelse(is.na(health_status_adm),0,1), outcome_score = ifelse(is.na(outcome_at_discharge),0,1),
#                                                 date_discharge_death_score = ifelse(is.na(date_of_discharge) & is.na(date_of_death) & los > 5,0,1),
#                                                 diagnosis_score=ifelse(is.na(dx_discharge___1) & is.na(dx_discharge___10) & is.na(dx_discharge___11) & is.na(dx_discharge___2) & is.na(dx_discharge___3)& is.na(dx_discharge___4)& is.na(dx_discharge___5) & is.na(dx_discharge___6) & is.na(dx_discharge___7) & is.na(dx_discharge___8) & is.na(dx_discharge____1) & los > 5,0,1)) %>%
#    group_by(hosp_id) %>% dplyr::summarise(status_adm_score = mean(status_adm_score,na.rm = T),diagnosis_score = mean(diagnosis_score,na.rm = T),outcome_score = mean(outcome_score,na.rm = T), date_discharge_death_score = mean(date_discharge_death_score,na.rm = T)) %>% filter(!is.na(hosp_id))
 
 # clinical_co <- filter(clinical.ds,!is.na(hosp_id)) %>%dplyr::group_by(hosp_id,outcome_at_discharge) %>%dplyr::summarise(total= length(record_id))  
 # levels(discharge_information$hosp_id) <- aa
 
 ##Reshape the data
 dx.final <- melt(as.data.frame(discharge_information), id="hosp_id")
 
 clinic_p <- ggplot(dx.final, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color ="black")+
    geom_text(aes(label=round(value,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
    theme_classic()+ scale_fill_brewer(type = "seq")+
    ylab("Completeness score")+xlab("Hospitals")+ggtitle("Discharge information per hospital(Realtime data)", subtitle = "Status at time of admission,discharge diagnosis & outcome at discharge")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
 ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
 ppt <- ph_with(x = ppt, value = clinic_p,
                location = ph_location_fullsize() ) 
 
 