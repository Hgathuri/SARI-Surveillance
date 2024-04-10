 symptoms <- c("date_onset_of_symptoms","onset_period")
 # symptoms.ds <- select(BDS_full, c(keys, symptoms))
 symptoms.ds <- select(Full.ds, c(keys, symptoms))
 symptoms.ds[symptoms.ds==""] <- NA
 symptoms.ds[symptoms.ds== "Empty"] <- NA
 symptoms.ds[symptoms.ds== "1913-01-01"] <- NA

##Compute the scores
 sys.missing <- symptoms.ds %>% mutate(sys_score = ifelse(is.na(date_onset_of_symptoms) & is.na(onset_period),0,1)) 
 
 sys.scores <- sys.missing %>% group_by(hosp_id) %>%  dplyr::summarise(sys_score = mean(sys_score,na.rm = T))
 
 # levels(sys.scores$hosp_id) <- aa
 

 
 sys.plot <- ggplot(sys.scores, 
                    aes(x = hosp_id, 
                        y = sys_score)) +
   geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(sys_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
   theme_classic()+
   ylab("completeness score")+xlab("Hospitals")+ggtitle("Onset of symptoms/onset period documented", subtitle = "Full datasets only")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
 ppt <- ph_with(x = ppt, value = sys.plot,
                location = ph_location_fullsize() )
 
 

 ###Vital signs
 vsigns <- c("temp","resp_rate","oxygen_sat","blood_pressure_sys","weight","height","muac","observed_weight")
 # vital.ds <- select(BDS_full,c(keys,vsigns))
 vital.ds <- select(Full.ds,c(keys,vsigns))
 vital.ds[vital.ds=="-1"] <- NA
 vital.ds[vital.ds== "Empty"] <- NA
 vital.ds[vital.ds== "-1.00"] <- NA

 vitals.scores <- vital.ds%>% mutate(temp_score = ifelse(is.na(temp),0,1)) %>% mutate(resp_score = ifelse(is.na(resp_rate),0,1)) %>%
   mutate(oxy_score = ifelse(is.na(oxygen_sat),0,1))%>% mutate(bp_score = ifelse(is.na(blood_pressure_sys),0,1)) %>% 
   mutate(nutritional_score = ifelse(is.na(weight) & is.na(height) & is.na(muac) & is.na(observed_weight),0,1))

 ##Summaries
 
 vital.summaries <- vitals.scores %>% group_by(hosp_id) %>% 
   dplyr::summarise(temp_score = mean(temp_score,na.rm = T),resp_score = mean(resp_score,na.rm = T),oxy_score = mean(oxy_score,na.rm = T),bp_score = mean(bp_score,na.rm = T),nutritional_score = mean(nutritional_score,na.rm = T))
 
 # levels(vital.summaries$hosp_id) <- aa

 ###Reshape the data
 
 vital.final <- melt(as.data.frame(vital.summaries), id="hosp_id")

 v.plot <- ggplot(vital.final, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color="black")+
   geom_text(aes(label=round(value,2)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
   scale_fill_brewer(type = "seq")+
   theme_classic()+
   ylab("Completeness score")+xlab("Hospitals")+ggtitle("Vital signs documentation(Full datasets)")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
 ppt <- ph_with(x = ppt, value = v.plot,
                location = ph_location_fullsize() )
 
 
 
 ###Diagnosis
 dx <- c(keys,grep("dx_adm",names(Full.ds),value = T),"other_diagnosis")
 dx.ds <- select(Full.ds,c(keys,dx))
 
 dx.ds[dx.ds=="No"] <- NA
 dx.ds[dx.ds== "Empty"] <- NA
 
 dx.ds.missing <- dx.ds %>% mutate(dx.score = ifelse(is.na(dx_adm___1) & is.na(dx_adm___2) & is.na(dx_adm___3) & is.na(dx_adm___4) & is.na(dx_adm___5) & is.na(dx_adm___6) & is.na(dx_adm___7) & is.na(dx_adm___8) & is.na(dx_adm___9) & is.na(other_diagnosis),0,1))

 dx.summaries <- dx.ds.missing %>% group_by(hosp_id) %>% dplyr::summarise(dx_score = mean(dx.score,na.rm = T))
 # levels(dx.summaries$hosp_id) <- aa
 ##Plot
 dx.plot <- ggplot(dx.summaries, 
                   aes(x = hosp_id, 
                       y = dx_score)) +
   geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(dx_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
   theme_classic()+
   ylab("completeness score")+xlab("Hospitals")+ggtitle("Admission diagnosis documented", subtitle = "Full datasets only")+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
 ppt <- ph_with(x = ppt, value = dx.plot,
                location = ph_location_fullsize() ) 
 