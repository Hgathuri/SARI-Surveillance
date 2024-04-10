Investigate <- c("full_haemogram_done","hb","malaria_test")
# Investigate.ds <- select(BDS_full,c(keys,Investigate))
Investigate.ds <- select(Full.ds,c(keys,Investigate))

Investigate.ds[Investigate.ds==""] <- NA
#Investigate.ds[Investigate.ds== "Empty"] <- NA
Investigate.ds[Investigate.ds== "-1.0"] <- NA

investige.missing <- Investigate.ds %>% mutate(haemo_score = ifelse(is.na(full_haemogram_done),0,1), hb_score = ifelse(is.na(hb) & is.na(full_haemogram_done),0,1), mal_test_score = ifelse(is.na(malaria_test),0,1)) 

invest.score <- investige.missing %>% group_by(hosp_id) %>%  dplyr::summarise(haemo_score = mean(haemo_score,na.rm = T),hb_score = mean(hb_score,na.rm = T),mal_test_score = mean(mal_test_score,na.rm = T) )
# levels(invest.score$hosp_id) <- aa

invest.final <- melt(as.data.frame(invest.score), id="hosp_id")

###Plot
inv.plot <- ggplot(invest.final, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color="black")+
  geom_text(aes(label=round(value,2)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+ scale_fill_brewer(type = "seq")+
  ylab("Completeness score")+xlab("Hospitals")+ggtitle("Investigations documentation(Full datasets)", subtitle = "Full haemogram, Hb reported and malaria test done ")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = inv.plot,
               location = ph_location_fullsize() )


###Initial management
Init_management <- c("received_oxygen","is_ventilated","iv_fluids_given")
# Init.ds <- select(BDS_full,c(keys,Init_management))
Init.ds <- select(Full.ds,c(keys,Init_management))
Init.ds[Init.ds==""] <- NA
#Init.ds[Init.ds== "Empty"] <- NA
Init.ds.missing <- Init.ds %>% mutate(received_oxy_score = ifelse(is.na(received_oxygen),0,1), is_ventilated_score = ifelse(is.na(is_ventilated),0,1),IV_fluids_score = ifelse(is.na(iv_fluids_given),0,1)) 
init.score <- Init.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(received_oxy_score = mean(received_oxy_score,na.rm = T),is_ventilated_score = mean(is_ventilated_score,na.rm = T),IV_fluids_score = mean(IV_fluids_score,na.rm = T))
# levels(init.score$hosp_id) <- aa

init.score <- melt(as.data.frame(init.score), id="hosp_id")


init.plot <- ggplot(init.score, aes(x=hosp_id,y=value, fill=variable)) +geom_bar(stat="identity", position=position_dodge(), color="black")+
  geom_text(aes(label=round(value,2)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+ scale_fill_brewer(type = "seq")+
  ylab("Completeness score")+xlab("Hospitals")+ggtitle("Initial management documentation(Full datasets)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = init.plot,
               location = ph_location_fullsize() )

