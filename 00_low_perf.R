
#Onset of symptoms below 100%
onset_df <-  sys.missing %>% filter(sys_score<1)

write.csv(onset_df, "onset_of_symptoms_doc_missing.csv", row.names = F)

#vitals below 100%
vitals_df<- vitals.scores %>% filter(temp_score<1 | oxy_score<1 | bp_score<1 | nutritional_score<1)
write.csv(vitals_df, "vital_signs_doc_missing.csv", row.names = F)

#investigations below 100%
invest_df <-  investige.missing %>% filter(haemo_score<1 | hb_score<1 | mal_test_score <1)
write.csv(invest_df, "investigations_doc_missing.csv", row.names = F)

#Initial management below 100%
init_df<- Init.ds.missing %>% filter(received_oxy_score<1 | is_ventilated_score<1 | IV_fluids_score<1)
write.csv(init_df, "initial_management_doc_missing.csv", row.names = F)

#specimen below 100%
specimen_score_df <- specimen.ds %>% mutate(specimen_collected_score = ifelse(is.na(was_specimen_collected),0,1))

specimen_df<- specimen_score_df %>% filter(specimen_collected_score<1)

write.csv(specimen_df, "specimen_doc_missing.csv", row.names = F)

#Residence missing
residence_df<- res.ds.missing %>% filter(l_score<1)
write.csv(residence_df, "residence_doc_missing.csv", row.names = F)
