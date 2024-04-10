specimen <- c("was_specimen_collected","why_specimen_not_collected","date_of_specimen_collection","preliminary_lab_results")

specimen.ds <- select(Full.ds,c(keys,specimen)) #%>% filter(type_of_entry == "Real-time")
# specimen.ds <- select(BDS_full,c(keys,"type_of_entry",specimen)) %>% filter(type_of_entry == "Real-time")

# levels(specimen.ds$hosp_id) <- aa

specimen.ds[specimen.ds==""] <- NA
specimen.ds[specimen.ds== "Empty"] <- NA

specimen.scores <- specimen.ds %>% mutate(specimen_collected_score = ifelse(is.na(was_specimen_collected),0,1)) %>% group_by(hosp_id) %>% 
  dplyr::summarise(specimen_collected_score = mean(specimen_collected_score,na.rm = T))

###Was specimen summaries
so <- ggplot(specimen.scores, 
             aes(x = hosp_id, 
                 y = specimen_collected_score)) +
  geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(specimen_collected_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+
  ylab("completeness score")+xlab("Hospitals")+ggtitle("Specimen collection documented", subtitle = "Real time assessment datasets only")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = so,
               location = ph_location_fullsize() )
