##Vaccination dates missing
vacc_weekly <- filter(fds_weekly, is_vaccinated == 1)



all.ds <- select(fds_weekly, c(keys,"sex_at_birth","date_adm")) %>% filter(!is.na(hosp_id)) 
all.ds <- select(fds_weekly, c(keys,"sex_at_birth","date_adm","first_dose_date")) %>% filter(!is.na(hosp_id)) 
all.ds[all.ds==""] <- NA
all.ds[all.ds== "Empty"] <- NA



##Exclude retrospective work
fds_weekly <- filter(fds_weekly, !(clerk_id %in% clerks))
all.ds <- select(fds_weekly, c(keys,"sex_at_birth","date_adm")) %>% filter(!is.na(hosp_id)) 
all.ds <- select(fds_weekly, c(keys,"sex_at_birth","date_adm","first_dose_date")) %>% filter(!is.na(hosp_id)) 
all.ds[all.ds==""] <- NA
all.ds[all.ds== "Empty"] <- NA

##Monthly data


### Mutate

all.ds.missing <- all.ds %>% mutate(gender_missing = ifelse(is.na(sex_at_birth),0,1)) %>% mutate(year=substr(date_adm,1,4)) %>% mutate(date_adm_missing = ifelse(year == 1913,0,1))

all.ds.missing <- all.ds %>% mutate(gender_missing = ifelse(is.na(sex_at_birth),0,1)) %>% mutate(year=substr(date_adm,1,4)) %>% mutate(date_adm_missing = ifelse(year == 1913,0,1)) %>% 
  mutate(year=substr(first_dose_date,1,4)) %>% mutate(date_vacc_missing = ifelse(year == 1913,0,1))




gender.missing <- all.ds.missing %>% group_by(hosp_id) %>% dplyr::summarise(g_score = mean(gender_missing,na.rm = T))
adm.missing <- all.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(adm_score = mean(date_adm_missing,na.rm = T))
vacc.missing <- all.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(vacc_score = mean(date_vacc_missing,na.rm = T))

# levels(gender.missing$hosp_id) <- aa
# levels(adm.missing$hosp_id) <- aa
##Join the 2 datasets
combined <- merge(gender.missing,adm.missing, by.x = "hosp_id", by.y = "hosp_id") 

##Plot a bar chart
sp <- ggplot(combined, 
             aes(x = hosp_id, 
                 y = g_score)) +
  geom_bar(stat = "identity", fill="steelblue")+geom_text(aes(label=round(g_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+
  ylab("completeness score")+xlab("Hospitals")+ggtitle("Sex at birth documented(all records)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = sp,
               location = ph_location_fullsize() )

###Date of admission
Ap <- ggplot(combined, 
             aes(x = hosp_id, 
                 y = adm_score)) +
  geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(adm_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+
  ylab("completeness score")+xlab("Hospitals")+ggtitle("Date of admission documented(all records)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = Ap,
               location = ph_location_fullsize() )



### Vaccine dates missing

vc <- ggplot(combined, 
             aes(x = hosp_id, 
                 y = vacc_score)) +
  geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(adm_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+
  ylab("completeness score")+xlab("Hospitals")+ggtitle("Date of First dose Vaccine documented)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = Ap,
               location = ph_location_fullsize() )



###Location details 
###Missing residence
res <- c("county","sub_county","division","location","sub_location","village","location_text")

###status
res.ds <- select(fds_weekly,c(keys,res))
res.ds[res.ds==""] <- NA
res.ds[res.ds== "Empty"] <- NA

##all

###Compute the scores for missingness of location, sub-location,village or location text.
res.ds.missing <- res.ds %>% mutate(l_score = ifelse(is.na(location) & is.na(sub_location) & is.na(village) & is.na(location_text),0,1)) %>% filter(!is.na(hosp_id))
res.missing <- res.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(res_score = mean(l_score,na.rm = T))
# levels(res.missing$hosp_id) <- aa

#res.plot <- ggplot(res.missing, 
#                   aes(x = hosp_id, 
#                       y = res_score)) +
#  geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(res_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
#  theme_classic()+
#  ylab("completeness score")+xlab("Hospitals")+ggtitle("Residence documented(all)", subtitle = "Any of location, sub_location, village or location_text documented.")+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
#ppt <- ph_with(x = ppt, value = res.plot,
#               location = ph_location_fullsize() )

###Retrospective
# res.ds <- select(Retro,c(keys,res))
# # res.ds[res.ds==""] <- NA
# res.ds[res.ds== "Empty"] <- NA
# 
# ###Compute the scores for missingness of location, sub-location,village or location text.
# res.ds.missing <- res.ds %>% mutate(l_score = ifelse(is.na(location) & is.na(sub_location) & is.na(village) & is.na(location_text),0,1)) %>% filter(!is.na(hosp_id))
# res.missing <- res.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(res_score = mean(l_score,na.rm = T))
# 
# res.plot <- ggplot(res.missing, 
#                    aes(x = hosp_id, 
#                        y = res_score)) +
#   geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(res_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
#   theme_classic()+
#   ylab("completeness score")+xlab("Hospitals")+ggtitle("Residence documented(Retrospective data)", subtitle = "Any of location, sub_location, village or location_text documented.")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
# ppt <- ph_with(x = ppt, value = res.plot,
#                location = ph_location_fullsize() )

##Realtime data
res.ds <- select(real_time,c(keys,res))
res.ds[res.ds==""] <- NA
res.ds[res.ds== "Empty"] <- NA

###Compute the scores for missingness of location, sub-location,village or location text.
res.ds.missing <- res.ds %>% mutate(l_score = ifelse(is.na(location) & is.na(sub_location) & is.na(village) & is.na(location_text),0,1)) %>% filter(!is.na(hosp_id))
res.missing <- res.ds.missing %>% group_by(hosp_id) %>%  dplyr::summarise(res_score = mean(l_score,na.rm = T))

res.plot <- ggplot(res.missing, 
                   aes(x = hosp_id, 
                       y = res_score)) +
  geom_bar(stat = "identity", fill = "steelblue")+geom_text(aes(label=round(res_score,3)), vjust=-0.5, color="black",position = position_dodge(0.9), size=2.5)+
  theme_classic()+
  ylab("completeness score")+xlab("Hospitals")+ggtitle("Residence documented(Realtime data)", subtitle = "Any of location, sub_location, village or location_text documented.")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = res.plot,
               location = ph_location_fullsize() )


