source("00-libraries.R")
##Pull data from both projects
all_token <- "EDDAB744C9EC03B919A268CBF2052D44"
retro_token <- "53B612AB3F0620E020CD296AC0B467A2"
url<- 'http://searchtrial.kemri-wellcome.org/redcap/api/'
dataObj <- redcap_project(api_url=url,
                          token = retro_token,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()

allDS <- dataObj$get_formatted_data()
retroDS <-dataObj$get_formatted_data()

keys <- c("record_id","redcap_event_name","hosp_id","clerk_id")
vars <- c("was_specimen_collected","preliminary_lab_results","outcome_at_discharge")

###Outcome covid testing
covid_outcome <- select(allDS, c(keys,vars))
 

covid_outcome <- filter(fds_all, was_specimen_collected == "Yes" & outcome_at_discharge == "Dead") %>%
  select(-c("redcap_event_name"))

write.csv(covid_outcome, "covid_19_testing.csv")

####Exclude  clerks 

retro_data <- allDS %>% filter(clerk_id %in% clerks)
DS <- lapply(unique(retro_data$record_id), function(x,var){
  DS.x<- filter(retro_data, record_id==x)
  vars = arm2
  x1 <- DS.x[DS.x$redcap_event_name  == "preliminary_admiss_arm_1",] %>% select(-vars)
  x2 <- DS.x[DS.x$redcap_event_name  == "clinical_progressi_arm_1",] %>% select(record_id,vars)
  FDS <-  left_join(x1,x2, by = "record_id")
})

fds_all <- do.call(rbind, DS) 
##format the retrospective database
DS <- lapply(unique(retroDS$record_id), function(x,var){
  DS.x<- filter(retroDS, record_id==x)
  vars = arm2
  x1 <- DS.x[DS.x$redcap_event_name  == "preliminary_admiss_arm_1",] %>% select(-vars)
  x2 <- DS.x[DS.x$redcap_event_name  == "clinical_progressi_arm_1",] %>% select(record_id,vars)
  FDS <-  left_join(x1,x2, by = "record_id")
})

fds_all_2 <- do.call(rbind, DS) 

combined <- rbind(fds_all, fds_all_2) %>% select("record_id","hosp_id","date_adm","dateofentry")
combined_yearly <- mutate(combined, year = format(as.Date(date_adm),"%b-%Y")) %>% filter(!is.na(year)) 
combined_yearly$year <- as.factor(combined_yearly$year)

###Group by hospital by month

summarized_ds <- combined_yearly%>%  dplyr::group_by(hosp_id,year) %>%dplyr::summarise(total= length(record_id))  
###

x = "Busia County Referral Hospital"

busia <- filter(summarized_ds, hosp_id == x) 
