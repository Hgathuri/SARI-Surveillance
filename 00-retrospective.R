##Pull data from Retrospective database
source("00-functions.R")
token <- "53B612AB3F0620E020CD296AC0B467A2"
url<- 'http://searchtrial.kemri-wellcome.org/redcap/api/'
dataObj <- redcap_project(api_url=url,
                          token = token,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()
DS.retrospective <- as.data.frame(dataObj$get_formatted_data())   
date_range <- Sys.Date()-15

DS <- lapply(unique(DS.retrospective$record_id), function(x,var){
  DS.x<- filter(DS.retrospective, record_id==x)
  var = arm2
  x1 <- DS.x[DS.x$redcap_event_name  == "preliminary_admiss_arm_1",] %>% select(-var)
  x2 <- DS.x[DS.x$redcap_event_name  == "clinical_progressi_arm_1",] %>% select(record_id,var)
  FDS <-  left_join(x1,x2, by = "record_id")
})

fds <- do.call(rbind, DS)  
# fds <- read.csv("fds_retrospective.csv")
fds_all <- read.csv("fds_all.csv")
fds_weekly <- filter(fds, as.Date(dateofentry) >= date_range )
fds_weekly_all <- filter(fds_all, clerk_id %in% clerks ) %>% filter(as.Date(dateofentry) >= date_range)


allDS <- rbind(fds_weekly, fds_weekly_all)
###Combine datasets


Retro_clerks <- select(allDS,"record_id","clerk_id","dateofentry","hosp_id") %>% dplyr::group_by(hosp_id,clerk_id,dateofentry) %>% dplyr::summarise(total= length(record_id))

##Bind datasets

# combined_all <- rbind(Retro_clerks_all, Retro_clerks)
mmly <- filter(combined_all, hosp_id == "Bungoma County Referral Hospital")

d1 <- read.csv("retro_main_12082020.csv")
d2 <- read.csv("retro_real_12082020.csv")

combined <- read.csv("retro.csv")
###Write to power point

###pLOT
ppt <- read_pptx()
ppt <- add_slide(ppt, "Title Slide", master = "Office Theme")

sapply(unique(combined$hosp_id), function(x){
  ds <- filter(combined, hosp_id == x)
  p <- ggplot(ds, aes(x=dateofentry,y=total)) +geom_bar(stat="identity", position=position_stack(),color="black")+
    geom_text(aes(label=total), vjust=-0.5, color="black",
              position = position_stack(1.0), size=2.5)+ scale_fill_brewer(palette="Paired")+
   ylab("Records")+xlab("Date of entry")+ggtitle("Retrospective Data Collection: Entries per day", subtitle = paste(ds$hosp_id, "Average entries per day:", round(mean(ds$total),2), sep = " - "))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
  
  ppt <- ph_with(x = ppt, value = p,
                 location = ph_location_fullsize() )
})
print(ppt, target = "Daily workload_report_per_clerk_per_day.pptx")


###Biodata details

Retro <- filter(allDS, type_of_entry == "Retrospective entry") 
ppt <- read_pptx()
