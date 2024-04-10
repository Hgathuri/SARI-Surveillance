   

library(doFuture)
library(parallel)


# Set project path
home.dir <- ifelse(Sys.info()["sysname"] == "Windows", Sys.getenv("USERPROFILE"), Sys.getenv("HOME"))
m.project.path <- path.expand(file.path(home.dir
                                        ,"hsuApps"
                                        ,"weekly_report"))

m.project.path <- gsub("\\\\","/",m.project.path)
if(!file.exists(m.project.path)) {
   if(dir.create(m.project.path, recursive = TRUE))
      stop("The project directory \""
           ,m.project.path
           ,"\"  has been created!\nPlease fill it with the relevant files and folders!")
   else
      stop("The project directory \""
           ,m.project.path
           ,"\"  could not be created!")
}

setwd(m.project.path)

source("00-functions.R")
##Pulling the data
#token <- ""
token <-  ""
url<- 'https://......./'
dataObj <- redcap_project(api_url=url,
                          token = token,
                          chunked=T,
                          chunksize = 1000,
                          local = FALSE,
                          parallel=T
                          
)
dataObj$load_data()
DS.surveillance <- as.data.frame(dataObj$get_formatted_data())   
date_range <- Sys.Date()-7
date_range2 <- Sys.Date()-9

# fds_weekly <- filter(DS.surveillance, as.Date(dateofentry) >= date_range ))

arm2 <- c("was_specimen_collected",
          "why_specimen_not_collected",
          "date_of_specimen_collection",
          "test_conducted___1",
          "test_conducted___2",
          "test_conducted____1",
          "specimen_type___1",                        
          "specimen_type___2",                         
          "specimen_type___3",                         
          "specimen_type___4",                         
          "specimen_type___5" ,                        
          "specimen_type___6",
          "other_specimen_type",
          "date_specimen_send_lab",
          "name_of_confirming_lab",
          "preliminary_lab_results",
          "health_status_adm",
          "if_severely_ill",
          "if_severely_ill_6___1",                     
          "if_severely_ill_6___2",                     
          "if_severely_ill_6___4",                    
          "if_severely_ill_6___3",                    
          "if_severely_ill_6____1",
          "health_status_day2",
          "if_severely_ill_2",
          "if_severely_ill_4___1",                     
          "if_severely_ill_4___2",                    
          "if_severely_ill_4___4",                     
          "if_severely_ill_4___3",                     
          "if_severely_ill_4____1",
          "health_status_day3",
          "if_severely_ill_3",
          "if_severely_ill_5___1",                     
          "if_severely_ill_5___2",                     
          "if_severely_ill_5___4",                     
          "if_severely_ill_5___3",                     
          "if_severely_ill_5____1",
          "dx_discharge___1",                          
          "dx_discharge___2"  ,                        
          "dx_discharge___3" ,                         
          "dx_discharge___4" ,                         
          "dx_discharge___5" ,                        
          "dx_discharge___6",                        
          "dx_discharge___7" ,                         
          "dx_discharge___8" ,                         
          "dx_discharge___10",
          "dx_discharge___11",
          "dx_discharge____1",
          "other_dx_discharge",
          "other_dx_discharge_2",
          "other_dx_discharge_3",
          "outcome_at_discharge",
          "date_of_discharge",
          "date_of_death"
)

##function to split data by arm and merge

# DS <- lapply(unique(DS.surveillance$record_id), function(x,var){
#    DS.x<- filter(DS.surveillance, record_id==x)
#    var = arm2
#    x1 <- DS.x[DS.x$redcap_event_name  == "preliminary_admiss_arm_1",] %>% select(-var)
#    x2 <- DS.x[DS.x$redcap_event_name  == "clinical_progressi_arm_1",] %>% select(record_id,var)
#    FDS <-  left_join(x1,x2, by = "record_id")
#  })
# 
# fds_all <- do.call(rbind, DS)  

x1=DS.surveillance %>% filter(redcap_event_name  == "preliminary_admiss_arm_1") %>% select(-arm2)
x2=DS.surveillance %>% filter(redcap_event_name  == "clinical_progressi_arm_1") %>% select(record_id,arm2)
FDS=left_join(x1,x2,by='record_id')

fds_all <- FDS

##remove adm dates before May 2020
#fds_all2 <- filter(fds_all) %>% filter(date_adm >= as.Date("2020-05-01")) %>% filter(date_adm <= as.Date("2022-11-30"))
##Remove records whose dateofentry < adm date
#fds_all3 <- filter(fds_all2, as.Date(dateofentry) >= as.Date(date_adm))
##Remove records whose date of discharge is before date of adm
#fds_all5 <- filter(fds_all3, as.Date(date_of_discharge) >= as.Date(date_adm) | as.Date(date_of_death) >= as.Date(date_adm))
#record_ids_R <- fds_all$record_id
#write.csv(record_ids_R,"C:\\Users\\hgathuri\\OneDrive - Kemri Wellcome Trust\\Git\\SARI Surveillance\\SARI_Full dataset\\record_ids_R.csv")


#write.csv(fds_all5,"C:\\Users\\hgathuri\\OneDrive - Kemri Wellcome Trust\\Git\\SARI Surveillance\\SARI_Full dataset\\Surveillance_data_Dec_1_v4.csv")
#fds_all <-subset(fds_all, hosp_id!="Kenyatta University Teaching and Referral Hospital")
##Filter based on dateofentry
fds_weekly <- filter(fds_all, as.Date(dateofentry) >= date_range)

##Hospital levels
aa <- c("Pumwani","Nakuru","Thika","Homabay","JOORTH","Naivasha","Kiambu","Machakos","MMLY","Mbagathi","Kerugoya","Karatina","Nyeri","Kisumu","Vihiga",
        "Kakamega","Busia","Kitale","Embu","Bungoma","World Friends","Migori","Macalder","Malela","Kisii","CGTRH", "KUTRH")

levels(fds_weekly$hosp_id) <- aa
#fds_weekly<- read.csv("weekly_all_14072021 (1).csv")
# fds_weekly_retro <- read.csv("weekly_ksm_30032021.csv")

# fds_weekly <- rbind(fds_weekly_real, fds_weekly_retro)
###Total summaries
###subset by the type of entries
clerks <- c("Purity Shilako Marani",
            "Charlene Makungu",
            "Sheila Inyanji Indolio",
            "Eunice Njeri Karanja",
            "Rosemary Wanjiku Kuria",
            "Kevin Otieno Ochogo",
            "Eunice Opondo",
            "Maureen Owenje",
            "Sharon Akoth Otieno",
            "Elizabeth Auma Oyugi",
            "Polyfine Ruguru Njiru",
            "Gloria Achieng Mbayi",
            "Elizabeth Gathoni",
            "Wanjiru Rose Wanjiku",
            "Rosemary Wanjiku Kuria"
            )

# Retro_clerks_all <- filter(fds_weekly, clerk_id %in% clerks) %>% select("record_id","clerk_id","dateofentry","hosp_id") %>% dplyr::group_by(hosp_id,clerk_id,dateofentry) %>% dplyr::summarise(total= length(record_id))
# 
# other_retro <- read.csv("retrospective.csv") %>% filter(as.Date(dateofentry) >= date_range)%>% select("record_id","clerk_id","dateofentry","hosp_id") %>% dplyr::group_by(hosp_id,clerk_id,dateofentry) %>% dplyr::summarise(total= length(record_id))
# combined <- rbind(Retro_clerks_all,other_retro)
#Retro <- filter(fds_weekly, type_of_entry == "Retrospective entry")  
#Retro <- filter(fds_weekly, type_of_entry == "Retrospective entry")
real_time <- filter(fds_weekly, date_adm >= date_range) 
retro <- filter (fds_weekly, date_adm < date_range2)
# May_data <- filter(fds, is_minimum==0) %>% filter(date_adm >= as.Date("2020-05-01")) %>% filter(date_adm <= as.Date("2020-05-31"))
##Filter today's data
fds_today <- filter(fds_weekly, as.Date(dateofentry) != as.Date("2021-03-10"))
###Key varibles
keys <- c("record_id","hosp_id")
fds.summary.retro <-as.data.frame(retro) %>%
   select(c("record_id","hosp_id","is_minimum"))%>% dplyr::group_by(hosp_id,is_minimum) %>%dplyr::summarise(total= length(record_id))  
##Exclude 
fds.summary.realtime <-as.data.frame(real_time)  %>%
   select(c("record_id","hosp_id","is_minimum"))%>% dplyr::group_by(hosp_id,is_minimum) %>%dplyr::summarise(total= length(record_id))  


###Convert is_minimum to a factor
fds.summary.realtime$is_minimum <- as.factor(fds.summary.realtime$is_minimum)
levels(fds.summary.realtime$is_minimum) <- c("Full dataset","Minimum dataset")

fds.summary.retro$is_minimum <- as.factor(fds.summary.retro$is_minimum)
levels(fds.summary.retro$is_minimum) <- c("Full dataset","Minimum dataset")
###Plot a stacked chart
###Age computations
# age <- select(May_data, c("age_days", "age_months","age_years", "was_specimen_collected"))
# age[is.na(age)] = 0
# age <- mutate(age,age_y = age_days/365+age_months/12 +age_years) 
# age_changed <- age %>% mutate(age_range = ifelse(age_y > 0 & age_y <34.5,"0-34","")) %>% mutate(age_range = ifelse( age_y >= 34.5 & age_y < 69,"34-69",age_range)) %>%
#               mutate(age_range = ifelse( age_y >= 69,"69-Above",age_range))

#                    transform, label_ypos=cumsum(total))
##Realtime

##Format vihiga/Nyeri Data
# errors <- fds_weekly %>% select("record_id","hosp_id","clerk_id") %>% filter(hosp_id )

###Add to powerpoint document
ppt <- read_pptx()
ppt <- add_slide(ppt, "Title Slide", master = "Office Theme")
#ppt <- ph_with(ppt, value = "Weekly Surveillance Report", location = ph_location_type(type = "title"))
p <- ggplot(fds.summary.realtime, aes(x=hosp_id,y=total, fill=is_minimum)) +geom_bar(stat="identity", position=position_stack(),color="black")+
   geom_text(aes(label=total), vjust=-0.5, color="black",
             position = position_stack(1.0), size=2.5)+
   scale_fill_brewer(palette="Paired")+ylab("Total entries")+xlab("Hospital")+ggtitle("Realtime: Total entries per site")+
   theme_classic()+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = p,
               location = ph_location_fullsize() )
###Retrospective entries
Qp <- ggplot(fds.summary.retro, aes(x=hosp_id,y=total, fill=is_minimum)) +geom_bar(stat="identity", position=position_stack(),color="black")+
   geom_text(aes(label=total), vjust=-0.5, color="black",
             position = position_stack(1.0), size=2.5)+
   scale_fill_brewer(palette="Paired")+ylab("Total entries")+xlab("Hospitals")+ggtitle("Retrospective: Total entries per site")+
   theme_classic()+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = Qp,
               location = ph_location_fullsize() )
###Full dataset
Full.ds <- filter(real_time, is_minimum == 0)

source("00-biodata.R")
source("00-symptoms.R")
source("00-investigations.R")
source("00-covid-lab.R")
source("00-diagnosis.R")
source("00_low_perf.R")

print(ppt, target = "surv_weekly_DQA_report.pptx")

##Homabay county
Totals_per_doe <- fds_weekly %>% dplyr::group_by(hosp_id)%>% dplyr::summarise(total = length(record_id))
ggplot(Totals_per_doe, aes(x=hosp_id,y=total)) +geom_bar(stat="identity",fill="steelblue",color="black")+
   geom_text(aes(label=total), vjust=-0.5, color="black",
             position = position_stack(1.0), size=2.5)+
   scale_fill_brewer(palette="Paired")+ylab("Total entries")+xlab("Hospital")+ggtitle("Total entries per site based on Date of Entry")+
   theme_classic()+
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

