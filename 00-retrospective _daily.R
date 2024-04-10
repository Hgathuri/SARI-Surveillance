##Pull data from Retrospective database
source("00-functions.R")
token_retro <- "53B612AB3F0620E020CD296AC0B467A2"
token_final <- "EDDAB744C9EC03B919A268CBF2052D44"
url<- 'http://searchtrial.kemri-wellcome.org/redcap/api/'
dataObj <- redcap_project(api_url=url,
                          token = token_retro,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()
DS.retrospective <- as.data.frame(dataObj$get_formatted_data())   
date_range <- Sys.Date()

fds_today <- filter(DS.retrospective, as.Date(dateofentry) == date_range )

###load the final_copy database for those using laptops
dataO <- redcap_project(api_url=url,
                          token = token_final,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataO$load_data()
fds_final <- dataO$get_formatted_data()
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

fds_daily_final_copy <- filter(fds_final, clerk_id %in% clerks ) %>% filter(as.Date(dateofentry) == date_range)


###Bind thw 2 datasets

allDS <- rbind(fds_today, fds_daily_final_copy)
###Shorten the hopital levels
##Hospital levels
aa <- c("Pumwani","Nakuru","Thika","Homabay","JOORTH","Naivasha","Kiambu","Machakos","MMLY","Mbagathi","Kerugoya","Karatina","Nyeri","Kisumu","Vihiga",
        "Kakamega","Busia","Kitale","Embu","Bungoma","World Friends","Migori","Macalder","Malela","Kisii")

levels(allDS$hosp_id) <- aa

Retro_clerks <- select(allDS,"record_id","clerk_id","dateofentry","hosp_id") %>% dplyr::group_by(hosp_id, clerk_id) %>% dplyr::summarise(total= length(record_id))

write.csv(Retro_clerks, "entries_per_clerk_per_day.csv")
###Write to power point
site_summary <- Retro_clerks %>% dplyr::group_by(hosp_id) %>% dplyr::summarise(total = sum(total))
###pLOT
ppt <- read_pptx()
ppt <- add_slide(ppt, "Title Slide", master = "Office Theme")

  p <- ggplot(site_summary, aes(x=hosp_id,y=total)) +geom_bar(stat = "identity", position=position_stack(),color="black")+
    geom_text(aes(label=total), vjust=-0.5, color="black",
              position = position_stack(1.0), size=2.5)+ scale_fill_brewer(palette="Paired")+
   ylab("Records")+xlab("Hospital")+ggtitle("Retrospective Data Collection: Entries per day")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
  
  ppt <- ph_with(x = ppt, value = p,
                 location = ph_location_fullsize() )

print(ppt, target = "Total summaries.pptx")


