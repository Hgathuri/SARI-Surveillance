# surv_data <- read.csv("sureveillance_data_29072020.csv")
##Pull fina copy database data
##Pull the retrospective data
###Combine the 2 datasets
combined <- rbind(surv_data,fds_all)
 combined <- read.csv("all_datasets.csv")
###Filter date_adm, dateofentry,hosp_id

admissions <- combined %>% select("record_id","hosp_id","date_adm","is_minimum","date_of_discharge","date_of_death") %>% filter(as.Date(date_adm) <= as.Date("2020-03-31"))

admission_year <- admissions %>% mutate( year = format(as.Date(date_adm), "%Y")) %>%
  dplyr::group_by(hosp_id,year) %>% dplyr::summarise(total = length(record_id))

Data2016 <- filter(admission_year, year >= 2016) ###89-68

ppt <- read_pptx()
ppt <- add_slide(ppt, "Title Slide", master = "Office Theme")

sapply(unique(Data2016$hosp_id), function(x){
  ds <- filter(Data2016, hosp_id == x)
  p <- ggplot(ds, aes(x=year,y=total)) +geom_bar(stat="identity", fill = "steelblue",position=position_dodge(),color="black")+
    geom_text(aes(label=total), vjust=-0.5, color="black",
              position = position_stack(1.0), size=2.5)+
    ylab("Cummulative entries for each year")+xlab("Year")+ggtitle("Retrospective Entries: Workload per year", subtitle = unique(ds$hosp_id))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
  
  ppt <- ph_with(x = ppt, value = p,
                 location = ph_location_fullsize() )
})
print(ppt, target = "Yearly_workload_report.pptx")
