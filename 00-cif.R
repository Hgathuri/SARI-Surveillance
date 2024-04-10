
cif <- filter(fds_all, dateofentry >= as.Date("2020-07-01")) %>% select("record_id","hosp_id","source_doc","dateofentry")%>% mutate(month = format(as.Date(dateofentry),"%b")) %>% dplyr::group_by(hosp_id,source_doc,month) %>% dplyr::summarise(total= length(record_id))

ppt <- read_pptx()
ppt <- add_slide(ppt, "Title Slide", master = "Office Theme")
sapply(unique(cif$hosp_id), function(x){
  ds <- filter(cif, hosp_id == x)
p <- ggplot(ds, aes(x=month,y=total, fill=source_doc)) +geom_bar(stat="identity", position=position_stack(),color="black")+
  geom_text(aes(label=total), vjust=-0.5, color="black",
            position = position_stack(1.0), size=2.5)+
  scale_fill_brewer(palette="Paired")+ylab("Total records")+xlab("Month")+ggtitle(paste(ds$hosp_id,": CIF used"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ppt <- add_slide(ppt, "Title and Content", master = "Office Theme")
ppt <- ph_with(x = ppt, value = p,
               location = ph_location_fullsize() )
})
print(ppt, target = "CIF.pptx")
