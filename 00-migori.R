fds.summary.retro <-as.data.frame(Migori) %>% 
  select(c("record_id","is_minimum","dateofentry"))%>% dplyr::group_by(dateofentry,is_minimum) %>%dplyr::summarise(total= length(record_id))  
##Exclude

fds.summary.retro$is_minimum <- as.factor(fds.summary.retro$is_minimum)
levels(fds.summary.retro$is_minimum) <- c("Full dataset","Minimum dataset")

p <- ggplot(fds.summary.retro, aes(x=dateofentry,y=total, fill=is_minimum)) +geom_bar(stat="identity", position=position_stack(),color="black")+
  geom_text(aes(label=total), vjust=-0.5, color="black",
            position = position_stack(1.0), size=2.5)+
  scale_fill_brewer(palette="Paired")+ylab("Total entries")+xlab("Hospital")+ggtitle("Retro/Prospective: Total entries per site", subtitle = "Migori County Hospital")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
