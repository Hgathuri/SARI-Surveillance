library(RedcapData)
library(tidyverse)
token <- "2AF49E677CD01D91014967057F2009DA"
url<- 'https://hsu.kemri-wellcome.org/redcap/api/'
dataObj <- redcap_project(api_url=url,
                          token = token,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()

Data <-  dataObj$get_formatted_data()
##subset data (2018/2019/2020)

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2020-12-31")

data2021 <- filter(Data, as.Date(date_discharge) >= start_date & as.Date(date_discharge) <= end_date) %>%  mutate(age=(age_years*12)+age_mths)


##Entry level
signs <- filter(data2021, cough == "Yes" | diff_breath == "Yes") %>% filter(age >= 2 & age <= 59)

##Exclude wheeze
without_wheeze <- filter(signs, wheeze != "Yes")

##Exclude sever acute malnaurished
#exclude_sam <- filter(without_wheeze, whz == "<=4SD" | muac < 11.5)


##Severe pneumonia by signs,diagnosis,treatment)

sev_pneumo_signs <- filter(without_wheeze, vomit_everything == "Yes" | convulsions =="Yes" | oxygen_sat < 90 | c_cyanosis == "Yes" | grunting == "Yes" | can_drink == "No" | avpu == "Verbal response" | avpu == "Pain response" | avpu == "Unresponsive") %>% mutate(year=substr(date_discharge,1,4)) %>% mutate(alive = ifelse(outcome == "Died","Died","Alive")) %>% 
  filter(hosp_id %nin% hosps)

died <- 

table(sev_pneumo_signs$year,sev_pneumo_signs$alive)
hosp_total_died <- sev_pneumo_signs %>% filter(alive =="Died")  %>% dplyr::group_by(hosp_id,year) %>% dplyr::summarise(total = length(hosp_id))
hosp_total <- sev_pneumo_signs %>% dplyr::group_by(hosp_id,year) %>% dplyr::summarise(total_adm = length(hosp_id))

merge <- left_join(hosp_total,hosp_total_died, by = c("hosp_id","year")) %>% mutate( rate = total/total_adm)
hosp_total_died <- sev_pneumo_signs %>% filter(alive =="Died")  %>% dplyr::group_by(hosp_id) %>% dplyr::summarise(total = length(hosp_id))
hosp_total <- sev_pneumo_signs %>% dplyr::group_by(hosp_id) %>% dplyr::summarise(total_adm = length(hosp_id))

merge_year <- left_join(hosp_total,hosp_total_died, by = c("hosp_id")) %>% mutate( rate = total/total_adm)


###Group by sites
hosps <- c("Migori County Hospital","World Friends/Ruaraka Neema Hospital","Mbale Rural Health DC Hospital","Karatina District Hospital","Naivasha Level 5 Hospital", "Jaramogi Oginga Odinga Teaching and Referral Hospital","Homabay County Referral Hospital")

###Cumulative

### site specific case mortality rate(number/admitted) children with severe pneumonia

###Definitions - diagnosis of sev.pneumonia, signs of sev. pneumonia, sev. _xpengenta

###Age 2-59