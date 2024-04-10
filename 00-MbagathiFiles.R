data <- read.csv("Mbagathi_data_all.csv")
mb <- filter(data, hosp_id == "Mbagathi County Hospital")
start_date <- as.Date("2021-01-01")
data_2021 <- filter(mb, as.Date(date_adm) >= start_date)
Naivasha <- filter(data, hosp_id == "Naivasha Level 5 Hospital")
Machakos <- filter(data, hosp_id == "Machakos Level 5 Hospital")

###Temperature documentation

temp <- data %>% select(hosp_id,temp,full_haemogram_done, is_minimum)

scores <- temp %>% mutate("documented" = ifelse(is.na(temp),"0","1")) %>% filter(is_minimum == "0")
