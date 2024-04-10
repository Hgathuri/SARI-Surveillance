
source("00-functions.R")
##Pulling the data
token <- "39A19BFCDD0BBBCBE5D5DAA74FC66348"
url<- 'https://hsu.kemri-wellcome.org/redcap/'
dataObj <- redcap_project(api_url=url,
                          token = token,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()
DS.surveillance <- as.data.frame(dataObj$get_formatted_data())   
date_range <- Sys.Date()-30