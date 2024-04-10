source("00-functions.R")
token =""
url = "https://searchtrial.kemri-wellcome.org/api/"

dataObj <- redcap_project(api_url=url,
                          token = token,
                          chunked=T,
                          chunksize = 10000,
                          local = FALSE
)
dataObj$load_data()

DS.surveillance <- as.data.frame(dataObj$get_formatted_data())   