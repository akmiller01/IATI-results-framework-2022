list.of.packages <- c("data.table","dotenv", "httr", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-results-framework-2022")

load_dot_env()
api_key = Sys.getenv("API_KEY")

results_list = list()
results_index = 1
docs = rep(0, 1000)

start_num = 0
api_url_base = paste0(
  "https://api.iatistandard.org/datastore/activity/select?",
  "q=transaction_value:[* TO *] AND transaction_transaction_type_code:(3 OR 4)&",
  "fl=iati_identifier default_currency reporting_org_ref ",
  "transaction_transaction_type_code transaction_transaction_date_iso_date ",
  "transaction_value transaction_value_value_date transaction_value_currency&",
  "sort=iati_identifier asc&",
  "rows=1000&start="
)
while(length(docs)==1000){
  message(start_num)
  req = GET(
    URLencode(paste0(api_url_base, format(start_num, scientific=F))),
    add_headers(`Ocp-Apim-Subscription-Key` = api_key)
  )
  res = content(req)
  docs = res$response$docs
  docs.df = rbindlist(docs, fill=T)
  docs.df = docs.df %>% mutate_all(function(x){
    x[which(sapply(x, length)==0)] = NA
    return(unlist(x))
  })
  docs.df = subset(docs.df, transaction_transaction_type_code %in% c(3, 4))
  results_list[[results_index]] = docs.df
  results_index = results_index + 1
  start_num = start_num + 1000
}

results = rbindlist(results_list, fill=T)
save(results,file="ds_api_results.RData")

