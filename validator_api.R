list.of.packages <- c("data.table", "jsonlite","reshape2","XML","httr","splitstackshape","dotenv")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/IATI-results-framework-2022"

setwd(wd)

# Load publisher table from registry, use to create full publisher reporting_org_ref table
org_ids = fread("iati-publishers.csv")
org_ids = unique(org_ids[,c("IATI Organisation Identifier","Publisher","publisher_name")])

# Load total spend, recalculate categories, merge publishers
setnames(org_ids,"IATI Organisation Identifier","reporting_org_ref")
org_categories = fread("IATI_publishers_by_spend.csv")
org_categories = merge(org_categories, org_ids, by="reporting_org_ref", all=T)
org_categories$category[which(is.na(org_categories$category))] = "No spend data for 2019-2021"

org_categories = subset(org_categories,year %in% c(2019,2020,2021))
org_cat_max = data.table(org_categories)[,.(category=category[which.max(.SD$value_usd)],year=year[which.max(.SD$value_usd)]),by=.(reporting_org_ref)]
org_cat_max = merge(org_cat_max, org_ids, by="reporting_org_ref",all=T)
org_cat_max$category[which(is.na(org_cat_max$category))] = "No spend data for 2019-2021"
org_cat_max$reporting_org_ref = NULL
setnames(org_cat_max,"year","spend_category_year")
setnames(org_cat_max,"category", "spend_category")

load_dot_env()
api_key = Sys.getenv("VALIDATOR_KEY")

base_url = "https://func-validator-services-prod.azurewebsites.net/api"

message_endpoint = paste0(base_url,"/pub/stats/all?date=2022-01-13&format=csv")

req = GET(
  URLencode(message_endpoint),
  add_headers(`x-functions-key` = api_key)
)
all_messages=content(req)

summary_endpoint = paste0(base_url,"/pub/stats/summary_aggregate?date=2022-01-13&format=csv")

req = GET(
  URLencode(summary_endpoint),
  add_headers(`x-functions-key` = api_key)
)
all_summary=content(req)

all_messages = all_messages[order(all_messages$publisher_name),]
all_messages = merge(all_messages, org_cat_max, by="publisher_name", all.x=T)
all_messages$spend_category[which(is.na(all_messages$spend_category))] = "No spend data for 2019-2021"

all_summary = merge(all_summary, org_cat_max, by="publisher_name", all.x=T)
all_summary$spend_category[which(is.na(all_summary$spend_category))] = "No spend data for 2019-2021"
fwrite(all_messages,"all_messages.csv")
fwrite(all_summary,"all_summaries.csv")
