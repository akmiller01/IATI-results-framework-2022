list.of.packages <- c("data.table", "jsonlite","reshape2","XML","httr","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "~/git/IATI-results-framework-2021"

setwd(wd)


# Load publisher table from registry, use to create full publisher reporting_org_ref table
org_ids = fread("iati-publishers.csv")
org_ids = unique(org_ids[,c("IATI Organisation Identifier","publisher")])

# Load total spend, recalculate categories, merge publishers
setnames(org_ids,"IATI Organisation Identifier","reporting_org_ref")
org_categories = fread("output/IATI_publishers_by_spend.csv")
org_categories = merge(org_categories, org_ids, by="reporting_org_ref", all=T)
org_categories$category[which(is.na(org_categories$category))] = "No spend data for 2018-2020"

org_categories = subset(org_categories,year %in% c(2018,2019,2020))
org_cat_max = data.table(org_categories)[,.(category=category[which.max(.SD$value_usd)],year=year[which.max(.SD$value_usd)]),by=.(reporting_org_ref)]
org_cat_max = merge(org_cat_max, org_ids, by="reporting_org_ref",all=T)
org_cat_max$category[which(is.na(org_cat_max$category))] = "No spend data for 2018-2020"
org_cat_max$reporting_org_ref = NULL
setnames(org_cat_max,"year","category_year")

test_url = "https://iativalidator.iatistandard.org/api/v1/stats?date=2021-02-15"

test_json = fromJSON(test_url)
publishers = test_json$publisher
meta.mess.list = list()
meta.sum.list = list()
meta.index = 1
pb = txtProgressBar(max=length(publishers),style=3)
for(test_publisher in publishers){
  setTxtProgressBar(pb, meta.index)
  row = subset(test_json,publisher==test_publisher)
  messStats = row$messageStats
  messCodes = names(messStats)
  messList = list()
  messIndex = 1
  for(messCode in messCodes){
    tmp.df = data.frame(code=messCode,text=messStats[,messCode]$text,count=messStats[,messCode]$count)
    messList[[messIndex]] = tmp.df
    messIndex = messIndex + 1
  }
  messDf = rbindlist(messList)
  messDf = subset(messDf,!is.na(count))
  if(nrow(messDf)==0){
    messDf = data.frame(code="0.0.0",text="No messages for publisher",count=0)
  }
  messDf$publisher = test_publisher
  sumStats = row$summaryStats
  sumDf = data.frame(names(sumStats),unlist(t(sumStats)))
  names(sumDf) = c("name","count")
  sumDf$publisher = test_publisher
  meta.mess.list[[meta.index]] = messDf
  meta.sum.list[[meta.index]] = sumDf
  meta.index = meta.index + 1
}
close(pb)
all.messages = rbindlist(meta.mess.list)
all.messages = all.messages[order(all.messages$publisher),]
all.messages = merge(all.messages, org_cat_max, by="publisher", all.x=T)
all.messages$category[which(is.na(all.messages$category))] = "No spend data for 2018-2020"
all.summaries = rbindlist(meta.sum.list)
setnames(all.summaries,"count","value")
all.summaries.wide = dcast(all.summaries,publisher~name)
all.summaries.wide = merge(all.summaries.wide, org_cat_max, by="publisher", all.x=T)
all.summaries.wide$category[which(is.na(all.summaries.wide$category))] = "No spend data for 2018-2020"
fwrite(all.messages,"output/all_messages.csv")
fwrite(all.summaries.wide,"output/all_summaries.csv")
