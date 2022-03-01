list.of.packages <- c("data.table","anytime","XML","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages,new.packages)

setwd("~/git/IATI-results-framework-2022")

load("ds_api_results.RData")
results$currency = results$transaction_value_currency
results$currency[which(results$currency=="")] = NA
results$currency[which(is.na(results$currency))] = results$default_currency[which(is.na(results$currency))]
results$date = results$transaction_transaction_date_iso_date
results$date[which(results$date=="")] = NA
results$date[which(is.na(results$date))] = results$transaction_value_value_date[which(is.na(results$date))]
results$year = as.numeric(substr(results$date, 1, 4))
setnames(
  results,
  c("transaction_value", "transaction_transaction_type_code"),
  c("value", "type")
)

dat_tab = results[,.(value=sum(value,na.rm=T)),by=.(reporting_org_ref,type,year,currency)]
dat_tab$currency = gsub(" ","",toupper(dat_tab$currency))
dat_tab$currency[which(dat_tab$currency=="BEF")] = "EUR"
dat_tab$currency[which(dat_tab$currency=="GIP")] = "GBP"
dat_tab$currency[which(dat_tab$currency=="AON")] = "AOA"
dat_tab$currency[which(dat_tab$currency=="USS")] = "USD"
dat_tab$currency[which(dat_tab$currency=="FKP")] = "GBP"
dat_tab$currency[which(dat_tab$currency=="ZMK")] = "ZMW"
dat_tab$currency[which(dat_tab$currency=="USN")] = "USD"
dat_tab$currency[which(dat_tab$currency=="FIM")] = "EUR"
dat_tab$currency[which(dat_tab$currency=="ERU")] = "EUR"

ex_rates = fread("ex_rates.csv")
setnames(ex_rates,"cc","currency")
setdiff(unique(dat_tab$currency),unique(ex_rates$currency))
dat_tab = subset(dat_tab, currency %in% unique(ex_rates$currency))
dat_tab = merge(dat_tab,ex_rates,by=c("year","currency"))
dat_tab = subset(dat_tab,ex.rate>0)
dat_tab$value_usd = dat_tab$value * dat_tab$ex.rate
publisher_tab = dat_tab[,.(value_usd=sum(value_usd,na.rm=T)),by=.(year,reporting_org_ref)]
publisher_tab = publisher_tab[order(publisher_tab$reporting_org_ref,-publisher_tab$year),]

publisher_tab$category = "<=1M"
publisher_tab$category[which(publisher_tab$value_usd<=1000000)] = "<=1M"
publisher_tab$category[which(publisher_tab$value_usd>1000000 & publisher_tab$value_usd<=10000000)] = "> 1M & <= 10M"
publisher_tab$category[which(publisher_tab$value_usd>10000000 & publisher_tab$value_usd<=100000000)] = "> 10M & <= 100M"
publisher_tab$category[which(publisher_tab$value_usd>100000000 & publisher_tab$value_usd<=1000000000)] = "> 100M & <= 1B"
publisher_tab$category[which(publisher_tab$value_usd>1000000000)] = "> 1B"

fwrite(publisher_tab,"IATI_publishers_by_spend.csv")

publisher_tab$category = factor(
  publisher_tab$category,
  levels=c(
    "<=1M",
    "> 1M & <= 10M",
    "> 10M & <= 100M",
    "> 100M & <= 1B",
    "> 1B"
  )
)

publisher_tab_2021 = subset(publisher_tab,year==2021)
ggplot(publisher_tab_2021,aes(x=category)) + geom_bar()
