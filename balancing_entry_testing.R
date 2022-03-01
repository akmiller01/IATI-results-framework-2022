list.of.packages <- c("data.table","ggplot2", "scales","foreach","doParallel")
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
res_tab = results[,.(value=sum(value, na.rm=T)),by=.(iati_identifier)]
neg_res_tab = subset(res_tab, value < 0)

neg = subset(results, value < 0)
pos = subset(results, value > 0)
unique_neg = unique(neg[,c("reporting_org_ref", "currency", "year", "type", "value")])
unique_pos = unique(pos[,c("reporting_org_ref", "currency", "year", "type", "value")])
unique_neg$value = unique_neg$value * -1

merged_results = merge(unique_pos, unique_neg)

message(
  "Found ",
  nrow(merged_results),
  " exact positive-valued matches for ",
  nrow(unique_neg),
  " negative balancing entries. For an overall matched percentage of ",
  percent(nrow(merged_results) / nrow(unique_neg)), "."
)
