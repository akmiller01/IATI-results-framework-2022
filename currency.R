list.of.packages <- c("data.table","readr","reshape2","jsonlite", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/IATI-results-framework-2022")

dat <- fread("WEOOct2021all.xls",sep="\t",na.strings=c("","n/a","--"),check.names=T)
dat = subset(dat,WEO.Subject.Code %in% c("NGDP","NGDPD"))
keep = c("ISO","Country","Units",paste0("X",1980:2026))
dat = dat[,keep,with=F]

mdat <- melt(dat,id.vars=c("ISO","Country","Units"))
mdat$year = as.numeric(substr(mdat$variable,2,5))
mdat <- mdat[complete.cases(mdat),]
mdat$value = as.numeric(gsub(",","",mdat$value))
wdat <- dcast(mdat,ISO+Country+year~Units)
wdat <- wdat[complete.cases(wdat),]
names(wdat) <- make.names(names(wdat))
wdat$ex.rate <- wdat$U.S..dollars/wdat$National.currency

ccs = fread("currency_codes.csv")
ccs = subset(ccs, ISO!="KOS") # Duplicate

setdiff(ccs$ISO,wdat$ISO)
setdiff(wdat$ISO,ccs$ISO)

ccs = subset(ccs,is.na(duplicate))
keep = c("ISO","cc")
ccs = ccs[,keep,with=F]

wdat = merge(wdat,ccs,by="ISO")

# Bitcoin transaction in 2015
xbt_df = data.frame(cc="XBT",year=2015,ex.rate=230.54)
wdat = rbindlist(list(wdat,xbt_df),fill=T)

# XDR
xdr <- fread("SDRV.xls",sep="\t",na.strings=c("","n/a","--"),fill=T,skip=1)
xdr$year = substr(xdr$`Report date`, nchar(xdr$`Report date`) - 3, nchar(xdr$`Report date`))
xdr$year = na.locf(xdr$year, na.rm=F)
xdr = xdr[which(xdr$`Exchange rate`=="SDR1 = US$"),]
xdr = xdr[,c("U.S. Dollar equivalent", "year")]
setnames(xdr,"U.S. Dollar equivalent", "ex.rate")
xdr = xdr[,.(ex.rate = mean(ex.rate)), by=.(year)]
xdr$cc = "XDR"
wdat = rbind(wdat,xdr,fill=T)

wdat = wdat[order(wdat$cc,wdat$year),]

fwrite(wdat[,c("year","cc","ex.rate"),with=F],"ex_rates.csv")
