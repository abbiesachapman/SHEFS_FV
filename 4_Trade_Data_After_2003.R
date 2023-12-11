##
## Date: 11th December 2023
## Author: Abbie S. A. Chapman
## Title: R script to accompany 'Quantifying the biodiversity pressures of fruit and vegetable consumption in the 
## United Kingdom, India, and South Africa.'
##

## 4_Trade_Data_After_2003

#####
# 1. Extracting trade data after 2003.
# For one of the analyses, I need to see what happens to the imports and production of fruits and vegetables
# in each of the focal countries after 2003. I therefore need trade averages for 2003-present.
# I explored the trade data and found anomalies in the data for 2018 and 2019, so I use years 2003-2017,
# in case 2018 and 2019 were incomplete records at the time of access/recording.

outdir_old = "SHEFS/SHEFS_March2023_Update/19July2023/"
cropdir = "SHEFS/6_Trade_Matrices/Corrected_trade_long/" # bringing in the long format data to try instead of the wide

# Please note that some of the script below is quite slow to run.

cropdata_1 = list.files(path = cropdir, pattern="*.csv", all.files = TRUE, full.names = TRUE)
cropdata_1
for (i in 1:length(cropdata_1)) assign(cropdata_1[i], read.csv(cropdata_1[i]))

yearslist_post2003 = c("1997", "1998", "1999", "2000", "2001", "2002", "2003",
                       "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                       "2011", "2012", "2013", "2014", "2015", "2016", "2017")
cropdata_sub_1 = lapply(yearslist_post2003, FUN = function(x) {
  cropdata_1[grepl(x, cropdata_1)]})
cropdata_sub_1
cropdata_sub1_1 = unlist(cropdata_sub_1)

cropdata_sub2_1 = lapply(fruitveg$crop, FUN = function(x) {
  cropdata_sub1_1[grepl(x, cropdata_sub1_1)]
})
cropdata_sub2_1
cropdata_sub3_1 = unlist(cropdata_sub2_1)
cropdata_1 = cropdata_sub3_1
str(cropdata_1)

library(stringr)
df_1 = NULL
for (m in 1:1173) {
  file1 = read.csv(cropdata_1[m])
  file2 = file1[complete.cases(file1),]
  cropname = stringr::str_remove(cropdata_1[m], "SHEFS/6_Trade_Matrices/Corrected_trade_long/")
  cropname1 = stringr::str_remove(cropname, ".csv")
  cropname2 = stringr::str_split_fixed(cropname1,"_", 2)
  file_with_name = cbind(file2, cropname2)
  df_1 = rbind(df_1, file_with_name)}

str(df_1)
#form_1 = paste0(outdir, "trade_data_all_28082023.csv")
#write.csv(df_1, file = form_1)

unique(df_1$`2`)
df_2 = read.csv(paste0(outdir_old,"trade_data_all_28082023.csv")) 
head(df_2)
tradedatapost2003 = subset(df_2, select = -c(X))
head(tradedatapost2003)
colnames(tradedatapost2003) = c("reporter", "partner", "dmi_tonnes", "crop", "year")
trade_data_post2003 = tradedatapost2003 %>%
  group_by(crop, reporter, partner) %>%
  summarise(dmi_tonnes_average_1997onwards = mean(dmi_tonnes, na.rm = TRUE))

# Tidying these average tonne data:
head(trade_data_post2003)
countrylist = data.frame(unique(trade_data_post2003$reporter))
colnames(countrylist) = "country"
countrylist2 = data.frame(unique(trade_data_post2003$partner))
colnames(countrylist2) = "country"
countrylist3 = rbind(countrylist, countrylist2)
head(countrylist3)
countrylist4 = data.frame(unique(countrylist3$country))
colnames(countrylist4) = "NAME"
worldnames = data.frame(wrld_simpl4)
head(worldnames, n = 5)
worldnames_short = subset(worldnames, select = c("NAME", "ISO3"))
head(worldnames_short)
testnames = full_join(countrylist4, worldnames_short)
testnames # this contains the reporters and partners from the trade data with ISO codes and has NAs for the ones I need to match
# View(worldnames_short)
outdir = "SHEFS/SHEFS_Sept2023_Update/4_TradeData_and_Outputs/"
# write.csv(testnames, paste0(outdir, "tradenamematching_post2003.csv")) # - exporting to try and fix using the worldnames from wrld_simpl  
namematches = read.csv(paste0(outdir, "tradenamematching_post2003_FILLED.csv"))
head(namematches)

# Next, I want to find a way to replace the names in the reporter and partner columns of trade data with the ISO codes in namematches
# noting that there are a couple of NAs where the wrld_simpl map does have some out of date countries included.
head(trade_data_post2003)
trade_data_post2003$NAME = trade_data_post2003$reporter
head(trade_data_post2003)
trade_data_test = trade_data_post2003
trade_data_test = left_join(trade_data_test, namematches, by = "NAME")
head(trade_data_test)
sum(is.na(trade_data_test$ISO3))
(na_rows = trade_data_test[!complete.cases(trade_data_test),]) # this is OK as it's Serbia and Montenegro, USSR, and Yugoslavia
sum(na_rows$dmi_tonnes_average_1997onwards) # 329,917 tonnes total globally across all partners
trade_data1 = subset(trade_data_test, select = -c(NAME, X))
trade_data1$NAME = trade_data1$partner
trade_data2 = left_join(trade_data1, namematches, by = "NAME")
head(trade_data2)
(na_rows1 = trade_data2[!complete.cases(trade_data2$ISO3.y),]) # again USSR, Yugoslav SFR and Serbia & Montenegro
na_rows1
sum(na_rows1$dmi_tonnes_average_1997onwards) # 198 tonnes
trade_data_post2003 = subset(trade_data2, select = -c(X, NAME))
colnames(trade_data_post2003) = c("crop", "reporter", "partner", "dmi_tonnes_average_1997onwards",
                                  "ISO_reporter", "ISO_partner")
head(trade_data_post2003)
