obs_url_orig <- "https://www.waterqualitydata.us/Result/search?siteid=KENAI_WQX-10000349%3BWIDNR_WQX-10012578%3BQIN_WQX-TM2G%3BWIDNR_WQX-133548%3BWIDNR_WQX-10039277%3BQIN_WQX-TM7-a%3BQUILEUTE_WQX-025%3BCBP_WQX-LE2.3%3BWIDNR_WQX-10039686%3BWIDNR_WQX-10016481%3BCBP_WQX-LE1.2%3BWIDNR_WQX-10038078%3BWIDNR_WQX-10011948%3BCBP_WQX-CB3.3W%3BWIDNR_WQX-10034036%3BTCEQMAIN-17017%3BMNPCA-10-0045-00-202%3BWIDNR_WQX-683125%3BCBP_WQX-PMS29%3B21VASWCB-6APNR001.82%3BWIDNR_WQX-10037203%3B21AWIC-199%3BMNPCA-82-0104-00-201%3BLRBOI_WQX-TDam%3B21AWIC-893%3BTCEQMAIN-11935%3BMNPCA-27-0133-03-202%3BGNLK01_WQX-BTLK%3BOREGONDEQ-11005-ORDEQ%3BCBP_WQX-903A%3BUSGS-01477050%3BTCEQMAIN-17016%3BUSGS-06428500%3BOREGONDEQ-30501-ORDEQ%3BUSGS-08317300%3B21IOWA_WQX-15220006%3B11NPSWRD_WQX-CONG_USGS_11%3BTCEQMAIN-11977%3B21SCSANT-SC-046%3B11NPSWRD_WQX-BICA_MFG_E%3BUSGS-07056000%3BUSGS-06644085%3BMNPCA-70-0120-01-401%3BWIDNR_WQX-213136%3BWIDNR_WQX-583067%3BOREGONDEQ-34755-ORDEQ%3BOREGONDEQ-24447-ORDEQ%3B11NPSWRD_WQX-CONG_USGS_01%3BUSGS-50043000%3B1VTDECWQ-504155%3BUSGS-04122030%3B21KAN001_WQX-SC207%3BSDDENR_WQX-460815%3B21FLSFWM_WQX-G342C%3B21FLEECO_WQX-10MIGR80%3B21AWIC-2101%3BIL_EPA_WQX-HCCC-02%3BWIDNR_WQX-363312%3BOREGONDEQ-36865-ORDEQ%3B21NC03WQ-CPF0884A%3BOREGONDEQ-31405-ORDEQ%3BUSGS-03383000%3B21GAEPD_WQX-LK_11_3520%3BMNPCA-82-0031-00-451%3BUSGS-05529000%3B21MSWQ_WQX-303ARK03%3BCOQUILLE_WA-29918-ORDEQ%3BUSGS-03049652%3B21AWIC-10328%3B21FLSFWM-G108%3BMNPCA-21-0216-00-201%3BUSGS-09144200%3BTCEQMAIN-10222%3BMNPCA-S000-173%3B21IOWA-21910001%3BAZDEQ_SW-663%3BUSGS-05320270%3BMNPCA-27-0133-02-402%3B21AWIC-50%3B21FLEECO_WQX-20A-11GR%3B1VTDECWQ-503001%3B21NCMONITORING-J6450000%3BUSGS-08079600%3BMDE_FIELDSERVICES_WQX-LDR0012%3BLADEQWPD-103%3BUSGS-15389000%3BUSGS-07211500%3BMENOM_WQX-LSKI%3BTCEQMAIN-10576%3BUSGS-01454700%3BUSGS-475301102220100%3BUSACOEND-3MAR20004%3BTSWQC_WQX-CFRPO-29%3B21DELAWQ_WQX-202021%3BUSGS-08037000%3BUSGS-02464360%3BUSGS-02297100%3BUSGS-05388250%3B21VASWCB-5ARDC007.30%3BUTAHDWQ_WQX-4995600%3B"
library(dataRetrieval)
x <- importWQP(obs_url_orig)

split <- strsplit(obs_url_orig, "?", fixed=TRUE)
obs_url <- split[[1]][1]
query <- split[[1]][2]
json_request_stuff <- strsplit(query, "=", fixed=TRUE)
things_to_request <- json_request_stuff[[1]][1]
things <- strsplit(json_request_stuff[[1]][2], "%3B", fixed=TRUE)
names(things) <- things_to_request

things <- strsplit(json_request_stuff[[1]][2], "%3B", fixed=TRUE)
things <- list(things[[1]][1:2])
names(things) <- things_to_request
things_json <- jsonlite::toJSON(things, pretty = TRUE)
####################################################
library(httr)
library(readr)
system.time({
  x <- POST(paste0(obs_url,"?mimeType=tsv"), 
            body = things_json, 
            content_type("application/json"),
            accept("text/tsv"),
            user_agent(dataRetrieval:::default_ua()))
  
  returnedDoc <- content(x,
                         type="text",
                         encoding = "UTF-8")
  retval <- suppressWarnings(read_delim(returnedDoc, 
                                        col_types = cols(`ActivityStartTime/Time` = col_character(),
                                                         `ActivityEndTime/Time` = col_character(),
                                                         USGSPCode = col_character(),
                                                         ResultCommentText=col_character(),
                                                         `ActivityDepthHeightMeasure/MeasureValue` = col_number(),
                                                         `DetectionQuantitationLimitMeasure/MeasureValue` = col_number(),
                                                         ResultMeasureValue = col_number(),
                                                         `WellDepthMeasure/MeasureValue` = col_number(),
                                                         `WellHoleDepthMeasure/MeasureValue` = col_number(),
                                                         `HUCEightDigitCode` = col_character(), 
                                                         `ActivityEndTime/TimeZoneCode` = col_character()),
                                        quote = "", delim = "\t"))
})
####################################################
system.time({
  temp <- tempfile()
  temp <- paste0(temp,".zip")
  x <- POST(paste0(obs_url,"?mimeType=tsv&zip=yes"), 
            body = things_json, 
            content_type("application/json"),
            accept("application/zip"),
            httr::write_disk(temp),
            user_agent(dataRetrieval:::default_ua()))
  headerInfo <- httr::headers(x)
  file1 <- tempdir()
  doc <- utils::unzip(temp, exdir=file1)
  unlink(temp)
  retval1 <- suppressWarnings(read_delim(doc, 
                                        col_types = cols(`ActivityStartTime/Time` = col_character(),
                                                         `ActivityEndTime/Time` = col_character(),
                                                         USGSPCode = col_character(),
                                                         ResultCommentText=col_character(),
                                                         `ActivityDepthHeightMeasure/MeasureValue` = col_number(),
                                                         `DetectionQuantitationLimitMeasure/MeasureValue` = col_number(),
                                                         ResultMeasureValue = col_number(),
                                                         `WellDepthMeasure/MeasureValue` = col_number(),
                                                         `WellHoleDepthMeasure/MeasureValue` = col_number(),
                                                         `HUCEightDigitCode` = col_character(), 
                                                         `ActivityEndTime/TimeZoneCode` = col_character()),
                                        quote = "", delim = "\t"))
  unlink(doc)

})

system.time({
  x <- GET(paste0(obs_url_orig,"&mimeType=tsv"), 
            accept("text/tsv"),
            user_agent(dataRetrieval:::default_ua()))
  
  returnedDoc <- content(x,
                         type="text",
                         encoding = "UTF-8")
  retval2 <- suppressWarnings(read_delim(doc, 
                                         col_types = cols(`ActivityStartTime/Time` = col_character(),
                                                          `ActivityEndTime/Time` = col_character(),
                                                          USGSPCode = col_character(),
                                                          ResultCommentText=col_character(),
                                                          `ActivityDepthHeightMeasure/MeasureValue` = col_number(),
                                                          `DetectionQuantitationLimitMeasure/MeasureValue` = col_number(),
                                                          ResultMeasureValue = col_number(),
                                                          `WellDepthMeasure/MeasureValue` = col_number(),
                                                          `WellHoleDepthMeasure/MeasureValue` = col_number(),
                                                          `HUCEightDigitCode` = col_character(), 
                                                          `ActivityEndTime/TimeZoneCode` = col_character()),
                                         quote = "", delim = "\t"))
})

library(readr)
retval <- suppressWarnings(read_delim(returnedDoc, 
                                      col_types = cols(`ActivityStartTime/Time` = col_character(),
                                                       `ActivityEndTime/Time` = col_character(),
                                                       USGSPCode = col_character(),
                                                       ResultCommentText=col_character(),
                                                       `ActivityDepthHeightMeasure/MeasureValue` = col_number(),
                                                       `DetectionQuantitationLimitMeasure/MeasureValue` = col_number(),
                                                       ResultMeasureValue = col_number(),
                                                       `WellDepthMeasure/MeasureValue` = col_number(),
                                                       `WellHoleDepthMeasure/MeasureValue` = col_number(),
                                                       `HUCEightDigitCode` = col_character(), 
                                                       `ActivityEndTime/TimeZoneCode` = col_character()),
                                      quote = "", delim = "\t"))

x <- importWQP(returnedDoc)



