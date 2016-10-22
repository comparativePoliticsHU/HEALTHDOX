require(needs)
needs(openxlsx,countrycode)

Sys.getlocale(category = "LC_ALL")
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

if ( sub(".*/","",getwd()) != "Country_Chapters" ) setwd("~/Documents/Humboldt/HEALTHDOX/Country_Chapters") ## set path to vaps-dashboard_public here ##

REF <- read.xlsx("./data/in/France_Matthias_Brunn_MB_2016_08_23.xlsx")

names(REF)
REF <- REF[,!grepl("X",names(REF))]
Labels <- REF[1,]

REF <- REF[-1,] 
REF[1:5,"ref_name"]

# get ISO country codes
REF$ctr_ccode <- countrycode::countrycode(REF$country,"country.name","iso3c")

# Clean Reform Name column
  # split column in  and in native language 
  ref_name_split <- strsplit(REF$ref_name," - ")
  # reform name in english
  REF$ref_name_en <- sapply(ref_name_split, "[", 1)
    # and remove anoying characters 
    REF$ref_name_en <- gsub("[^[:alnum:]|[:blank:]]","",REF$ref_name_en)
    
  # reform name in native languag
  REF$ref_name <- sapply(ref_name_split, "[", 2)
#     # and remove anoying characters 
#     REF$ref_name <- gsub("[^[:alnum:]|[:blank:]]","",REF$ref_name)

# Clean and Format Date Columns      
  
  date_cols <- colnames(REF)[grepl("date",colnames(REF))]
    # check formatting
    ColCheck <- data.frame(col=date_cols, stringsAsFactors=F)
    for (c in seq_along(date_cols)) {
      ColCheck$any_contains_blank[c] <- any(grepl("[[:blank:]]", REF[, date_cols[c] ]))   # any element contains space
      ColCheck$which_contains_blank[c] <- list(which(grepl("[[:blank:]]", REF[, date_cols[c] ])))  # which element (i.e, row) contains space
      ColCheck$any_not_numeric[c] <- any(grepl("[^[1-9]]", REF[, date_cols[c] ]))  # any element not numeric
      ColCheck$which_not_numeric[c]  <- list(which(grepl("[^[1-9]]", REF[, date_cols[c] ])))  # which element (i.e, row) is not completely numeric space
      ColCheck$has_hyphen[c] <- any(grepl("-", REF[, date_cols[c] ]))  # any element has hyphen
      ColCheck$which_has_hyphen[c] <- list(which(grepl("-", REF[, date_cols[c] ])))  # which element has hyphen
    }
    
    # clean date columns
      # if only blank
      blank_date_entries <- subset(ColCheck,any_contains_blank==T,c(col,which_contains_blank))
      if ( nrow(blank_date_entries) > 1L ) {
        target_cols <- REF[unlist(blank_date_entries[2]),as.character(blank_date_entries[1])]
        if( !any(grepl("[[:alnum:]]|[-/.]",target_cols)) ) {  # if not contains alpha-numeric characters, or dot, dash or hyphen
          REF[unlist(blank_date_entries[2]),as.character(blank_date_entries[1])] <- NA  # .. replace element with NA
        }
      }
      # if non-numeric 
      nonnumeric_date_entries <- subset(ColCheck,any_not_numeric==T,c(col,which_not_numeric))
      if ( nrow(nonnumeric_date_entries) > 1L ) {
  #      if gsub("[[:digit:]]|[-/.]","",REF$check) %in% month.abb
      }
      # if hyphen'd
      hyphened_date_entries <- subset(ColCheck,has_hyphen==T,c(col,which_has_hyphen))
      if ( nrow(hyphened_date_entries) > 1L ) {
        for( c in 1:nrow(hyphened_date_entries) ) {
          REF[, hyphened_date_entries$col[c] ] <- as.Date(REF[, hyphened_date_entries$col[c] ], format="%Y-%m-%d")
        }
      } else {
        for (c in seq_along(date_cols)) {
          # add condition that checks for formating of column first
          REF[, date_cols[c] ] <- as.Date(as.integer(REF[, date_cols[c] ]), format="%Y-%m-%d", origin="1899-12-30")
        }
      }

REF      

# source configurations-veto events dataframe with HOG information
source("./jobs/cr_config_with_hog_and_veto_info.R")

  # clean global environment
  rm(list=setdiff(ls(),c("configs","cab_parties","hlth_minister_parties","REF","Labels","veto_points_table")))

# source matchEventsonConfigs() function 
source("./jobs/matchEventsOnConfigs.R")

# match reforms on configurations by date_law 
ReformConfigVtoEvents <- matchEventsOnConfigs(events = REF, event_dates_col = "date_law",
                                           config_ts = configs, start_date_var = "sdate",
                                           group_var = "ctr_ccode")
Encoding(ReformConfigVtoEvents$ref_name) <- "UTF-8"
Encoding(ReformConfigVtoEvents$ref_meas) <- "UTF-8"

rm(configs)

# list of all parties who where in reforming cabinets and/or who were sponsoring the minister of health
CabinetParties <- subset(cab_parties, subset = pty_abr %in% unique(unlist(strsplit(ReformConfigVtoEvents$in_cab, ", "))) & ctr_ccode=="FRA")
HealthMinisterParties <- subset(hlth_minister_parties, subset = pty_abr %in% gsub("[^[a-zA-Z]|[.]]","",unique(unlist(strsplit(ReformConfigVtoEvents$hlth_minister_pty, ", "))) ) & ctr_ccode=="FRA")

listOfPartiesInTable <- unique(rbind(CabinetParties ,HealthMinisterParties)) %>% arrange(pty_abr)
names(listOfPartiesInTable) <- c("ISO-3-character Country Code", "Party Abbreviation", "Party Name", "Party Name in English") 

rm(cab_parties,hlth_minister_parties)

# subset
ReformConfigVtoEventsSub <- ReformConfigVtoEvents[,c("country", "year",
                                                 "ref_name_en", "ref_name", "date_law", "date_impl", 
                                                 "head_of_gov", "in_cab", "hlth_minister_pty", 
                                                 "cab_lh_sts_shr","vto_lh",
                                                 "cab_uh_sts_shr","vto_uh",
                                                 "vto_prs","vto_jud","vto_terr","vto_elct", "open_veto_points", 
                                                 "ref_meas")]

# column labels
Labels
attrLabels <- attr(Labels,"names")

Labels <- gsub("[[:space:]]"," ",Labels)
Labels <- trimws(gsub("[(:].*"," ",Labels))
attr(Labels, "names") <- attrLabels
Labels
rm(attrLabels)

names(ReformConfigVtoEventsSub)[ names(ReformConfigVtoEventsSub) %in% names(Labels) ] <- Labels[ names(Labels) %in% names(ReformConfigVtoEventsSub) ]

names(ReformConfigVtoEventsSub)[ grepl("_",names(ReformConfigVtoEventsSub)) ] <- c("Reform Name in English"
                                                                                   , "Last Name of the Head of Government (Party)"
                                                                                   , "Parties in Cabinet"
                                                                                   , "Party of Minister of Health (in office since)"
                                                                                   , "Cabinet Parties' Cumulated Seat Share in the Lower House", "Lower House Veto Point" 
                                                                                   , "Cabinet Parties' Cumulated Seat Share in the Upper House", "Upper House Veto Point" 
                                                                                   , "President Veto Point", "Judicial Veto Point", "Territorial Unit Veto Point", "Electorate Veto Point"
                                                                                   , "Open Veto Points")
names(ReformConfigVtoEventsSub)

# write to .xlsx workbook
FranceHandbookTable4 <- list("Reforms"=ReformConfigVtoEventsSub[,-c(11:17)],
                             "Reforms_detailed"=ReformConfigVtoEventsSub,
                             "Veto_Institutions"=subset(veto_points_table[,-1],ctr_ccode=="FRA"),
                             "Parties" = listOfPartiesInTable)
str(FranceHandbookTable4)

style_table <- createStyle(fontName = "Helvetica", fontSize = "12",
                           numFmt = "GENERAL",
                           halign = "left", valign = "top")

write.xlsx(FranceHandbookTable4
           , creator = "Hauke Licht"
           , file = "./data/out/France_handbook_table4.xlsx"
           , borders = "rows", colNames = T, rowNames = F
           , style = style_table, overwrite = T)


system("open data/out/France_handbook_table4.xlsx")

