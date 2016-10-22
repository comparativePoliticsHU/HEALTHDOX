
require(needs)
needs(R.cache,dplyr,openxlsx)
getwd()

# (1) get data from PCDB
setCacheRootPath(path="~/Documents/Humboldt/HEALTHDOX/Country_Chapters/data/in/") 

# load cached data list
allPCDBObjects <- loadCache(key=list("PCDB","data"))   # key is PCDB + data

# retrieve data from list
for ( o in seq_along(allPCDBObjects) ) { 
  assign(names(allPCDBObjects)[ o ], allPCDBObjects[[ o ]])
}; rm(o)

# (1a) combine configuration events with veto configuration views
veto_institutions <- c("hog", "lh", "uh", "prs", "jud", "terr", "elct")
DFs <- c("mv_configuration_events",paste("view_configuration_vto_",veto_institutions,sep=""))
  # list of data frames
  config_vetos <- lapply(DFs,function(x) get(x))
  # merge all veto views on configuration events by coountry ID and configuration start date
  config_vetos <- Reduce(function(...) merge( ... , by=c("ctr_id", "sdate"),all.x=T), config_vetos)

# (1b) subset  
config_vetos <- config_vetos[, c("ctr_id", "sdate", "cab_id.x", "lh_id.x", "lhelc_id", "uh_id.x", "prselc_id", 
                                 "hog_not_member_of_cab_parties", "vto_hog", 
                                 "vto_lh", "cab_lh_sts_shr", 
                                 "cab_uh_sts_shr", "vto_uh", 
                                 "cohabitation",  "vto_prs", 
                                 "vto_jud", "vto_terr", "vto_elct") ]
colnames(config_vetos)[c(3,4,6)] <- c("cab_id", "lh_id", "uh_id")
head(config_vetos)

# (1c) get veto institution table
veto_points_table <- merge(country[,c("ctr_id", "ctr_ccode")],veto_points[ ,-1],by="ctr_id",all.y=T)

# (2) cabinet table with HOG name and party namees
cabinet_table <- merge(country[,c("ctr_id", "ctr_ccode")],
                                 cabinet[,c("cab_id", "ctr_id", "cab_sdate", "cab_hog_n")],
                                 by="ctr_id",all.y=T)
cabinet_portfolio_table <- merge(cabinet_table,
                                 subset(cabinet_portfolios, pty_cab==TRUE,c("cab_id", "pty_id", "pty_cab", "pty_cab_hog")),
                                 by="cab_id",all.y=T)
cabinet_parties_table <- merge(cabinet_portfolio_table,
                                 party[, c("pty_id", "pty_abr", "pty_n", "pty_n_en")],
                                 by="pty_id", all.x=T)

cabinet_parties_table$cab_hog_n <- trimws(gsub("\t","", cabinet_parties_table$cab_hog_n))

cabinet_parties_table <- cabinet_parties_table %>% 
  arrange(cab_id, pty_id) %>% 
  group_by(cab_id) %>% 
  mutate(in_cab = paste(pty_abr, collapse=", ") ) %>%  # list all cabinet parties
  as.data.frame()

  # keep code list with party abbreviations and names
  cab_parties <- unique(cabinet_parties_table[, c("ctr_ccode", "pty_abr", "pty_n", "pty_n_en")])

# subset: keep only rows of HOG's party  
cabinet_hog_table <- subset(cabinet_parties_table, pty_cab_hog==TRUE, -c(pty_id,pty_cab,pty_cab_hog))
  # NOTE: rows are now again uniquely identified by cab_id (i.e. ctr_id and cab_sdate)
  nrow(cabinet_hog_table) == nrow(unique(cabinet_hog_table))

head(cabinet_hog_table)

cabinet_hog_table <- within(cabinet_hog_table, head_of_gov <-  paste(cab_hog_n, " (", pty_abr, ")",sep=""))

# (3) cabinet table with health minister party and start date information
names(cabinet_table)
cabinet_minsiters_table <- merge(cabinet_table,
                                 subset(minister,min_hlth>0,c("cab_id", "pty_id", "min_sdate", "min_hlth")),
                                 by="cab_id",all.y=T)
minister_parties_table <- merge(cabinet_minsiters_table,
                                party[, c("pty_id", "pty_abr", "pty_n", "pty_n_en")],
                                by="pty_id", all.x=T)

head(minister_parties_table)
minister_parties_table$cab_hog_n <- trimws(gsub("\t","", minister_parties_table$cab_hog_n))

minister_parties_table <- minister_parties_table %>% 
  arrange(cab_id, pty_id) %>% 
  group_by(cab_id) %>% 
  mutate(hlth_minister_pty = paste(paste(pty_abr," (",min_sdate,")",sep=""), collapse=", ") ) %>%  # list all cabinet parties
  as.data.frame()

subset(minister_parties_table, ctr_id == 29, c(1:9,12))

# keep code list with party abbreviations and names
hlth_minister_parties <- unique(minister_parties_table[, c("ctr_ccode", "pty_abr", "pty_n", "pty_n_en")])

# subset: keep only rows of HOG's party  
cabinet_hlth_min_table <- unique(minister_parties_table[ , c("ctr_id", "cab_id", "cab_sdate", "hlth_minister_pty") ])

subset(cabinet_hlth_min_table, ctr_id == 29)

# join cabinet HOG table with cabinet health minsiter table
cabinet_data_table <- merge(cabinet_hog_table,cabinet_hlth_min_table,by=c("ctr_id", "cab_id", "cab_sdate"), all.x=T)

  cabinet_data_table <- cabinet_data_table %>% arrange(ctr_id, cab_sdate)
  
  subset(cabinet_data_table, ctr_id == 29)
  names(cabinet_data_table)

# (4) join cabinet data table on configuration-veto events table 
configs <- merge(config_vetos,cabinet_data_table,by=c("ctr_id","cab_id"),all.x=T)
head(configs)
unique(veto_points_table$vto_inst_typ)

# get names of veto variables 
veto_variables <- colnames(configs)[grepl("vto_",colnames(configs))]
veto_variables_labels <- c(vto_lh="lower house"
                           , vto_uh="upper house"
                           , vto_prs="head of state"
                           , vto_jud="judicial"
                           , vto_elct="electoral"
                           , vto_terr="territorial")

configs$open_veto_points <- ""  # create empty string vector 
  head(configs)
  
  for (config in 1:nrow(configs) ) {
    for (c in rev(seq_along(veto_variables))) {
      if ( configs[config, veto_variables[c]] == 1 && !is.na(configs[config, veto_variables[c]]) ) {
        configs[config, "open_veto_points"] <- paste(veto_variables_labels[veto_variables[c]],configs[config, "open_veto_points"],sep=", ")
      } 
      
    }
  }
  
  for ( c in seq_along(unique(configs$ctr_id)) ) {

    country_veto_inst <- unique(subset(veto_points_table,ctr_id==c,c(vto_inst_typ,vto_inst_n)))
    country_veto_inst <- within(country_veto_inst, paste_names <- paste(vto_inst_typ, " (", vto_inst_n, ")", sep=""))
    
    veto_variables_labels_ctr <- veto_variables_labels[veto_variables_labels %in% country_veto_inst$vto_inst_typ]
    
    for ( inst in seq_along(veto_variables_labels_ctr) ) { 
      configs[configs$ctr_id==c,"open_veto_points"] <- gsub(veto_variables_labels_ctr[inst]
                                                            , country_veto_inst[country_veto_inst$vto_inst_typ == veto_variables_labels_ctr[inst], "paste_names"]
                                                            , configs[configs$ctr_id==c,"open_veto_points"])
    }
  }
  
  # clean open veto points variable 
  configs$open_veto_points <- gsub("(, )*$","", configs$open_veto_points)  
  head(configs)
  
# prepare for matching on reform events:
  # date must have 'Date' class
  configs$sdate <- as.Date(configs$sdate, format="%Y-%m-%d")
  
  # ctr_id can be dropped, is only internal identifier (use ISO character code instead)
  configs <- configs[, c("ctr_ccode","sdate"
                         ,"head_of_gov", "in_cab"
                         ,"hlth_minister_pty"
                         ,"cab_lh_sts_shr","vto_lh"
                         ,"cab_uh_sts_shr","vto_uh"
                         ,"vto_prs","vto_jud","vto_terr","vto_elct"
                         ,"open_veto_points")]
  head(configs)
 




