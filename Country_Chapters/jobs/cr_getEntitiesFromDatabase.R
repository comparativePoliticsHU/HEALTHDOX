#########################################################################
### Title:  Access PCDB on HU-server, read data from tables and view,
###          and create lists with variable codes and descriptive labels
###
### Author: Hauke Licht (HL)
### Date:   August 25, 2016
### produced under R version 3.2.3

### Comment: (HL) This code produces a cache'd file that is then used in ui.R and server.R to render the RShiny app; 
###           To foreclose this code from public visibility in the GitHub repos, it is placed in the git-ignore folders 
###           But then the  p a t h s  u s e d   i n   t h i s   c o d e   n e e d   t o   b e  c h a n g e d  accordingly! 

rm(list=ls())

if (!require('needs')) install.packages("needs")
needs(RPostgreSQL,dplyr,countrycode,R.cache)

# Set path to local 'vaps-dashboard_public' directory 
if ( Sys.info()["user"] == "lichthau" ) { path <- "~/Documents/Humboldt/HEALTHDOX/Country_Chapters"
} else if ( Sys.info()["user"] == "johndoe" ) { path <- ""
} else if ( Sys.info()["user"] == "mio-mio1" ) {   path <- ""
} else warning("Sorry, but could not find path to 'vaps-dashboard_public' directory.")


if ( sub(".*/","",getwd()) != "Country_Chapters" ) setwd(path) ## set path to vaps-dashboard_public here ##
rm(path)

# (1) Connect to Database

  # define parameters as object
  dbname <- "polconfdb"
  dbuser <- "polconfdb_4"
  dbhost <- "moodledb.cms.hu-berlin.de"
  dbport <- "5432"
  dbpass <-   "Zs%7f_+9;hcRRw" #"S8:kFG%6+u_Zrw" #
  
  # connect to PostgreSQL Server
  drv <- dbDriver("PostgreSQL") 
  con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname, user=dbuser, password=dbpass) 

  # remove parameter objects
  rm(list=(ls()[grepl("[db.{4}]",ls())]))

# (2) Read data tables
  # get dataframe with columns in TABLES in config_data schema and beta_version.minister 
  ColumnsInTables <- dbGetQuery(con,"SELECT table_catalog, table_schema, table_name, column_name, ordinal_position, data_type 
                        FROM information_schema.columns
                        WHERE table_schema = 'config_data'
                        AND (table_name NOT LIKE 'view_%' 
                             AND table_name NOT LIKE 'cc_%'
                             AND table_name NOT LIKE 'minister')
                        OR (table_schema = 'beta_version' AND table_name = 'minister');")
  head(ColumnsInTables)
  # list tables ...
  TABLES <- unique(ColumnsInTables[!grepl("update_|matview|pview",ColumnsInTables$table_name), c("table_schema", "table_name")])

    # ... and read Tables in beta_version schema into dataframes 
    for (i in 1:nrow(TABLES)) { 
      assign(TABLES[i,2], dbReadTable(con, c(TABLES[i,1],TABLES[i,2])))
    } 
  # get dataframe with columns in VIEWS in config_data schema
  ColumnsInViews <- dbGetQuery(con,"SELECT table_catalog, table_schema, table_name, column_name, ordinal_position, data_type 
                        FROM information_schema.columns
                                WHERE table_schema = 'config_data'
                                AND table_name LIKE '%view_%';")

  # list views ...
  VIEWS <-  unique(ColumnsInViews[!grepl("matview",ColumnsInViews$table_name), c("table_schema", "table_name")])

    # ... and read Views in beta_version schema into dataframes 
    for (i in 1:nrow(VIEWS)) {
      assign(VIEWS[i,2], dbReadTable(con, c(VIEWS[i,1],VIEWS[i,2])))
    }  
  
  # before proceeding, disconnect from database  
  dbDisconnect(con)
  rm(con)
 
  
  
# (5) Cache list with all objects required for webpages with key=list("PCDB", "data")
  allPCDBObjects <- lapply(ls(.GlobalEnv, all.names = F), function(o) get(o, envir = .GlobalEnv)) 

  names(allPCDBObjects) <- ls(.GlobalEnv, all.names = F)[!grepl("allPCDBObjects",ls())]
  str(allPCDBObjects)
  if ( sub(".*/","",getwd()) == "Country_Chapters" ) {
    setCacheRootPath(path=paste(getwd(), "/data/in/", sep="")) 
    saveCache(allPCDBObjects, key=list("PCDB","data"))   
  } else warning("Cannot cache list. Please setwd() or setCacheRootPath() to Country_Chapters directory!")
  
  rm(allPCDBObjects)
    

  