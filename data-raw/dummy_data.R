#' ---
#' title: "Set-up dummy biodiversity data for Bavaria"
#' author: "RS-eco"
#' ---

rm(list=ls()); invisible(gc())

#+ data_setup, results="hide"
# Load packages
library(data.table); library(dtplyr); library(dplyr, warn.conflicts = FALSE)
library(magrittr); library(tidyr); library(DBI)

########################

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "extdata/ASK.db")
DBI::dbListTables(my_db)

# Pull part of ASK database including data on species 
ask_art <- dplyr::tbl(my_db, paste("ask_art")) %>% collect()

# Load ask_fuo
ask_fuo <- dplyr::tbl(my_db, paste("ask_fuo")) %>% collect()
head(ask_fuo)

# Load background grid
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect() 
tk25_grid <- tk25 %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="/", remove = TRUE) %>% 
  dplyr::select(-c("KARTE_QUAD")) 

# Disconnect from database
DBI::dbDisconnect(my_db)

set.seed(123)
n <- 26
art_sub <- unique(ask_art$art) %>% sample(n)
art_sub

ask_art_sub <- ask_art %>% filter(art %in% art_sub) %>% select(-art_id) %>% 
  mutate(art = factor(art, labels=paste("Species", LETTERS)))
head(ask_art_sub)

ask_fuo <- ask_fuo %>% filter(ora_fuo_id %in% ask_art_sub$ora_fuo_id)

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = "data/database.db")

dbListTables(con)
dbWriteTable(con, "art", ask_art_sub)
dbWriteTable(con, "fuo", ask_fuo)
dbWriteTable(con, "tk25", tk25)
dbListTables(con)

dbDisconnect(con)
rm(list=ls()); gc()
