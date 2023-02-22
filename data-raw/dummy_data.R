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
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "rawdata/ASK.db")
DBI::dbListTables(my_db)

# Pull part of ASK database including data on species 
ask_art <- dplyr::tbl(my_db, paste("ask_art")) %>% collect()

# Load ask_fuo
ask_fuo <- dplyr::tbl(my_db, paste("ask_fuo")) %>% collect()
head(ask_fuo)

# load TK data
tk25 <- dplyr::tbl(my_db, "geo_tk25_quadranten") %>% dplyr::collect()

# Disconnect from database
DBI::dbDisconnect(my_db)

set.seed(123)
ask_art$taxon <- substring(ask_art$art_id, 1,3)
unique(ask_art$taxon)
art_sub <- ask_art %>% select(taxon, art) %>% 
  filter(taxon %in% c("5IX", "1V0", "5IC", "5IF")) %>% 
  group_by(taxon) %>% sample_n(13)
art_sub$art2 = rep(LETTERS[1:13], 4)

ask_art_sub <- ask_art %>% inner_join(art_sub) %>%
  unite("art2", c(taxon, art2), sep="_") %>%
  select(ora_fuo_id, id, karte, jahr, mon, art, art2)
colnames(ask_art_sub)

ask_fuo <- ask_fuo %>% filter(ora_fuo_id %in% ask_art_sub$ora_fuo_id) %>%
  select(ora_fuo_id, id, karte, quadrant)
colnames(ask_fuo)

# Create an ephemeral in-memory RSQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/extdata/database.db")

dbListTables(con)
dbWriteTable(con, "art", ask_art_sub, overwrite=T)
dbWriteTable(con, "fuo", ask_fuo, overwrite=T)
dbWriteTable(con, "tk25", tk25, overwrite=T)
dbListTables(con)

dbDisconnect(con)
rm(list=ls()); gc()
