# README
# Load packages and establish DB connection. 
# Note: you will be prompted for a password via pop-up.

clipr::write_clip(capture.output(cat(keyring::key_get("login"))))

# library("gt")
# library("gtExtras")
library("sf")
library("DBI")
# library("pak")
library("here")
library("odbc")
# library("mgcv")
library("arrow")
library("broom")
library("dplyr")
library("purrr")
# library("furrr")
library("readr")
library("tidyr")
library("tibble")
library("readxl")
library("forcats")
library("ggplot2")
# library("ggrepel")
library("janitor")
library("keyring")
library("stringr")
# library("tsibble")
library("lubridate")
# library("patchwork")
# library("yardstick")

options(scipen=999) 

source(here("R", "003_charlson_function.R"))


server <- keyring::key_get("server")
db <- keyring::key_get("db")


con_one <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 18 for SQL Server",
  Server = server,
  Database = db,
  Authentication = "ActiveDirectoryInteractive"
)

# REMEMBER POP-UP!

