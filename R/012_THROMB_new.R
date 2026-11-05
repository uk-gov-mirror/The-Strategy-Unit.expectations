
# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_thomb1 <- here("sql", "[UDAL]query_thromb_1.sql")

query_thromb_1 <- readChar(sql_script_thomb1, file.info(sql_script_thomb1)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

dbExecute(con_one, query_thromb_1)

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_thomb2 <- here("sql", "[UDAL]query_thromb_2.sql")

query_thromb_2 <- readChar(sql_script_thomb2, file.info(sql_script_thomb2)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_stroke_raw <- dbGetQuery(con_one, query_thromb_2) |>
  as_tibble() |>
  clean_names() |> 
  mutate(nhs_no = as.character(nhs_no)) |> 
  mutate(across(ends_with("date"), ~ as_date(.)))

gc()
gc()

# df_stroke_raw |> saveRDS(here("data_raw",  "df_stroke_raw.rds"))
df_stroke_raw <- readRDS(here("data_raw",  "df_stroke_raw.rds")) |>  
  # select(ends_with("date"))
  mutate(nhs_no = as.character(nhs_no)) |> 
  mutate(across(ends_with("date"), ~ as_date(.)))

# 1.36M
df_stroke_raw |> colnames()
df_stroke_exclusions |> select(first_spell_id)
# bring through der diagnoses all - then you have all 1.3 spells 
# then lookback two years of those spell ids in the spells table.
# Then bring the minimum cols from that subset through into R 
# and match on first spell_id of super-spell


# QA BLOCK ----------------------------------------------------------------

# /* QA 1: I63 spells lost because the pseudonymised NHS number is missing.
# These cannot be linked into superspells. If non-trivial, report it;
# they could be kept as standalone spells, but you cannot de-duplicate them. */
# 12, 302
# IS THERE LINK BETWEEN THIS AND DEPRIVATION?

#/* QA 3: 
# spells whose episodes carry more than one patient ID */
# indicationg placeholder / default spell IDs?
# dbGetQuery(con_one, 
#            "SELECT COUNT(*) AS n_spells_multi_nhs_no FROM #spells WHERE n_nhs_no > 1"
# ) |> 
#   as_tibble() |>
#   clean_names()
# 367 spells whose episodes carry more than one nhs no
# same der_spell_id for multiple patients.
df_stroke_raw |> 
  # count(n_nhs_no > 1) |> 
  # count(is.na(nhs_no))
  
  filter(n_nhs_no == 254)
  count(n_nhs_no, sort = T) |> 
  filter(n_nhs_no == max(n_nhs_no)) # 1 spell has 254 nhs numbers
  # filter(n_nhs_no > 1) |> 
  # count(der_spell_id, sort = T)


# /* QA 4: placeholder / default patient IDs. A dummy pseudonym shared by many
# real patients would chain unrelated admissions into giant "superspells".
# Look for implausible spell counts at the top of this list. */
dbGetQuery(con_one,
"SELECT TOP 20 nhs_no, COUNT(*) AS n_spells
FROM #spells
GROUP BY nhs_no
ORDER BY COUNT(*) DESC"
)|> 
    as_tibble() |>
    clean_names()
# NOPE. BUT NOTE 215 NA nhs numberS.


# /* QA 5: is site_code unique in the site lookup? If not, joining it to
# activity rows (as the original query did) silently duplicates records. */
# dbGetQuery(con_one,  
# "SELECT [site_code], COUNT(*) AS n_rows
# FROM Reporting_UKHD_ODS.Provider_Site
# GROUP BY [site_code]
# HAVING COUNT(*) > 1"
# )
# CODE IS UNIQUE ==> REPLACE IN QUERY?


# /* QA 6: share of spells with no recorded discharge date (end imputed from
# the last episode end). Should be ~0 in a finished-episode snapshot. */
tmp_checking |> 
  count(discharge_date_imputed)
# ONLY 1 IMPUTED AND IT SEEMS A PLAUSIBLE IMPUTATION.
tmp_checking |> 
  filter(discharge_date_imputed == 1) |> 
  select(admission_date, discharge_date)
  


# tmp_query <- "SELECT TOP 10 
#     [der_pseudo_nhs_number]                                  AS nhs_no,
#     [der_spell_id],
#      MIN(COALESCE([Admission_Date], [episode_start_date]))    AS spell_adm,
#     MAX(COALESCE([Discharge_Date], [episode_end_date]))      AS spell_dis
# INTO #i64_spells
# FROM [Reporting_MESH_APC].[APCE_Core_Monthly_Snapshot]
# GROUP BY [der_pseudo_nhs_number], [der_spell_id]"
#  
#  dbGetQuery(con_one, tmp_query) |>
#   as_tibble() |>
#   clean_names()
#  
# dbExecute(con_one, "DROP TABLE #i64_spells")
# dbGetQuery(con_one, "SELECT * FROM #i64_spells") |>
#   as_tibble() |>
#   clean_names()
# 
# 
# # 
# # gc()
# # gc()