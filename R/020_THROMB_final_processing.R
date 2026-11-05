
# LINEAR ONLY FROM 2017/18 (AFTER COMMISSIONED BY NHSE)
df_stroke_join_tables_scored |>
  # colnames()
  filter(thromb) |> 
  count(fyear) |> 
  ggplot(aes(fyear, n))+
  geom_point()

# CCI ENDS UP AS NA IF NULL DIAGNOSIS ALL. BASE ON AGE AT MINIMUM. 
# (COULD BE CLEANED UP IN CHARLSON FUNCTION.)
df_stroke_join_tables_scored_edit <- df_stroke_join_tables_scored |> 
  mutate(cci = case_when(
    is.na(cci) & age >= 80 ~ 4,
    is.na(cci) & age >= 70 ~ 3,
    is.na(cci) & age >= 60 ~ 2,
    is.na(cci) & age >= 50 ~ 1,
    is.na(cci) & age < 50 ~ 0,
    TRUE ~ cci
  )) 

df_stroke_cleaned <- df_stroke_join_tables_scored_edit |>
  left_join(lkp_imd25_rank |> select(-imd_decile), join_by(lsoa21_bfit == lsoa_code_2021)) |> 
  mutate(rural_urban_classification = case_when(
    str_detect(rural_urban_classification, "Urban") ~ "urban",
    str_detect(rural_urban_classification, "Larger") ~ "larger_rural",
    T ~ "smaller_rural"
  )) |>
  mutate(rural_urban_classification = fct_relevel(
    rural_urban_classification, "urban"
  )) |>
  mutate(yr = as.integer(str_sub(fyear, 6, 7)) - 17) |> 
  # DISTANCE FROM m TO km:
  mutate(min_thromb_dist = round(min_thromb_dist / 1e3, 1)) |>
  mutate(covid_effect = as_factor(
    case_when(
      # fyear == "2019/20" ~ "19/20",
      fyear == "2020/21" ~ "20/21",
      # fyear == "2021/22" ~ "21/22",
      # fyear == 13 ~ "22/23",
      T ~ "none"
    )
  )) |>
  mutate(thromb = if_else(thromb == T, 1L, 0L)) |> 
  mutate(covid_effect = fct_relevel(covid_effect, "none")) |>
  # CENTRE AND SCALE:
  mutate(age_std = as.numeric(scale(age))) |>
  mutate(cci_std = as.numeric(scale(cci))) |>
  mutate(hfrs_std = as.numeric(scale(hfrs_score))) |>
  mutate(distance_std = as.numeric(scale(min_thromb_dist))) |>
  mutate(across(
    c(
      sex, rural_urban_classification, rgn22nm, sicbl25nm, icb25nm, imd_decile,
      any_prior_admission, starts_with("lb_"), thromb_centre_first,  
      thromb, is_wkend
    ),
    ~ as.factor(.)
  )) |>
  mutate(imd_top_quartile = as.factor(if_else(imd_quartile == 4, 1, 0))) |> 
  # identity()
  # df_stroke_cleaned <- df_stroke_cleaned |> 
  select(
    fyear, yr,
    thromb, 
    sex, age, age_std, sicbl25nm, icb25nm,
    imd_decile, imd_top_quartile, rural_urban_classification, rgn22nm, 
    thromb_centre_first, cci_std, 
    hfrs_std, any_prior_admission, covid_effect, distance_std,
    # died_in_ss,
    starts_with("lb_"), 
    is_wkend,
    # days_since_prev_stroke,
    covid_effect
    
  ) 


df_stroke_cleaned |> glimpse()

# df_stroke_cleaned |>
#     write_parquet(
#       sink = here("data_raw", "df_stroke_cleaned.parquet"),
#       compression = "gzip"
#     )
# 
# gc()
# gc()

# df_stroke_cleaned <- open_dataset(here("data_raw","df_stroke_cleaned.parquet")) |>
#   collect() |>
#   filter(yr > 0)

# TAKE LINEAR PART OF THE DIFFUSION ONLY
# FROM 2017/18 (AFTER COMMISSIONED BY NHSE)
df_stroke_cleaned <- df_stroke_cleaned |> 
  filter(yr > 0) 



# df_stroke_cleaned |> mutate(thromb = as.integer(thromb)) |> count(thromb)
  

df_stroke_cleaned |> glimpse()
df_stroke_cleaned |> count(yr)
df_stroke_cleaned$thromb |> levels()
