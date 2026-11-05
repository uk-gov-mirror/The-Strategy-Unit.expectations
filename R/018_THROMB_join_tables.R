
df_stroke_join_tables <- df_stroke_exclusions_with_diag |> 
  left_join(lkp_imd25, join_by(lsoa21_bfit == lsoa_code_2021)) |>
  left_join(lkp_rural, join_by(lsoa21_bfit == lsoa_code)) |>
  left_join(lkp_icb, join_by(lsoa21_bfit == lsoa21cd)) |> 
  left_join(lkp_regione, join_by(lsoa21_bfit == lsoa21cd)) |>
  left_join(
    lkp_thromb_centre |> rename(thromb_centre_first = thromb_centre),
    join_by(
      fyear,
      admitting_site == thromb_site
    )
  ) |>
  mutate(thromb_centre_first = if_else(is.na(thromb_centre_first), 0, thromb_centre_first)) |> 
  add_charlson_score() |>
  left_join(lkp_min_distance_thromb, join_by(lsoa21_bfit, fyear))

gc()
gc()
gc()

stopifnot(
  nrow(df_stroke_join_tables) == nrow(df_stroke_exclusions_with_diag)
)

# df_stroke_join_tables |>
#     write_parquet(
#       sink = here("data_raw", "df_stroke_join_tables.parquet"),
#       compression = "gzip"
#     )

