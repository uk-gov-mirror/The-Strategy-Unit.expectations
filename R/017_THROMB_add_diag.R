# README
# Add der_diagnosis_all to cohort.


# FILTER LOOKBACK SO AS TO INCLUDE COHORT ONLY:
df_lookback_for_supers <- df_lookback_thromb |> 
  semi_join(
    df_stroke_exclusions,
    join_by(
      der_spell_id == first_spell_id,
      nhs_no
    ))
gc()
gc()


# THESE SHOULD BE THE DIAGNOSES FOR THE INDEX SPELL IN SUPERSPELL ONLY:
df_diagnosis_all_for_supers <- df_lookback_for_supers |> 
  group_by(der_spell_id, nhs_no) |> 
  filter(prior_discharge_date == max(prior_discharge_date)) |> 
  ungroup()

# BUT IN <0.1% CASES THERE IS 2+ MAX DISCHARGE DATE, SO SELECT 1 ROW:
tmp_select_rows <- df_diagnosis_all_for_supers |> 
  # 1. SELECT ANY 1 ROW WHICH HAS AN I63 DIAG. IF STILL 2+ THEN
  # 2. SELECT A RANDOM ROW
  mutate(has_i63 = str_detect(prior_diagnosis_all, "I63")) |>
  arrange(has_i63) |> 
  group_by(nhs_no, der_spell_id) |> 
  mutate(cs_i63 = cumsum(has_i63)) |>
  filter(cs_i63 == max(cs_i63)) |>
  filter(row_number() == min(row_number())) |> 
  ungroup()

df_stroke_exclusions_with_diag <- df_stroke_exclusions |> 
  left_join(
    tmp_select_rows |> select(der_spell_id, nhs_no, der_diagnosis_all = prior_diagnosis_all), 
    join_by(
      first_spell_id == der_spell_id, 
      nhs_no
    ))

stopifnot(df_stroke_exclusions |> nrow() == df_stroke_exclusions_with_diag |> nrow())



