# 1. DECILE MODEL ------------------------------------------------

# df_stroke_cleaned |> glimpse()
Sys.time()
mod_thromb_01 <- mgcv::gam(
  formula = thromb ~
    yr * imd_decile +
    covid_effect +
    is_wkend +
    sex + s(age_std, by = sex) +
    distance_std + rural_urban_classification + rgn22nm + 
    thromb_centre_first +
    s(cci_std) + 
    s(hfrs_std) + # any_prior_admission +
    lb_hypertension + lb_ihd  + lb_diabetes +
    lb_copd + lb_ckd + lb_valve_disease + lb_obesity +
    # (RANDOM INTERCEPT):
    s(sicbl25nm, bs = "re"),
  family = "binomial",
  method = "REML",
  data = df_stroke_cleaned 
)
# 13:54 - 15:02
Sys.time()
gc()
gc()



mod_thromb_01 |> saveRDS(here("data_raw", "mod_thromb_01.rds"))
mod_thromb_01 <- readRDS(here("data_raw", "mod_thromb_01.rds"))


gc()
gc()
gc()

mgcv::plot.gam(mod_thromb_01, pages =0)

mod_thromb_01 |> 
  broom::tidy(parametric = T) |> 
  mutate(odds = exp(estimate)) |> 
  mutate(lci = exp(estimate - 1.96 * std.error)) |> 
  mutate(uci = exp(estimate + 1.96 * std.error)) |> 
  select(term, estimate, odds, lci, uci) |> 
  view("coeffs_thromb_01")

