# ==============================================================================
# THROMBECTOMY COHORT -- BUILD SUPERSPELLS AND THE ANALYSIS TABLE
# ==============================================================================
# Input : spell-level export of thromb_01_spells.sql (one row per spell)
# Output: one row per stroke superspell (= one stroke event), with
#         thrombectomy outcome, admitting-site anchor, and diagnostic flags.
#
# Run order: thromb_01_spells.sql -> (export) -> this script.
# test_superspells.R checks the linking rules on synthetic patients.
# ==============================================================================

source(here("R",  "013_THROMB_funs.R"))
# library(readr)

# TODO GET RID OF MULTI PATIENT SINGLE SPELLS SEE QA 3


# ---- 0. Parameters -----------------------------------------------------------
# df_stroke_raw    <- readRDS(here("data_raw",  "df_stroke_raw.rds"))
# Sys.getenv("THROMB_SPELLS_CSV", "data/thromb_spells.csv")  # TODO: your path
# OUT_RDS       <- 
#Sys.getenv("THROMB_OUT_RDS",    "data/thromb_cohort_superspells.rds")

STUDY_START   <- as.Date("2014-04-01")
STUDY_END     <- as.Date("2026-03-31")
SS_DEFINITION <- "ss_cip"   # primary; re-run with "ss_strict" / "ss_union" for sensitivity
MIN_AGE       <- 18L        # CHANGE from the original query (no lower bound) -- see notes
MAX_AGE       <- 112L       # AS original

# ---- 1. Load the spell extract -----------------------------------------------
# Every code column is read as character: admission method "2B" would become
# NA if read as a number, silently breaking transfer detection.
# (If you pull straight from SQL Server with DBI instead, check the same.)
# spells <- read_csv(
#   df_stroke_raw |> glimpse()
#   col_types = cols(
#     .default               = col_character(),
#     admission_date         = col_date(),
#     discharge_date         = col_date(),
#     thromb_ep_start_date   = col_date(),
#     age                    = col_integer(),
#     n_nhs_no               = col_integer(),
#     n_episodes             = col_integer(),
#     discharge_date_imputed = col_integer(),
#     i63_primary_any_ep     = col_integer(),
#     i63_primary_first_ep   = col_integer(),
#     thromb                 = col_integer(),
#     thromb_in_i63_ep       = col_integer()
#   )
# )



# df_stroke_raw |> colnames()

stopifnot(!any(duplicated(df_stroke_raw$der_spell_id)))
message("Spells loaded: ", nrow(df_stroke_raw), " for ", n_distinct(df_stroke_raw$nhs_no), " patients")
message("Spells with >1 patient ID (SQL QA 3): ", sum(df_stroke_raw$n_nhs_no > 1, na.rm = TRUE))

# ---- 2. Link spells into superspells (all three definitions) -----------------
linked <- link_superspells(df_stroke_raw)

# linked |> colnames()

# ---- 3. Diagnostics -- look at these before settling on a definition ---------
# (a) The 1/2/3-day "readmissions": how many carry a transfer code, and how
#     many involve a change of provider? Transfer-coded + provider change =
#     continuous care; neither = genuine readmission.
gap_tab <- gap_transfer_table(linked)
print(gap_tab, n = 36)

# (b) How much does the choice of definition matter?
def_tab <- compare_definitions(linked)
print(def_tab)

# ---- 4. One row per superspell -----------------------------------------------
superspells <- summarise_superspells(linked, SS_DEFINITION)

superspells_union <- summarise_superspells(linked, "ss_union")
# superspells <- summarise_superspells(linked, SS_DEFINITION)


superspells |> 
  filter(
  i63_qualifying,  # >= 1 I63-primary spell admitted as emergency or transfer (original rule)
  ss_start >= STUDY_START, ss_start <= STUDY_END, # window applies to stroke ONSET
  !is.na(age), age >= MIN_AGE, age <= MAX_AGE
) |> 
  count(thromb) |> 
  mutate(p = n/sum(n))

superspells_union |>
  filter(
    i63_qualifying,  # >= 1 I63-primary spell admitted as emergency or transfer (original rule)
    ss_start >= STUDY_START, ss_start <= STUDY_END, # window applies to stroke ONSET
    !is.na(age), age >= MIN_AGE, age <= MAX_AGE
  ) |> 
  count(thromb) |> 
  mutate(p = n/sum(n))


# (c) Implausible superspells: very long or very many spells usually means a
#     placeholder patient ID chaining unrelated admissions (SQL QA 4).
superspells_union |> 
  filter(n_spells > 10 | ss_los_days > 365) |> 
  arrange(desc(n_spells)) |>
  # arrange(desc(ss_los_days)) |> 
  select(nhs_no, ss_id, ss_start, ss_end, n_spells, n_providers, ss_los_days) |> 
  print(n = 20)

# ---- 5. Cohort: stroke events starting in the study window -------------------

cohort <- superspells_union |> 
  filter(
    i63_qualifying,  # >= 1 I63-primary spell admitted as emergency or transfer (original rule)
    ss_start >= STUDY_START, ss_start <= STUDY_END, # window applies to stroke ONSET
    !is.na(age), age >= MIN_AGE, age <= MAX_AGE
  ) |> 
  mutate(
    fyear    = fyear_of(ss_start),
    is_wkend = as.integer(format(ss_start, "%u") %in% c("6", "7")),  # Sat/Sun of onset
    sex      = if_else(sex %in% c("1", "2"), sex, "not specified")
  ) |> 
  arrange(nhs_no, ss_start) |> 
  mutate(
    # Recurrent strokes: the same patient can contribute several events.
    # EITHER keep all events (and cluster on patient), or keep the
    # first event per patient. Very short gaps after a home discharge may be
    # the same stroke rather than a recurrence -- see days_since_prev_stroke.
    same_pt_as_prev        = coalesce(nhs_no == lag(nhs_no), FALSE),
    stroke_event_n         = row_number() - match(nhs_no, nhs_no) + 1L,
    days_since_prev_stroke = if_else(same_pt_as_prev,
                                     as.integer(ss_start - lag(ss_start)),
                                     NA_integer_)
  ) |> 
  select(-same_pt_as_prev)

# TAKE THE LAST STROKE EVENT PER PATIENT (KEEPS ~90% OF RECORDS)
# nrow(tmp_cohort)/ nrow(cohort)
# SHORT GAPS SINCE PREV STROKE ARE LIKELY PART OF THE SAME SPELL THAT WASN'T
# PICKED UP BY UNION/CIP RULES. LONGER GAPS MEAN 2+ RECORDS FOR SAME INDIVIDUAL 
# = VIOLATING MODEL ASSUMPTIONS. 


# CIP RULES
# ALL ZEROS ; 1 AND 2 DAY GAPS WITH LINKING RULES.

# WE DO GET STROKE RECURRENCE FOR SAME INDIVIDUAL OVER 13 YR PERIOD.
# HERE IT RUNS ABOUT 10% 
# ISCHAEMIC STROKE RECURRENCE IS <5% OVER 90 DAYS.
# SOME OF THESE, WHERE THE GAP BETWEEN SPELLS IS SHORT, WILL BE 
# CIP SPELLS THAT GOT MISSED BY RULES.

# COUNT ONLY FIRST SUPERSPELL? MISS 5% OF THROMBS
# OR 
# COUNT ALL? VIOLATE MODEL ASSUMPTIONS, RISK DOUBLE COUNTING SS.


# WHAT TO DO?



# TODO THE LATEST SPELL WHERE THE DAYS SINCE PREV IS MEANINGFUL?

cohort |> count(is.na(days_since_prev_stroke)) |> mutate(p = n/sum(n))

tmp_cohort <- cohort |> 
  arrange(ss_start) |> 
  group_by(nhs_no) |> 
  # filter(row_number() == max(row_number())) |> 
  filter(row_number() == 1) |> 
  ungroup()

cohort |> 
  count(thromb) |> 
  mutate(p = n/sum(n))

# BY TAKING ONLY LAST SS WE MISS 1k THROMB
# BY TAKING ONLY FIRST SS WE MISS 1.25k THROMB
# THROMB BIAS IN EARLY YEARS? (LESS LIKELY TO HAVE HAD A PREVIOUS)
# WE'RE IGNORING TIME WHERE THROMBS RARE ANYWAY -- BUT BIAS TO STUDY.
# PROBABLY FIRST (RECORDED) STROKE EVENT.

tmp_cohort |>
  count(thromb) |> 
  mutate(p = n/sum(n))
# 17724

tmp_cohort |> 
  # count(thromb) |> 
  # mutate(p = n/sum(n))
  left_join(lkp_lsoa_bestfit, join_by(lsoa11code == lsoa11cd)) |> 
  rename(lsoa21_bfit = lsoa21cd) |> 
  count(fyear, a = str_detect(lsoa21_bfit, "^E")) |> 
  group_by(fyear) |> 
  mutate(p = n/sum(n)) |> 
  filter(a == T)
  
  
# TODO: apply your existing LSOA-based exclusions here.
  
df_stroke_exclusions <- tmp_cohort |> 
  # ONLY 70% HAVE LSOA 21 CODE SO USE 2011 WITH BEST FIT CONVERSION LOOKUP:
  left_join(lkp_lsoa_bestfit, join_by(lsoa11code == lsoa11cd)) |> 
  rename(lsoa21_bfit = lsoa21cd) |> 
  select(everything(), -contains("lsoa"), lsoa21_bfit) |> 
  # 99.1% HAVE ENGLISH BEST-FIT LSOA: 
  filter(str_detect(lsoa21_bfit, "^E")) |> 
  filter(sex %in% c("1", "2")) |> 
  # REMOVE ~250 RECORDS FROM PRIVATE PROVIDERS: THIS REMOVES SOME LONG LoS SPELLS
  filter(!str_detect(admitting_provider, "^N")) |> 
  # REMOVE ONE RECORD WITH AN SS LoS < 0
  filter(ss_los_days >= 0) 
  
tmp_cohort |> count(str_detect(admitting_provider, "^N"))

# TODO POSSIBLE FURTHER EXCLUSIONS FROM COHORT:
# TODO CHECK N NHS NUMBERS 
# TODO CHECK LONG SPELLS
# TODO CHECK DAYS SINCE PREVIOUS STROKE
df_stroke_exclusions |> count()

df_stroke_exclusions |> count(n_nhs_no > 1)
df_stroke_exclusions |> count(n_nhs_no)

df_stroke_exclusions |> count(ss_los_days <0)
df_stroke_exclusions |> count(is.na(ss_los_days))
df_stroke_exclusions |> 
  filter(ss_los_days <0) |> 
  select(nhs_no, adm_trust_name, admitting_provider, ss_id, ss_start, ss_end, n_spells, n_providers, ss_los_days, thromb)
  
df_stroke_exclusions |> count(ss_los_days >3*365)
df_stroke_exclusions |> count(ss_los_days) |> tail()

df_stroke_exclusions |> #colnames()
  # count(str_sub(thromb_site, 1, 1))
  # TODO 1: REMOVE PRIVATE ADMITTINGS: -TAKES CARE OF LONG LENGTH OF STAY 
  filter(!str_detect(admitting_provider, "^N")) |> 
  # count(str_sub(admitting_provider, 1, 1))
  filter(ss_los_days>1200) |> 
  select(nhs_no, admitting_provider, ss_id, ss_start, ss_end, n_spells, n_providers, ss_los_days, thromb)

cohort |> 
  count(days_since_prev_stroke, sort = T) |> 
  mutate(p = n/sum(n)) |> 
  mutate(cs = cumsum(p)) |> 
  filter(days_since_prev_stroke == 50) |> 
  print(n=20)

cohort |> 
  # filter(days_since_prev_stroke > 50) |>
  filter(days_since_prev_stroke == 0) |>
  # filter(between(days_since_prev_stroke, 0, 0)) |> 
  count(thromb) |> 
  mutate(p = n/sum(n))


cohort |> 
  count(days_since_prev_stroke, thromb) |> 
  pivot_wider(names_from = thromb, values_from = n, names_prefix = "THROMB_") |> 
  mutate(p = THROMB_TRUE / (THROMB_FALSE + THROMB_TRUE))|> 
  print(n=20)

# GIVEN HIGH THROMB RATE, ZERO-DAY READMISSIONS ARE CERTAINLY 
# SSs THAT SHOULD HAVE BEEN LINKED TO PREVIOUS SS. 
df_stroke_exclusions |> 
  filter(between(days_since_prev_stroke, 0, 14)| is.na(days_since_prev_stroke)) |> 
  count(prev_stroke = days_since_prev_stroke >=0, thromb) |> 
  group_by(prev_stroke) |> 
  mutate(p = n/sum(n)) 
df_stroke_exclusions |> 
  # filter(days_since_prev_stroke == 1) |>
  filter(nhs_no == "100000015228") |> 
  select(ss_start, ss_end, days_since_prev_stroke, thromb)

df_stroke_exclusions |> 
  # filter(days_since_prev_stroke == 0) |>
  filter(nhs_no == "100000009456") |>
  select(ss_start, ss_end, days_since_prev_stroke, thromb)

# TODO LOOK AT DAYS SINCE PREV STROKE
  


# ---- 6. What changed versus the episode-level query --------------------------
# Spells that met the original rule and started in the window, versus the
# stroke events they collapse into:
n_qual_spells <- linked |> 
  filter(coalesce(i63_primary_any_ep == 1, FALSE),
         is_emerg_or_transfer(admission_method),
         admission_date >= STUDY_START, admission_date <= STUDY_END) |> 
  nrow()

message("Qualifying I63 spells in window: ", n_qual_spells,
        " -> stroke events (", SS_DEFINITION, "): ", nrow(cohort), 
        " - > applying exclusions: ", nrow(df_stroke_exclusions))

df_stroke_exclusions |> 
  summarise(
    events                    = n(),
    thromb_new                = sum(thromb),
    thromb_seen_by_old_query  = sum(thromb_old_method),
    thromb_recovered          = sum(thromb & !thromb_old_method),
    pct_thromb                = round(100 * mean(thromb), 2),
    pct_multi_provider        = round(100 * mean(n_providers > 1), 1),
    pct_starts_elective       = round(100 * mean(starts_elective), 2),
    pct_starts_transfer_in    = round(100 * mean(starts_transfer_in), 2),
    pct_died_in_ss            = round(100 * mean(died_in_ss), 1)
  ) |> 
  print(width = Inf)

# Thrombectomy timing: should be day 0 or 1 for almost everyone. Later
# procedures suggest an in-hospital second stroke or a linking error.
df_stroke_exclusions |> 
  filter(thromb) |> 
  # count(days_to_thromb) |> 
  count(days_to_thromb = pmin(days_to_thromb, 5L)) |>
  mutate(p = n/sum(n)) |> 
  print(n=70)

# ---- 7. Save -----------------------------------------------------------------
# df_stroke_exclusions |> saveRDS(here("data_raw", "df_stroke_cohort_exclusions.rds"))
df_stroke_exclusions <- readRDS(here("data_raw", "df_stroke_cohort_exclusions.rds"))

df_stroke_exclusions |> colnames()





