# ==============================================================================
# SUPERSPELL (CONTINUOUS INPATIENT SPELL) FUNCTIONS -- THROMBECTOMY COHORT
# ==============================================================================
# Sourced by thromb_02_superspells.R and test_superspells.R.
#
# Three linking definitions are built side by side, so the analysis can be
# run under each as a sensitivity check:
#
#   "strict" : your original rule -- the next spell starts on the day the
#              previous one ended -- plus overlapping spells. No transfer
#              codes required.
#   "cip"    : NHS Digital continuous inpatient (CIP) spell rules, as
#              implemented in UKHSA's epidm::cip_spells(): the next spell
#              starts 0-2 days after the previous discharge AND a transfer is
#              recorded (previous discharge destination, next admission
#              source, or next admission method), minus one exclusion
#              (discharged home, then readmitted via A&E). Overlapping
#              spells are also merged (see LINK_OVERLAPS).
#   "union"  : linked if either rule links. Catches same-day transfers whose
#              transfer codes were not recorded.
#
# Regular day/night attenders (patient classification 3/4) are dropped
# before linking, as in the NHS Digital method.
#
# PERFORMANCE NOTE: with ~800k patients, dplyr's per-group evaluation
# (group_by + mutate/summarise) takes many minutes. The grouped steps below
# therefore use vectorised base-R tools on data sorted by group:
# cumsum(), cummax(), rowsum(), order() and !duplicated(). Same logic,
# seconds instead of minutes.
# ==============================================================================


# ---- Linking parameters ------------------------------------------------------
MAX_GAP_DAYS <- 2L # NHS Digital CIP tolerance (0, 1 or 2 days)
LINK_OVERLAPS <- TRUE # deliberate addition: concurrent spells always merge

DISDEST_TRANSFER <- c("49", "50", "51", "52", "53", "84") # previous spell's discharge destination
ADMISORC_TRANSFER <- c("49", "50", "51", "52", "53", "87") # next spell's admission source
ADMIMETH_TRANSFER <- c("2B", "81") # next spell's admission method
REGULAR_ATTENDER <- c("3", "4") # patient classification

# ---- Helpers -----------------------------------------------------------------

# Running maximum of a Date, restarting at each new group. Data must be
# sorted by group; group_index must be a non-decreasing integer (1, 1, 2, ...).
# Trick: add a per-group offset larger than any date value, so one
# vectorised cummax() can never carry a value across a group boundary.
cummax_by <- function(x, group_index) {
  offset <- group_index * 1e6
  as.Date(cummax(as.numeric(x) + offset) - offset, origin = "1970-01-01")
}

# Financial year label ("2014/15") from a date
fyear_of <- function(d) {
  y <- as.integer(format(d, "%Y")) - (as.integer(format(d, "%m")) < 4L)
  paste0(y, "/", str_pad((y + 1L) %% 100L, 2, pad = "0"))
}

# Emergency (2x) or non-emergency transfer (81) admission method?
is_emerg_or_transfer <- function(admimeth) {
  coalesce(str_sub(admimeth, 1, 1) == "2" | admimeth == "81", FALSE)
}

# df_stroke_raw |>
#   count(admission_method, sort = T)


# ---- 1. Link spells into superspells -----------------------------------------
# Input:  one row per spell (output of thromb_01_spells.sql). Code columns
#         MUST be character ("2B" becomes NA if read as a number).
# Output: same rows (minus regular attenders) with link diagnostics and three
#         integer superspell IDs: ss_strict, ss_cip, ss_union.
link_superspells <- function(spells,
                             max_gap_days = MAX_GAP_DAYS,
                             link_overlaps = LINK_OVERLAPS) {
  n_reg <- sum(spells$patient_class %in% REGULAR_ATTENDER)
  n_nodate <- sum(is.na(spells$admission_date))
  if (n_reg > 0) message("Dropping ", n_reg, " regular-attender spells before linking.")
  if (n_nodate > 0) message("Dropping ", n_nodate, " spells with no admission date.")

  spells |>
    filter(!patient_class %in% REGULAR_ATTENDER, !is.na(admission_date)) |>
    mutate(dis_for_link = coalesce(discharge_date, admission_date)) |>
    arrange(nhs_no, admission_date, dis_for_link, der_spell_id) |>
    mutate(
      has_prev = coalesce(nhs_no == lag(nhs_no), FALSE),
      pt_index = cumsum(!has_prev),
      # Gap is measured from the LATEST discharge so far (running max), not
      # just the previous row, so a short spell nested inside a long one
      # cannot make the next spell look like it starts after a gap.
      prev_max_dis = if_else(has_prev, lag(cummax_by(dis_for_link, pt_index)), as.Date(NA)),
      gap_days = as.integer(admission_date - prev_max_dis),
      # Transfer codes are read from the immediately preceding spell.
      prev_disdest = if_else(has_prev, lag(discharge_destination), NA_character_),
      prev_provider = if_else(has_prev, lag(provider_code), NA_character_),
      transfer_evidence =
        prev_disdest %in% DISDEST_TRANSFER |
          admission_source %in% ADMISORC_TRANSFER |
          admission_method %in% ADMIMETH_TRANSFER,
      # NHS Digital exclusion: sent home, then back in via (another
      # provider's) A&E -- a readmission, not a transfer.
      cip_exclusion =
        prev_disdest %in% "19" &
          admission_source %in% "51" &
          admission_method %in% "21",
      is_overlap = has_prev & gap_days < 0L,
      link_strict = has_prev & gap_days <= 0L,
      link_cip = has_prev & (
        (link_overlaps & is_overlap) |
          (gap_days >= 0L & gap_days <= max_gap_days &
            transfer_evidence & !cip_exclusion)
      ),
      link_union = link_strict | link_cip,
      # A patient's first spell never links, so a running count of
      # "not linked" gives IDs that are unique across the whole dataset.
      ss_strict = cumsum(!link_strict),
      ss_cip = cumsum(!link_cip),
      ss_union = cumsum(!link_union)
    )
}

# ---- 2. Collapse linked spells to one row per superspell ---------------------
# ss_col: which definition to use ("ss_strict", "ss_cip" or "ss_union").
summarise_superspells <- function(linked, ss_col = "ss_cip") {
  d <- linked %>%
    mutate(
      ss_id = .data[[ss_col]],
      # A spell that would have met the ORIGINAL cohort rule on its own
      i63_qualifying = coalesce(i63_primary_any_ep == 1, FALSE) &
        is_emerg_or_transfer(admission_method),
      thromb_flag = coalesce(thromb == 1, FALSE),
      thromb_old = coalesce(thromb_in_i63_ep == 1, FALSE) &
        is_emerg_or_transfer(admission_method),
      died_flag = discharge_method %in% "4"
    ) |>
    arrange(ss_id, admission_date, dis_for_link, der_spell_id)

  # --- vectorised grouped helpers (data sorted by ss_id) ---
  g <- d$ss_id
  ids <- g[!duplicated(g)]
  grp_sum <- function(x) as.vector(rowsum(as.numeric(x), g, reorder = FALSE))
  grp_any <- function(x) grp_sum(x) > 0
  # value of x on the first row (in current order) where cond is TRUE
  grp_first_where <- function(x, cond) {
    idx <- which(cond)
    idx <- idx[!duplicated(g[idx])]
    x[idx[match(ids, g[idx])]] # NA for groups with no such row
  }
  # minimum of x over rows where cond is TRUE
  grp_min_where <- function(x, cond) {
    idx <- which(cond & !is.na(x))
    idx <- idx[order(g[idx], as.numeric(x[idx]))]
    idx <- idx[!duplicated(g[idx])]
    x[idx[match(ids, g[idx])]]
  }
  grp_max <- function(x) {
    o <- order(g, -as.numeric(x))
    x[o][!duplicated(g[o])]
  }

  d |>
    filter(!duplicated(ss_id)) |> # first spell of each superspell
    transmute(
      nhs_no,
      ss_id,
      ss_start = admission_date,
      first_spell_id = der_spell_id,
      first_adm_method = admission_method,
      first_adm_source = admission_source,
      admitting_provider = provider_code,
      admitting_site, # pre-decision anchor
      # TODO
      adm_site_name, 
      adm_trust_name, 
      #
      i63_in_first_spell = coalesce(i63_primary_any_ep == 1, FALSE),
      age, sex, ethnic_group, lsoa11code, lsoa21code
    ) |>
    mutate(
      n_nhs_no = grp_max(d$n_nhs_no),
      ss_end = grp_max(d$dis_for_link),
      n_spells = as.integer(grp_sum(rep(1, length(g)))),
      n_providers = as.integer(grp_sum(!duplicated(paste(g, d$provider_code)))),
      i63_qualifying = grp_any(d$i63_qualifying),
      thromb = grp_any(d$thromb_flag),
      thromb_old_method = grp_any(d$thromb_old),
      thromb_ep_start = grp_min_where(d$thromb_ep_start_date, d$thromb_flag),
      # TODO
      thromb_site = grp_first_where(d$thromb_site, d$thromb_flag),
      thromb_site_name = grp_first_where(d$thromb_site_name, d$thromb_flag),
      thromb_trust_name = grp_first_where(d$thromb_trust_name, d$thromb_flag),
      #
      died_in_ss = grp_any(d$died_flag),
      days_to_thromb = as.integer(thromb_ep_start - ss_start),
      ss_los_days = as.integer(ss_end - ss_start),
      starts_elective = coalesce(str_sub(first_adm_method, 1, 1) == "1", FALSE),
      # Superspell begins with a transfer-in: its onset spell is not in the
      # data (another UK nation, independent sector, or an unlinked record)
      starts_transfer_in = first_adm_method %in% ADMIMETH_TRANSFER |
        first_adm_source %in% ADMISORC_TRANSFER
    )
}

# ---- 3. Diagnostics ----------------------------------------------------------

# Gap (days) between consecutive spells vs whether a transfer is recorded.
# The 1/2/3-day "readmissions" you saw should be mostly transfer-coded.
gap_transfer_table <- function(linked, max_gap_shown = 7L) {
  linked |>
    filter(has_prev, gap_days <= max_gap_shown) |>
    mutate(
      gap = if_else(gap_days < 0L, "overlap", as.character(gap_days)),
      gap = factor(gap, levels = c("overlap", as.character(0:max_gap_shown))),
      provider_change = provider_code != prev_provider
    ) |>
    count(gap, transfer_evidence, provider_change) |>
    group_by(gap) |>
    mutate(pct_of_gap = round(100 * n / sum(n), 1)) |>
    ungroup() |>
    arrange(gap, desc(transfer_evidence), desc(provider_change))
}

# Headline counts for stroke superspells under each linking definition
compare_definitions <- function(linked) {
  q <- coalesce(linked$i63_primary_any_ep == 1, FALSE) &
    is_emerg_or_transfer(linked$admission_method)
  th <- coalesce(linked$thromb == 1, FALSE)

  bind_rows(lapply(c("ss_strict", "ss_cip", "ss_union"), function(def) {
    agg <- rowsum(cbind(i63 = q, thromb = th, n = 1), linked[[def]])
    keep <- agg[, "i63"] > 0
    tibble(
      definition           = def,
      n_stroke_superspells = sum(keep),
      n_with_thromb        = sum(agg[keep, "thromb"] > 0),
      pct_thromb           = round(100 * mean(agg[keep, "thromb"] > 0), 2),
      pct_multi_spell      = round(100 * mean(agg[keep, "n"] > 1), 1),
      max_spells_in_one    = max(agg[keep, "n"])
    )
  }))
}
