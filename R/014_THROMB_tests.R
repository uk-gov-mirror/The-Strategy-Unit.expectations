
# ==============================================================================
# TESTS FOR THE SUPERSPELL LINKING -- synthetic patients with known answers
# ==============================================================================
# Run:  Rscript test_superspells.R   (from the folder holding the functions file)
# Each patient is a small scenario; `expect` gives the number of superspells
# each definition should produce. Any mismatch stops the script.
# ==============================================================================

source("thromb_superspell_functions.R")

sp <- function(nhs_no, id, adm, dis, meth, src, dest,
               i63 = 0L, thromb = 0L, thromb_i63 = 0L,
               prov = "P1", site = "S1", class = "1", dismeth = "1") {
  tibble(
    nhs_no = nhs_no, der_spell_id = id,
    admission_date = as.Date(adm), discharge_date = as.Date(dis),
    admission_method = meth, admission_source = src,
    discharge_destination = dest, discharge_method = dismeth,
    patient_class = class, provider_code = prov, admitting_site = site,
    i63_primary_any_ep = i63, i63_primary_first_ep = i63,
    thromb = thromb, thromb_in_i63_ep = thromb_i63,
    thromb_ep_start_date = if (thromb == 1) as.Date(adm) else as.Date(NA),
    thromb_site = if (thromb == 1) site else NA_character_,
    age = 70L, sex = "1", ethnic_group = "A",
    lsoa11code = "E01000001", lsoa21code = "E01000001"
  )
}

spells <- bind_rows(
  # A: spoke -> hub (coded I66, thrombectomy) -> repatriation, all same-day
  sp("A", "A1", "2015-01-10", "2015-01-11", "21", "19", "51", i63 = 1L, prov = "SPOKE", site = "SPK"),
  sp("A", "A2", "2015-01-11", "2015-01-14", "2B", "51", "51", thromb = 1L, prov = "HUB", site = "HUB"),
  sp("A", "A3", "2015-01-14", "2015-02-01", "81", "51", "19", i63 = 1L, prov = "SPOKE", site = "SPK"),
  # B: transfer recorded, 1-day gap
  sp("B", "B1", "2015-01-10", "2015-01-11", "21", "19", "51", i63 = 1L),
  sp("B", "B2", "2015-01-12", "2015-01-14", "2B", "51", "19", thromb = 1L, prov = "HUB"),
  # C: genuine readmission: home, then back via A&E next day
  sp("C", "C1", "2015-01-10", "2015-01-15", "21", "19", "19", i63 = 1L),
  sp("C", "C2", "2015-01-16", "2015-01-20", "21", "19", "19", i63 = 1L),
  # D: same-day readmission, no transfer codes
  sp("D", "D1", "2015-01-10", "2015-01-15", "21", "19", "19", i63 = 1L),
  sp("D", "D2", "2015-01-15", "2015-01-20", "21", "19", "19"),
  # E: overlapping spells
  sp("E", "E1", "2015-01-10", "2015-01-13", "21", "19", "19", i63 = 1L),
  sp("E", "E2", "2015-01-12", "2015-01-20", "21", "19", "19", prov = "P2"),
  # F: long spell, a day case nested inside it, then a transfer 1 day after
  #    the LONG spell ends (the running max must be used for the gap)
  sp("F", "F1", "2015-01-01", "2015-02-28", "21", "19", "51", i63 = 1L),
  sp("F", "F2", "2015-01-20", "2015-01-20", "13", "19", "19", prov = "P2"),
  sp("F", "F3", "2015-03-01", "2015-03-10", "81", "51", "19", prov = "P3"),
  # G: transfer recorded, 2-day gap (inside CIP tolerance)
  sp("G", "G1", "2015-01-10", "2015-01-11", "21", "19", "51", i63 = 1L),
  sp("G", "G2", "2015-01-13", "2015-01-15", "2B", "51", "19", prov = "P2"),
  # H: transfer recorded, 3-day gap (outside CIP tolerance)
  sp("H", "H1", "2015-01-10", "2015-01-11", "21", "19", "51", i63 = 1L),
  sp("H", "H2", "2015-01-14", "2015-01-15", "2B", "51", "19", prov = "P2"),
  # I: NHS Digital exclusion -- sent home, then in via A&E (source 51)
  sp("I", "I1", "2015-01-10", "2015-01-11", "21", "19", "19", i63 = 1L),
  sp("I", "I2", "2015-01-11", "2015-01-14", "21", "51", "19", prov = "P2"),
  # J: left edge -- stroke on 30 Mar 2014, hub transfer on 1 Apr 2014
  sp("J", "J1", "2014-03-30", "2014-04-01", "21", "19", "51", i63 = 1L),
  sp("J", "J2", "2014-04-01", "2014-04-05", "2B", "51", "19", i63 = 1L,
     thromb = 1L, thromb_i63 = 1L, prov = "HUB"),
  # K: regular day attender on the discharge day (dropped before linking)
  sp("K", "K1", "2015-01-10", "2015-01-15", "21", "19", "19", i63 = 1L),
  sp("K", "K2", "2015-01-15", "2015-01-15", "11", "19", "19", class = "3"),
  # L: in-hospital stroke after an elective admission, then transfer
  sp("L", "L1", "2015-01-10", "2015-01-12", "11", "19", "51"),
  sp("L", "L2", "2015-01-12", "2015-01-20", "81", "51", "19", i63 = 1L, prov = "P2")
)

expect <- tribble(
  ~nhs_no, ~strict, ~cip, ~union,
  "A", 1, 1, 1,
  "B", 2, 1, 1,
  "C", 2, 2, 2,
  "D", 1, 2, 1,
  "E", 1, 1, 1,
  "F", 2, 1, 1,
  "G", 2, 1, 1,
  "H", 2, 2, 2,
  "I", 1, 2, 1,
  "J", 1, 1, 1,
  "K", 1, 1, 1,
  "L", 1, 1, 1
)

linked <- link_superspells(spells)

got <- linked %>%
  group_by(nhs_no) %>%
  summarise(strict = n_distinct(ss_strict),
            cip    = n_distinct(ss_cip),
            union  = n_distinct(ss_union))

check <- expect %>%
  inner_join(got, by = "nhs_no", suffix = c("_expected", "_got"))
print(check, n = Inf)

bad <- check %>%
  filter(strict_expected != strict_got | cip_expected != cip_got | union_expected != union_got)
if (nrow(bad) > 0) stop("Linking tests FAILED for: ", paste(bad$nhs_no, collapse = ", "))
message("Linking tests passed (", nrow(check), " scenarios).")

# ---- Superspell summary checks ----------------------------------------------
ss <- summarise_superspells(linked, "ss_cip")

a <- ss %>% filter(nhs_no == "A")
stopifnot(
  nrow(a) == 1,
  a$thromb,                       # thrombectomy captured from the I66-coded hub spell...
  !a$thromb_old_method,           # ...which the original query would have missed
  a$admitting_site == "SPK",      # pre-decision anchor is the spoke
  a$thromb_site == "HUB",
  a$n_providers == 2,
  a$days_to_thromb == 1
)
j <- ss %>% filter(nhs_no == "J")
stopifnot(j$ss_start == as.Date("2014-03-30"))   # dated to onset -> falls before the window
l <- ss %>% filter(nhs_no == "L")
stopifnot(l$starts_elective, l$i63_qualifying)   # flagged, not silently dropped
c_ss <- ss %>% filter(nhs_no == "C")
stopifnot(nrow(c_ss) == 2, all(!c_ss$thromb), all(is.na(c_ss$thromb_site)))
message("Summary tests passed.")

# ---- Diagnostics run without error ------------------------------------------
print(gap_transfer_table(linked))
print(compare_definitions(linked))




