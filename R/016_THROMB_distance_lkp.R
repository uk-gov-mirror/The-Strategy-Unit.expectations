
# ALL THROMB SITES HAVE POSTCODES WITHOUT NEED FOR EDIT.
# THESE ARE THE 29 UNIQUE POSTCODES:
df_thromb_centre_postcodes <- lkp_thromb_centre_for_distance |>
  distinct(thromb_site) |>
  left_join(eric, join_by(thromb_site == site_code)) |>
  arrange(site_name) |> 
  left_join(lkp_postcode_coords, join_by(post_code)) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(27700) |> 
  print(n=45)


df_thromb_relevant_centroids <- df_stroke_exclusions |> 
  distinct(lsoa21_bfit) |>
  left_join(lkp_lsoa_centroids, join_by(lsoa21_bfit == lsoa21cd)) |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(27700)

gc()
gc()


df_thromb_distance_combos <- df_thromb_relevant_centroids |> 
  cross_join(df_thromb_centre_postcodes |> select(-post_code)) |> 
  tibble()

df_thromb_distance_combos <- 
  st_distance(df_thromb_relevant_centroids, df_thromb_centre_postcodes) |> 
  as_tibble() |> 
  # ADD THE LSOA COLUMN BACK IN:
  mutate(lsoa21_bfit = df_thromb_relevant_centroids$lsoa21_bfit, .before = 1)

colnames(df_thromb_distance_combos)[2:30] <- df_thromb_centre_postcodes$thromb_site

df_thromb_distance_combos_long <- df_thromb_distance_combos |> 
  pivot_longer(cols = 2:30, names_to = "thromb_site", values_to = "distance")

gc()
gc()

df_thromb_distance_by_year <- df_thromb_distance_combos_long |> 
  # left_join(lkp_thrombo_centres, join_by(site_code),
  #           relationship = "many-to-many")
  full_join(
    lkp_thromb_centre_for_distance |> 
      select(fyear, thromb_site),
    join_by(thromb_site),
    relationship = "many-to-many"
  )

gc()
gc()

# -------------------------------------------------------------------------
# THIS TAKES A MINUTE OR SO: DISTANCE IN METERS.
lkp_min_distance_thromb <- df_thromb_distance_by_year |> 
  group_by(fyear, lsoa21_bfit) |> 
  filter(distance == min(distance)) |> 
  ungroup() |> 
  select(lsoa21_bfit, fyear, distance) |> 
  mutate(distance = as.numeric(distance)) |> 
  rename(min_thromb_dist = distance) |> 
  distinct()

gc()
gc()
gc()


# lkp_min_distance_thromb |> saveRDS(here("data", "lkp_min_distance_thromb"))
lkp_min_distance_thromb <- readRDS(here("data", "lkp_min_distance_thromb"))
