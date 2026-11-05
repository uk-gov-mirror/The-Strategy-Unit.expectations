/*
================================================================================
 THROMBECTOMY COHORT -- SPELL-LEVEL EXTRACT FOR SUPERSPELL CONSTRUCTION
================================================================================
# NOTES
 1. The cohort filter (I63 primary diagnosis, emergency/transfer admission)
    only identifies WHICH PATIENTS to pull. For those patients we take every spell near
    their stroke, whatever its diagnosis or admission method. Reason: the
    hub (thrombectomy-centre) spell is often coded differently from the
    spoke spell (e.g. I66, or I63 only as a secondary diagnosis)
 2. Thrombectomy is flagged on EVERY episode and rolled up to the spell (and,
    in R, to the superspell). 
 3. Episodes are collapsed to SPELLS here, with explicit rules (admission
    fields from the first episode, discharge fields from the last).
    Superspell linking is then done in R (thromb_02_superspells.R), so the
    linking rules can be varied for sensitivity analysis without re-querying.
 4. The study window is applied in R to the SUPERSPELL START DATE, not here
    to individual episodes. A filter on Der_Financial_Year could
    split spells straddling 31 March and mis-date transfers (a spoke spell on
    30 March 2014 + hub spell on 1 April 2014 was counted as a 2014/15 stroke).
    We pull a buffer either side of the window so edge superspells are whole.
 5. Age bounds, LSOA exclusions, and the site-name lookup move to R (the
    site lookup can duplicate rows if site_code is not unique -- see QA 5).

================================================================================
*/

SET DATEFIRST 1;
SET NOCOUNT ON;

DECLARE @study_start DATE = '2014-04-01';
DECLARE @study_end   DATE = '2026-03-31';
/* Window around each I63 spell within which all of that patient's
   spells are pulled. Superspell links are <= 2 days apart, so 60 days is
   generous: it only needs to exceed the length of any single linking spell
   that precedes the stroke spell. It also acts as the left/right buffer for
   superspells straddling the window edges. */
DECLARE @window_days   INT  = 30;


/*------------------------------------------------------------------------------
 STEP 1: I63 SPELLS -- every spell with >= 1 episode with I63 primary diagnosis.
 No admission-method filter here: that is applied at superspell level in R,
 because a transferred hub spell (2B / 81) is part of the same stroke event.
------------------------------------------------------------------------------*/
IF OBJECT_ID('tempdb..#i63_spells') IS NOT NULL DROP TABLE #i63_spells;

SELECT
    [der_pseudo_nhs_number]                                  AS nhs_no,
    [der_spell_id],
    MIN(COALESCE([Admission_Date], [episode_start_date]))    AS spell_adm,
    MAX(COALESCE([Discharge_Date], [episode_end_date]))      AS spell_dis
INTO #i63_spells
FROM [Reporting_MESH_APC].[APCE_Core_Monthly_Snapshot]
WHERE [Deleted] = 0
  AND [Der_Primary_Diagnosis_Code] LIKE 'I63%'
  AND [der_pseudo_nhs_number] IS NOT NULL       /* unlinkable without an ID: see QA 1 */
  AND COALESCE([Admission_Date], [episode_start_date]) <= @study_end
  AND COALESCE([Discharge_Date], [episode_end_date])   >= DATEADD(DAY, -@window_days, @study_start)
GROUP BY [der_pseudo_nhs_number], [der_spell_id];

/*CREATE CLUSTERED INDEX ix_i63 ON #i63_spells (nhs_no, spell_adm); */


/*------------------------------------------------------------------------------
 STEP 2: WINDOW SPELL IDs -- any spell (any diagnosis, any admission
 method) for the same patient that overlaps [I63 admission - 30 days,
 I63 discharge + 30 days]. Selected by SPELL ID so that, in step 3, we take
 every episode of each selected spell (never a partial spell).
------------------------------------------------------------------------------*/
IF OBJECT_ID('tempdb..#window_spell_ids') IS NOT NULL DROP TABLE #window_spell_ids;

SELECT DISTINCT e.[der_spell_id]
INTO #window_spell_ids
FROM [Reporting_MESH_APC].[APCE_Core_Monthly_Snapshot] e
INNER JOIN #i63_spells s
    ON  e.[der_pseudo_nhs_number] = s.nhs_no
    AND COALESCE(e.[Admission_Date], e.[episode_start_date]) <= DATEADD(DAY,  @window_days, s.spell_dis)
    AND COALESCE(e.[Discharge_Date], e.[episode_end_date])   >= DATEADD(DAY, -@window_days, s.spell_adm)
WHERE e.[Deleted] = 0;

/*CREATE CLUSTERED INDEX ix_window ON #window_spell_ids ([der_spell_id]);*/


/*------------------------------------------------------------------------------
 STEP 3: ALL EPISODES OF THOSE SPELLS, with episode-level flags.
------------------------------------------------------------------------------*/
IF OBJECT_ID('tempdb..#episodes') IS NOT NULL DROP TABLE #episodes;

SELECT
    e.[der_pseudo_nhs_number]                          AS nhs_no,
    e.[der_spell_id],
    e.[Der_Episode_Number],
    e.[episode_start_date],
    e.[episode_start_time],
    e.[episode_end_date],
    e.[Admission_Date],
    e.[Admission_Time],
    e.[Discharge_Date],
    e.[Admission_Method],
    e.[Source_of_Admission]                            AS admission_source,       
    e.[Discharge_Destination]                          AS discharge_destination,  
    e.[Discharge_Method]                               AS discharge_method,       
    e.[Patient_Classification]                         AS patient_class,          
    e.[Der_Provider_Code]                              AS provider_code,          
    e.[Der_Provider_Site_Code],
    e.[site_code_of_treatment_at_episode_start_date]   AS site_start,
    e.[Der_Primary_Diagnosis_Code],
    e.[sex],
    e.[der_age_at_cds_activity_date]                   AS age,
    e.[ethnic_group],
    e.[der_postcode_lsoa_2011_code]                    AS lsoa11code,
    e.[der_postcode_lsoa_2021_code]                    AS lsoa21code,
    CASE WHEN e.[Der_Primary_Diagnosis_Code] LIKE 'I63%' THEN 1 ELSE 0 END AS i63_primary,
    /* THROMBECTOMY -- your definition, unchanged (NICE HTG403 + legacy CCSD codes) */
    CASE
        WHEN e.[Der_Procedure_All] LIKE '%L354%'
          OR (
               e.[Der_Procedure_All] LIKE '%L712%'
           AND e.[Der_Procedure_All] LIKE '%Y53%'
           AND (   e.[Der_Procedure_All] LIKE '%Z35%'
                OR e.[Der_Procedure_All] LIKE '%O28[189]%')
             )
        THEN 1 ELSE 0
    END
INTO #episodes
FROM [Reporting_MESH_APC].[APCE_Core_Monthly_Snapshot] e
INNER JOIN #window_spell_ids n
    ON n.[der_spell_id] = e.[der_spell_id]
WHERE e.[Deleted] = 0;


/*------------------------------------------------------------------------------
 STEP 4: COLLAPSE EPISODES TO SPELLS.
   Admission-side fields  <- first episode of the spell
   Discharge-side fields  <- last episode (DISDEST / DISMETH are only
                             meaningful on the discharge episode)
------------------------------------------------------------------------------*/
IF OBJECT_ID('tempdb..#spells') IS NOT NULL DROP TABLE #spells;

WITH ep AS (
    SELECT
        *,
        ROW_NUMBER() OVER (
            PARTITION BY [der_spell_id]
            ORDER BY [episode_start_date], [episode_start_time],
                     TRY_CAST([Der_Episode_Number] AS INT)
        ) AS ep_asc,
        ROW_NUMBER() OVER (
            PARTITION BY [der_spell_id]
            ORDER BY [episode_end_date] DESC, [episode_start_date] DESC,
                     [episode_start_time] DESC,
                     TRY_CAST([Der_Episode_Number] AS INT) DESC
        ) AS ep_desc
    FROM #episodes
)

SELECT
    [der_spell_id],
    MAX(CASE WHEN ep_asc = 1 THEN nhs_no END)                         AS nhs_no,
    COUNT(DISTINCT nhs_no)                                            AS n_nhs_no,   /* QA 3: should be 1 */
    MIN(COALESCE([Admission_Date], [episode_start_date]))             AS admission_date,
    COALESCE(MAX([Discharge_Date]), MAX([episode_end_date]))          AS discharge_date,
    CASE WHEN MAX([Discharge_Date]) IS NULL THEN 1 ELSE 0 END         AS discharge_date_imputed,
    MAX(CASE WHEN ep_asc  = 1 THEN [Admission_Method] END)            AS admission_method,
    MAX(CASE WHEN ep_asc  = 1 THEN admission_source END)              AS admission_source,
    MAX(CASE WHEN ep_desc = 1 THEN discharge_destination END)         AS discharge_destination,
    MAX(CASE WHEN ep_desc = 1 THEN discharge_method END)              AS discharge_method,
    MAX(CASE WHEN ep_asc  = 1 THEN patient_class END)                 AS patient_class,
    MAX(CASE WHEN ep_asc  = 1 THEN provider_code END)                 AS provider_code,
    /* Admitting site = pre-decision anchor for the hospital effect */
    MAX(CASE WHEN ep_asc  = 1 THEN COALESCE(site_start, [Der_Provider_Site_Code]) END) AS admitting_site,
    MAX(CASE WHEN ep_asc  = 1 THEN [Der_Primary_Diagnosis_Code] END)  AS primary_diag_first_ep,
    MAX(i63_primary)                                                  AS i63_primary_any_ep,
    MAX(CASE WHEN ep_asc  = 1 THEN i63_primary END)                   AS i63_primary_first_ep,
    MAX(thromb)                                                       AS thromb,
    /* Would the ORIGINAL query have seen this thrombectomy? (thromb coded in
       an episode that itself had an I63 primary diagnosis.) For comparison only. */
    MAX(CASE WHEN thromb = 1 AND i63_primary = 1 THEN 1 ELSE 0 END)   AS thromb_in_i63_ep,
    MIN(CASE WHEN thromb = 1 THEN [episode_start_date] END)           AS thromb_ep_start_date,
    MAX(CASE WHEN thromb = 1 THEN COALESCE(site_start, [Der_Provider_Site_Code]) END) AS thromb_site,
    MAX(CASE WHEN ep_asc  = 1 THEN age END)                           AS age,
    MAX(CASE WHEN ep_asc  = 1 THEN [sex] END)                         AS sex,
    MAX(CASE WHEN ep_asc  = 1 THEN [ethnic_group] END)                AS ethnic_group,
    MAX(CASE WHEN ep_asc  = 1 THEN lsoa11code END)                    AS lsoa11code,
    MAX(CASE WHEN ep_asc  = 1 THEN lsoa21code END)                    AS lsoa21code,
    COUNT(*)                                                          AS n_episodes
INTO #spells
FROM ep
GROUP BY [der_spell_id];


