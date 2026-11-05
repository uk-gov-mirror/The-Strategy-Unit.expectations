SELECT *
FROM #spells s
   LEFT JOIN (
        SELECT site_code AS site_code1,
            site_name AS adm_site_name,
            trust_name AS adm_trust_name
        from Reporting_UKHD_ODS.Provider_Site
    ) sites1 
    ON s.admitting_site = sites1.site_code1
   LEFT JOIN (
        SELECT site_code AS site_code2,
            site_name AS thromb_site_name,
            trust_name AS thromb_trust_name
        from Reporting_UKHD_ODS.Provider_Site
    ) sites2 
    ON s.thromb_site = sites2.site_code2
ORDER BY nhs_no, admission_date, discharge_date, [der_spell_id];