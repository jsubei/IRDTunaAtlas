-- Catches for a given year per species per month per fishing gear plus mean of same indicator in the 5 previous years 

SELECT sub_qry3.year, sub_qry3.month, espece.lc_esp AS species, g_engin.lc_g_engin AS gear_type, sub_qry3.value, sub_qry2.mean_prev_5_years, sub_qry2.stddev_prev_5_years, sub_qry3.geom
   FROM ( SELECT temps.an AS year, temps.n_mois AS month, espece.c_esp, g_engin.c_g_engin, sum(capture_rf1.v_capt_rf1) AS value, st_setsrid(st_envelope(st_collect(carre.g_carre)), 4030) AS geom
           FROM capture_rf1
      JOIN temps USING (id_date)
   JOIN espece USING (c_esp)
   JOIN g_engin USING (c_g_engin)
   JOIN carre USING (id_carre)
  GROUP BY temps.an, temps.n_mois, espece.c_esp, g_engin.c_g_engin) sub_qry3
NATURAL JOIN ( SELECT subsub.reference_year AS year, subsub.month, subsub.c_esp, subsub.c_g_engin, avg(subsub.sum) AS mean_prev_5_years, stddev_samp(subsub.sum) AS stddev_prev_5_years
           FROM ( SELECT sub_qry.reference_year, temps.an AS sub_year, temps.n_mois AS month, espece.c_esp, g_engin.c_g_engin, sum(capture_rf1.v_capt_rf1) AS sum
                   FROM capture_rf1
              JOIN temps USING (id_date)
         JOIN espece USING (c_esp)
    JOIN g_engin USING (c_g_engin)
   JOIN ( SELECT DISTINCT temps.an AS reference_year
            FROM temps
           WHERE temps.an > ((( SELECT min(temps.an) AS min
                    FROM temps)) + 5::numeric)) sub_qry ON temps.an < sub_qry.reference_year AND temps.an >= (sub_qry.reference_year - 5::numeric)
  GROUP BY sub_qry.reference_year, temps.an, temps.n_mois, espece.c_esp, g_engin.c_g_engin) subsub
          GROUP BY subsub.reference_year, subsub.month, subsub.c_esp, subsub.c_g_engin) sub_qry2
   JOIN espece USING (c_esp)
   JOIN g_engin USING (c_g_engin);
