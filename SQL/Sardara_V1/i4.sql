SELECT espece.lc_esp AS species, ocean.lc_ocean AS ocean, temps.an AS year, temps.n_mois AS month, sum(capture_rf1.v_capt_rf1) AS value
   FROM capture_rf1
   JOIN espece USING (c_esp)
   JOIN ocean USING (c_ocean)
   JOIN temps USING (id_date)
  GROUP BY espece.lc_esp, ocean.lc_ocean, temps.an, temps.n_mois;
