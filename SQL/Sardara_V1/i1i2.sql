SELECT sum(capture_rf1.v_capt_rf1) AS value, ocean.lc_ocean AS ocean, temps.an AS year, g_engin.lc_g_engin AS gear_type, espece.lc_esp AS species
   FROM capture_rf1
   JOIN espece USING (c_esp)
   JOIN ocean USING (c_ocean)
   JOIN temps USING (id_date)
   JOIN g_engin USING (c_g_engin)
  GROUP BY ocean.lc_ocean, temps.an, g_engin.lc_g_engin, espece.lc_esp;
