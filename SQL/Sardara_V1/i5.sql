SELECT espece.lc_esp AS species, temps.an AS year, t_carre.l_t_carre AS square_type, count(DISTINCT capture_rf1.id_carre) AS count
   FROM capture_rf1
   JOIN espece USING (c_esp)
   JOIN temps USING (id_date)
   JOIN t_carre USING (c_t_carre)
  GROUP BY espece.lc_esp, temps.an, t_carre.l_t_carre;
