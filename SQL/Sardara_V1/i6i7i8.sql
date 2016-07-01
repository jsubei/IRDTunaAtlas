---Catches per species per 5 degrees square

SELECT st_transform(subqry.geom, 4030)::geometry(Polygon,4030) AS geom, subqry.geom_id, subqry.year, espece.lc_esp AS species, sum(subqry.value) AS value, sum(subqry.count) AS count
   FROM (         SELECT carre.g_carre5 AS geom, carre.c_cwp5 AS geom_id, temps.an AS year, capture_rf1.c_esp, sum(capture_rf1.v_capt_rf1) AS value, count(capture_rf1.v_capt_rf1) AS count
                   FROM capture_rf1
              JOIN temps USING (id_date)
         JOIN carre USING (id_carre)
        WHERE carre.c_t_carre = 5::numeric
        GROUP BY carre.g_carre5, carre.c_cwp5, temps.an, capture_rf1.c_esp
        UNION 
                 SELECT carre.g_carre5 AS geom, carre.c_cwp5 AS geom_id, temps.an AS year, capture_rf1.c_esp, sum(capture_rf1.v_capt_rf1) AS value, count(capture_rf1.v_capt_rf1) AS count
                   FROM capture_rf1
              JOIN temps USING (id_date)
         JOIN carre USING (id_carre)
        WHERE carre.c_t_carre = 6::numeric
        GROUP BY carre.g_carre5, carre.c_cwp5, temps.an, capture_rf1.c_esp) subqry
   JOIN espece USING (c_esp)
  GROUP BY subqry.geom, subqry.geom_id, subqry.year, espece.lc_esp;
