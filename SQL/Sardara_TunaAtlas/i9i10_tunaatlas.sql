 SELECT
	species_labels.codesource_species AS species,
	"time".year AS year,
	schooltype.schooltype AS school,
		 CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t
		    ELSE mensur_rf1.v_class_t - 1::numeric
		END AS class_low, 
		CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t + 2::numeric
		    ELSE mensur_rf1.v_class_t + 1::numeric
		END AS class_up,
	sum(mensur_rf1.v_mensur_rf1) AS fish_count,
	count(mensur_rf1.v_mensur_rf1) AS data_count
        
	FROM mensur_rf1
		JOIN espece USING (c_esp)
		JOIN temps USING (id_date)
		JOIN banc USING (c_banc)
		
	WHERE 
		mensur_rf1.c_type_mens = 1::numeric 
		AND 
		temps.an <= 2005::numeric 
	
	GROUP BY 
		espece.lc_esp,
		temps.an,
		banc.lc_banc,
		CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t
		    ELSE mensur_rf1.v_class_t - 1::numeric
		END, 
		CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t + 2::numeric
		    ELSE mensur_rf1.v_class_t + 1::numeric
		END
		  ORDER BY espece.lc_esp, temps.an, banc.lc_banc, 
		CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t
		    ELSE mensur_rf1.v_class_t - 1::numeric
		END, 
		CASE
		    WHEN (mensur_rf1.v_class_t % 2::numeric) = 0::numeric THEN mensur_rf1.v_class_t + 2::numeric
		    ELSE mensur_rf1.v_class_t + 1::numeric
		END;
