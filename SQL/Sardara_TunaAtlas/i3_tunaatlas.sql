SELECT 
	sub_qry3.year, 
	sub_qry3.month, 
	sub_qry3.c_esp, 
	sub_qry3.c_g_engin, 		
--species_labels.codesource_species AS species 
--geargroup_label.gear_labels AS gear_type,  
	sub_qry3.value, 
	sub_qry2.mean_prev_5_years, 
	sub_qry2.stddev_prev_5_years 

FROM (
	SELECT 
		"time".year AS year,
		"time".month AS month, 
		species_labels.codesource_species AS c_esp, 
		geargroup_label.codesource_gear AS c_g_engin, 
		sum(tab.value) AS value 
	   		
		
	FROM fact_tables.catch tab
		LEFT JOIN gear.gear_labels USING (id_gear)
		LEFT JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from = tab.id_gear
		LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear = gear_mapping.gear_mapping_id_to
		LEFT JOIN "time"."time" USING (id_time)
		LEFT JOIN species.species_labels USING (id_species)
		LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
		LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to	


	WHERE "time".year <= 2005::numeric 

	GROUP BY "time".year, "time".month, species_labels.codesource_species, geargroup_label.codesource_gear
	
	
	) sub_qry3 NATURAL JOIN (
				SELECT 
					subsub.reference_year AS year, 
					subsub.month, 
					subsub.c_esp, 
					subsub.c_g_engin, 
					avg(subsub.sum) AS mean_prev_5_years, 
					stddev_samp(subsub.sum) AS stddev_prev_5_years 
				FROM (

				SELECT 
					sub_qry.reference_year, 
					"time".year AS sub_year, 
					"time".month AS month, 
					species_labels.codesource_species AS c_esp, 
					geargroup_label.codesource_gear AS c_g_engin, 
					sum(tab.value) AS sum 
					
				FROM fact_tables.catch tab
--						LEFT JOIN unit.unit_labels USING (id_unit)
					LEFT JOIN gear.gear_labels USING (id_gear)
					LEFT JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from = tab.id_gear
					LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear = gear_mapping.gear_mapping_id_to
					LEFT JOIN "time"."time" USING (id_time)
					LEFT JOIN species.species_labels USING (id_species)
					LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
					LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to	



				JOIN ( 
					SELECT
						DISTINCT "time".year AS reference_year 
						
					FROM "time"."time"
					
					WHERE 
						"time".year > (((SELECT min("time".year) AS min FROM "time"."time")) + 5::numeric) 
						AND 
						"time".year <= 2005::numeric
						
						) sub_qry ON "time".year < sub_qry.reference_year AND "time".year >= (sub_qry.reference_year - 5::numeric)
						
					GROUP BY sub_qry.reference_year, "time".year, "time".month, species_labels.codesource_species, geargroup_label.codesource_gear 

				) subsub 

				GROUP BY subsub.reference_year, subsub.month, subsub.c_esp, subsub.c_g_engin 
          
          ) sub_qry2 
          
	
--	LEFT JOIN species.species_labels USING (id_species)
--	LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
--	LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to
--	LEFT JOIN gear.gear_labels USING (id_gear)
--	LEFT JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from = tab.id_gear
--	LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear = gear_mapping.gear_mapping_id_to
   
   LIMIT 10 ;

