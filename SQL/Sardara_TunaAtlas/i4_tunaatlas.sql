-- SELECT 
-- 
--	espece.lc_esp AS species,
--	ocean.lc_ocean AS ocean, 
--	temps.an AS year, 
--	temps.n_mois AS month, 
--	sum(capture_rf1.v_capt_rf1) AS value 

--   FROM capture_rf1 
--	JOIN espece USING (c_esp)
--	JOIN ocean USING (c_ocean)
--	JOIN temps USING (id_date)
   
--  WHERE temps.an <= 2005::numeric
--  GROUP BY espece.lc_esp, ocean.lc_ocean, temps.an, temps.n_mois;

 SELECT 
	species_labels.codesource_species AS species, 
--	speciesgroup_label.codesource_species AS species,
 	source_labels.codesource_source AS ocean,
--	tab.id_area,
--	st_astext(area_labels.geom) AS geom_wkt,
	"time".year AS year,
	"time".month AS month,
 	sum(tab.value) AS value,
	unit_labels.codesource_unit AS unit
	
   FROM fact_tables.catch tab
	LEFT JOIN unit.unit_labels USING (id_unit)
	LEFT JOIN source.source_labels USING (id_source)
--	LEFT JOIN area.area USING (id_area)	
--	LEFT JOIN area.area_labels USING (id_area)
	LEFT JOIN "time"."time" USING (id_time)
	LEFT JOIN species.species_labels USING (id_species)
	LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
	LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to

  WHERE 
	"time".year <= 2005::numeric 
	AND unit_labels.codesource_unit= 't'
  	AND (species_mapping.id_metadata = (( SELECT metadata.id_metadata FROM metadata.metadata WHERE metadata.identifier = 'codelist_mapping_species_asfis_speciesgroup_tunaatlas'::text)) 
	OR species_mapping.species_mapping_id_from = 0) 
	AND tab.id_metadata = 199 
  
  GROUP BY 
  
	unit_labels.codesource_unit,
	source_labels.codesource_source,
--	st_astext(area_labels.geom),
	"time".year,
	"time".month,
	species_labels.codesource_species  
	
ORDER BY species_labels.codesource_species,source_labels.codesource_source,"time".year, "time".month DESC
	
--LIMIT 1000;
