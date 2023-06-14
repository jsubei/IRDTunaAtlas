--SELECT 
--row_number() OVER () AS ogc_fid,
--wkt 
--FROM (
--SELECT
--	 distinct st_astext(geom)  AS wkt
--FROM (

	SELECT 
		row_number() OVER () AS ogc_fid,
		fact.id_area AS geom_id,
		area_labels.geom AS geom,
--		st_astext(area_labels.geom) AS geom_wkt,
		"time".year AS year,
--		fact.id_species  AS species,
		species_labels.codesource_species AS species,
		fishingfleet_labels.codesource_fishingfleet AS country,
		sum(fact.value) AS value,
		count(fact.value) AS count 
		
	FROM fact_tables.catch fact
	
		LEFT JOIN "time"."time" USING (id_time)
		
		LEFT JOIN area.area USING (id_area)	
		LEFT JOIN area.area_labels USING (id_area)
		
		LEFT JOIN species.species_labels USING (id_species)
		LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = fact.id_species
		LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to 
		
		LEFT JOIN fishingfleet.fishingfleet_labels  USING (id_fishingfleet)
		LEFT JOIN fishingfleet.fishingfleet_mapping ON fishingfleet_mapping.fishingfleet_mapping_id_from = fact.id_fishingfleet
		LEFT JOIN fishingfleet.fishingfleet_labels fishingfleetgroup_label ON fishingfleetgroup_label.id_fishingfleet = fishingfleet_mapping.fishingfleet_mapping_id_to 

        WHERE 
        	area.area_labels.tablesource_area = 'cwp_grid' 
        	AND 
        	substring(area.area_labels.codesource_area from 1 for 1) ='6' 
        	AND 
        	st_area(area.area_labels.geom)=25
--      	AND 
--      	"time".year <= 2005::numeric 
        	
	GROUP BY 
		fact.id_area, geom, "time".year, species_labels.codesource_species, fishingfleet_labels.codesource_fishingfleet
		-- fact.id_area, geom_wkt, "time".year, species_labels.codesource_species, fact.id_species, fact.id_fishingfleet
		
		
--) subqry
	
--) AS foo 	
