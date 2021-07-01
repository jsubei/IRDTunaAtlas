 SELECT sum(capture_rf1.v_capt_rf1) AS value,
 	ocean.lc_ocean AS ocean,
	temps.an AS year,
	g_engin.lc_g_engin AS gear_type,
	espece.lc_esp AS species
   FROM capture_rf1
   JOIN espece USING (c_esp)
   JOIN ocean USING (c_ocean)
   JOIN temps USING (id_date)
   JOIN g_engin USING (c_g_engin)
  WHERE temps.an <= 2005::numeric
  GROUP BY ocean.lc_ocean, temps.an, g_engin.lc_g_engin, espece.lc_esp;


 SELECT 
 
 	sum(tab.value) AS value,
-- 	tab.value AS value,
	unit_labels.codesource_unit AS unit,
 	source_labels.codesource_source AS ocean,
--	gear_labels.codesource_gear AS gear,
	geargroup_label.codesource_gear AS gear_group,
--	tab.id_area,
--	st_astext(area_labels.geom) AS geom_wkt,
	"time".year AS year,
	species_labels.codesource_species AS species 
--	speciesgroup_label.codesource_species AS species
	
   FROM fact_tables.catch tab
	LEFT JOIN unit.unit_labels USING (id_unit)
	LEFT JOIN source.source_labels USING (id_source)
	LEFT JOIN gear.gear_labels USING (id_gear)
	LEFT JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from = tab.id_gear
	LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear = gear_mapping.gear_mapping_id_to
	LEFT JOIN area.area USING (id_area)	
	LEFT JOIN area.area_labels USING (id_area)
	LEFT JOIN "time"."time" USING (id_time)
	LEFT JOIN species.species_labels USING (id_species)
	LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
	LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to


  WHERE 
	"time".year <= 2005::numeric 
	AND 
	unit_labels.codesource_unit= 't'
	AND 
	(gear_mapping.id_metadata = (( SELECT metadata.id_metadata
           FROM metadata.metadata
          WHERE metadata.identifier = 'codelist_mapping_isscfg_revision_1_geargroup_tunaatlas'::text)) OR gear_mapping.gear_mapping_id_from = 0) AND (species_mapping.id_metadata = (( SELECT metadata.id_metadata
           FROM metadata.metadata
          WHERE metadata.identifier = 'codelist_mapping_species_asfis_speciesgroup_tunaatlas'::text)) OR species_mapping.species_mapping_id_from = 0) AND tab.id_metadata = 199 
  
  GROUP BY 
  
	unit_labels.codesource_unit,
	source_labels.codesource_source,
	geargroup_label.codesource_gear,
--	st_astext(area_labels.geom),
	"time".year,
	species_labels.codesource_species  
	
--  LIMIT 10 ;
