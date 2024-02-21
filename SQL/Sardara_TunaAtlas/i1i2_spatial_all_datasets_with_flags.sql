-- View: public.i1i2_spatial_all_datasets_with_fishingfleets

-- DROP MATERIALIZED VIEW public.i1i2_spatial_all_datasets_with_fishingfleets;

CREATE MATERIALIZED VIEW public.i1i2_spatial_all_datasets_with_fishingfleets
TABLESPACE pg_default
AS

 SELECT metadata.identifier AS dataset,
    source_labels.codesource_source AS ocean,
    "time".year,
    geargroup_label.codesource_gear AS gear_group,
    fishingfleet_labels.source_label AS fishingfleet,
    species_labels.codesource_species AS species,
    sum(tab.value) AS value,
    unit_labels.codesource_unit AS unit,
    st_area(area_labels.geom) AS area,
    area_labels.geom
   FROM metadata.metadata,
    fact_tables.catch tab
     LEFT JOIN unit.unit_labels USING (id_unit)
     LEFT JOIN source.source_labels USING (id_source)
     LEFT JOIN gear.gear_labels USING (id_gear)
     LEFT JOIN gear.gear_mapping ON gear_mapping.gear_mapping_id_from = tab.id_gear
     LEFT JOIN gear.gear_labels geargroup_label ON geargroup_label.id_gear = gear_mapping.gear_mapping_id_to
     LEFT JOIN fishingfleet.fishingfleet_labels USING (id_fishingfleet)
     LEFT JOIN area.area USING (id_area)
     LEFT JOIN area.area_labels USING (id_area)
     LEFT JOIN "time"."time" USING (id_time)
     LEFT JOIN species.species_labels USING (id_species)
     LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
     LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to
  WHERE 
  (metadata.identifier = ANY (ARRAY['global_nominal_catch_firms_level0'::text, 'global_catch_firms_level0'::text, 'global_catch_5deg_1m_firms_level0'::text, 'global_catch_1deg_1m_ps_bb_firms_level0'::text, 'global_catch_1deg_1m_ps_bb_ird_level1'::text, 'global_catch_1deg_1m_ps_bb_ird_level2'::text, 'global_catch_5deg_1m_ird_level1'::text, 'global_catch_5deg_1m_ird_level2'::text,  'global_catch_ird_level1'::text, 'global_catch_ird_level2'::text] )) 
  	AND metadata.id_metadata = tab.id_metadata
  GROUP BY metadata.identifier, source_labels.codesource_source, geargroup_label.codesource_gear,fishingfleet_labels.source_label, "time".year, species_labels.codesource_species, unit_labels.codesource_unit, area_labels.geom
 
WITH DATA;

ALTER TABLE public.i1i2_spatial_all_datasets_with_fishingfleets
    OWNER TO tunaatlas_u;

GRANT ALL ON TABLE public.i1i2_spatial_all_datasets_with_fishingfleets TO tunaatlas_u;
GRANT SELECT ON TABLE public.i1i2_spatial_all_datasets_with_fishingfleets TO tunaatlas_inv;
