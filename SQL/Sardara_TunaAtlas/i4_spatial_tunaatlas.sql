 SELECT species_labels.codesource_species AS species,
    source_labels.codesource_source AS ocean,
    "time".year,
    "time".month,
    sum(tab.value) AS value,
    unit_labels.codesource_unit AS unit,
    area_labels.geom
   FROM fact_tables.catch tab
     LEFT JOIN unit.unit_labels USING (id_unit)
     LEFT JOIN source.source_labels USING (id_source)
     LEFT JOIN "time"."time" USING (id_time)
     LEFT JOIN area.area USING (id_area)
     LEFT JOIN area.area_labels USING (id_area)
     LEFT JOIN species.species_labels USING (id_species)
     LEFT JOIN species.species_mapping ON species_mapping.species_mapping_id_from = tab.id_species
     LEFT JOIN species.species_labels speciesgroup_label ON speciesgroup_label.id_species = species_mapping.species_mapping_id_to
  WHERE "time".year::numeric <= 2005::numeric AND unit_labels.codesource_unit = 't'::text AND (species_mapping.id_metadata = (( SELECT metadata.id_metadata
           FROM metadata.metadata
          WHERE metadata.identifier = 'codelist_mapping_species_asfis_speciesgroup_tunaatlas'::text)) OR species_mapping.species_mapping_id_from = 0) AND tab.id_metadata = 199
  GROUP BY unit_labels.codesource_unit, source_labels.codesource_source, "time".year, "time".month, species_labels.codesource_species, area_labels.geom
  ORDER BY species_labels.codesource_species, source_labels.codesource_source, "time".year, "time".month DESC;
