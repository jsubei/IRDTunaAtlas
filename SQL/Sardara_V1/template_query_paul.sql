WITH code_esp as (SELECT id_species,english_name_species,codesource_species FROM species.species_labels WHERE species_labels.codesource_species IN ('YFT','BET','SKJ'))
SELECT year, codesource_species,code_esp.english_name_species AS species,codesource_flag,english_name_flag AS flag,english_name_ocean AS ocean,codesource_area as cwp_code,SUM(v_catch_rf1) AS catch
FROM tunaatlas.catches
INNER JOIN code_esp ON (code_esp.id_species=catches.id_species_standard)
LEFT OUTER JOIN time.time USING (id_time)
LEFT OUTER JOIN area.rfmos_convention_areas_fao ON (rfmos_convention_areas_fao.id_origin_institution=catches.id_ocean)
LEFT OUTER JOIN area.areas_with_geom USING (id_area)
LEFT OUTER JOIN flag.flag_labels ON (flag_labels.id_flag=catches.id_flag_standard)
WHERE
time_start >= '1950-01-01' AND time_end<'2014-01-01'
AND
ST_Within(
areas_with_geom.geom,
public.ST_GeomFromText('POLYGON((-160.83984375 -16.096484835514698,-92.98828125 -16.096484835514698,-92.98828125 -66.28807090570483,-160.83984375 -66.28807090570483,-160.83984375 -16.096484835514698))'::text,4326)
)
AND
codesource_flag IN ('FRA','ESP','JPN')
AND
id_catchunit IN (1,3)
GROUP BY year,codesource_species,code_esp.english_name_species,ocean,codesource_area,codesource_flag,english_name_flag ORDER BY year,catch  desc

