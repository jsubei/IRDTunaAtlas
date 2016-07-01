--Global attributes:
--mdst.RDBMSSource="http://www.ecoscope.org/ontologies/resources/dbSardara"
--mdst.view="i1i2"
--mdst.description="Catches per species per ocean per year per fishing gear"
--mdst.creator="http://www.ecoscope.org/ontologies/agents/norbertBillet"
--mdst.confidentiality="public"
--Variables attributes:
--mdst.criteria.title.ocean="observation location"
--mdst.criteria.definition.ocean="location of the observation"
--mdst.criteria.label.ocean="ocean"
--mdst.criteria.datatype.ocean="geometry"
--mdst.criteria.status.ocean="PUBLIC"
--mdst.criteria.title.year="observation date"
--mdst.criteria.definition.year="date of the observation... "
--mdst.criteria.label.year="year"
--mdst.criteria.datatype.year="timestamp"
--mdst.criteria.status.year="PUBLIC"
--mdst.criteria.title.gear_type="Fishing gear"
--mdst.criteria.definition.gear_type="Type of gear used to catch... "
--mdst.criteria.label.gear_type="gear_type"
--mdst.criteria.datatype.gear_type="FAO codelist"
--mdst.criteria.status.gear_type="PUBLIC"
--mdst.criteria.title.species="Name of species"
--mdst.criteria.definition.species="Scientific name of species based on Worms "
--mdst.criteria.label.species="species"
--mdst.criteria.datatype.species="Worms codelist"
--mdst.criteria.status.species="PUBLIC"

SELECT sum(capture_rf1.v_capt_rf1) AS value, ocean.lc_ocean AS ocean, temps.an AS year, g_engin.lc_g_engin AS gear_type, espece.lc_esp AS species
   FROM capture_rf1
   JOIN espece USING (c_esp)
   JOIN ocean USING (c_ocean)
   JOIN temps USING (id_date)
   JOIN g_engin USING (c_g_engin)
  GROUP BY ocean.lc_ocean, temps.an, g_engin.lc_g_engin, espece.lc_esp;
