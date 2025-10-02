-- Total de profesionales por zona censal

CREATE TABLE output.tasa_profesionales AS
SELECT z.geocodigo,
    c.nom_comuna, 
    COUNT (*) FILTER (WHERE p.p15 >= 12 AND p.p15 <=14) AS total_profesionales,
    ROUND(COUNT(*) FILTER (WHERE p.p15 >= 12 AND p.p15 <=14) * 100.0/ COUNT(*)FILTER (WHERE p.p09 > 18) ,2) AS tasa_profesionales
FROM personas p
JOIN hogares h ON h.hogar_ref_id = p.hogar_ref_id 
JOIN viviendas v ON h.vivienda_ref_id = v.vivienda_ref_id 
JOIN zonas z ON z.zonaloc_ref_id = v.zonaloc_ref_id
JOIN comunas c ON z.codigo_comuna = c.codigo_comuna 
GROUP BY z.geocodigo, c.nom_comuna
ORDER BY tasa_profesionales DESC;

-- Unir la geometría a la tabla de profesionales

SELECT shp.geocodigo, shp.geom, tp.nom_comuna, tp.tasa_profesionales
FROM output.tasa_profesionales AS tp
JOIN dpa.zonas_censales_v AS shp
ON shp.geocodigo = tp.geocodigo::double precision;