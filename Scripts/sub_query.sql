-- creamos una CTE o sub-query (consultas anidadas)

WITH agg AS 
(

SELECT 

z.geocodigo::double precision AS geocodigo,
c.nom_comuna,

-- % migrantes

ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0 / COUNT(*), 2
    ) AS ptje_migrantes,
    
-- % escolaridad

ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0 / COUNT(*), 2
    ) AS ptje_esc_mayor_16

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna
JOIN public.provincias AS pr ON pr.provincia_ref_id = c.provincia_ref_id
WHERE pr.nom_provincia = 'SAN ANTONIO'
GROUP BY z.geocodigo, c.nom_comuna

)

SELECT a.geocodigo,
    shp.geom,
    a.nom_comuna,
    a.ptje_migrantes,
    a.ptje_esc_mayor_16
FROM agg AS a
JOIN dpa.zonas_censales_v AS shp ON shp.geocodigo = a.geocodigo;