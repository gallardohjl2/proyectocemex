
## Renombrar un campo
ALTER TABLE nombre_tabla
CHANGE COLUMN nombre_actual nombre_nuevo TIPO;


## Agregar dos nuevos campos

ALTER TABLE nombre_tabla
ADD COLUMN nuevo_campo1 TIPO,
ADD COLUMN nuevo_campo2 TIPO;


# SOLUCION

ALTER TABLE aditivos.BatchTable
CHANGE COLUMN OrdenFab Lote CHAR(12),
ADD COLUMN OrdenFab  INT NOT NULL DEFAULT 0,
ADD COLUMN CodigoSAP  INT NOT NULL DEFAULT 0,
ADD COLUMN SPare   INT NOT NULL DEFAULT 0;


# Cambiar densidad a 1 y establecer como default 1

SET SQL_SAFE_UPDATES = 0;

ALTER TABLE aditivos.TotalLab
ALTER COLUMN Densidad SET DEFAULT 1;

UPDATE  TotalLab
SET densidad = 1
where densidad is null;

SET SQL_SAFE_UPDATES = 1;