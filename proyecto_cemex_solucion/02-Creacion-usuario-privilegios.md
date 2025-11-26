## Conéctate como administrador

-- Desde consola:
mysql -u root -p

## Crea el usuario

- Cambia usuario/host/contraseña según tu entorno.

```sql
CREATE USER 'lector_aditivos'@'%' 
  IDENTIFIED BY 'ContraseñaSegura!2025'
  PASSWORD EXPIRE NEVER
  ACCOUNT UNLOCK;
  ```

## Concede solo lectura en dos tablas
Ejemplo con base aditivos y tablas BatchTable y TotalLab:

```sql
GRANT SELECT ON aditivos.BatchTable TO 'lector_aditivos'@'%';
GRANT SELECT ON aditivos.Muestras    TO 'lector_aditivos'@'%';
 ```

Con esto el usuario solo puede ejecutar SELECT en esas tablas.
DESCRIBE también queda permitido (está cubierto por SELECT).

### (Opcional) Si necesitas que pueda ver definiciones de VISTAS

 ```sql
GRANT SHOW VIEW ON aditivos.* TO 'lector_aditivos'@'%';
 ```
Aplica cambios

(MySQL 8 los aplica al instante; FLUSH PRIVILEGES no es necesario, pero no hace daño.)

```sql

FLUSH PRIVILEGES;

```

### Prueba

-- Debe funcionar:
SELECT COUNT(*) FROM aditivos.BatchTable;

-- Debe FALLAR (solo lectura):
INSERT INTO aditivos.BatchTable(...) VALUES (...);
UPDATE aditivos.Muestras SET ...;
DELETE FROM aditivos.Muestras WHERE ...;


### Variantes útiles

Cambiar la contraseña más tarde

ALTER USER 'lector_aditivos'@'%' IDENTIFIED BY 'NuevaContraseña!2025';

Revocar permisos

REVOKE SELECT ON aditivos.BatchTable FROM 'lector_aditivos'@'%';
REVOKE SELECT ON aditivos.Muestras    FROM 'lector_aditivos'@'%';

### Eliminar el usuario

DROP USER 'lector_aditivos'@'%';

### Dar lectura a todas las tablas de un esquema (si algún día lo necesitas)

GRANT SELECT ON aditivos.* TO 'lector_aditivos'@'%';




