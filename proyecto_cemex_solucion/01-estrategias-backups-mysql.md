# Estrategias de Backups en Mysql

## imagen MySQL + XtraBackup

```sql
docker run -d --name mysql-percona \
  -e MYSQL_ROOT_PASSWORD=tu_pass \
  -v mysql_data:/var/lib/mysql \
  percona/percona-server
```

## Cómo hacer un backup binario en Docker con XtraBackup
```sql
docker run --rm -v mysql_data:/var/lib/mysql -v $(pwd)/backups:/backup \
  percona/percona-xtrabackup \
  xtrabackup --backup --target-dir=/backup/full
```

  ## Restaurar
```
docker stop mysql-percona

docker run --rm -v mysql_data:/var/lib/mysql -v $(pwd)/backups:/backup \
  percona/percona-xtrabackup \
  xtrabackup --copy-back --target-dir=/backup/full

docker start mysql-percona
```

## Utilizando mysqlpumb dentro del contenedor

### Para respaldar una base completa:

docker exec my-mysql \
  mysqlpump -u root -p --databases aditivos > aditivos_backup.sql


### Para respaldar todas las bases:

docker exec my-mysql \
  mysqlpump -u root -p --all-databases > backup_full.sql


### Para no pedir contraseña: (si quieres simplificar)

docker exec my-mysql \
  mysqlpump -u root -pTU_PASSWORD --all-databases > backup_full.sql


### Guardar el backup en tu equipo Windows (no dentro del contenedor)

En vez de guardar dentro del contenedor, redirige la salida a una carpeta Windows:

docker exec my-mysql \  
  mysqlpump -u root -pTU_PASSWORD --all-databases > C:\mysql_backups\respaldo_2025_11_10.sql

### Restaurar un backup hecho con mysqlpump

docker exec -i my-mysql \
  mysql -u root -pTU_PASSWORD < C:\mysql_backups\respaldo_2025_11_10.sql

docker exec -i nombre_del_contenedor \
  mysql -u root -p < /backup/respaldo.sql


### RESPALDO CON mysqlpump en Windows

Respaldo de todas las bases de datos

Abre Símbolo del Sistema o PowerShell.

Ejecuta:

mysqlpump -u root -p --all-databases > C:\backups\backup_full.sql

## Respaldo de una base en específico

mysqlpump -u root -p --databases aditivos > C:\backups\aditivos_backup.sql


## RESTAURACIÓN CON mysql

### Restaurar todas las bases desde el respaldo

mysql -u root -p < C:\backups\backup_full.sql

### Restaurar solo una base

mysql -u root -p aditivos < C:\backups\aditivos_backup.sql


### Plantilla recomendada (grande, InnoDB, Windows)

Respaldo completo paralelo y comprimido:

mysqlpump -u root -p `
  --all-databases `
  --single-transaction `
  --lock-tables=FALSE `
  --events --routines --triggers `
  --default-parallelism=6 `
  --compress-output=GZIP `
  > C:\backups\full_$(Get-Date -Format yyyy-MM-dd_HH-mm).sql.gz


### Con dump 

====================================

docker exec mysql_8_0_39 sh -c "MYSQL_PWD='admin1234' mysqldump -uroot \
  --databases aditivos \
  --single-transaction --quick --skip-lock-tables \
  --routines --triggers --events \
  --hex-blob --no-tablespaces --column-statistics=0 \
  --skip-add-drop-database --skip-add-drop-table \
  --set-gtid-purged=OFF" \
| gzip > /c/mysql_backups/mysql_docker_full_$(date +'%Y-%m-%d_%H-%M').sql.gz


-- Restaurar

gzip -cd /c/mysql_backups/mysql_docker_full_2025-11-12_22-45.sql.gz \
| docker exec -i mysql_8_0_39 mysql -uroot -padmin1234



- PowerShell





