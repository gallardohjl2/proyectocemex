🟧 1. Archivo .bat con logs avanzados

Guárdalo como, por ejemplo:

C:\scripts_respaldo\backup-aditivos-log.bat

y pega esto tal cual:

``` sql
@echo off
setlocal ENABLEDELAYEDEXPANSION

rem ===== CONFIGURACIÓN =====
set "DBNAME=northwind"
set "BACKUPDIR=C:\mysql_backups"
set "MYSQLBIN=C:\Program Files\MySQL\MySQL Server 8.0\bin"
set "LOGDIR=%BACKUPDIR%\logs"

rem Crear carpetas de respaldo y logs si no existen
mkdir "%BACKUPDIR%" 2>nul
mkdir "%LOGDIR%" 2>nul

rem ===== GENERAR TIMESTAMP yyyy-MM-dd_HHmmss =====
set "DATESTR=%DATE%"
set "TIMESTR=%TIME%"

rem Ojo: esto asume formato de fecha dd/mm/aaaa
set "YYYY=%DATESTR:~-4%"
set "MM=%DATESTR:~3,2%"
set "DD=%DATESTR:~0,2%"

set "HH=%TIMESTR:~0,2%"
set "NN=%TIMESTR:~3,2%"
set "SS=%TIMESTR:~6,2%"

rem Si la hora viene con espacio (ej. " 9"), se corrige a "09"
if "%HH:~0,1%"==" " set "HH=0%HH:~1,1%"

set "STAMP=%YYYY%-%MM%-%DD%_%HH%%NN%%SS%"

set "BACKUPFILE=%BACKUPDIR%\%DBNAME%_backup_%STAMP%.sql"
set "LOGFILE=%LOGDIR%\%DBNAME%_backup_%STAMP%.log"

echo ============================================ 
echo   Iniciando respaldo de la base: %DBNAME%
echo   Fecha/Hora: %DATE% %TIME%
echo ============================================ 

echo [%DATE% %TIME%] Iniciando respaldo de base %DBNAME% > "%LOGFILE%"
echo Archivo destino: %BACKUPFILE% >> "%LOGFILE%"

rem ===== VALIDAR QUE EXISTA MYSQLDUMP =====
if not exist "%MYSQLBIN%\mysqldump.exe" (
    echo [%DATE% %TIME%] ERROR: No se encontro mysqldump en "%MYSQLBIN%" >> "%LOGFILE%"
    echo ERROR: No se encontro mysqldump en "%MYSQLBIN%"
    echo Revisa la ruta MYSQLBIN dentro del .bat
    goto :FIN
)

rem ===== EJECUTAR RESPALDO SOLO DE ESA BASE =====
echo Ejecutando mysqldump... >> "%LOGFILE%"

"%MYSQLBIN%\mysqldump.exe" --login-path=backup %DBNAME% ^
  --single-transaction --quick --skip-lock-tables ^
  --routines --triggers --events ^
  --hex-blob --no-tablespaces --column-statistics=0 ^
  --skip-add-drop-database --skip-add-drop-table ^
  --set-gtid-purged=OFF ^
  --result-file="%BACKUPFILE%" >> "%LOGFILE%" 2>&1

if errorlevel 1 (
    echo [%DATE% %TIME%] ERROR: mysqldump devolvio codigo %ERRORLEVEL% >> "%LOGFILE%"
    echo ERROR en el respaldo, revisa el log:
    echo   %LOGFILE%
    goto :FIN
)

rem ===== VALIDAR QUE EL ARCHIVO EXISTA =====
if not exist "%BACKUPFILE%" (
    echo [%DATE% %TIME%] ERROR: Archivo de respaldo no fue creado. >> "%LOGFILE%"
    echo ERROR: El archivo de respaldo no fue creado.
    goto :FIN
)

rem ===== VALIDAR QUE EL ARCHIVO NO ESTE VACIO =====
for %%A in ("%BACKUPFILE%") do set "SIZE=%%~zA"

if "!SIZE!"=="0" (
    echo [%DATE% %TIME%] ERROR: Archivo de respaldo vacio. >> "%LOGFILE%"
    echo ERROR: El archivo de respaldo esta vacio.
    goto :FIN
)

echo [%DATE% %TIME%] Respaldo completado correctamente. Tamano: !SIZE! bytes >> "%LOGFILE%"

echo.
echo ============================================
echo   Respaldo completado correctamente
echo   Archivo: %BACKUPFILE%
echo   Log    : %LOGFILE%
echo   Tamano : !SIZE! bytes
echo ============================================

:FIN
echo.
pause
endlocal
```

🟧 2. ¿Qué hace este .bat exactamente?

Configura:

Base: DBNAME=aditivos

Carpeta de respaldo: C:\mysql_backups

Binarios MySQL: C:\Program Files\MySQL\MySQL Server 8.0\bin

Carpeta de logs: C:\mysql_backups\logs

Genera un timestamp tipo:

``` 
2025-11-13_213045

```
y crea archivos como:


- C:\mysql_backups\aditivos_backup_2025-11-13_213045.sql

- C:\mysql_backups\logs\aditivos_backup_2025-11-13_213045.log


Ejecuta mysqldump con:

- --single-transaction --quick --skip-lock-tables → respaldo consistente en InnoDB sin bloquear y apto para millones de filas.
- --routines --triggers --events → incluye lógica de BD.
- --skip-add-drop-database --skip-add-drop-table → no borra tablas al restaurar.
- --result-file=... → escribe directo al archivo (no se corta por consola).

Escribe logs de:

- Inicio

- Errores de ruta

- Código de salida de mysqldump

- Validación de tamaño del archivo

- Mensaje de éxito + tamaño

Detiene el script si:

- No encuentra mysqldump.exe
- mysqldump falla
- El archivo no existe
- El archivo está vacío

🟧 3.  Requisito: login-path

Como usa --login-path=backup, antes debes haber hecho (una sola vez):

``` cmd
"C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql_config_editor.exe" set --login-path=backup --host=localhost --user=root --password
``` 


🟧 4. Restaurar el respaldo

En CMD:

```
"C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe" --login-path=backup aditivos < "C:\mysql_backups\aditivos_backup_2025-11-13_213045.sql"
```