
1️⃣ Script PowerShell: Backup-MySQL-Full.ps1

1. Abre Bloc de notas.
2. Copia esto tal cual:


``` sql

# Backup-MySQL-Full.ps1
# Respaldo completo de todas las bases, pensado para millones de registros

param(
    [string]$MySQLBin    = "C:\Program Files\MySQL\MySQL Server 8.0\bin",
    [string]$BackupDir   = "C:\mysql_backups",
    [string]$LoginPath   = "backup"   # login-path configurado con mysql_config_editor
)

$ErrorActionPreference = "Stop"

# Marcar fecha/hora para el archivo
$timestamp  = Get-Date -Format "yyyy-MM-dd_HH-mm-ss"
$backupFile = Join-Path $BackupDir "mysql_full_$timestamp.sql"
$logFile    = Join-Path $BackupDir "mysql_full_$timestamp.log"

# Crear carpeta si no existe
New-Item -ItemType Directory -Force -Path $BackupDir | Out-Null

# Rutas a binarios
$mysqldump = Join-Path $MySQLBin "mysqldump.exe"

if (!(Test-Path $mysqldump)) {
    Write-Host "❌ No se encontró mysqldump en: $mysqldump"
    exit 1
}

Write-Host "▶ Iniciando respaldo completo..."
Write-Host "  Archivo SQL : $backupFile"
Write-Host "  Log         : $logFile"
"[$(Get-Date)] Iniciando respaldo..." | Out-File -FilePath $logFile -Encoding UTF8

# Ejecutar mysqldump escribiendo DIRECTO al archivo (sin tuberías)
& $mysqldump `
  --login-path=$LoginPath `
  --all-databases `
  --single-transaction `
  --quick `
  --skip-lock-tables `
  --routines --triggers --events `
  --hex-blob `
  --no-tablespaces `
  --column-statistics=0 `
  --skip-add-drop-database `
  --skip-add-drop-table `
  --set-gtid-purged=OFF `
  --result-file="$backupFile" `
  2>> $logFile

if ($LASTEXITCODE -ne 0) {
    "[$(Get-Date)] ERROR: mysqldump terminó con código $LASTEXITCODE" | Out-File -FilePath $logFile -Append -Encoding UTF8
    Write-Host "❌ Error en el respaldo. Revisa el log: $logFile"
    exit $LASTEXITCODE
}

# Validación mínima: archivo existe y pesa más de 0 bytes
if (!(Test-Path $backupFile) -or ((Get-Item $backupFile).Length -le 0)) {
    "[$(Get-Date)] ERROR: Archivo de respaldo vacío o no creado." | Out-File -FilePath $logFile -Append -Encoding UTF8
    Write-Host "❌ El archivo de respaldo está vacío o no se creó."
    exit 2
}

"[$(Get-Date)] Respaldo finalizado correctamente." | Out-File -FilePath $logFile -Append -Encoding UTF8
Write-Host "✅ Respaldo completo generado:"
Write-Host "   $backupFile"

```

3. Guárdalo como, por ejemplo:
C:\scripts\Backup-MySQL-Full.ps1

2️⃣ Configurar el login-path (una sola vez)

Para no poner la contraseña en el script:

1. Abre CMD o PowerShell.
2. Ejecuta (ajusta user/host si hace falta):

``` powershell

&"C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql_config_editor.exe" set --login-path=backup --host=localhost --user=root --password
```


3️⃣ Cómo ejecutarlo

Abre PowerShell (normal):

``` powershell
C:\scripts\Backup-MySQL-Full.ps1
```

```
Al terminar verás algo como:

▶ Iniciando respaldo completo...
  Archivo SQL : C:\mysql_backups\mysql_full_2025-11-13_19-45-01.sql
  Log         : C:\mysql_backups\mysql_full_2025-11-13_19-45-01.log
✅ Respaldo completo generado:
   C:\mysql_backups\mysql_full_2025-11-13_19-45-01.sql

```

Si luego quieres comprimirlo:

```
gzip "C:\mysql_backups\mysql_full_2025-11-13_19-45-01.sql"
```

4️⃣ Cómo restaurar ese respaldo en Windows

Desde PowerShell:

```
"C:\Program Files\MySQL\MySQL Server 8.0\bin\mysql.exe" --login-path=backup < "C:\mysql_backups\mysql_full_2025-11-13_19-45-01.sql"
```





# Script para una sola Base de Datos

```
# Backup-MySQL-ONE.ps1
# Respaldo completo de UNA base de datos (ej. "aditivos")
# Sin bloqueo, robusto para millones de registros

param(
    [string]$MySQLBin  = "C:\Program Files\MySQL\MySQL Server 8.0\bin",
    [string]$BackupDir = "C:\mysql_backups",
    [string]$LoginPath = "backup",     # login-path configurado
    [string]$Database  = "aditivos"    # <-- BASE DE DATOS A RESPALDAR
)

$ErrorActionPreference = "Stop"

# Fecha/hora
$timestamp  = Get-Date -Format "yyyy-MM-dd_HH-mm-ss"
$backupFile = Join-Path $BackupDir "$Database`_backup_$timestamp.sql"
$logFile    = Join-Path $BackupDir "$Database`_backup_$timestamp.log"

# Crear carpeta
New-Item -ItemType Directory -Force -Path $BackupDir | Out-Null

# Ruta de mysqldump
$mysqldump = Join-Path $MySQLBin "mysqldump.exe"

if (!(Test-Path $mysqldump)) {
    Write-Host "❌ No se encontró mysqldump en: $mysqldump"
    exit 1
}

Write-Host "▶ Respaldando la base: $Database"
Write-Host "  Archivo SQL : $backupFile"
Write-Host "  Log         : $logFile"
"[$(Get-Date)] Iniciando respaldo..." | Out-File $logFile

# RESPALDO SOLO DE UNA BASE
& $mysqldump `
  --login-path=$LoginPath `
  $Database `
  --single-transaction `
  --quick `
  --skip-lock-tables `
  --routines --triggers --events `
  --hex-blob `
  --no-tablespaces `
  --column-statistics=0 `
  --skip-add-drop-database `
  --skip-add-drop-table `
  --set-gtid-purged=OFF `
  --result-file="$backupFile" `
  2>> $logFile

# Validación resultante
if (!(Test-Path $backupFile) -or ((Get-Item $backupFile).Length -le 0)) {
    "[$(Get-Date)] ERROR: Archivo de respaldo vacío o no creado." | Out-File -Append $logFile
    Write-Host "❌ El respaldo no se creó correctamente."
    exit 2
}

"[$(Get-Date)] Respaldo completado correctamente." | Out-File -Append $logFile
Write-Host "✅ Respaldo generado: $backupFile"

```