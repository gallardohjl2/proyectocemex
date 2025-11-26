# Sistema de Aditivos – Actualización Delphi y Estrategia de Respaldo MySQL

## Descripción general

Este proyecto documenta las mejoras realizadas al **Sistema de Aditivos** desarrollado en **Delphi**, junto con la implementación de una **estrategia de respaldo y seguridad en MySQL**.  

Incluye:

- Ajustes y correcciones al sistema principal en Delphi.
- Automatización de respaldos de bases de datos MySQL mediante scripts **`.bat`**.
- Creación y administración de **usuarios y privilegios** en MySQL para mejorar la seguridad y la trazabilidad.

El objetivo es contar con un sistema más estable, mantenible y con procedimientos claros para la protección de la información.

---

## Alcance del proyecto

1. **Aplicación Delphi (Sistema de Aditivos)**
   - Corrección de errores en formularios y consultas.
   - Ajustes en la conexión a base de datos vía ODBC/ADO.
   - Mejoras en la experiencia de usuario (búsquedas, grids, validaciones).

2. **Respaldo de MySQL**
   - Creación de scripts `.bat` para generar respaldos completos de la(s) base(s) de datos.
   - Organización de respaldos por fecha en carpetas específicas.
   - Posibilidad de programar los respaldos mediante **Tareas Programadas de Windows**.

3. **Usuarios y privilegios en MySQL**
   - Definición de usuarios con permisos específicos (administración, solo lectura, operación).
   - Aplicación de buenas prácticas básicas de seguridad (contraseñas, privilegios mínimos necesarios).
   - Scripts SQL para creación y mantenimiento de usuarios.

---

## Requerimientos

### Software

- **Sistema Operativo:** Windows 10 o superior.
- **Delphi:** (Delphi 7 / XE / 10.x – ajustar según el entorno real).
- **MySQL Server:** versión 5.7 o superior (8.0 recomendado).
- **MySQL Client / MySQL Command Line Tools** (para `mysqldump`).
- **Conector ODBC para MySQL** (si el sistema Delphi lo utiliza).
- Editor de texto (Notepad++, VS Code, etc.) para edición de scripts `.bat` y `.sql`.

### Accesos

- Usuario con permisos administrativos en MySQL para:
  - Crear usuarios.
  - Otorgar y revocar privilegios.
  - Ejecutar `mysqldump` para respaldos.

---

## Estructura del proyecto

```text
/Proyecto-Aditivos
│
├─ /src-delphi
│   ├─ Forms
│   ├─ DataModules
│   └─ Units
│
├─ /db
│   ├─ scripts-usuarios
│   │   ├─ 01_create_users.sql
│   │   └─ 02_grant_privileges.sql
│   └─ scripts-estructura
│       └─ estructura_inicial.sql
│
├─ /backups
│   ├─ scripts
│   │   └─ backup-aditivos.bat
│   └─ respaldos
│       └─ (se generan carpetas por fecha)
│
└─ README.md
