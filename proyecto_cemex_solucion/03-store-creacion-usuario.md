DELIMITER $$

DROP PROCEDURE IF EXISTS sp_create_readonly_user_for_tables $$
CREATE PROCEDURE sp_create_readonly_user_for_tables(
    IN p_user        VARCHAR(80),        -- e.g. 'lector_aditivos'
    IN p_host        VARCHAR(255),       -- e.g. '%', 'localhost' o '192.168.1.%'
    IN p_password    VARCHAR(255),       -- contraseña del usuario
    IN p_db          VARCHAR(64),        -- nombre del esquema/base
    IN p_tables_json JSON,               -- JSON array: '["BatchTable","Muestras"]'
    IN p_grant_show_view BOOLEAN         -- TRUE para conceder SHOW VIEW en el esquema
)
BEGIN
    DECLARE v_i INT DEFAULT 0;
    DECLARE v_n INT;

    -- Versiones escapadas de identificadores/cadenas
    DECLARE q_user TEXT;
    DECLARE q_host TEXT;
    DECLARE q_pass TEXT;
    DECLARE qi_db  TEXT;

    DECLARE v_tab_name VARCHAR(64);
    DECLARE qi_tab     TEXT;

    DECLARE v_sql TEXT;    -- string local; para PREPARE usaremos @sql

    -- Helpers de escape (cadenas entre comillas, identificadores entre backticks)
    SET q_user = CONCAT('\'', REPLACE(p_user,  '''',  ''''''), '\'');      -- 'user'
    SET q_host = CONCAT('\'', REPLACE(p_host,  '''',  ''''''), '\'');      -- 'host'
    SET q_pass = CONCAT('\'', REPLACE(p_password, '''',  ''''''), '\'');   -- 'pass'
    SET qi_db  = CONCAT('`', REPLACE(p_db, '`', '``'), '`');               -- `db`

    -- 1) Crear (si no existe) y asegurar contraseña
    SET v_sql = CONCAT('CREATE USER IF NOT EXISTS ', q_user, '@', q_host, ' IDENTIFIED BY ', q_pass);
    SET @sql := v_sql; PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

    SET v_sql = CONCAT('ALTER USER ', q_user, '@', q_host, ' IDENTIFIED BY ', q_pass);
    SET @sql := v_sql; PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

    -- 2) Conceder SELECT tabla por tabla (si el JSON es válido)
    SET v_n = JSON_LENGTH(p_tables_json);
    IF v_n IS NULL THEN
        SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'p_tables_json no es un arreglo JSON válido';
    END IF;

    WHILE v_i < v_n DO
        SET v_tab_name = JSON_UNQUOTE(JSON_EXTRACT(p_tables_json, CONCAT('$[', v_i, ']')));
        SET qi_tab = CONCAT('`', REPLACE(v_tab_name, '`', '``'), '`');

        SET v_sql = CONCAT('GRANT SELECT ON ', qi_db, '.', qi_tab, ' TO ', q_user, '@', q_host);
        SET @sql := v_sql; PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

        SET v_i = v_i + 1;
    END WHILE;

    -- 3) Opcional: SHOW VIEW a nivel esquema
    IF p_grant_show_view THEN
        SET v_sql = CONCAT('GRANT SHOW VIEW ON ', qi_db, '.* TO ', q_user, '@', q_host);
        SET @sql := v_sql; PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;
    END IF;

    -- (Opcional) forzar solo lectura estricta revocando grants previos
    -- SET v_sql = CONCAT('REVOKE ALL PRIVILEGES, GRANT OPTION FROM ', q_user, '@', q_host);
    -- SET @sql := v_sql; PREPARE stmt FROM @sql; EXECUTE stmt; DEALLOCATE PREPARE stmt;

    -- MySQL 8 aplica los GRANT/ALTER al instante; FLUSH no es necesario.
END $$
DELIMITER ;


-- Ejemplos de uso

-- 1) Conceder lectura en dos tablas específicas


CALL sp_create_readonly_user_for_tables(
  'lector_aditivos',        -- p_user
  '%',                      -- p_host
  'ContraFuerte!2025',      -- p_password
  'aditivos',               -- p_db
  JSON_ARRAY('BatchTable','Muestras'),  -- p_tables_json
  TRUE                      -- p_grant_show_view
);


-- Conceder lectura a tres tablas, sin SHOW VIEW

CALL sp_create_readonly_user_for_tables(
  'lector_reports',
  'localhost',
  'SoloLectura#1',
  'reporting',
  JSON_ARRAY('ventas_2025','clientes','agentes'),
  FALSE
);


Notas y buenas prácticas

Case-sensitivity: en Linux, respeta mayúsculas/minúsculas de las tablas (usa el nombre exacto).

JSON array: si pasas una tabla que no existe, el GRANT fallará para esa tabla; puedes manejarlo envolviendo cada EXECUTE en un DECLARE CONTINUE HANDLER … si quieres que continúe.

Revocar privilegios previos: si el usuario ya existía con permisos más amplios, puedes descomentar el REVOKE ALL PRIVILEGES, GRANT OPTION y luego volver a conceder los GRANT de solo lectura como arriba.

Auditoría: si quieres registrar lo que se concedió, puedes insertar en una tabla de auditoría dentro del procedimiento.