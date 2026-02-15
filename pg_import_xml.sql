\set ON_ERROR_STOP on

-- ==========================================
-- Import users from XML into PostgreSQL users table
-- Expected XML: <users><user><id>..</id><name>..</name><gender>..</gender><age>..</age></user>...</users>
-- ==========================================

BEGIN;

-- Set the XML file path (adjust as needed)
TRUNCATE TABLE users;

-- Create a temporary table to hold the XML lines
DROP TABLE IF EXISTS _xml_lines;
CREATE TEMP TABLE _xml_lines(line text) ON COMMIT DROP;

-- Show the XML file path
\echo xmlfile = :xmlfile
\echo xmlfile_quoted = :'xmlfile'
\copy _xml_lines(line) FROM :xmlfile (FORMAT text, ENCODING 'UTF8');

-- 
WITH xml_src AS (
  SELECT xmlparse(document string_agg(line, E'\n')) AS x
  FROM _xml_lines
),
user_nodes AS (
  -- Extract user nodes
  SELECT unnest(
           xpath('/*[local-name()="users"]/*[local-name()="user"]', x)
         ) AS u
  FROM xml_src
)
INSERT INTO users (id, name, gender, age)
SELECT
  ((xpath('./*[local-name()="id"]/text()', u))[1]::text)::int                         AS id,
  left(((xpath('./*[local-name()="name"]/text()', u))[1]::text), 32)                  AS name,
  ((xpath('./*[local-name()="gender"]/text()', u))[1]::text)                          AS gender,
  ((xpath('./*[local-name()="age"]/text()', u))[1]::text)::int                        AS age
FROM user_nodes;

COMMIT;
