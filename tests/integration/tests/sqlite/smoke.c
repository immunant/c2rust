#include "sqlite3.h"
#include <stdio.h>
#include <stdlib.h>

#define CHECK(condition) do { \
    if (!(condition)) { \
        fprintf(stderr, "%s:%d: %s failed\n", __FILE__, __LINE__, #condition); \
        exit(1); \
    } \
} while (0)

static void exec(sqlite3 *db, const char *sql) {
    char *error = NULL;
    if (sqlite3_exec(db, sql, NULL, NULL, &error) != SQLITE_OK) {
        fprintf(stderr, "%s: %s\n", sql, error);
        sqlite3_free(error);
        exit(1);
    }
}

static void expect_int(sqlite3 *db, const char *sql, int expected) {
    sqlite3_stmt *stmt = NULL;
    CHECK(sqlite3_prepare_v2(db, sql, -1, &stmt, NULL) == SQLITE_OK);
    CHECK(sqlite3_step(stmt) == SQLITE_ROW);
    int actual = sqlite3_column_int(stmt, 0);
    if (actual != expected) {
        fprintf(stderr, "%s: expected %d, got %d\n", sql, expected, actual);
        exit(1);
    }
    CHECK(sqlite3_step(stmt) == SQLITE_DONE);
    CHECK(sqlite3_finalize(stmt) == SQLITE_OK);
}

int main(int argc, char **argv) {
    CHECK(argc == 2);
    sqlite3 *db = NULL;
    CHECK(sqlite3_open(argv[1], &db) == SQLITE_OK);
    exec(db,
        "PRAGMA journal_mode=WAL;"
        "PRAGMA foreign_keys=ON;"
        "CREATE TABLE parent(id INTEGER PRIMARY KEY);"
        "CREATE TABLE child(id INTEGER REFERENCES parent, value TEXT, data BLOB);"
        "CREATE TABLE audit(value TEXT);"
        "CREATE TRIGGER inserted AFTER INSERT ON child BEGIN "
        "  INSERT INTO audit VALUES(new.value); END;"
        "BEGIN;"
        "INSERT INTO parent VALUES(1);"
        "INSERT INTO child VALUES(1, 'hello', x'00ff80');"
        "SAVEPOINT s;"
        "INSERT INTO child VALUES(1, 'rolled back', NULL);"
        "ROLLBACK TO s;"
        "RELEASE s;"
        "COMMIT;");
    expect_int(db, "SELECT journal_mode = 'wal' FROM pragma_journal_mode", 1);
    expect_int(db, "SELECT count(*) FROM audit", 1);
    expect_int(db, "SELECT hex(data) = '00FF80' AND value = 'hello' FROM child", 1);
    CHECK(sqlite3_exec(db, "INSERT INTO child VALUES(2, 'invalid', NULL)",
                        NULL, NULL, NULL) == SQLITE_CONSTRAINT);
    expect_int(db, "SELECT json_extract('{\"a\":[1,2,3]}', '$.a[2]')", 3);
    expect_int(db,
        "SELECT json(jsonb_set(jsonb('{\"a\":[1,2,3]}'), '$.a[1]', 9)) "
        "= '{\"a\":[1,9,3]}'", 1);
    expect_int(db,
        "SELECT json(jsonb_remove(jsonb_insert(jsonb('{}'), '$.x', 7, '$.y', 8), '$.x')) "
        "= '{\"y\":8}'", 1);
    expect_int(db,
        "WITH t(x) AS (VALUES(1),(2),(3)) "
        "SELECT json(jsonb_group_array(x)) = '[1,2,3]' FROM t", 1);
    expect_int(db, "SELECT CAST('1_000' AS INTEGER)", 1);
    expect_int(db, "SELECT 1_000 + CAST('2.5e2' AS INTEGER)", 1002);
    expect_int(db, "SELECT CAST(CAST('2.5e2' AS REAL) AS INTEGER)", 250);
    expect_int(db,
        "SELECT printf('%lld', 8_227_256_643_844_975_616) = '8227256643844975616' "
        "AND printf('%lld', -9223372036854775808) = '-9223372036854775808'", 1);
    expect_int(db,
        "SELECT printf('%.6f', 8.227256643844975616) = '8.227257' "
        "AND printf('%.8f', CAST('1.23456e-3' AS REAL)) = '0.00123456' "
        "AND printf('%.4f', 1_234.5_6e-2) = '12.3456'", 1);
    expect_int(db,
        "WITH t(x) AS (VALUES(1),(2),(3)) "
        "SELECT sum(s) FROM (SELECT sum(x) OVER (ORDER BY x) AS s FROM t)", 10);
    CHECK(sqlite3_close(db) == SQLITE_OK);

    CHECK(sqlite3_open(argv[1], &db) == SQLITE_OK);
    expect_int(db, "SELECT journal_mode = 'wal' FROM pragma_journal_mode", 1);
    expect_int(db, "SELECT count(*) FROM audit", 1);
    expect_int(db, "SELECT integrity_check = 'ok' FROM pragma_integrity_check", 1);
    CHECK(sqlite3_close(db) == SQLITE_OK);
    puts("SQLite SQL and persistence checks passed");
    return 0;
}
