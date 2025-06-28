import hashlib
import logging
import os
import sqlite3
from pathlib import Path
from typing import Optional

LOGGER = logging.getLogger(__name__)
class SQLiteCache:
    """Lightweight, zero-setup persistent cache.

    One row per prompt; prompt is SHA-256-hashed for compact keys and
    to avoid huge primary keys in the DB.
    """

    def __init__(self, db_path: str | Path = ".c2rust_postprocess.sqlite"):
        db_path = os.path.abspath(db_path)
        self.conn = sqlite3.connect(db_path)
        self.conn.execute(
            """CREATE TABLE IF NOT EXISTS cache (
                   prompt_hash TEXT PRIMARY KEY,
                   prompt      TEXT,
                   model       TEXT,
                   response    TEXT
               )"""
        )
        self.conn.commit()
        LOGGER.info(f"Using cache at {db_path}")

    @staticmethod
    def _hash(prompt: str) -> str:
        return hashlib.sha256(prompt.encode("utf-8")).hexdigest()

    def get(self, prompt: str, model: str) -> Optional[str]:
        h = self._hash(prompt)
        row = self.conn.execute(
            "SELECT response FROM cache WHERE prompt_hash = ? AND model = ?", (h, model)
        ).fetchone()
        return row[0] if row else None

    def set(self, prompt: str, model: str, response: str) -> None:
        h = self._hash(prompt)
        self.conn.execute(
            "INSERT OR REPLACE INTO cache(prompt_hash, prompt, model, response) "
            "VALUES (?, ?, ?, ?)",
            (h, prompt, model, response),
        )
        self.conn.commit()

