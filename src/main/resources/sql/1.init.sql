CREATE TABLE IF NOT EXISTS "message_record"
(
    `id`         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `nos`        TEXT    NOT NULL UNIQUE,
    `bot`        INTEGER NOT NULL,
    `sender`     INTEGER NOT NULL,
    `target`     INTEGER NOT NULL,
    `time`       INTEGER NOT NULL,
    `msg_raw`   TEXT    NOT NULL,
    `msg_type`   TEXT    NOT NULL,
    `msg_recode` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS group_id_index ON "message_record" (`id`);
CREATE INDEX IF NOT EXISTS group_nos_index ON "message_record" (`nos`);
CREATE INDEX IF NOT EXISTS group_bot_index ON "message_record" (`bot`);
CREATE INDEX IF NOT EXISTS group_nos_bot_index ON "message_record" (`nos`, `bot`);
CREATE INDEX IF NOT EXISTS group_msg_raw_index ON "message_record" (`msg_raw`);
CREATE INDEX IF NOT EXISTS group_msg_type_index ON "message_record" (`msg_type`);
CREATE INDEX IF NOT EXISTS group_msg_recode_index ON "message_record" (`msg_recode`);

