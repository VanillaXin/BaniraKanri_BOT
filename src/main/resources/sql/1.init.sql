CREATE TABLE IF NOT EXISTS "message_record"
(
    `id`         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `msg_id`     TEXT    NOT NULL UNIQUE,
    `bot_id`     INTEGER NOT NULL,
    `sender_id`  INTEGER NOT NULL,
    `target_id`  INTEGER NOT NULL,
    `group_id`   INTEGER NOT NULL,
    `time`       INTEGER NOT NULL,
    `msg_raw`    TEXT    NOT NULL,
    `msg_type`   TEXT    NOT NULL,
    `msg_recode` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS id_index ON "message_record" (`id`);
CREATE INDEX IF NOT EXISTS msg_id_index ON "message_record" (`msg_id`);
CREATE INDEX IF NOT EXISTS bot_id_index ON "message_record" (`bot_id`);
CREATE INDEX IF NOT EXISTS msg_id_bot_id_index ON "message_record" (`msg_id`, `bot_id`);
CREATE INDEX IF NOT EXISTS msg_raw_index ON "message_record" (`msg_raw`);
CREATE INDEX IF NOT EXISTS msg_type_index ON "message_record" (`msg_type`);
CREATE INDEX IF NOT EXISTS msg_recode_index ON "message_record" (`msg_recode`);

