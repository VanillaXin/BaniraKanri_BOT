CREATE TABLE IF NOT EXISTS "group"
(
    `id`       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `nos`      TEXT    NOT NULL UNIQUE,
    `bot`      INTEGER NOT NULL,
    `sender`   INTEGER NOT NULL,
    `target`   INTEGER NOT NULL,
    `time`     INTEGER NOT NULL,
    `msg_json` TEXT    NOT NULL,
    `msg_text` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS group_bot_index ON "group" (`bot`);
CREATE INDEX IF NOT EXISTS group_id_index ON "group" (`id`);
CREATE INDEX IF NOT EXISTS group_msg_json_index ON "group" (`msg_json`);
CREATE INDEX IF NOT EXISTS group_msg_text_index ON "group" (`msg_text`);
CREATE INDEX IF NOT EXISTS group_nos_bot_index ON "group" (`nos`, `bot`);
CREATE INDEX IF NOT EXISTS group_nos_index ON "group" (`nos`);



CREATE TABLE IF NOT EXISTS "friend"
(
    `id`       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `nos`      TEXT    NOT NULL UNIQUE,
    `bot`      INTEGER NOT NULL,
    `sender`   INTEGER NOT NULL,
    `target`   INTEGER NOT NULL,
    `time`     INTEGER NOT NULL,
    `msg_json` TEXT    NOT NULL,
    `msg_text` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS friend_bot_index ON "friend" (`bot`);
CREATE INDEX IF NOT EXISTS friend_id_index ON "friend" (`id`);
CREATE INDEX IF NOT EXISTS friend_msg_json_index ON "friend" (`msg_json`);
CREATE INDEX IF NOT EXISTS friend_msg_text_index ON "friend" (`msg_text`);
CREATE INDEX IF NOT EXISTS friend_nos_bot_index ON "friend" (`nos`, `bot`);
CREATE INDEX IF NOT EXISTS friend_nos_index ON "friend" (`nos`);



CREATE TABLE IF NOT EXISTS "member"
(
    `id`       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `nos`      TEXT    NOT NULL UNIQUE,
    `bot`      INTEGER NOT NULL,
    `sender`   INTEGER NOT NULL,
    `target`   INTEGER NOT NULL,
    `time`     INTEGER NOT NULL,
    `msg_json` TEXT    NOT NULL,
    `msg_text` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS member_bot_index ON "member" (`bot`);
CREATE INDEX IF NOT EXISTS member_id_index ON "member" (`id`);
CREATE INDEX IF NOT EXISTS member_msg_json_index ON "member" (`msg_json`);
CREATE INDEX IF NOT EXISTS member_msg_text_index ON "member" (`msg_text`);
CREATE INDEX IF NOT EXISTS member_nos_bot_index ON "member" (`nos`, `bot`);
CREATE INDEX IF NOT EXISTS member_nos_index ON "member" (`nos`);



CREATE TABLE IF NOT EXISTS "stranger"
(
    `id`       INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `nos`      TEXT    NOT NULL UNIQUE,
    `bot`      INTEGER NOT NULL,
    `sender`   INTEGER NOT NULL,
    `target`   INTEGER NOT NULL,
    `time`     INTEGER NOT NULL,
    `msg_json` TEXT    NOT NULL,
    `msg_text` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS stranger_bot_index ON "stranger" (`bot`);
CREATE INDEX IF NOT EXISTS stranger_id_index ON "stranger" (`id`);
CREATE INDEX IF NOT EXISTS stranger_msg_json_index ON "stranger" (`msg_json`);
CREATE INDEX IF NOT EXISTS stranger_msg_text_index ON "stranger" (`msg_text`);
CREATE INDEX IF NOT EXISTS stranger_nos_bot_index ON "stranger" (`nos`, `bot`);
CREATE INDEX IF NOT EXISTS stranger_nos_index ON "stranger" (`nos`);

