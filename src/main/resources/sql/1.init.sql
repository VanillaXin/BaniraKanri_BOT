-- 消息记录
CREATE TABLE IF NOT EXISTS "message_record"
(
    `id`         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `msg_id`     TEXT    NOT NULL,
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


-- 抽老婆记录
CREATE TABLE IF NOT EXISTS "wife_record"
(
    `id`        INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `msg_id`    TEXT    NOT NULL,
    `group_id`  INTEGER NOT NULL,
    `sender_id` INTEGER NOT NULL,
    `time`      INTEGER NOT NULL,
    `wife_id`   INTEGER NOT NULL,
    `wife_name` TEXT    NOT NULL,
    `wife_nick` TEXT    NOT NULL
);
CREATE INDEX IF NOT EXISTS id_index ON "wife_record" (`id`);
CREATE INDEX IF NOT EXISTS group_id_index ON "wife_record" (`group_id`);
CREATE INDEX IF NOT EXISTS sender_id_index ON "wife_record" (`sender_id`);
CREATE INDEX IF NOT EXISTS time_index ON "wife_record" (`time`);
CREATE INDEX IF NOT EXISTS group_id_sender_id_index ON "wife_record" (`group_id`, `sender_id`);
CREATE INDEX IF NOT EXISTS group_id_time_index ON "wife_record" (`group_id`, `time`);
CREATE INDEX IF NOT EXISTS group_id_sender_id_time_index ON "wife_record" (`group_id`, `sender_id`, `time`);


-- 关键词记录
CREATE TABLE IF NOT EXISTS "keyword_record"
(
    `id`           INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `bot_id`       INTEGER NOT NULL,
    `group_id`     INTEGER NOT NULL DEFAULT 0,
    `creator_id`   INTEGER NOT NULL,
    `time`         INTEGER NOT NULL,
    `keyword_type` TEXT    NOT NULL,
    `keyword`      TEXT    NOT NULL,
    `reply_msg`    TEXT    NOT NULL,
    `enable`       BOOLEAN NOT NULL DEFAULT TRUE,
    `audited`      BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE INDEX IF NOT EXISTS id_index ON "keyword_record" (`id`);
CREATE INDEX IF NOT EXISTS bot_id_index ON "keyword_record" (`bot_id`);
CREATE INDEX IF NOT EXISTS group_id_index ON "keyword_record" (`group_id`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_index ON "keyword_record" (`bot_id`, `group_id`);
CREATE INDEX IF NOT EXISTS time_index ON "keyword_record" (`time`);
CREATE INDEX IF NOT EXISTS keyword_type_index ON "keyword_record" (`keyword_type`);
CREATE INDEX IF NOT EXISTS keyword_index ON "keyword_record" (`keyword`);
CREATE INDEX IF NOT EXISTS enable_index ON "keyword_record" (`enable`);
CREATE INDEX IF NOT EXISTS audited_index ON "keyword_record" (`audited`);
CREATE INDEX IF NOT EXISTS bot_id_enable_index ON "keyword_record" (`bot_id`, `enable`);
CREATE INDEX IF NOT EXISTS bot_id_audited_index ON "keyword_record" (`bot_id`, `audited`);
CREATE INDEX IF NOT EXISTS bot_id_enable_audited_index ON "keyword_record" (`bot_id`, `enable`, `audited`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_enable_index ON "keyword_record" (`bot_id`, `group_id`, `enable`);


-- 定时任务记录
CREATE TABLE IF NOT EXISTS "timer_record"
(
    `id`         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `bot_id`     INTEGER NOT NULL,
    `group_id`   INTEGER NOT NULL DEFAULT 0,
    `creator_id` INTEGER NOT NULL,
    `time`       INTEGER NOT NULL,
    `cron`       TEXT    NOT NULL,
    `reply_msg`  TEXT    NOT NULL,
    `enable`     BOOLEAN NOT NULL DEFAULT TRUE,
    `audited`    BOOLEAN NOT NULL DEFAULT FALSE
);
CREATE INDEX IF NOT EXISTS id_index ON "timer_record" (`id`);
CREATE INDEX IF NOT EXISTS bot_id_index ON "timer_record" (`bot_id`);
CREATE INDEX IF NOT EXISTS group_id_index ON "timer_record" (`group_id`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_index ON "timer_record" (`bot_id`, `group_id`);
CREATE INDEX IF NOT EXISTS time_index ON "timer_record" (`time`);
CREATE INDEX IF NOT EXISTS cron_index ON "timer_record" (`cron`);
CREATE INDEX IF NOT EXISTS enable_index ON "timer_record" (`enable`);
CREATE INDEX IF NOT EXISTS audited_index ON "timer_record" (`audited`);
CREATE INDEX IF NOT EXISTS bot_id_enable_index ON "timer_record" (`bot_id`, `enable`);
CREATE INDEX IF NOT EXISTS bot_id_audited_index ON "timer_record" (`bot_id`, `audited`);
CREATE INDEX IF NOT EXISTS bot_id_enable_audited_index ON "timer_record" (`bot_id`, `enable`, `audited`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_enable_index ON "timer_record" (`bot_id`, `group_id`, `enable`);


-- MC服务器记录
CREATE TABLE IF NOT EXISTS "minecraft_record"
(
    `id`         INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    `bot_id`     INTEGER NOT NULL,
    `group_id`   INTEGER NOT NULL DEFAULT 0,
    `creator_id` INTEGER NOT NULL,
    `time`       INTEGER NOT NULL,
    `name`       TEXT    NOT NULL,
    `query_ip`   TEXT    NOT NULL,
    `query_port` INTEGER NOT NULL,
    `rcon_ip`    TEXT    NOT NULL,
    `rcon_port`  INTEGER NOT NULL,
    `rcon_psw`   TEXT    NOT NULL,
    `enable`     BOOLEAN NOT NULL DEFAULT TRUE
);
CREATE INDEX IF NOT EXISTS id_index ON "minecraft_record" (`id`);
CREATE INDEX IF NOT EXISTS bot_id_index ON "minecraft_record" (`bot_id`);
CREATE INDEX IF NOT EXISTS group_id_index ON "minecraft_record" (`group_id`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_index ON "minecraft_record" (`bot_id`, `group_id`);
CREATE INDEX IF NOT EXISTS time_index ON "minecraft_record" (`time`);
CREATE INDEX IF NOT EXISTS name_index ON "minecraft_record" (`name`);
CREATE INDEX IF NOT EXISTS group_id_name_index ON "minecraft_record" (`group_id`, `name`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_name_index ON "minecraft_record" (`bot_id`, `group_id`, `name`);
CREATE INDEX IF NOT EXISTS enable_index ON "minecraft_record" (`enable`);
CREATE INDEX IF NOT EXISTS bot_id_enable_index ON "minecraft_record" (`bot_id`, `enable`);
CREATE INDEX IF NOT EXISTS bot_id_group_id_enable_index ON "minecraft_record" (`bot_id`, `group_id`, `enable`);
