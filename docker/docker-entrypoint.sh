#!/bin/sh
set -e

# region 从镜像种子目录初始化 Playwright 缓存（供宿主机卷持久化，仅目录为空时执行）
seed_dir() {
    src=$1
    dest=$2
    if [ ! -d "$src" ] || [ -z "$(ls -A "$src" 2>/dev/null)" ]; then
        return 0
    fi
    if [ ! -d "$dest" ] || [ -z "$(ls -A "$dest" 2>/dev/null)" ]; then
        mkdir -p "$dest"
        cp -a "$src/." "$dest/"
    fi
}

seed_dir /app/.playwright-seed/ms-playwright /app/.cache/ms-playwright
# endregion 从镜像种子目录初始化 Playwright 缓存

exec java -jar /app/app.jar "$@"
