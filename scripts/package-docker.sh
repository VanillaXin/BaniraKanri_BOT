#!/usr/bin/env bash
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 镜像固定为 banira-kanri:latest，与 compose 中 image 一致。
# 用法：在项目根目录执行 ./scripts/package-docker.sh（需 chmod +x）

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

IMAGE="banira-kanri:latest"
TS="$(date +%Y%m%d-%H%M%S)"
OUT_DIR="$ROOT/dist"
TAR_NAME="banira-kanri-latest-${TS}.tar"
OUT_FILE="$OUT_DIR/$TAR_NAME"

echo "[banira] 项目目录: $ROOT"
echo "[banira] 镜像: $IMAGE"
echo "[banira] docker build ..."
docker build -t "$IMAGE" .

mkdir -p "$OUT_DIR"
echo "[banira] docker save -> $OUT_FILE"
docker save -o "$OUT_FILE" "$IMAGE"

echo "[banira] 完成: $OUT_FILE ($(du -h "$OUT_FILE" | cut -f1))"
