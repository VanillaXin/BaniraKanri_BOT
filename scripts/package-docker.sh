#!/usr/bin/env bash
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 用法：在项目根目录执行 ./scripts/package-docker.sh（需 chmod +x）
# 可选环境变量：BANIRA_DOCKER_TAG（覆盖从 build.gradle 解析的版本）

set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

IMAGE_BASE="banira-kanri"
if [[ -n "${BANIRA_DOCKER_TAG:-}" ]]; then
  VERSION="$BANIRA_DOCKER_TAG"
else
  VERSION="$(sed -n "s/^[[:space:]]*version[[:space:]]*=[[:space:]]*'\([^']*\)'.*/\1/p" build.gradle | head -n1)"
  if [[ -z "$VERSION" ]]; then
    VERSION="$(sed -n 's/^[[:space:]]*version[[:space:]]*=[[:space:]]*"\([^"]*\)".*/\1/p' build.gradle | head -n1)"
  fi
  VERSION="${VERSION:-latest}"
fi

IMAGE="${IMAGE_BASE}:${VERSION}"
TS="$(date +%Y%m%d-%H%M%S)"
OUT_DIR="$ROOT/dist"
TAR_NAME="${IMAGE_BASE}-${VERSION}-${TS}.tar"
OUT_FILE="$OUT_DIR/$TAR_NAME"

echo "[banira] 项目目录: $ROOT"
echo "[banira] 镜像: $IMAGE"
echo "[banira] docker build ..."
docker build -t "$IMAGE" .

echo "[banira] docker tag ${IMAGE_BASE}:latest（便于本地 compose 使用）"
docker tag "$IMAGE" "${IMAGE_BASE}:latest"

mkdir -p "$OUT_DIR"
echo "[banira] docker save -> $OUT_FILE"
docker save -o "$OUT_FILE" "$IMAGE"

echo "[banira] 完成: $OUT_FILE ($(du -h "$OUT_FILE" | cut -f1))"
