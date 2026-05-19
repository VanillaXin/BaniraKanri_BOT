#!/usr/bin/env bash
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 镜像固定为 banira-kanri:latest，与 compose 中 image 一致。
# 构建时通过 BuildKit 挂载本机 Maven 本地仓库（与 IDEA 中 mavenLocal() 一致。
# 用法：在项目根目录执行 ./scripts/package-docker.sh（需 chmod +x）
# 可选环境变量：MAVEN_LOCAL_REPO — 覆盖默认的 ~/.m2/repository

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

# region Maven 本地仓库（供 Dockerfile RUN --mount=from=maven_local）
MAVEN_LOCAL="${MAVEN_LOCAL_REPO:-$HOME/.m2/repository}"
MAVEN_CONTEXT="$MAVEN_LOCAL"
if [[ ! -d "$MAVEN_LOCAL" ]]; then
  MAVEN_CONTEXT="$(mktemp -d banira-empty-m2.XXXXXX)"
  echo "[banira] 警告: 未找到 Maven 本地仓库: $MAVEN_LOCAL，容器内将仅从远程解析依赖" >&2
else
  echo "[banira] Maven 本地仓库: $MAVEN_LOCAL"
fi
# endregion Maven 本地仓库

export DOCKER_BUILDKIT=1
echo "[banira] docker build ..."
docker build -t "$IMAGE" --build-context "maven_local=$MAVEN_CONTEXT" .

mkdir -p "$OUT_DIR"
echo "[banira] docker save -> $OUT_FILE"
docker save -o "$OUT_FILE" "$IMAGE"

echo "[banira] 完成: $OUT_FILE ($(du -h "$OUT_FILE" | cut -f1))"
