# 基础镜像：默认使用 DaoCloud 对 Docker Hub library 的同步（直连 Hub 超时时可不用改命令直接构建）。
# 能访问 Docker Hub 时可用：docker build --build-arg JAVA_IMAGE=docker.io/library/eclipse-temurin -t banira-kanri:latest .
# 构建阶段默认从腾讯云拉 Gradle 发行包（容器内访问 services.gradle.org 易超时）。若需官方地址：--build-arg GRADLE_DIST_PATH=services.gradle.org/distributions
# Playwright 在构建阶段预装无头 shell（含系统依赖），避免运行时从 GCS 下载。
# 运行时通过 ./playwright -> /app/.cache 持久化。
# 构建若仍慢可设：--build-arg PLAYWRIGHT_DOWNLOAD_HOST=https://cdn.npmmirror.com/binaries/playwright

ARG JAVA_IMAGE=docker.m.daocloud.io/library/eclipse-temurin

# region 构建阶段
FROM ${JAVA_IMAGE}:21-jdk-jammy AS builder
ARG GRADLE_DIST_PATH=mirrors.cloud.tencent.com/gradle
WORKDIR /workspace

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends git \
    && rm -rf /var/lib/apt/lists/*

COPY . .

RUN sed -i "s|services.gradle.org/distributions|${GRADLE_DIST_PATH}|g" gradle/wrapper/gradle-wrapper.properties \
    && chmod +x gradlew

# 挂载主机 Maven 本地仓库，使 mavenLocal() 与 IDEA 一致
RUN --mount=type=bind,from=maven_local,source=.,target=/root/.m2/repository,readonly \
    ./gradlew jar --no-daemon -x test \
    && mkdir -p /out \
    && cd build/libs \
    && cp "$(ls banira-kanri-*.jar | grep -v plain)" /out/app.jar \
    && cp -r libs /out/libs

# region Playwright（无头 shell，路径与运行时一致；--with-deps 安装 Chromium 所需系统库）
ARG PLAYWRIGHT_DOWNLOAD_HOST=
ENV PLAYWRIGHT_BROWSERS_PATH=/app/.cache/ms-playwright
ENV PLAYWRIGHT_DOWNLOAD_HOST=${PLAYWRIGHT_DOWNLOAD_HOST}

RUN mkdir -p /app/.cache/ms-playwright \
    && java -cp "/out/libs/*" com.microsoft.playwright.CLI install --with-deps --only-shell
# endregion Playwright
# endregion 构建阶段

# region 运行阶段
ARG JAVA_IMAGE=docker.m.daocloud.io/library/eclipse-temurin
FROM ${JAVA_IMAGE}:21-jre-jammy

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    libglib2.0-0 libnss3 libnspr4 libatk1.0-0 libatk-bridge2.0-0 libcups2 \
    libdrm2 libdbus-1-3 libxcb1 libxkbcommon0 libx11-6 libxcomposite1 \
    libxdamage1 libxext6 libxfixes3 libxrandr2 libgbm1 libpango-1.0-0 libcairo2 \
    libasound2 libatspi2.0-0 fonts-liberation fontconfig \
    && rm -rf /var/lib/apt/lists/*

COPY docker/fonts /tmp/host-fonts
RUN mkdir -p /usr/local/share/fonts/custom \
    && copied=0 \
    && for ext in ttf TTF otf OTF ttc TTC; do \
         for f in /tmp/host-fonts/*."$ext"; do \
           [ -f "$f" ] || continue; \
           cp "$f" /usr/local/share/fonts/custom/; \
           copied=1; \
         done; \
       done \
    && if [ "$copied" -eq 0 ]; then \
         apt-get update \
         && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends fonts-noto-cjk \
         && rm -rf /var/lib/apt/lists/*; \
       fi \
    && rm -rf /tmp/host-fonts \
    && fc-cache -fv

WORKDIR /app

ENV HOME=/app
ENV PLAYWRIGHT_BROWSERS_PATH=/app/.cache/ms-playwright
ENV LANG=C.UTF-8

COPY --from=builder /out/app.jar /app/app.jar
COPY --from=builder /out/libs /app/libs
COPY --from=builder /app/.cache/ms-playwright /app/.playwright-seed/ms-playwright
COPY docker/docker-entrypoint.sh /docker-entrypoint.sh
RUN sed -i 's/\r$//' /docker-entrypoint.sh \
    && chmod +x /docker-entrypoint.sh

EXPOSE 8080

VOLUME ["/app/data", "/app/config", "/app/logs", "/app/cache", "/app/.cache"]

ENTRYPOINT ["/docker-entrypoint.sh"]
# endregion 运行阶段
