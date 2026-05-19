#requires -Version 5.1
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 镜像固定为 banira-kanri:latest，与 compose 中 image 一致。
# 构建时通过 BuildKit 挂载本机 Maven 本地仓库（与 IDEA 中 mavenLocal() 一致。
# 用法：在仓库根目录执行 .\scripts\package-docker.ps1
#       或任意目录：powershell -File "F:\...\scripts\package-docker.ps1"
# 可选环境变量：MAVEN_LOCAL_REPO — 覆盖默认的 %USERPROFILE%\.m2\repository

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

$Image = "banira-kanri:latest"
$Ts = Get-Date -Format "yyyyMMdd-HHmmss"
$OutDir = Join-Path $Root "dist"
$TarName = "banira-kanri-latest-${Ts}.tar"
$OutFile = Join-Path $OutDir $TarName

Write-Host "[banira] 项目目录: $Root"
Write-Host "[banira] 镜像: $Image"

# region Maven 本地仓库（供 Dockerfile RUN --mount=from=maven_local）
$MavenLocal = if ($env:MAVEN_LOCAL_REPO) { $env:MAVEN_LOCAL_REPO.Trim() } else { Join-Path $env:USERPROFILE ".m2\repository" }
$MavenContext = $MavenLocal
if (-not (Test-Path -LiteralPath $MavenLocal)) {
    $MavenContext = Join-Path $env:TEMP ("banira-empty-m2-{0}" -f [guid]::NewGuid().ToString("N"))
    New-Item -ItemType Directory -Path $MavenContext -Force | Out-Null
    Write-Warning "[banira] 未找到 Maven 本地仓库: $MavenLocal，容器内将仅从远程解析依赖"
} else {
    Write-Host "[banira] Maven 本地仓库: $MavenLocal"
}
# endregion Maven 本地仓库

$env:DOCKER_BUILDKIT = "1"
Write-Host "[banira] docker build ..."
docker build -t $Image --build-context "maven_local=$MavenContext" .
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

New-Item -ItemType Directory -Path $OutDir -Force | Out-Null
Write-Host "[banira] docker save -> $OutFile"
docker save -o $OutFile $Image
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$sizeMb = [math]::Round((Get-Item -LiteralPath $OutFile).Length / 1MB, 2)
Write-Host ('[banira] 完成: {0} ({1} MiB)' -f $OutFile, $sizeMb)
