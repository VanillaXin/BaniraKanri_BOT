#requires -Version 5.1
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 用法：在仓库根目录执行 .\scripts\package-docker.ps1
#       或任意目录：powershell -File "F:\...\scripts\package-docker.ps1"
# 可选环境变量：BANIRA_DOCKER_TAG（覆盖从 build.gradle 解析的版本，默认用作镜像 tag）

$ErrorActionPreference = "Stop"
$Root = Split-Path -Parent $PSScriptRoot
Set-Location $Root

function Get-ProjectVersion {
    param([string]$GradleFile)
    foreach ($line in Get-Content -LiteralPath $GradleFile) {
        if ($line -match "^\s*version\s*=\s*'([^']+)'") {
            return $Matches[1]
        }
        if ($line -match '^\s*version\s*=\s*"([^"]+)"') {
            return $Matches[1]
        }
    }
    return "latest"
}

$ImageBase = "banira-kanri"
$Version = if ($env:BANIRA_DOCKER_TAG) { $env:BANIRA_DOCKER_TAG } else { Get-ProjectVersion (Join-Path $Root "build.gradle") }
$Image = "${ImageBase}:${Version}"
$Ts = Get-Date -Format "yyyyMMdd-HHmmss"
$OutDir = Join-Path $Root "dist"
$TarName = "${ImageBase}-${Version}-${Ts}.tar"
$OutFile = Join-Path $OutDir $TarName

Write-Host "[banira] 项目目录: $Root"
Write-Host "[banira] 镜像: $Image"
Write-Host "[banira] docker build ..."
docker build -t $Image .
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

Write-Host "[banira] docker tag ${ImageBase}:latest（便于本地 compose 使用）"
docker tag $Image "${ImageBase}:latest"

New-Item -ItemType Directory -Path $OutDir -Force | Out-Null
Write-Host "[banira] docker save -> $OutFile"
docker save -o $OutFile $Image
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$sizeMb = [math]::Round((Get-Item -LiteralPath $OutFile).Length / 1MB, 2)
Write-Host ('[banira] 完成: {0} ({1} MiB)' -f $OutFile, $sizeMb)
