#requires -Version 5.1
# 一键：docker build + docker save，产物输出到仓库根目录 dist/
# 镜像固定为 banira-kanri:latest，与 compose 中 image 一致。
# 用法：在仓库根目录执行 .\scripts\package-docker.ps1
#       或任意目录：powershell -File "F:\...\scripts\package-docker.ps1"

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
Write-Host "[banira] docker build ..."
docker build -t $Image .
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

New-Item -ItemType Directory -Path $OutDir -Force | Out-Null
Write-Host "[banira] docker save -> $OutFile"
docker save -o $OutFile $Image
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$sizeMb = [math]::Round((Get-Item -LiteralPath $OutFile).Length / 1MB, 2)
Write-Host ('[banira] 完成: {0} ({1} MiB)' -f $OutFile, $sizeMb)
