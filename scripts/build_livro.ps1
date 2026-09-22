param(
  [string]$Source = "apostila_sdm_r_modular.tex",
  [string]$OutputDirectory = "build/livro",
  [switch]$Clean
)

$ErrorActionPreference = "Stop"

$projectRoot = Split-Path -Parent $PSScriptRoot
Set-Location -LiteralPath $projectRoot

$candidates = @(
  (Get-Command latexmk.exe -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Source -First 1),
  "$env:APPDATA/TinyTeX/bin/windows/latexmk.exe",
  "$env:LOCALAPPDATA/Programs/MiKTeX/miktex/bin/x64/latexmk.exe"
) | Where-Object { $_ -and (Test-Path -LiteralPath $_) }

if ($candidates.Count -eq 0) {
  throw "latexmk não foi localizado. Instale TinyTeX ou MiKTeX e tente novamente."
}

$latexmk = $candidates[0]
New-Item -ItemType Directory -Force -Path $OutputDirectory | Out-Null

if ($Clean) {
  & $latexmk -C -outdir=$OutputDirectory $Source
  if ($LASTEXITCODE -ne 0) {
    throw "A limpeza dos artefatos LaTeX falhou com código $LASTEXITCODE."
  }
}

& $latexmk -xelatex -interaction=nonstopmode -file-line-error -outdir=$OutputDirectory $Source
if ($LASTEXITCODE -ne 0) {
  throw "A compilação do livro falhou com código $LASTEXITCODE. Consulte o log em $OutputDirectory."
}

Write-Host "Livro compilado em $OutputDirectory."
