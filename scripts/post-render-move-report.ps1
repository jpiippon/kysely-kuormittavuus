$ErrorActionPreference = 'Stop'

$projectRoot = (Get-Location).Path
$outDirRel = $env:QUARTO_PROJECT_OUTPUT_DIR
if ([string]::IsNullOrWhiteSpace($outDirRel)) { $outDirRel = 'output' }
$outDir = Join-Path $projectRoot $outDirRel

if (!(Test-Path -LiteralPath $outDir)) {
  New-Item -ItemType Directory -Path $outDir | Out-Null
}

$srcHtml = Join-Path $projectRoot 'report.html'
$srcFilesDir = Join-Path $projectRoot 'report_files'

$dstHtmlAbs = Join-Path $outDir 'report.html'

function Move-IfExists {
  param(
    [Parameter(Mandatory=$true)][string]$SourcePath,
    [Parameter(Mandatory=$true)][string]$DestinationPath
  )
  if (Test-Path -LiteralPath $SourcePath) {
    if (Test-Path -LiteralPath $DestinationPath) {
      Remove-Item -LiteralPath $DestinationPath -Recurse -Force -ErrorAction SilentlyContinue
    }
    Move-Item -LiteralPath $SourcePath -Destination $DestinationPath
  }
}

# 1) Move the main HTML + its asset directory.
Move-IfExists -SourcePath $srcHtml -DestinationPath $dstHtmlAbs
Move-IfExists -SourcePath $srcFilesDir -DestinationPath (Join-Path $outDir 'report_files')

# 2) Fix hard-coded links that assume report.html is in the project root.
if (Test-Path -LiteralPath $dstHtmlAbs) {
  $text = Get-Content -LiteralPath $dstHtmlAbs -Raw

  # When report.html is moved into `output/`, references like `output/figures/...`
  # should become `figures/...` (same for tables).
  $text2 = $text `
    -replace 'output/figures/', 'figures/' `
    -replace 'output/tables/', 'tables/'

  if ($text2 -ne $text) {
    Set-Content -LiteralPath $dstHtmlAbs -Value $text2 -Encoding UTF8
  }
}
