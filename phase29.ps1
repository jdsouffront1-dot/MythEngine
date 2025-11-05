$scriptPath = ".\phase29_causal_reconstruction.py"
$logPath = "data\phase29_run.log"
python $scriptPath *>&1 | Tee-Object -FilePath $logPath
