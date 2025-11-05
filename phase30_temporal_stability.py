# -*- coding: utf-8 -*-
import os, sys, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

try:
    sys.stdout.reconfigure(encoding="utf-8")
except Exception:
    pass

print("\n--- MythEngine Phase 30: Temporal Stability Field Analysis ---")

DATA_DIR = "data"
os.makedirs(DATA_DIR, exist_ok=True)

# Load previous phase data
summary29 = os.path.join(DATA_DIR, "phase29_summary.csv")
if not os.path.exists(summary29):
    print("ERROR: Missing phase29_summary.csv")
    sys.exit(1)

phase29 = pd.read_csv(summary29)
if "causal_reconstruction_index" not in phase29:
    print("ERROR: Missing CRI in phase29_summary.csv")
    sys.exit(2)

CRI = phase29["causal_reconstruction_index"].iloc[-1]
print(f"Loaded CRI from Phase 29: {CRI:.4f}")

# Load metrics for time stability check
metrics_path = os.path.join(DATA_DIR, "metrics.csv")
if not os.path.exists(metrics_path):
    print("ERROR: Missing metrics.csv")
    sys.exit(3)

metrics = pd.read_csv(metrics_path)
metrics.columns = [c.lower() for c in metrics.columns]
metrics = metrics.sort_values("cycle").reset_index(drop=True)

# Compute temporal windows
window = max(10, len(metrics)//10)
step = max(5, window//3)
cycles = metrics["cycle"].values
phi = metrics["coherence"].values
eps = metrics["epsilon"].values

temporal_CRI = []
windows = []
for i in range(0, len(metrics)-window, step):
    phi_win = phi[i:i+window]
    eps_win = eps[i:i+window]
    if len(phi_win) == len(eps_win) and len(phi_win) > 5:
        mi = np.histogram2d(phi_win, eps_win, bins=24)[0]
        p = mi / np.sum(mi)
        px, py = np.sum(p, axis=1), np.sum(p, axis=0)
        mask = p > 0
        mi_val = np.sum(p[mask]*np.log2(p[mask]/(px[:,None]*py[None,:])[mask]))
        mi_norm = mi_val / np.log2(24)
        causal_shift = np.mean(np.diff(phi_win)) - np.mean(np.diff(eps_win))
        CRI_local = (mi_norm * (1 + np.tanh(causal_shift))) / 2
        temporal_CRI.append(CRI_local)
        windows.append(cycles[i])
    else:
        temporal_CRI.append(np.nan)
        windows.append(cycles[i])

temporal_CRI = np.array(temporal_CRI)
TSI = np.nanmean(temporal_CRI)
CD  = np.nanstd(temporal_CRI)

print(f"Temporal Stability Index (TSI): {TSI:.4f}")
print(f"Causal Drift (CD):              {CD:.4f}")

summary = pd.DataFrame({
    "temporal_stability_index":[TSI],
    "causal_drift":[CD],
    "source_cri":[CRI]
})
out_csv = os.path.join(DATA_DIR,"phase30_summary.csv")
summary.to_csv(out_csv,index=False)

plt.figure(figsize=(10,6))
plt.plot(windows, temporal_CRI, lw=2, color="purple", alpha=0.8)
plt.xlabel("Cycle")
plt.ylabel("Local CRI(t)")
plt.title("Phase 30 - Temporal Stability Field")
plt.grid(True, ls="--", alpha=0.3)
plt.tight_layout()
plot_path = os.path.join(DATA_DIR,"phase30_temporal_field.png")
plt.savefig(plot_path, dpi=150)
plt.close()

print(f"\nPhase 30 summary saved → {out_csv}")
print(f"Temporal field plot saved → {plot_path}")
print("\n--- Phase 30 complete ---")
