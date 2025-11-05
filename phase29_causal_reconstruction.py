# -*- coding: utf-8 -*-
# -*- coding: utf-8 -*-
import os, sys, numpy as np, pandas as pd, matplotlib
# phase29_causal_reconstruction.py
import os, sys, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

try: sys.stdout.reconfigure(encoding="utf-8")
except Exception: pass

print("\n--- MythEngine Phase 29: Causal Reconstruction ---")

DATA_DIR = "data"
os.makedirs(DATA_DIR, exist_ok=True)

def load_csv(name):
    path = os.path.join(DATA_DIR, name)
    if not os.path.exists(path):
        print(f"  - missing: {name}")
        return None
    df = pd.read_csv(path)
    df.columns = [c.lower() for c in df.columns]
    print(f"  - loaded: {name} ({len(df)} rows)")
    return df

metrics = load_csv("metrics.csv")
if metrics is None or "coherence" not in metrics or "epsilon" not in metrics:
    print("ERROR: metrics.csv must contain 'coherence' and 'epsilon'")
    sys.exit(2)

# --- normalize ---
metrics = metrics.sort_values("cycle").reset_index(drop=True)
phi = metrics["coherence"].values
eps = metrics["epsilon"].values
n = len(phi)

# --- compute lagged correlations up to ï¿½LAG_MAX ---
LAG_MAX = min(30, n//5)
lags = np.arange(-LAG_MAX, LAG_MAX+1)
corrs = []
for l in lags:
    if l > 0:
        x = phi[l:]
        y = eps[:n - l]
    elif l < 0:
        x = phi[:n + l]
        y = eps[-l:]
    else:
        x, y = phi, eps
    if len(x) > 1 and len(x) == len(y):
        corrs.append(np.corrcoef(x, y)[0, 1])
    else:
        corrs.append(np.nan)

# --- causal asymmetry (difference between positive & negative lag correlation) ---
corr_forward  = np.nanmean([c for l,c in zip(lags,corrs) if l>0])
corr_backward = np.nanmean([c for l,c in zip(lags,corrs) if l<0])
causal_bias   = corr_forward - corr_backward

# --- mutual information proxy via normalized cross entropy ---
def mutual_information(x, y, bins=32):
    from math import log2
    c_xy = np.histogram2d(x, y, bins=bins)[0]
    p_xy = c_xy / np.sum(c_xy)
    p_x = np.sum(p_xy, axis=1)
    p_y = np.sum(p_xy, axis=0)
    mask = p_xy > 0
    return np.sum(p_xy[mask] * np.log2(p_xy[mask]/(p_x[:,None]*p_y[None,:])[mask]))

mi = mutual_information(phi, eps)
mi_norm = mi / max(1e-9, np.log2(32))  # normalize to 0..1 scale

# --- combine into causal reconstruction index ---
CRI = (mi_norm * (1 + causal_bias)) / 2
print(f"Causal Bias (F?e): {causal_bias:.4f}")
print(f"Normalized MI(F,e): {mi_norm:.4f}")
print(f"Causal Reconstruction Index (CRI): {CRI:.4f}")

# --- save results ---
summary = pd.DataFrame({
    "causal_bias":[causal_bias],
    "mutual_information_norm":[mi_norm],
    "causal_reconstruction_index":[CRI]
})
out_csv = os.path.join(DATA_DIR,"phase29_summary.csv")
summary.to_csv(out_csv, index=False)

# --- plot ---
plt.figure(figsize=(10,6))
plt.subplot(2,1,1)
plt.plot(lags, corrs, lw=2, color="darkcyan")
plt.axvline(0, color="gray", ls="--")
plt.title("Lagged Correlation ï¿½ F(t) vs e(t+t)")
plt.xlabel("Lag (t)")
plt.ylabel("Correlation")

plt.subplot(2,1,2)
plt.scatter(phi, eps, alpha=0.6, color="teal", s=20)
plt.xlabel("F (coherence)")
plt.ylabel("e (epsilon)")
plt.title("Phase 29 ï¿½ Causal Reconstruction Map")

plt.tight_layout()
plot_path = os.path.join(DATA_DIR,"phase29_causality_map.png")
plt.savefig(plot_path, dpi=150)
plt.close()

print(f"\nPhase 29 summary saved ? {out_csv}")
print(f"Diagnostic plot saved ? {plot_path}")
print("\n--- Phase 29 complete ---")
