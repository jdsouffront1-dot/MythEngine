# phase28_entropy_reconcile.py
"""
Phase 28 — Entropy Reconciliation & Equilibrium Constant
- Defensive: checks for required artifacts, normalizes column names.
- Computes distributional (Shannon) entropy for coherence and epsilon,
  compares them, and computes an "Equilibrium Constant" and
  "Entropy Reconciliation Index".
- Writes numeric summary to data/phase28_summary.csv and a diagnostic PNG.
"""

import os, sys, math
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# make stdout utf-8 safe (prevents cp1252 errors on Windows consoles)
try:
    sys.stdout.reconfigure(encoding="utf-8")
except Exception:
    pass

DATA_DIR = "data"
os.makedirs(DATA_DIR, exist_ok=True)

def load_csv_safe(path):
    if not os.path.exists(path):
        print(f"  - missing: {os.path.basename(path)}")
        return None
    try:
        df = pd.read_csv(path)
        df.columns = [c.lower() for c in df.columns]
        print(f"  - loaded: {os.path.basename(path)} ({len(df)} rows)")
        return df
    except Exception as e:
        print(f"  ! error loading {path}: {e}")
        return None

print("\\n--- MythEngine Phase 28: Entropy Reconciliation & Equilibrium Constant ---\\n")

# Required artifacts (defensive)
metrics = load_csv_safe(os.path.join(DATA_DIR, "metrics.csv"))
rec24  = load_csv_safe(os.path.join(DATA_DIR, "phase24_recursive_field.csv"))
rec25  = load_csv_safe(os.path.join(DATA_DIR, "phase25_summary.csv"))
phase20 = load_csv_safe(os.path.join(DATA_DIR, "phase20_summary.csv"))

# Basic failure if no metrics
if metrics is None:
    print("ERROR: metrics.csv required. Aborting Phase 28.")
    sys.exit(2)

# canonical columns to analyze
for expected in ["coherence","epsilon","cycle"]:
    if expected not in metrics.columns:
        # try fuzzy fallback
        candidates = [c for c in metrics.columns if expected in c]
        if candidates:
            metrics = metrics.rename(columns={candidates[0]: expected})
        else:
            print(f"ERROR: metrics missing column '{expected}'. Aborting.")
            sys.exit(3)

# --- helper: Shannon entropy of a 1D numeric array (histogram-based) ---
def shannon_entropy(arr, bins=64):
    arr = np.asarray(arr, dtype=float)
    arr = arr[~np.isnan(arr)]
    if arr.size == 0:
        return float('nan')
    # histogram over bins (adaptive range)
    h, edges = np.histogram(arr, bins=bins, density=True)
    # convert to probability mass over bin widths
    # density=True gives pdf; convert pdf->pmf by multiplying by bin width
    widths = np.diff(edges)
    pmf = h * widths
    pmf = pmf[pmf > 0.0]
    if pmf.size == 0:
        return 0.0
    H = -np.sum(pmf * np.log2(pmf))
    return float(H)

# compute entropies for coherence and epsilon
H_phi = shannon_entropy(metrics["coherence"].values, bins=min(64, max(8, len(metrics)//3)))
H_eps = shannon_entropy(metrics["epsilon"].values, bins=min(64, max(8, len(metrics)//3)))

# normalized entropies (0..1) using max possible ~ log2(bins)
bins_used = min(64, max(8, len(metrics)//3))
H_max = math.log2(bins_used)
H_phi_norm = H_phi / H_max if not math.isnan(H_phi) else float('nan')
H_eps_norm = H_eps / H_max if not math.isnan(H_eps) else float('nan')

# entropy difference and reconciliation
delta_H = H_phi - H_eps
delta_H_abs = abs(delta_H)
# Equilibrium constant-like measure (bounded 0..1): closer entropies -> closer to 1
K_eq = math.exp(-delta_H_abs) if not math.isnan(delta_H_abs) else float('nan')
# Entropy Reconciliation Index (ERI) - combines K_eq with Phase20 closure if present
phase20_closure = float(phase20["closure_index"].iloc[0]) if (phase20 is not None and "closure_index" in phase20.columns) else float('nan')
# ERI: weighted product (robust to missing phase20)
if not math.isnan(phase20_closure):
    ERI = K_eq * (0.8 * phase20_closure + 0.2)
else:
    ERI = K_eq

# If recursive field present, compare distributions there too
if rec24 is not None and "resonant_field_smooth" in rec24.columns:
    H_rf = shannon_entropy(rec24["resonant_field_smooth"].values, bins=32)
else:
    H_rf = float('nan')

# Save numeric summary
summary = {
    "H_phi": H_phi,
    "H_eps": H_eps,
    "H_phi_norm": H_phi_norm,
    "H_eps_norm": H_eps_norm,
    "delta_H": delta_H,
    "K_eq": K_eq,
    "phase20_closure": phase20_closure,
    "entropy_reconciliation_index": ERI,
    "H_resonant_field": H_rf
}
summary_df = pd.DataFrame([summary])
summary_path = os.path.join(DATA_DIR, "phase28_summary.csv")
summary_df.to_csv(summary_path, index=False)

# Diagnostic plot: entropies and small histogram overlay
plt.figure(figsize=(9,6))
plt.subplot(2,1,1)
plt.title("Phase 28 — Entropy Reconciliation")
plt.bar([0,1],[H_phi_norm if not math.isnan(H_phi_norm) else 0, H_eps_norm if not math.isnan(H_eps_norm) else 0], color=["#1f77b4","#ff7f0e"], width=0.6)
plt.xticks([0,1],["Φ (coherence)","ε (epsilon)"])
plt.ylabel("Normalized Shannon Entropy")

plt.subplot(2,1,2)
# show small histograms side-by-side for visual comparison
n = min(200, len(metrics))
plt.hist(metrics["coherence"].dropna().values, bins=32, alpha=0.6, label="coherence", density=True)
plt.hist(metrics["epsilon"].dropna().values, bins=32, alpha=0.5, label="epsilon", density=True)
plt.legend(loc="best")
plt.xlabel("Value")
plt.ylabel("PDF")
plt.tight_layout()
plot_path = os.path.join(DATA_DIR, "phase28_entropy_diag.png")
plt.savefig(plot_path, dpi=150)
plt.close()

print("Phase 28 summary saved to:", summary_path)
print("Diagnostic plot saved to:", plot_path)
print("\\nKey numbers:")
for k,v in summary.items():
    print(f"  - {k:28s} : {v}")
print("\\n--- Phase 28 complete ---")
