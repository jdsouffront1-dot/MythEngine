# phase24_recursive_attractor.py
# Phase 24: Metastructural Recursion & Harmonic Folding
# Defensive, reproducible script to take Phase 23 geometry and
# produce a recursively-smoothed attractor, convergence metrics,
# and diagnostics saved to data/phase24_*.csv/png.

import os, sys, math
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")   # non-interactive backend
import matplotlib.pyplot as plt

DATA_DIR = "data"
os.makedirs(DATA_DIR, exist_ok=True)

print("")
print("--- MythEngine Phase 24: Metastructural Recursion & Harmonic Folding ---")
# Load geometry field
in_path = os.path.join(DATA_DIR, "phase23_geometry_field.csv")
if not os.path.exists(in_path):
    print("ERROR: required file missing:", in_path)
    print("Phase 24 aborted.")
    sys.exit(2)

try:
    df = pd.read_csv(in_path)
except Exception as e:
    print("ERROR reading file:", e)
    sys.exit(3)

# Ensure required columns exist
cols = [c.lower() for c in df.columns]
df.columns = cols
required = ["cycle", "coherence", "epsilon", "resonant_field", "resonant_field_norm"]
if not all(r in df.columns for r in required):
    # try fallback names
    if "resonant_field_norm" not in df.columns and "resonant_field" in df.columns:
        # normalize field to [0,1]
        rf = df["resonant_field"].astype(float)
        mn, mx = rf.min(), rf.max()
        if mx > mn:
            df["resonant_field_norm"] = (rf - mn) / (mx - mn)
        else:
            df["resonant_field_norm"] = 0.0
    # If still missing critical columns, abort cleanly
    missing = [r for r in required if r not in df.columns]
    if missing:
        print("ERROR: Missing expected columns:", missing)
        print("Phase 24 aborted.")
        sys.exit(4)

# Work on a copy of the normalized resonant field
field = df["resonant_field_norm"].fillna(0.0).astype(float).values.copy()
n = len(field)
print("Loaded geometry rows:", n)

# Parameters for recursion / smoothing
max_iter = 2000
tol = 1e-6
alpha = 0.5           # relaxation toward neighbor average
neighbor_window = 3   # smoothing window radius

# Helper: neighbor-average convolution (1D)
def neighbor_avg(arr, w):
    # simple symmetric window, normalized
    k = 2*w + 1
    out = np.zeros_like(arr)
    for i in range(len(arr)):
        lo = max(0, i-w)
        hi = min(len(arr), i+w+1)
        out[i] = arr[lo:hi].mean()
    return out

# Iterative recursive smoothing with convergence check
prev = field.copy()
for it in range(1, max_iter+1):
    avg = neighbor_avg(prev, neighbor_window)
    # relaxation step: move toward neighbor average
    new = (1.0 - alpha) * prev + alpha * avg
    # optional nonlinearity: re-weight by resonance sign to encourage folding
    # use small nonlinearity to keep stability
    new = np.tanh(2.0 * (new - 0.5)) * 0.5 + 0.5
    diff = np.linalg.norm(new - prev) / (math.sqrt(n) + 1e-12)
    prev = new
    if diff < tol:
        converged_iter = it
        break
else:
    converged_iter = max_iter

smoothed = prev.copy()

# Compute diagnostics
orig_mean = float(np.mean(field))
sm_mean = float(np.mean(smoothed))
orig_var = float(np.var(field))
sm_var = float(np.var(smoothed))
final_change = float(np.linalg.norm(smoothed - field) / (math.sqrt(n) + 1e-12))

# Recursive Equilibrium Index (REI): higher -> better folded equilibrium
# Combine: reduction in variance (improvement), inverse mean gradient, and convergence speed
var_reduction = (orig_var - sm_var) if (orig_var + 1e-12) != 0 else 0.0
norm_var_reduction = var_reduction / (abs(orig_var) + 1e-12)
mean_grad = float(np.mean(np.abs(np.gradient(smoothed))))
speed_factor = 1.0 - (converged_iter / float(max_iter))
rei = 0.0
# Build bounded REI in [0,1]
try:
    rei_raw = (0.6 * max(0.0, norm_var_reduction)) + (0.3 * (1.0 / (1.0 + mean_grad))) + (0.1 * max(0.0, speed_factor))
    rei = max(0.0, min(1.0, rei_raw))
except Exception:
    rei = 0.0

# Attach results back to dataframe for saving
df_out = df.copy()
df_out["resonant_field_smooth"] = smoothed
df_out["resonant_field_delta"] = df_out["resonant_field_norm"].fillna(0.0) - df_out["resonant_field_smooth"]
df_out["rei"] = rei

# Save CSV
out_csv = os.path.join(DATA_DIR, "phase24_recursive_field.csv")
df_out.to_csv(out_csv, index=False)

# Save summary
summary = {
    "rows": n,
    "orig_mean": orig_mean,
    "sm_mean": sm_mean,
    "orig_var": orig_var,
    "sm_var": sm_var,
    "var_reduction_norm": norm_var_reduction,
    "mean_gradient_final": mean_grad,
    "converged_iter": int(converged_iter),
    "final_change": final_change,
    "recursive_equilibrium_index": float(rei)
}
summary_df = pd.DataFrame([summary])
summary_csv = os.path.join(DATA_DIR, "phase24_summary.csv")
summary_df.to_csv(summary_csv, index=False)

# Plotting diagnostics: original vs smoothed field and attractor (2D & 3D)
# 1) Field before/after over cycles
plt.figure(figsize=(10,4))
plt.plot(df_out["cycle"], df_out["resonant_field_norm"], label="orig_field_norm", linewidth=1)
plt.plot(df_out["cycle"], df_out["resonant_field_smooth"], label="smoothed_field", linewidth=1.5)
plt.xlabel("cycle")
plt.ylabel("resonant_field (norm)")
plt.title("Phase 24: Resonant Field - original vs smoothed")
plt.legend()
plt.tight_layout()
plot1 = os.path.join(DATA_DIR, "phase24_field_compare.png")
plt.savefig(plot1, dpi=150)
plt.close()

# 2) Attractor view: cycle vs coherence vs smoothed resonant field (3D)
# Normalize coherence for plotting if needed
coh = df_out["coherence"].astype(float).fillna(0.0).values
# If coherence range is small, scale for visualization
coh_min, coh_max = coh.min(), coh.max()
if coh_max > coh_min:
    coh_norm = (coh - coh_min) / (coh_max - coh_min)
else:
    coh_norm = coh
eps = df_out["epsilon"].astype(float).fillna(0.0).values
fig = plt.figure(figsize=(9,7))
ax = fig.add_subplot(111, projection='3d')
ax.plot(df_out["cycle"], eps, df_out["resonant_field_smooth"], color='cyan', lw=1.3)
ax.set_xlabel("cycle")
ax.set_ylabel("epsilon")
ax.set_zlabel("resonant_field_smooth")
ax.set_title("Phase 24: Recursive Attractor (cycle - epsilon - resonant_field)")
plt.tight_layout()
plot2 = os.path.join(DATA_DIR, "phase24_attractor.png")
plt.savefig(plot2, dpi=180)
plt.close()

# Console-safe ASCII report
print("")
print("Phase 24 completed. Summary:")
for k, v in summary.items():
    if isinstance(v, float):
        print("  - {0:30s} : {1:.6f}".format(str(k), float(v)))
    else:
        print("  - {0:30s} : {1}".format(str(k), v))
print("")
print("Saved files:")
print("  - " + out_csv)
print("  - " + summary_csv)
print("  - " + plot1)
print("  - " + plot2)
print("")
print("--- End Phase 24 ---")
