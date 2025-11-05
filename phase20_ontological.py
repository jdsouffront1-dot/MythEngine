"""
phase20_ontological.py
Phase 20: Ontological Stability / Closure
Reads continuity, validation, and projection artifacts, computes closure metrics,
produces a numeric "closure index" in [0,1], writes data/phase20_summary.csv and
saves a diagnostic plot to data/phase20_closure.png.
"""

import os
import sys
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

DATA_DIR = "data"
os.makedirs(DATA_DIR, exist_ok=True)

# File candidates (some phases may be absent; script is defensive)
files = {
    "continuity": os.path.join(DATA_DIR, "phase14_continuity_final.csv"),
    "validation": os.path.join(DATA_DIR, "phase15_validation.csv"),
    "projection": os.path.join(DATA_DIR, "phase19_projection.csv"),
    "metrics": os.path.join(DATA_DIR, "metrics.csv")
}

print("\\n--- MythEngine Phase 20: Ontological Stability / Closure ---\\n")

# Helper to try load CSV; returns None if missing
def load_df(path, lowercase_cols=True):
    if not os.path.exists(path):
        print(f"  - missing: {os.path.basename(path)}")
        return None
    try:
        df = pd.read_csv(path)
        if lowercase_cols:
            df.columns = [c.lower() for c in df.columns]
        print(f"  - loaded: {os.path.basename(path)} ({len(df)} rows)")
        return df
    except Exception as e:
        print(f"  ! error loading {path}: {e}")
        return None

continuity = load_df(files["continuity"])
validation = load_df(files["validation"])
projection = load_df(files["projection"])
metrics = load_df(files["metrics"])

# We'll build a merged frame where possible on 'cycle'
frames_for_merge = []
if metrics is not None:
    frames_for_merge.append(metrics[['cycle','coherence','epsilon']].rename(columns={'coherence':'coh_hist','epsilon':'eps_hist'}))
if continuity is not None:
    # continuity may already contain 'coherence' and predicted columns
    c = continuity.copy()
    # normalize names if they vary
    for col in ['coherence','phi_pred','phi_predicted','coherence_hist','coh_hist']:
        if col in c.columns:
            if col != 'coh_hist':
                c = c.rename(columns={col:'coh_hist'})
            break
    for col in ['eps_pred','epsilon_pred','eps_predicted','epsilon_predicted']:
        if col in c.columns:
            if col != 'eps_pred':
                c = c.rename(columns={col:'eps_pred'})
            break
    # if continuity holds historical values, use them; else keep as-is
    frames_for_merge.append(c[['cycle','coh_hist','eps_pred']].drop_duplicates(subset=['cycle']))
if projection is not None:
    # projection likely has: cycle, phi_pred, eps_pred
    p = projection.copy()
    # canonicalize
    if 'phi_pred' in p.columns:
        p = p.rename(columns={'phi_pred':'phi_proj'})
    if 'eps_pred' in p.columns:
        p = p.rename(columns={'eps_pred':'eps_proj'})
    frames_for_merge.append(p[['cycle','phi_proj','eps_proj']].drop_duplicates(subset=['cycle']))

# Merge all available frames on 'cycle'
if not frames_for_merge:
    print("No data frames available to compute closure. Exiting.")
    sys.exit(1)

from functools import reduce
merged = reduce(lambda a,b: pd.merge(a,b,on='cycle',how='outer'), frames_for_merge)

# Sort and fill small gaps
merged = merged.sort_values('cycle').reset_index(drop=True)

# Basic statistics and overlap detection
stats = {}
# Overlap counts
stats['rows_merged'] = len(merged)
stats['overlap_with_projection'] = int(merged['phi_proj'].notna().sum()) if 'phi_proj' in merged.columns else 0
stats['overlap_with_validation'] = int(validation.shape[0]) if validation is not None else 0

# Compute pairwise comparisons where possible
def mean_abs_rel_error(a,b):
    a = np.array(a)
    b = np.array(b)
    mask = np.isfinite(a) & np.isfinite(b)
    if mask.sum() == 0:
        return np.nan
    # avoid division by zero: normalize by mean absolute of reference (b)
    denom = np.mean(np.abs(b[mask]))
    if denom == 0:
        denom = 1.0
    return np.mean(np.abs(a[mask]-b[mask])) / denom

# 1) Predictive continuity: historical coh vs projected phi (where both exist)
if 'coh_hist' in merged.columns and 'phi_proj' in merged.columns:
    mask = merged['coh_hist'].notna() & merged['phi_proj'].notna()
    stats['n_overlap_pred_hist'] = int(mask.sum())
    stats['phi_gap_mean_abs'] = float(np.mean(np.abs(merged.loc[mask,'coh_hist'] - merged.loc[mask,'phi_proj']))) if mask.sum()>0 else float('nan')
    stats['phi_gap_rel'] = float(mean_abs_rel_error(merged.loc[mask,'phi_proj'], merged.loc[mask,'coh_hist'])) if mask.sum()>0 else float('nan')
else:
    stats['n_overlap_pred_hist'] = 0
    stats['phi_gap_mean_abs'] = float('nan')
    stats['phi_gap_rel'] = float('nan')

# 2) Epsilon continuity
if 'eps_hist' in merged.columns and 'eps_proj' in merged.columns:
    mask2 = merged['eps_hist'].notna() & merged['eps_proj'].notna()
    stats['n_overlap_eps'] = int(mask2.sum())
    stats['eps_gap_mean_abs'] = float(np.mean(np.abs(merged.loc[mask2,'eps_hist'] - merged.loc[mask2,'eps_proj']))) if mask2.sum()>0 else float('nan')
    stats['eps_gap_rel'] = float(mean_abs_rel_error(merged.loc[mask2,'eps_proj'], merged.loc[mask2,'eps_hist'])) if mask2.sum()>0 else float('nan')
else:
    stats['n_overlap_eps'] = 0
    stats['eps_gap_mean_abs'] = float('nan')
    stats['eps_gap_rel'] = float('nan')

# 3) Validation consistency: reconstructed phi vs historical
if validation is not None and 'phi_reconstructed' in validation.columns and 'coherence' in validation.columns:
    v = validation.rename(columns={'coherence':'coh_valid'})
    maskv = v['coh_valid'].notna() & v['phi_reconstructed'].notna()
    stats['validation_rows'] = int(maskv.sum())
    stats['val_mae'] = float(np.mean(np.abs(v.loc[maskv,'coh_valid'] - v.loc[maskv,'phi_reconstructed']))) if maskv.sum()>0 else float('nan')
    stats['val_corr'] = float(np.corrcoef(v.loc[maskv,'coh_valid'], v.loc[maskv,'phi_reconstructed'])[0,1]) if maskv.sum()>1 else float('nan')
else:
    stats['validation_rows'] = 0
    stats['val_mae'] = float('nan')
    stats['val_corr'] = float('nan')

# 4) Long-term drift: fit linear slope of historical coherence (if present)
if 'coh_hist' in merged.columns and merged['coh_hist'].notna().sum() >= 3:
    sub = merged.dropna(subset=['cycle','coh_hist'])
    slope, intercept = np.polyfit(sub['cycle'].astype(float), sub['coh_hist'].astype(float), 1)
    stats['drift_slope'] = float(slope)
    stats['drift_intercept'] = float(intercept)
else:
    stats['drift_slope'] = float('nan')
    stats['drift_intercept'] = float('nan')

# closure index computation (simple, interpretable)
# We'll combine three normalized error measures:
#  A = normalized phi gap (phi_gap_rel)
#  B = normalized eps gap  (eps_gap_rel)
#  C = 1 - val_corr  (closer to 0 is good)
A = stats.get('phi_gap_rel', float('nan'))
B = stats.get('eps_gap_rel', float('nan'))
C = 1.0 - stats.get('val_corr', float('nan')) if not math.isnan(stats.get('val_corr', float('nan'))) else float('nan')

# convert NaN to safe defaults: treat missing components as neutral (no penalty)
def safe_norm(x):
    return x if (x is not None and not math.isnan(x)) else 0.0

A_s = safe_norm(A)
B_s = safe_norm(B)
C_s = safe_norm(C)

# Weighted sum penalty then convert to closure index in [0,1]
wA, wB, wC = 0.4, 0.35, 0.25
penalty = (wA * A_s) + (wB * B_s) + (wC * C_s)
closure_index = max(0.0, min(1.0, 1.0 - penalty))  # higher = better closure

stats['closure_index'] = float(closure_index)

# Write numeric summary
summary = pd.DataFrame([stats])
summary_path = os.path.join(DATA_DIR, "phase20_summary.csv")
summary.to_csv(summary_path, index=False)

# Create a diagnostic plot
plt.figure(figsize=(10,6))
ax = plt.gca()
if 'coh_hist' in merged.columns:
    ax.plot(merged['cycle'], merged['coh_hist'], label='Coherence (hist)', linewidth=2, alpha=0.9)
if 'phi_proj' in merged.columns:
    ax.plot(merged['cycle'], merged['phi_proj'], label='Phi (proj)', linestyle='--', alpha=0.8)
if 'phi_reconstructed' in validation.columns if (validation is not None) else False:
    # try to plot validation's last values aligned by cycle if cycle present
    if 'cycle' in validation.columns:
        ax.plot(validation['cycle'], validation['phi_reconstructed'], label='Phi (recon)', linestyle=':', alpha=0.8)

ax.set_xlabel('Cycle')
ax.set_ylabel('Coherence / Phi')
ax.set_title(f"MythEngine Phase 20 Closure (index={closure_index:.4f})")
ax.legend(loc='best')
plt.grid(True)
plot_path = os.path.join(DATA_DIR, "phase20_closure.png")
plt.tight_layout()
plt.savefig(plot_path, dpi=150)
plt.close()

# Friendly console output
print("Phase 20 summary saved to:", summary_path)
print("Diagnostic plot saved to:", plot_path)
print("\\nSummary (key fields):")
for k,v in stats.items():
    try:
        print(f"  {k:25s} : {v}")
    except:
        print(f"  {k:25s} : (unprintable)")

print("\\n--- Phase 20 complete ---\\n")
