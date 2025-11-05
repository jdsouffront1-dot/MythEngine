# phase21_synthesis.py  — MythEngine Phase 21: Emergent Synthesis / Genesis Map

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401

print("\n--- MythEngine Phase 21: Emergent Synthesis & Genesis Map ---")

# ─── Load datasets ───────────────────────────────────────────────
metrics = pd.read_csv("data/metrics.csv")
validation = pd.read_csv("data/phase15_validation.csv")
projection = pd.read_csv("data/phase19_projection.csv")
closure = pd.read_csv("data/phase20_summary.csv")

# Normalize headers
metrics.columns = [c.lower() for c in metrics.columns]
validation.columns = [c.lower() for c in validation.columns]
projection.columns = [c.lower() for c in projection.columns]

# ─── Merge core data ─────────────────────────────────────────────
combined = metrics.copy()
if "phi_reconstructed" in validation.columns:
    combined["phi_reconstructed"] = validation["phi_reconstructed"]
combined = pd.concat([combined, projection], ignore_index=True)

# ─── Derive emergent quantities ──────────────────────────────────
combined["dphi"] = combined["coherence"].diff()
combined["deps"] = combined["epsilon"].diff()
combined["entropy_flux"] = np.abs(combined["dphi"] / (combined["deps"] + 1e-9))
combined["closure_energy"] = combined["coherence"] * combined["epsilon"]

# ─── Compute synthesis metrics ───────────────────────────────────
mean_ce = combined["closure_energy"].mean()
std_ce = combined["closure_energy"].std(ddof=0)
genesis_ratio = float(mean_ce / std_ce) if std_ce > 0 else np.nan
entropy_balance = float(combined["entropy_flux"].mean())
stability_index = float(closure.get("closure_index", [np.nan])[0])

# ─── Save results BEFORE any printing ────────────────────────────
out_csv = "data/phase21_genesis_field.csv"
combined.to_csv(out_csv, index=False)

# Plot Genesis Map
fig = plt.figure(figsize=(10, 7))
ax = fig.add_subplot(111, projection="3d")
ax.plot(
    combined["cycle"],
    combined["epsilon"],
    combined["coherence"],
    color="cyan",
    lw=2,
)
ax.set_xlabel("Cycle (t)")
ax.set_ylabel("Efficiency (epsilon)")
ax.set_zlabel("Coherence (phi)")
ax.set_title("MythEngine Phase 21 – Genesis Map (phi-epsilon-t space)")
plt.tight_layout()
out_img = "data/phase21_genesis_map.png"
plt.savefig(out_img, dpi=300)
plt.close()

# ─── Print numeric summary safely (ASCII-only) ───────────────────
print(f"Genesis Ratio (Ephi/sigma_Ephi): {genesis_ratio:.3f}")
print(f"Mean Entropy Flux              : {entropy_balance:.3f}")
print(f"Stability Index (Phase20)      : {stability_index:.3f}")
print(f"\nGenesis Field saved → {out_csv}")
print(f"Genesis Map saved   → {out_img}")
print("\n--- Phase 21 complete ---")
