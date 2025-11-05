# phase23_resonant_geometry.py — MythEngine Phase 23: Resonant Geometry Reconstruction
import pandas as pd, numpy as np, matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  # noqa: F401

print("\n--- MythEngine Phase 23: Resonant Geometry Reconstruction ---")

# Load harmonic data
harmonics = pd.read_csv("data/phase22_harmonics.csv")

# Compute gradients (temporal)
harmonics["d_coh"] = harmonics["coherence"].diff()
harmonics["d_eff"] = harmonics["epsilon"].diff()
harmonics["gradient_magnitude"] = np.sqrt(harmonics["d_coh"]**2 + harmonics["d_eff"]**2)

# Define resonance intensity in 3D energy space
harmonics["resonant_field"] = harmonics["harmonic_energy"] * (1 - harmonics["gradient_magnitude"].fillna(0))

# Normalize for geometry mapping
for col in ["coherence", "epsilon", "resonant_field"]:
    minv, maxv = harmonics[col].min(), harmonics[col].max()
    if maxv > minv:
        harmonics[f"{col}_norm"] = (harmonics[col] - minv) / (maxv - minv)
    else:
        harmonics[f"{col}_norm"] = 0.0

# Compute geometry-level indices
resonance_density = harmonics["resonant_field"].mean()
resonant_balance = harmonics["harmonic_ratio"].mean() / (harmonics["phase_shift"].std() + 1e-9)
geometry_stability = (resonant_balance * resonance_density) / (1 + harmonics["gradient_magnitude"].mean())

print(f"Resonance Density         : {resonance_density:.4f}")
print(f"Resonant Balance Index    : {resonant_balance:.4f}")
print(f"Geometry Stability Metric : {geometry_stability:.4f}")

# Save geometry data
out_csv = "data/phase23_geometry_field.csv"
harmonics.to_csv(out_csv, index=False)

# Visualization
fig = plt.figure(figsize=(9,7))
ax = fig.add_subplot(111, projection="3d")
ax.scatter(
    harmonics["coherence_norm"],
    harmonics["epsilon_norm"],
    harmonics["resonant_field_norm"],
    c=harmonics["resonant_field_norm"], cmap="plasma", s=20, alpha=0.8
)
ax.set_xlabel("Coherence Φ (norm)")
ax.set_ylabel("Efficiency ε (norm)")
ax.set_zlabel("Resonant Field (norm)")
ax.set_title("MythEngine Phase 23 — Resonant Geometry Reconstruction")
plt.tight_layout()
out_img = "data/phase23_resonant_geometry.png"
plt.savefig(out_img, dpi=300)
plt.close()

print(f"\nResonant geometry field saved -> {out_csv}")
print(f"Resonant geometry map -> {out_img}")
print("\n--- Phase 23 complete ---")
