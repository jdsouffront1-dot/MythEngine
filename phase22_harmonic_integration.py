# phase22_harmonic_integration.py — MythEngine Phase 22: Harmonic Integration Thresholds (ASCII-safe)
import pandas as pd, numpy as np, matplotlib.pyplot as plt

print("\n--- MythEngine Phase 22: Harmonic Integration Thresholds ---")

# Load Genesis Field
genesis = pd.read_csv("data/phase21_genesis_field.csv")

# Derive harmonic properties
genesis["dphi"] = genesis["coherence"].diff()
genesis["deps"] = genesis["epsilon"].diff()
genesis["phase_shift"] = np.arctan2(genesis["dphi"], genesis["deps"])
genesis["harmonic_ratio"] = np.cos(genesis["phase_shift"])**2
genesis["resonance_intensity"] = np.exp(-np.abs(genesis["phase_shift"]))
genesis["harmonic_energy"] = genesis["harmonic_ratio"] * genesis["closure_energy"]

# Compute summary indices
harmonic_ratio_mean = genesis["harmonic_ratio"].mean()
resonance_index = genesis["resonance_intensity"].mean()
phase_shift_std = genesis["phase_shift"].std()

print(f"Harmonic Ratio (cos^2 dPhi): {harmonic_ratio_mean:.4f}")
print(f"Resonance Intensity (e^-|dPhi|): {resonance_index:.4f}")
print(f"Phase Shift Std (sigma dPhi): {phase_shift_std:.4f}")

# Save data
out_csv = "data/phase22_harmonics.csv"
genesis.to_csv(out_csv, index=False)

# Visualization
plt.figure(figsize=(9,6))
plt.scatter(genesis["cycle"], genesis["harmonic_ratio"], s=15, c="cyan", label="Harmonic ratio")
plt.plot(genesis["cycle"], genesis["resonance_intensity"], c="magenta", lw=1.5, label="Resonance intensity")
plt.title("MythEngine Phase 22 - Harmonic Integration Thresholds")
plt.xlabel("Cycle (t)")
plt.ylabel("Normalized Harmonic Quantities")
plt.legend()
plt.tight_layout()
out_img = "data/phase22_threshold_map.png"
plt.savefig(out_img, dpi=300)
plt.close()

print(f"\nHarmonics dataset saved -> {out_csv}")
print(f"Harmonic threshold map -> {out_img}")
print("\n--- Phase 22 complete ---")
