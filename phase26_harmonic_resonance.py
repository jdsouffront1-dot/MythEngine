# phase26_harmonic_resonance.py - MythEngine Phase 26: Harmonic Resonance Mapping
import pandas as pd, numpy as np, matplotlib.pyplot as plt
import sys
sys.stdout.reconfigure(encoding="utf-8")

print("\n--- MythEngine Phase 26: Harmonic Resonance Mapping ---")

# ─── Load equilibrium data ─────────────────────────────────────────────
eq = pd.read_csv("data/phase25_equilibrium.csv")

# ─── Derive harmonic resonance fields ──────────────────────────────────
eq["grad_phi"] = eq["coherence_geo"].diff()
eq["grad_eps"] = eq["epsilon_geo"].diff()
eq["harmonic_amplitude"] = np.sqrt(eq["grad_phi"]**2 + eq["grad_eps"]**2)
eq["resonance_flux"] = np.sin(eq["harmonic_amplitude"]) * np.exp(-eq["harmonic_amplitude"]**2)
eq["alignment_index"] = np.cos(eq["harmonic_amplitude"])**2

# ─── Aggregate global metrics ──────────────────────────────────────────
resonance_mean = eq["resonance_flux"].mean()
alignment_mean = eq["alignment_index"].mean()
flux_var = eq["resonance_flux"].std()

print(f"Harmonic Resonance Mean     : {resonance_mean:.4f}")
print(f"Alignment Coherence Index   : {alignment_mean:.4f}")
print(f"Flux Variance σ(RΦ)         : {flux_var:.4f}")

# ─── Save datasets ─────────────────────────────────────────────────────
out_csv = "data/phase26_resonance_field.csv"
eq.to_csv(out_csv, index=False)
pd.DataFrame({
    "resonance_mean":[resonance_mean],
    "alignment_index":[alignment_mean],
    "flux_variance":[flux_var]
}).to_csv("data/phase26_summary.csv", index=False)

# ─── Visualization ────────────────────────────────────────────────────
plt.figure(figsize=(9,6))
plt.plot(eq["cycle"], eq["resonance_flux"], color="violet", lw=1.8, label="Resonance Flux")
plt.plot(eq["cycle"], eq["alignment_index"], color="limegreen", lw=1.2, label="Alignment Index")
plt.title("MythEngine Phase 26 - Harmonic Resonance Mapping")
plt.xlabel("Cycle")
plt.ylabel("Resonance / Alignment")
plt.legend()
plt.tight_layout()
plt.savefig("data/phase26_resonance_map.png", dpi=300)
plt.close()

print(f"\nPhase 26 resonance field saved → {out_csv}")
print("Phase 26 summary saved → data/phase26_summary.csv")
print("Phase 26 map saved → data/phase26_resonance_map.png")
print("\n--- Phase 26 complete ---")
