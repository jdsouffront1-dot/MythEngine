import pandas as pd
import matplotlib.pyplot as plt

print("\n--- MythEngine Phase 14: Unified Visualization ---")

hist = pd.read_csv("data/metrics.csv")
pred = pd.read_csv("data/phase13_predictions.csv")

fig, ax1 = plt.subplots(figsize=(10,6))
ax2 = ax1.twinx()

ax1.plot(hist["cycle"], hist["coherence"], color="tab:blue", label="Φ (Historical)")
ax1.plot(pred["cycle"], pred["phi_pred"], "--", color="cyan", label="Φ (Predicted)")
ax2.plot(hist["cycle"], hist["epsilon"], color="tab:red", alpha=0.6, label="ε (Historical)")
ax2.plot(pred["cycle"], pred["eps_pred"], "--", color="orange", label="ε (Predicted)")

ax1.set_xlabel("Cycle")
ax1.set_ylabel("Coherence Φ", color="tab:blue")
ax2.set_ylabel("Efficiency ε", color="tab:red")
plt.title("MythEngine Phase 14 – Unified Adaptive Timeline")

lines, labels = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines + lines2, labels + labels2, loc="best")

plt.tight_layout()
plt.show()
# === Quantitative bridge ===
import numpy as np

common = pred[pred["cycle"] <= hist["cycle"].max()]
merge = pd.merge(hist, common, on="cycle", suffixes=("_hist", "_pred"))

phi_gap = np.mean(np.abs(merge["coherence_hist"] - merge["phi_pred"]))
eps_gap = np.mean(np.abs(merge["epsilon_hist"] - merge["eps_pred"]))

print(f"\nContinuity gap Φ : {phi_gap:.4f}")
print(f"Continuity gap ε : {eps_gap:.4f}")

if phi_gap < 0.1 and eps_gap < 100:
    print("→ Temporal continuity achieved.")
else:
    print("→ Temporal discontinuity detected: re-tuning required.")
