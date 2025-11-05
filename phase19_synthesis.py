import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

print("\n--- MythEngine Phase 19: Temporal Synthesis ---")

df = pd.read_csv("data/phase17_reflective.csv")

cycles = df["cycle"].to_numpy()
phi_pred = df["phi_poly"].to_numpy()

phi_future = []
window = 5
for i in range(len(phi_pred), len(phi_pred)+10):
    recent = phi_pred[-window:]           # last 5 values
    next_phi = np.mean(recent) + 0.2*(recent[-1] - recent[0])
    phi_future.append(next_phi)
    phi_pred = np.append(phi_pred, next_phi)

# create new cycles continuing the old spacing
cycle_step = cycles[1] - cycles[0] if len(cycles) > 1 else 10
new_cycles = np.arange(cycles[-1] + cycle_step, cycles[-1] + cycle_step*(len(phi_future)+1), cycle_step)

projection = pd.DataFrame({"cycle": new_cycles, "phi_projected": phi_future})
projection.to_csv("data/phase19_projection.csv", index=False)

plt.figure(figsize=(8,5))
plt.plot(df["cycle"], df["coherence"], 'o-', label="Historical Φ", alpha=0.7)
plt.plot(projection["cycle"], projection["phi_projected"], 'x--', color="gold", label="Projected Φ")
plt.title("Phase 19 – Temporal Synthesis Layer")
plt.xlabel("Cycle")
plt.ylabel("Coherence Φ")
plt.legend()
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig("data/phase19_projection.png", dpi=200)
print("Projection written to data/phase19_projection.csv and plotted to phase19_projection.png")
