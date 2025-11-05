import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

print("\n--- MythEngine Phase 13: Predictive Extrapolation ---")

# Load data and summary
metrics = pd.read_csv("data/metrics.csv")
summary = pd.read_csv("data/phase12_summary.csv")

# Basic linear model fit: Φ and ε vs Cycle
x = metrics["cycle"]
phi = metrics["coherence"]
eps = metrics["epsilon"]

coeff_phi = np.polyfit(x, phi, 1)
coeff_eps = np.polyfit(x, eps, 1)

# Predict next 50 cycles
future_cycles = np.arange(x.max()+10, x.max()+60, 10)
phi_pred = np.polyval(coeff_phi, future_cycles)
eps_pred = np.polyval(coeff_eps, future_cycles)

print(f"Trend Φ slope: {coeff_phi[0]:.5f}")
print(f"Trend ε slope: {coeff_eps[0]:.5f}")

# Plot
plt.figure(figsize=(10,6))
plt.plot(x, phi, 'b-', label='Φ (past)')
plt.plot(x, eps/eps.max(), 'r--', label='ε normalized (past)')
plt.plot(future_cycles, phi_pred, 'bo--', label='Φ (forecast)')
plt.plot(future_cycles, eps_pred/eps.max(), 'ro--', label='ε norm (forecast)')
plt.xlabel("Cycle")
plt.ylabel("Normalized values")
plt.title("MythEngine Phase 13 – Predictive Extrapolation")
plt.legend()
plt.tight_layout()
plt.show()

# Save predictions
pred_df = pd.DataFrame({"cycle": future_cycles, "phi_pred": phi_pred, "eps_pred": eps_pred})
pred_df.to_csv("data/phase13_predictions.csv", index=False)
print("\nPredictions written to data/phase13_predictions.csv")
