import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

print("\n--- MythEngine Phase 18: Stabilization & Graphical Verification ---")

df = pd.read_csv("data/phase17_reflective.csv")

phi_true = df["coherence"]
phi_pred = df["phi_poly"]
eps = df["epsilon"]

# Residual analysis
error = phi_true - phi_pred
mean_err = np.mean(error)
std_err  = np.std(error)

print(f"Mean residual     : {mean_err:.5f}")
print(f"Std of residuals  : {std_err:.5f}")

plt.figure(figsize=(8,6))
plt.scatter(eps, phi_true, s=12, label='True Φ', alpha=0.7)
plt.plot(eps, phi_pred, color='gold', linewidth=2, label='Φ̂ (Nonlinear Model)')
plt.title('Phase 18 – Reflective Equilibrium')
plt.xlabel('ε (Entropy)')
plt.ylabel('Φ (Coherence)')
plt.legend()
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('data/phase18_equilibrium.png', dpi=200)
print("Equilibrium plot saved as data/phase18_equilibrium.png")
