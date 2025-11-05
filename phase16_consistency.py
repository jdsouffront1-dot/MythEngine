import pandas as pd
import numpy as np

print("\n--- MythEngine Phase 16: Self-Consistency & Ontological Closure ---")

df = pd.read_csv("data/phase15_validation.csv")

# Compute residuals between true and reconstructed Φ
df["phi_error"] = df["coherence"] - df["phi_reconstructed"]

mae = df["phi_error"].abs().mean()
corr = df["coherence"].corr(df["phi_reconstructed"])

print(f"Mean Absolute Error: {mae:.4f}")
print(f"Correlation(Φ, Φ̂): {corr:.4f}")

if corr > 0.8 and mae < 0.1:
    print("→ System achieves reflective stability: Φ mirrors Φ̂ (self-consistent).")
else:
    print("→ Reflective instability detected: further adaptation required.")

df.to_csv("data/phase16_consistency.csv", index=False)
print("Consistency written to data/phase16_consistency.csv")
