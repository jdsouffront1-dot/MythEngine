import pandas as pd
import numpy as np
from sklearn.linear_model import Ridge

print("\n--- MythEngine Phase 15: Validation & Reverse Inference (Ridge) ---")

df = pd.read_csv("data/metrics.csv")

X = df["epsilon"].values.reshape(-1,1)
y = df["coherence"].values

model = Ridge(alpha=1e-3).fit(X, y)
r2 = model.score(X, y)

print(f"Inverse correlation R²: {r2:.4f}")
print(f"Φ ≈ {model.coef_[0]:.6f} * ε + {model.intercept_:.3f}")

df["phi_reconstructed"] = model.predict(X)
df.to_csv("data/phase15_validation.csv", index=False)
print("Validation written to data/phase15_validation.csv")
