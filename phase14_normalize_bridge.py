import pandas as pd
import numpy as np
import os

hist = pd.read_csv("data/metrics.csv")
pred = pd.read_csv("data/phase13_predictions.csv")

# Lowercase columns
hist.columns = [c.lower() for c in hist.columns]
pred.columns = [c.lower() for c in pred.columns]

# Get last historical point
phi_last = hist["coherence"].iloc[-1]
eps_last = hist["epsilon"].iloc[-1]

# Compute correction factors based on first prediction
phi_factor = phi_last / pred["phi_pred"].iloc[0]
eps_factor = eps_last / pred["eps_pred"].iloc[0]

pred["phi_pred"] *= phi_factor
pred["eps_pred"] *= eps_factor

pred.to_csv("data/phase13_predictions_corrected.csv", index=False)
print(f"Applied renormalization factors: Φ×{phi_factor:.3f}, ε×{eps_factor:.3f}")
print("Corrected predictions saved → data/phase13_predictions_corrected.csv")
