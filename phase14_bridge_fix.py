import pandas as pd
import numpy as np
import os

print("\n--- MythEngine Phase 14: Continuity Bridge (Direct Column Mode) ---")

hist_path = "data/metrics.csv"
pred_path = "data/phase13_predictions_corrected.csv"

if not os.path.exists(hist_path) or not os.path.exists(pred_path):
    print("Missing required data files.")
    exit()

hist = pd.read_csv(hist_path)
pred = pd.read_csv(pred_path)

# Normalize headers
hist.columns = [c.lower() for c in hist.columns]
pred.columns = [c.lower() for c in pred.columns]

# Align cycle overlap
max_hist = hist["cycle"].max()
pred["cycle"] = pred["cycle"].astype(float)
if pred["cycle"].min() > max_hist:
    pred["cycle"] = pred["cycle"] - pred["cycle"].min() + max_hist

merge = pd.merge(hist, pred, on="cycle", how="inner")
print(f"Merged {len(merge)} overlapping cycles.")

# Compute continuity gaps directly
phi_gap = np.mean(np.abs(merge["coherence"] - merge["phi_pred"]))
eps_gap = np.mean(np.abs(merge["epsilon"] - merge["eps_pred"]))

print(f"Continuity gap F : {phi_gap:.4f}")
print(f"Continuity gap e : {eps_gap:.4f}")

if phi_gap < 0.1 and eps_gap < 100:
    print("? Temporal continuity restored.")
else:
    print("? Residual misalignment remains (tune coefficients).")

merge.to_csv("data/phase14_continuity_final.csv", index=False)
print("Final continuity dataset written to data/phase14_continuity_final.csv")
