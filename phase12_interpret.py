import pandas as pd
import numpy as np

print("\n--- MythEngine Phase 12: Interpretation & Inference ---")

df = pd.read_csv("data/metrics.csv")
cols = [c.lower() for c in df.columns]
df.columns = cols

# Basic checks
if not all(k in cols for k in ["cycle","coherence","epsilon","epsilon_gw"]):
    print("Missing expected columns in metrics.csv. Aborting.")
    exit()

# Compute correlations
corr_phi_eps = df["coherence"].corr(df["epsilon"])
corr_phi_gw  = df["coherence"].corr(df["epsilon_gw"])

# Detect regime shifts
diff = df["coherence"].diff()
sign_changes = np.sign(diff).diff().ne(0).sum()
trend = "declining" if diff.mean() < -0.001 else ("rising" if diff.mean() > 0.001 else "stable")

# Infer regime
if sign_changes > 10:
    regime = "oscillatory–adaptive"
elif trend == "declining":
    regime = "entropic decay"
elif trend == "rising":
    regime = "self-reinforcing"
else:
    regime = "metastable equilibrium"

# Summarize
print(f"Mean Coherence Φ : {df['coherence'].mean():.3f}")
print(f"Mean Efficiency ε : {df['epsilon'].mean():.1f}")
print(f"Mean ε_GW        : {df['epsilon_gw'].mean():.3f}")
print(f"Correlation(Φ,ε)  : {corr_phi_eps:.3f}")
print(f"Correlation(Φ,ε_GW): {corr_phi_gw:.3f}")
print(f"Regime Detected   : {regime}")
print(f"Trend Direction   : {trend}")
print(f"Oscillation Count : {sign_changes}")

# Optional save
summary = {
    "mean_phi": df["coherence"].mean(),
    "mean_eps": df["epsilon"].mean(),
    "corr_phi_eps": corr_phi_eps,
    "corr_phi_gw": corr_phi_gw,
    "trend": trend,
    "regime": regime,
    "oscillations": int(sign_changes)
}
pd.DataFrame([summary]).to_csv("data/phase12_summary.csv", index=False)
print("\nInterpretation written to data/phase12_summary.csv")
