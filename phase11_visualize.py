import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# === MythEngine Phase 11 Visualization ===
print("\n--- MythEngine Phase 11: Visualization & Interpretation ---")

# Load metrics
df = pd.read_csv("data/metrics.csv")

# Ensure required columns exist
cols = [c.lower() for c in df.columns]
df.columns = cols

if not all(k in cols for k in ["cycle","coherence","epsilon"]):
    print("Missing columns in metrics.csv. Aborting.")
    exit()

# Compute moving averages (for pattern recognition)
df["coh_avg"] = df["coherence"].rolling(window=10, min_periods=1).mean()
df["eps_avg"] = df["epsilon"].rolling(window=10, min_periods=1).mean()

# Detect rebounds (where coherence rises after being below threshold)
rebounds = df[(df["coherence"] <= 0.11) & (df["coh_avg"].shift(-1) > df["coh_avg"])]

# === Plot ===
fig, ax1 = plt.subplots(figsize=(10,6))
ax2 = ax1.twinx()

ax1.plot(df["cycle"], df["coherence"], color="tab:blue", alpha=0.7, label="Φ (Coherence)")
ax1.plot(df["cycle"], df["coh_avg"], color="tab:cyan", linestyle="--", label="Φ avg")

ax2.plot(df["cycle"], df["epsilon"], color="tab:red", alpha=0.5, label="ε (Efficiency)")
ax2.plot(df["cycle"], df["eps_avg"], color="tab:orange", linestyle="--", label="ε avg")

# Mark rebounds
for _, row in rebounds.iterrows():
    ax1.axvline(x=row["cycle"], color="green", alpha=0.3)

ax1.set_xlabel("Cycle")
ax1.set_ylabel("Coherence Φ", color="tab:blue")
ax2.set_ylabel("Efficiency ε", color="tab:red")
plt.title("MythEngine Phase 11 — Adaptive Dynamics Visualization")

# Legend
lines, labels = ax1.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax1.legend(lines + lines2, labels + labels2, loc="upper right")

plt.tight_layout()
plt.show()

# === Summary ===
print("\n--- Phase 11 Summary ---")
print(f"Mean Φ: {df['coherence'].mean():.3f}")
print(f"Variance Φ: {df['coherence'].var():.4f}")
print(f"Rebound events detected: {len(rebounds)}")
print("Interpretation: " +
      ("System recovered adaptively." if len(rebounds) > 0 else
       "System reached a stable low-coherence basin."))
