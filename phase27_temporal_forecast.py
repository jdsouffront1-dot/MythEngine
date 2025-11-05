# phase27_temporal_forecast.py - MythEngine Phase 27: Harmonic Temporal Forecast
import pandas as pd, numpy as np, matplotlib.pyplot as plt
import sys
sys.stdout.reconfigure(encoding="utf-8")

print("\n--- MythEngine Phase 27: Harmonic Temporal Forecast ---")

# ─── Load resonance field ────────────────────────────────────────────────
res = pd.read_csv("data/phase26_resonance_field.csv")

# ─── Prepare base arrays ─────────────────────────────────────────────────
res = res.dropna(subset=["resonance_flux"])
cycles = res["cycle"].to_numpy()
R = res["resonance_flux"].to_numpy()

# ─── Compute temporal derivative ─────────────────────────────────────────
dR = np.gradient(R)
alpha, beta = 0.8, 0.2

# ─── Forecast next 50 cycles ─────────────────────────────────────────────
future_cycles = np.arange(cycles[-1]+1, cycles[-1]+51)
R_pred = [R[-1]]
for _ in range(50):
    dR_new = alpha * dR[-1] + beta * np.sin(R_pred[-1])
    R_pred.append(R_pred[-1] + dR_new)
    dR = np.append(dR, dR_new)

# ─── Compute stability forecast ──────────────────────────────────────────
R_pred = np.array(R_pred)
S_pred = np.exp(-np.abs(np.diff(R_pred, prepend=R_pred[0])))

forecast_df = pd.DataFrame({
    "cycle": future_cycles,
    "predicted_resonance": R_pred[1:], 
    "stability_forecast": S_pred[1:]
})

# ─── Summary metrics ─────────────────────────────────────────────────────
mean_stability = forecast_df["stability_forecast"].mean()
stability_decay = 1 - mean_stability
print(f"Mean Forecast Stability      : {mean_stability:.4f}")
print(f"Projected Stability Decay     : {stability_decay:.4f}")

# ─── Save outputs ────────────────────────────────────────────────────────
out_csv = "data/phase27_forecast.csv"
forecast_df.to_csv(out_csv, index=False)
pd.DataFrame({
    "mean_stability":[mean_stability],
    "stability_decay":[stability_decay]
}).to_csv("data/phase27_summary.csv", index=False)

# ─── Visualization ───────────────────────────────────────────────────────
plt.figure(figsize=(9,6))
plt.plot(res["cycle"], res["resonance_flux"], color="violet", lw=1.6, label="Observed Resonance")
plt.plot(forecast_df["cycle"], forecast_df["predicted_resonance"], color="orange", lw=2.0, label="Predicted Resonance")
plt.fill_between(forecast_df["cycle"], forecast_df["stability_forecast"], alpha=0.2, color="gold", label="Stability Forecast")
plt.title("MythEngine Phase 27 - Temporal Harmonic Forecast")
plt.xlabel("Cycle")
plt.ylabel("Resonance / Stability")
plt.legend()
plt.tight_layout()
plt.savefig("data/phase27_forecast_map.png", dpi=300)
plt.close()

print(f"\nPhase 27 forecast saved → {out_csv}")
print("Phase 27 summary saved → data/phase27_summary.csv")
print("Phase 27 map saved → data/phase27_forecast_map.png")
print("\n--- Phase 27 complete ---")
