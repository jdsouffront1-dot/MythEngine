import pandas as pd, numpy as np, matplotlib.pyplot as plt
import sys
sys.stdout.reconfigure(encoding="utf-8")

print("\n--- MythEngine Phase 25: Recursive Coherence Equilibrium ---")

geo = pd.read_csv("data/phase23_geometry_field.csv")
recur = pd.read_csv("data/phase24_recursive_field.csv")

merged = pd.merge_asof(
    geo.sort_values("cycle"),
    recur.sort_values("cycle"),
    on="cycle",
    direction="nearest",
    suffixes=("_geo", "_rec")
)

merged["delta_phi"] = merged["coherence_geo"] - merged["coherence_rec"]
merged["delta_eps"] = merged["epsilon_geo"] - merged["epsilon_rec"]
merged["recursive_energy"] = np.sqrt(merged["delta_phi"]**2 + merged["delta_eps"]**2)
merged["stability_ratio"] = np.exp(-np.abs(merged["recursive_energy"]))

REI = merged["stability_ratio"].mean()
osc_var = merged["recursive_energy"].std()

print(f"Recursive Equilibrium Index (REI): {REI:.4f}")
print(f"Oscillatory Variance (RE):        {osc_var:.4f}")

out_csv = "data/phase25_equilibrium.csv"
merged.to_csv(out_csv, index=False)
pd.DataFrame({"recursive_equilibrium_index":[REI],"oscillatory_variance":[osc_var]}).to_csv("data/phase25_summary.csv",index=False)

plt.figure(figsize=(8,6))
plt.plot(merged["cycle"], merged["stability_ratio"], color="teal", lw=2)
plt.fill_between(merged["cycle"], merged["stability_ratio"], alpha=0.2, color="teal")
plt.title("MythEngine Phase 25 – Recursive Coherence Equilibrium")
plt.xlabel("Cycle"); plt.ylabel("Stability Ratio")
plt.tight_layout()
plt.savefig("data/phase25_equilibrium_map.png", dpi=300)
plt.close()

print("\nPhase 25 equilibrium data saved → data/phase25_equilibrium.csv")
print("Phase 25 summary saved → data/phase25_summary.csv")
print("Phase 25 map saved → data/phase25_equilibrium_map.png")
print("\n--- Phase 25 complete ---")
