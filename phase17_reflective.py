import pandas as pd
import numpy as np
from sklearn.preprocessing import PolynomialFeatures
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_absolute_error

print("\n--- MythEngine Phase 17: Nonlinear Reflective Correction ---")

df = pd.read_csv("data/metrics.csv")
X = df["epsilon"].values.reshape(-1,1)
y = df["coherence"].values

# Quadratic features
poly = PolynomialFeatures(degree=2)
X_poly = poly.fit_transform(X)

model = Ridge(alpha=1e-3).fit(X_poly, y)
pred_phi = model.predict(X_poly)

mae = mean_absolute_error(y, pred_phi)
corr = np.corrcoef(y, pred_phi)[0,1]

print(f"Mean Absolute Error: {mae:.4f}")
print(f"Correlation(Φ, Φ̂): {corr:.4f}")
print(f"Model coefficients: {model.coef_}")

df["phi_poly"] = pred_phi
df.to_csv("data/phase17_reflective.csv", index=False)
print("Nonlinear correction written to data/phase17_reflective.csv")
