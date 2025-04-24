import pandas as pd

# Load the data
df = pd.read_csv("auto_exports_all.csv", parse_dates=["ref_date"])

# Clean up NaNs and Infs
df["pct_change"] = df["pct_change"].replace([float("inf"), float("-inf")], pd.NA)
df.dropna(subset=["pct_change"], inplace=True)

# Filter data from Oct 2024 onward
df_filtered = df[df["ref_date"] >= "2024-10-01"].copy()

# Sum total_exports by province and month
df_summary = (
    df_filtered.groupby(["ref_date", "geo"])
    .agg(total_exports=("total_exports", "sum"), pct_change=("pct_change", "mean"))
    .reset_index()
)

# Normalize for plotting in Manim
df_summary["x"] = df_summary["total_exports"] / df_summary["total_exports"].max() * 8  # Range ~[0, 8]
df_summary["y"] = df_summary["pct_change"].clip(-50, 50) / 50 * 3  # Range [-3, 3]

# Optional: scale size of star (use log to prevent huge size gaps)
import numpy as np
df_summary["scale"] = np.log1p(df_summary["total_exports"]) / np.log1p(df_summary["total_exports"].max()) * 1.5

# Save to CSV for Manim import
df_summary.to_csv("manim_auto_exports.csv", index=False)


# Group and filter for Oct 2024 and most recent
df_oct = df[df["ref_date"] == "2024-10-01"]
df_latest = df[df["ref_date"] == df["ref_date"].max()]

# Sum by province
exports_oct = df_oct.groupby("geo")["total_exports"].sum()
exports_latest = df_latest.groupby("geo")["total_exports"].sum()

# Join and calculate percent change
scatter_data = pd.DataFrame({
    "total_oct": exports_oct,
    "total_latest": exports_latest
})
scatter_data.dropna(inplace=True)
scatter_data["pct_change"] = ((scatter_data["total_latest"] - scatter_data["total_oct"]) / scatter_data["total_oct"]) * 100

