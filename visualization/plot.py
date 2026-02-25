"""Plot functions with their AD-computed derivatives up to third order."""

import csv
import glob
import os
import matplotlib.pyplot as plt
import matplotlib
matplotlib.rcParams.update({"font.size": 11})

LABELS = {
    "polynomial": "x⁴ − 3x² + 2",
    "sinusoidal": "sin(x)",
    "exponential": "exp(x)",
    "composite": "exp(−x²)",
}


def main():
    data_dir = "visualization/data"
    csv_files = sorted(glob.glob(os.path.join(data_dir, "*.csv")))

    if not csv_files:
        print(f"No CSV files found in {data_dir}/")
        return

    for path in csv_files:
        name = os.path.splitext(os.path.basename(path))[0]
        xs, fs, d1s, d2s, d3s = [], [], [], [], []
        with open(path) as f:
            reader = csv.DictReader(f)
            for row in reader:
                xs.append(float(row["x"]))
                fs.append(float(row["f(x)"]))
                d1s.append(float(row["f'(x)"]))
                d2s.append(float(row["f''(x)"]))
                d3s.append(float(row["f'''(x)"]))

        label = LABELS.get(name, name)
        fig, ax = plt.subplots(figsize=(7, 4.5))
        ax.plot(xs, fs, label=f"f(x) = {label}", linewidth=2)
        ax.plot(xs, d1s, label="f′(x)", linewidth=2, linestyle="--")
        ax.plot(xs, d2s, label="f″(x)", linewidth=2, linestyle="-.")
        ax.plot(xs, d3s, label="f‴(x)", linewidth=2, linestyle=":")
        ax.set_xlabel("x")
        ax.set_title(f"Higher-Order Derivatives of {label}")
        ax.legend(fontsize=9)
        ax.grid(True, alpha=0.3)
        plt.tight_layout()
        out = f"visualization/images/{name}.png"
        fig.savefig(out, dpi=150, bbox_inches="tight")
        plt.close(fig)
        print(f"Saved {out}")


if __name__ == "__main__":
    main()
