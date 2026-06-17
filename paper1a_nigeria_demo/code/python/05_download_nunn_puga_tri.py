"""
Script 05: Download Nunn & Puga TRI Grid and Aggregate to Nigeria LGAs
Paper 1a: Nigeria Demonetization — Instrument 1

Downloads the Nunn & Puga (2012) global 30 arc-second TRI grid directly
from diegopuga.org (~600 MB), clips to Nigeria, and computes mean TRI
per LGA using the GADM admin-2 shapefile.

Citation: Nunn, N. & Puga, D. (2012). Ruggedness: The Blessing of Bad
Geography in Africa. Review of Economics and Statistics, 94(1), 20-36.
Data: https://diegopuga.org/data/rugged/

Install: pip install requests geopandas numpy pandas tqdm
         (rasterio and rasterstats for zonal statistics)
         pip install rasterio rasterstats

Usage:
    python 05_download_nunn_puga_tri.py

Output: data/instruments/tri_lga_nigeria.csv
Runtime: ~15 min (10 min download + 5 min processing)
"""

import os
import struct
import zipfile
import math
import requests
import numpy as np
import pandas as pd
import geopandas as gpd
from pathlib import Path

ROOT  = Path(__file__).parents[2]
GADM  = ROOT / "data/raw/shapefiles/gadm_nigeria/gadm41_NGA_2.shp"
INST  = ROOT / "data/instruments"
OUT   = INST / "tri_lga_nigeria.csv"
INST.mkdir(exist_ok=True)

TRI_URL  = "https://diegopuga.org/data/rugged/tri.zip"
TRI_ZIP  = INST / "tri_global.zip"
TRI_TXT  = INST / "tri.txt"

# Nigeria bounding box (lon_min, lat_min, lon_max, lat_max)
NGA_BOUNDS = (2.5, 4.0, 14.8, 14.0)


# ── Step 1: Download ──────────────────────────────────────────────────────────
def download_tri():
    if TRI_ZIP.exists():
        print(f"ZIP already exists: {TRI_ZIP} — skipping download")
        return
    if TRI_TXT.exists():
        print(f"TRI text file already exists: {TRI_TXT} — skipping download")
        return

    print(f"Downloading TRI grid from {TRI_URL}")
    print("File size: ~600 MB — this will take 5-15 minutes depending on connection")

    try:
        from tqdm import tqdm
        use_tqdm = True
    except ImportError:
        use_tqdm = False

    response = requests.get(TRI_URL, stream=True, timeout=300)
    response.raise_for_status()
    total = int(response.headers.get("content-length", 0))

    with open(TRI_ZIP, "wb") as f:
        if use_tqdm:
            with tqdm(total=total, unit="B", unit_scale=True, desc="tri.zip") as bar:
                for chunk in response.iter_content(chunk_size=1024 * 1024):
                    f.write(chunk)
                    bar.update(len(chunk))
        else:
            downloaded = 0
            for chunk in response.iter_content(chunk_size=1024 * 1024):
                f.write(chunk)
                downloaded += len(chunk)
                pct = 100 * downloaded / total if total else 0
                print(f"\r  Downloaded: {downloaded/1e6:.0f} MB / {total/1e6:.0f} MB ({pct:.0f}%)", end="")
    print(f"\nSaved: {TRI_ZIP}")


# ── Step 2: Extract ───────────────────────────────────────────────────────────
def extract_tri():
    if TRI_TXT.exists():
        print(f"TRI text file already extracted: {TRI_TXT}")
        return
    print("Extracting tri.zip...")
    with zipfile.ZipFile(TRI_ZIP, "r") as z:
        z.extract("tri.txt", INST)
    print(f"Extracted: {TRI_TXT}")


# ── Step 3: Parse ASCII grid and clip to Nigeria ──────────────────────────────
def parse_tri_nigeria():
    """
    Parse the Nunn-Puga ASCII grid (ESRI ASCII raster format).
    Header lines: ncols, nrows, xllcorner, yllcorner, cellsize, NODATA_value
    Then rows of values, top-to-bottom, left-to-right.
    Clips to Nigeria bounding box to reduce memory.
    """
    print("Parsing TRI ASCII grid (global, ~30-arc-second)...")
    print("This reads ~600 MB of text — takes a few minutes")

    header = {}
    with open(TRI_TXT, "r") as f:
        for _ in range(6):
            line = f.readline().strip().split()
            header[line[0].lower()] = float(line[1])

    ncols    = int(header["ncols"])
    nrows    = int(header["nrows"])
    xll      = header["xllcorner"]
    yll      = header["yllcorner"]
    cellsize = header["cellsize"]
    nodata   = header["nodata_value"]

    print(f"  Grid: {ncols} cols × {nrows} rows, cell size = {cellsize}°")
    print(f"  Origin: ({xll}, {yll})")

    # Compute row/col indices covering Nigeria
    lon_min, lat_min, lon_max, lat_max = NGA_BOUNDS
    col_start = max(0, int((lon_min - xll) / cellsize))
    col_end   = min(ncols, int((lon_max - xll) / cellsize) + 1)
    # Rows are stored top-to-bottom; top-left lat = yll + nrows*cellsize
    lat_top   = yll + nrows * cellsize
    row_start = max(0, int((lat_top - lat_max) / cellsize))
    row_end   = min(nrows, int((lat_top - lat_min) / cellsize) + 1)
    n_rows_nga = row_end - row_start
    n_cols_nga = col_end - col_start

    print(f"  Nigeria rows: {row_start} to {row_end} ({n_rows_nga} rows)")
    print(f"  Nigeria cols: {col_start} to {col_end} ({n_cols_nga} cols)")

    nga_grid = np.full((n_rows_nga, n_cols_nga), np.nan, dtype=np.float32)

    with open(TRI_TXT, "r") as f:
        # Skip header
        for _ in range(6):
            f.readline()
        # Read rows
        for row_i in range(nrows):
            line = f.readline().split()
            if row_i < row_start or row_i >= row_end:
                continue
            vals = [float(v) for v in line[col_start:col_end]]
            arr = np.array(vals, dtype=np.float32)
            arr[arr == nodata] = np.nan
            nga_grid[row_i - row_start, :] = arr
            if (row_i - row_start) % 50 == 0:
                print(f"\r  Row {row_i}/{nrows} ...", end="")

    print(f"\n  Nigeria grid extracted: {nga_grid.shape}")
    print(f"  TRI range: {np.nanmin(nga_grid):.0f} – {np.nanmax(nga_grid):.0f} mm")

    # Geotransform: (x_min, cellsize, 0, y_max, 0, -cellsize)
    x_min_nga = xll + col_start * cellsize
    y_max_nga = lat_top - row_start * cellsize
    transform = (x_min_nga, cellsize, 0, y_max_nga, 0, -cellsize)

    return nga_grid, transform, cellsize


# ── Step 4: Zonal statistics per LGA ─────────────────────────────────────────
def zonal_tri(grid, transform, cellsize):
    try:
        import rasterio
        from rasterio.transform import from_origin
        from rasterio.crs import CRS
        from rasterstats import zonal_stats
    except ImportError:
        import subprocess, sys
        subprocess.check_call([sys.executable, "-m", "pip", "install",
                               "rasterio", "rasterstats"])
        import rasterio
        from rasterio.transform import from_origin
        from rasterio.crs import CRS
        from rasterstats import zonal_stats

    # Write Nigeria TRI grid as a temporary GeoTIFF
    tmp_tif = INST / "tri_nigeria_tmp.tif"
    x_min, cs, _, y_max, _, _ = transform
    rast_transform = from_origin(x_min, y_max, cs, cs)

    with rasterio.open(
        tmp_tif, "w", driver="GTiff",
        height=grid.shape[0], width=grid.shape[1],
        count=1, dtype="float32",
        crs=CRS.from_epsg(4326),
        transform=rast_transform,
        nodata=np.nan
    ) as dst:
        dst.write(grid, 1)
    print(f"Temp TIF written: {tmp_tif}")

    # Compute zonal statistics
    print("Computing mean TRI per LGA...")
    lgas = gpd.read_file(GADM)
    lgas_utm = lgas.to_crs(epsg=32632)
    lgas["centroid_lon"] = lgas_utm.centroid.to_crs(epsg=4326).x
    lgas["centroid_lat"] = lgas_utm.centroid.to_crs(epsg=4326).y

    stats = zonal_stats(str(GADM), str(tmp_tif),
                        stats=["mean", "std", "count"], nodata=np.nan)

    result = lgas[["GID_2", "NAME_2", "NAME_1",
                    "centroid_lon", "centroid_lat"]].copy()
    result.columns = ["GID_2", "lga_name", "state_name",
                      "centroid_lon", "centroid_lat"]
    result["mean_tri"]    = [s["mean"]  for s in stats]
    result["sd_tri"]      = [s["std"]   for s in stats]
    result["pixel_count"] = [s["count"] for s in stats]
    result["tri_std"]     = ((result["mean_tri"] - result["mean_tri"].mean())
                              / result["mean_tri"].std())

    result.to_csv(OUT, index=False)
    tmp_tif.unlink(missing_ok=True)  # clean up temp file

    print(f"\nSaved: {OUT} ({len(result)} LGAs)")
    print("\nMost rugged LGAs (high TRI → low agent density expected):")
    print(result.nlargest(8, "mean_tri")[["lga_name","state_name","mean_tri"]].to_string())
    print("\nFlattest LGAs (low TRI → high agent density expected):")
    print(result.nsmallest(8, "mean_tri")[["lga_name","state_name","mean_tri"]].to_string())
    return result


if __name__ == "__main__":
    download_tri()
    extract_tri()
    grid, transform, cellsize = parse_tri_nigeria()
    df = zonal_tri(grid, transform, cellsize)
    print("\nDone.")
