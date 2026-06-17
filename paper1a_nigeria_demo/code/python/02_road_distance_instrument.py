"""
Script 02: Road Distance to Commercial Hub — Instrument 2
Paper 1a: Nigeria Demonetization

Computes the road network distance from each LGA centroid to the nearest of
five major Nigerian commercial hubs. Used as Instrument 2 for pre-shock
mobile-money agent density (agent networks diffused outward from these hubs).

Two methods offered:
  Method A: Geodesic (straight-line) distance  — fast, ~1 minute, good approximation
  Method B: Road network distance via OSM PBF  — precise, ~30–60 min, requires pyrosm

For the instrument, Method A is standard in the literature and sufficient for
the first stage. Method B serves as robustness. Run Method A first.

Output: data/instruments/road_dist_hub_lga.csv
  Columns: GID_2, lga_name, state_name, centroid_lon, centroid_lat,
           dist_lagos_km, dist_abuja_km, dist_kano_km, dist_portharcourt_km,
           dist_onitsha_km, min_dist_km, nearest_hub, method

Requirements (pip install):
  geopandas shapely pyproj pandas numpy
  [Method B only]: pyrosm networkx pandana
"""

import geopandas as gpd
import pandas as pd
import numpy as np
from pathlib import Path
from pyproj import Geod

# ── Paths ────────────────────────────────────────────────────────────────────
ROOT   = Path(__file__).parents[2]
GADM   = ROOT / "data/raw/shapefiles/gadm_nigeria/gadm41_NGA_2.shp"
PBF    = ROOT / "data/instruments/osm_roads/nigeria-260608.osm.pbf"
OUT    = ROOT / "data/instruments/road_dist_hub_lga.csv"

# ── Commercial hub coordinates ────────────────────────────────────────────────
HUBS = {
    "lagos":        (3.3841, 6.4550),   # lon, lat
    "abuja":        (7.4951, 9.0579),
    "kano":         (8.5167, 12.0000),
    "portharcourt": (7.0494, 4.8156),
    "onitsha":      (6.7833, 6.1667),
}

# ─────────────────────────────────────────────────────────────────────────────
# METHOD A: Geodesic (straight-line) distance — DEFAULT
# ─────────────────────────────────────────────────────────────────────────────
def geodesic_distance(lon1, lat1, lon2, lat2):
    """Return geodesic distance in km between two (lon, lat) points."""
    geod = Geod(ellps="WGS84")
    _, _, dist_m = geod.inv(lon1, lat1, lon2, lat2)
    return dist_m / 1000.0


def compute_geodesic_distances():
    print("Loading GADM LGA shapefile...")
    lgas = gpd.read_file(GADM)

    # Compute LGA centroids in WGS84
    # Project to UTM zone 32N (covers most of Nigeria) for accurate centroids
    lgas_utm = lgas.to_crs(epsg=32632)
    lgas["centroid_lon"] = lgas_utm.centroid.to_crs(epsg=4326).x
    lgas["centroid_lat"] = lgas_utm.centroid.to_crs(epsg=4326).y

    print(f"Computing geodesic distances for {len(lgas)} LGAs to {len(HUBS)} hubs...")

    for hub_name, (hub_lon, hub_lat) in HUBS.items():
        col = f"dist_{hub_name}_km"
        lgas[col] = lgas.apply(
            lambda row: geodesic_distance(
                row["centroid_lon"], row["centroid_lat"],
                hub_lon, hub_lat
            ),
            axis=1
        )

    # Minimum distance and nearest hub
    dist_cols = [f"dist_{h}_km" for h in HUBS]
    lgas["min_dist_km"]  = lgas[dist_cols].min(axis=1)
    lgas["nearest_hub"]  = lgas[dist_cols].idxmin(axis=1).str.replace("dist_", "").str.replace("_km", "")
    lgas["method"]       = "geodesic"

    # Select output columns
    out_cols = ["GID_2", "NAME_2", "NAME_1",
                "centroid_lon", "centroid_lat"] + dist_cols + ["min_dist_km", "nearest_hub", "method"]
    result = lgas[out_cols].rename(columns={"NAME_2": "lga_name", "NAME_1": "state_name"})

    result.to_csv(OUT, index=False)
    print(f"\nSaved: {OUT}")
    print(f"Rows: {len(result)}")
    print(f"\nSummary statistics — min_dist_km:")
    print(result["min_dist_km"].describe().round(1))
    print(f"\nNearest hub distribution:")
    print(result["nearest_hub"].value_counts())
    return result


# ─────────────────────────────────────────────────────────────────────────────
# METHOD B: Road network distance from OSM PBF (robustness check)
# ─────────────────────────────────────────────────────────────────────────────
def compute_road_network_distances():
    """
    Extracts trunk + primary roads from PBF, builds a graph, and
    computes shortest-path distances. Slower but more precise.

    Requires: pip install pyrosm networkx pandana
    Runtime: ~30–60 minutes on a standard laptop.
    """
    try:
        import pyrosm
        import networkx as nx
    except ImportError:
        print("Method B requires: pip install pyrosm networkx")
        print("Falling back to Method A (geodesic).")
        return compute_geodesic_distances()

    print("Loading OSM PBF (this takes ~5 minutes for Nigeria)...")
    osm = pyrosm.OSM(str(PBF))

    print("Extracting road network (trunk + primary + secondary)...")
    # Filter to trunk/primary roads only for faster routing
    # (local roads add noise and computation cost for a macro-level instrument)
    network = osm.get_network(
        network_type="driving",
        extra_attributes=["highway"]
    )
    network = network[network["highway"].isin([
        "motorway", "trunk", "primary", "secondary",
        "motorway_link", "trunk_link", "primary_link"
    ])]

    print("Building graph...")
    G = osm.to_graph(network, graph_type="networkx")

    print("Computing LGA centroid → hub distances via shortest path...")
    lgas = gpd.read_file(GADM)
    lgas_utm = lgas.to_crs(epsg=32632)
    lgas["centroid_lon"] = lgas_utm.centroid.to_crs(epsg=4326).x
    lgas["centroid_lat"] = lgas_utm.centroid.to_crs(epsg=4326).y

    def nearest_node(G, lon, lat):
        nodes = {n: (d["x"], d["y"]) for n, d in G.nodes(data=True) if "x" in d and "y" in d}
        return min(nodes, key=lambda n: (nodes[n][0]-lon)**2 + (nodes[n][1]-lat)**2)

    hub_nodes = {name: nearest_node(G, lon, lat) for name, (lon, lat) in HUBS.items()}

    for hub_name, hub_node in hub_nodes.items():
        print(f"  Routing to {hub_name}...")
        lengths = nx.single_source_dijkstra_path_length(G, hub_node, weight="length")
        col = f"dist_{hub_name}_km"
        lgas[col] = lgas["centroid_lon"].apply(
            lambda lon: lengths.get(nearest_node(G, lon,
                lgas.loc[lgas["centroid_lon"]==lon, "centroid_lat"].values[0]), np.nan)
        ) / 1000.0  # metres → km

    dist_cols = [f"dist_{h}_km" for h in HUBS]
    lgas["min_dist_km"] = lgas[dist_cols].min(axis=1)
    lgas["nearest_hub"] = lgas[dist_cols].idxmin(axis=1).str.replace("dist_", "").str.replace("_km", "")
    lgas["method"]      = "road_network_osm"

    out_cols = ["GID_2", "NAME_2", "NAME_1",
                "centroid_lon", "centroid_lat"] + dist_cols + ["min_dist_km", "nearest_hub", "method"]
    result = lgas[out_cols].rename(columns={"NAME_2": "lga_name", "NAME_1": "state_name"})

    out_road = OUT.with_name("road_dist_hub_lga_network.csv")
    result.to_csv(out_road, index=False)
    print(f"\nSaved: {out_road}")
    return result


# ─────────────────────────────────────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────────────────────────────────────
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--method", choices=["geodesic", "network"], default="geodesic",
                        help="geodesic = fast straight-line (default); network = OSM road routing")
    args = parser.parse_args()

    if args.method == "network":
        df = compute_road_network_distances()
    else:
        df = compute_geodesic_distances()

    print("\nDone. First 5 rows:")
    print(df.head())
