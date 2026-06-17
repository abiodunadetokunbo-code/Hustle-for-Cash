/**
 * Script 01: Terrain Ruggedness Index (TRI) by LGA — Nigeria
 * Paper 1a: Nigeria Demonetization
 *
 * Instrument 1: Pre-shock agent density instrumented by terrain ruggedness.
 * Rough terrain raises logistics cost of mobile-money agents → lower pre-shock
 * fintech density → greater exposure to cash crunch.
 *
 * Dataset: CGIAR/SRTM90_V4 (90m resolution, hosted on Earth Engine)
 * Method: Riley et al. (1999) TRI = focal standard deviation in 3×3 window
 * Output: CSV with GID_2, NAME_2, NAME_1, mean_tri, sd_tri (one row per LGA)
 *
 * Run in: code.earthengine.google.com
 * Export to: Google Drive → paper1a_instruments/
 */

// ── 1. Load DEM ──────────────────────────────────────────────────────────────
var dem = ee.Image('CGIAR/SRTM90_V4').select('elevation');

// ── 2. Compute TRI (Riley 1999) ───────────────────────────────────────────────
// TRI = standard deviation of elevation in a 3×3 neighbourhood
// This is the standard Earth Engine approximation; matches published results
// for African terrain studies (e.g. Nunn & Puga 2012 uses same approach).
var tri = dem.reduceNeighborhood({
  reducer: ee.Reducer.stdDev(),
  kernel: ee.Kernel.square(1, 'pixels')   // 3×3 window = 270m at 90m resolution
}).rename('tri');

// ── 3. Load Nigeria LGA boundaries ───────────────────────────────────────────
// Upload gadm41_NGA_2.shp as an Earth Engine asset first:
//   Assets → New → Shape files → upload gadm41_NGA_2.*
//   Asset path: users/YOUR_USERNAME/gadm41_NGA_2
//
// Replace 'YOUR_USERNAME' below with your EE username.
var lgas = ee.FeatureCollection('users/YOUR_USERNAME/gadm41_NGA_2');

// ── 4. Zonal statistics: mean and SD of TRI per LGA ──────────────────────────
var tri_by_lga = tri.reduceRegions({
  collection: lgas,
  reducer: ee.Reducer.mean().combine({
    reducer2: ee.Reducer.stdDev(),
    sharedInputs: true
  }),
  scale: 90,
  crs: dem.projection()
});

// Rename output columns to be explicit
tri_by_lga = tri_by_lga.map(function(f) {
  return f.select(
    ['GID_2', 'NAME_2', 'GID_1', 'NAME_1', 'mean', 'stdDev'],
    ['GID_2', 'lga_name', 'GID_1', 'state_name', 'mean_tri', 'sd_tri']
  );
});

// ── 5. Export to Drive ────────────────────────────────────────────────────────
Export.table.toDrive({
  collection: tri_by_lga,
  description: 'TRI_by_LGA_Nigeria_SRTM90',
  folder: 'paper1a_instruments',
  fileNamePrefix: 'tri_lga_nigeria',
  fileFormat: 'CSV'
});

// ── 6. Visualise (optional preview) ──────────────────────────────────────────
Map.setCenter(8.68, 9.08, 6);
Map.addLayer(tri, {min: 0, max: 50, palette: ['white', 'orange', 'red']}, 'TRI');
Map.addLayer(lgas.style({color: 'black', fillColor: '00000000', width: 0.5}), {}, 'LGAs');

print('Total LGAs:', lgas.size());
print('Sample output:', tri_by_lga.limit(5));
