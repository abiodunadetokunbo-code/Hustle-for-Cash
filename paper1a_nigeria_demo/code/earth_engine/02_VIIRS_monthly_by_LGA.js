/**
 * Script 02: VIIRS Monthly Nighttime Lights by LGA — Nigeria
 * Paper 1a: Nigeria Demonetization
 *
 * Main outcome variable: district-level economic activity proxy at monthly
 * resolution. Captures the output contraction during the Jan–Feb 2023 cash
 * crunch and subsequent recovery.
 *
 * Dataset: NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG
 *   - avg_rad: average radiance (nanoWatts/sr/cm²)
 *   - cf_cvg:  cloud-free coverage count (use as quality weight)
 * Resolution: 463.83 m (~500m)
 * Coverage: 2012-04 to present (monthly)
 *
 * Output: Single CSV — LGA × month panel, columns:
 *   GID_2, lga_name, state_name, year, month, mean_rad, cf_coverage
 *
 * Run in: code.earthengine.google.com
 * Export to: Google Drive → paper1a_outcomes/
 *
 * NOTE: This generates one Export task. EE will time out on very large
 * collections — if it does, split into two tasks: 2019–2021 and 2022–2024.
 */

// ── 1. Load LGA boundaries ────────────────────────────────────────────────────
// Replace 'YOUR_USERNAME' with your EE username after uploading gadm41_NGA_2
var lgas = ee.FeatureCollection('users/YOUR_USERNAME/gadm41_NGA_2');

// Nigeria bounding box (clip rasters for speed)
var nigeria_bbox = ee.Geometry.Rectangle([2.6, 4.2, 14.7, 13.9]);

// ── 2. Define time range ──────────────────────────────────────────────────────
// Pre-shock baseline: Jan 2019 – Sep 2022 (pre-announcement)
// Shock window:       Oct 2022 – Apr 2023
// Post-shock:         May 2023 – Dec 2024
var start_date = '2019-01-01';
var end_date   = '2024-12-31';

// ── 3. Load VIIRS collection ──────────────────────────────────────────────────
var viirs = ee.ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMCFG')
  .filterDate(start_date, end_date)
  .filterBounds(nigeria_bbox)
  .select(['avg_rad', 'cf_cvg']);

// ── 4. Build monthly LGA × radiance panel ─────────────────────────────────────
// Create a list of all month start dates
var n_months = 72; // Jan 2019 – Dec 2024
var month_starts = ee.List.sequence(0, n_months - 1).map(function(offset) {
  return ee.Date('2019-01-01').advance(offset, 'month');
});

// For each month: compute mean radiance per LGA, tag with year+month
var monthly_panels = month_starts.map(function(start) {
  start = ee.Date(start);
  var end = start.advance(1, 'month');

  // Get image for this month (first() — each month has exactly one composite)
  var img = viirs.filterDate(start, end).first();

  // Handle months with no data (gaps in coverage)
  img = ee.Image(ee.Algorithms.If(img, img,
    ee.Image.constant(-9999).rename(['avg_rad', 'cf_cvg'])));

  // Zonal statistics
  var stats = img.reduceRegions({
    collection: lgas,
    reducer: ee.Reducer.mean(),
    scale: 463.83,
    crs: 'EPSG:4326'
  });

  // Tag each feature with year and month
  return stats.map(function(f) {
    return f.set({
      'year':  start.get('year'),
      'month': start.get('month')
    });
  });
});

// Flatten to a single FeatureCollection
var panel = ee.FeatureCollection(monthly_panels).flatten();

// ── 5. Select and rename output columns ──────────────────────────────────────
panel = panel.map(function(f) {
  return f.select(
    ['GID_2', 'NAME_2', 'NAME_1', 'year', 'month', 'avg_rad', 'cf_cvg'],
    ['GID_2', 'lga_name', 'state_name', 'year', 'month', 'mean_rad', 'cf_coverage']
  );
});

// ── 6. Export ─────────────────────────────────────────────────────────────────
Export.table.toDrive({
  collection: panel,
  description: 'VIIRS_monthly_LGA_Nigeria_2019_2024',
  folder: 'paper1a_outcomes',
  fileNamePrefix: 'viirs_monthly_lga_nigeria',
  fileFormat: 'CSV'
});

print('Months to process:', n_months);
print('LGAs:', lgas.size());
print('Expected rows:', ee.Number(n_months).multiply(lgas.size()));
print('Sample (first 5 rows):', panel.limit(5));

// ── 7. Visualise shock period ─────────────────────────────────────────────────
// Compare Oct 2022 (pre-shock) vs Feb 2023 (peak shock)
var pre  = viirs.filterDate('2022-10-01', '2022-11-01').first().select('avg_rad');
var peak = viirs.filterDate('2023-02-01', '2023-03-01').first().select('avg_rad');
var diff = peak.subtract(pre).rename('delta_rad');

Map.setCenter(8.68, 9.08, 6);
Map.addLayer(pre,  {min:0, max:5, palette:['black','yellow','white']}, 'Oct 2022 (pre)');
Map.addLayer(peak, {min:0, max:5, palette:['black','yellow','white']}, 'Feb 2023 (peak shock)');
Map.addLayer(diff, {min:-2, max:2, palette:['red','white','blue']},    'Delta (peak - pre)');
