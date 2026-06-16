################################################################################
# Project: China's Sponge City Programme and rain-induced road congestion
# File:    05_main_figure5_cmip6_threshold_days.py
# Purpose: Count annual threshold-exceeding precipitation days from CMIP6 files
# Author:  Yanghan Lin et al.
################################################################################

from __future__ import annotations

import re
from pathlib import Path

import geopandas as gpd
import numpy as np
import pandas as pd
import xarray as xr
import rioxarray  # noqa: F401
from rioxarray.exceptions import NoDataInBounds


PROJECT_ROOT = Path(r"C:/Users/Administrator/Documents/Sponge City and road congestion paper")
SUBMISSION_DIR = PROJECT_ROOT / "submission_code"
INTERMEDIATE_DIR = SUBMISSION_DIR / "data" / "intermediate"

# Edit these two paths before running the script on the raw CMIP6 files.
CMIP6_ROOT = Path(r"E:/CMIP6/SSPData")
CITY_SHAPEFILE = Path(r"D:/city_boundaries/city.shp")

OUTPUT_FILE = INTERMEDIATE_DIR / "fig5_cmip6_city_year_threshold_days.csv"

SCENARIOS = ("ssp245", "ssp585")
THRESHOLDS_MM = (20, 30, 50, 80)
VARIABLE_NAME = "PREC"
PRECIP_MULTIPLIER = 1.0
ALL_TOUCHED = False
SPATIAL_AGGREGATION = "max"


def find_model_folders(root: Path) -> list[Path]:
    folders = []
    for path in sorted(root.iterdir()):
        if path.is_dir() and all((path / scenario / "PREC").exists() for scenario in SCENARIOS):
            folders.append(path)
    if not folders:
        raise FileNotFoundError(
            f"No model folders with scenario/PREC subfolders were found under {root}."
        )
    return folders


def list_nc_files(model_folder: Path, scenario: str) -> list[Path]:
    files = sorted((model_folder / scenario / "PREC").glob("*.nc"), key=infer_year)
    if not files:
        raise FileNotFoundError(f"No NetCDF files found for {model_folder.name}/{scenario}.")
    return files


def infer_year(path: Path) -> int:
    match = re.search(r"(19\d{2}|20\d{2})", path.name)
    if match is None:
        raise ValueError(f"Could not infer a year from file name: {path.name}")
    return int(match.group(1))


def choose_city_name_column(gdf: gpd.GeoDataFrame) -> str:
    candidates = [col for col in gdf.columns if col != gdf.geometry.name]
    if not candidates:
        raise ValueError("The city shapefile has no non-geometry columns.")
    return candidates[0]


def prepare_dataarray(path: Path) -> tuple[xr.DataArray, xr.Dataset]:
    dataset = xr.open_dataset(path)
    if VARIABLE_NAME not in dataset:
        dataset.close()
        raise KeyError(f"{VARIABLE_NAME} was not found in {path}.")

    data = dataset[VARIABLE_NAME] * PRECIP_MULTIPLIER

    lon_name = next((dim for dim in data.dims if dim.lower() in ("lon", "longitude", "x")), None)
    lat_name = next((dim for dim in data.dims if dim.lower() in ("lat", "latitude", "y")), None)
    if lon_name is None or lat_name is None:
        dataset.close()
        raise ValueError(f"Could not identify longitude and latitude dimensions in {path}.")

    data = data.rio.set_spatial_dims(x_dim=lon_name, y_dim=lat_name, inplace=False)
    data = data.rio.write_crs("EPSG:4326", inplace=False)
    return data, dataset


def city_daily_values(data: xr.DataArray, geometry) -> tuple[np.ndarray, np.ndarray]:
    try:
        clipped = data.rio.clip([geometry], drop=True, all_touched=ALL_TOUCHED)
    except NoDataInBounds:
        empty = np.array([], dtype=float)
        return empty, empty

    spatial_dims = [dim for dim in clipped.dims if dim.lower() not in ("time",)]
    if not spatial_dims:
        threshold_values = clipped.values
        annual_precip_values = clipped.values
    elif SPATIAL_AGGREGATION == "mean":
        threshold_values = clipped.mean(dim=spatial_dims, skipna=True).values
        annual_precip_values = threshold_values
    else:
        threshold_values = clipped.max(dim=spatial_dims, skipna=True).values
        annual_precip_values = clipped.mean(dim=spatial_dims, skipna=True).values

    return (
        np.asarray(threshold_values, dtype=float).ravel(),
        np.asarray(annual_precip_values, dtype=float).ravel(),
    )


def count_threshold_days(values: np.ndarray, threshold: float) -> float:
    if values.size == 0 or np.all(np.isnan(values)):
        return np.nan
    return float(np.nansum(values > threshold))


def annual_precipitation(values: np.ndarray) -> float:
    if values.size == 0 or np.all(np.isnan(values)):
        return np.nan
    return float(np.nansum(values))


def main() -> None:
    INTERMEDIATE_DIR.mkdir(parents=True, exist_ok=True)

    model_folders = find_model_folders(CMIP6_ROOT)
    cities = gpd.read_file(CITY_SHAPEFILE)
    if cities.crs is None:
        cities = cities.set_crs("EPSG:4326")
    else:
        cities = cities.to_crs("EPSG:4326")

    city_name_column = choose_city_name_column(cities)
    records: list[dict[str, object]] = []

    for model_folder in model_folders:
        model = model_folder.name
        for scenario in SCENARIOS:
            files = list_nc_files(model_folder, scenario)
            for file_index, nc_file in enumerate(files, start=1):
                year = infer_year(nc_file)
                print(f"{model} {scenario} {year}: {file_index}/{len(files)}")

                data, source_dataset = prepare_dataarray(nc_file)
                try:
                    for _, city_row in cities.iterrows():
                        city = city_row[city_name_column]
                        threshold_values, annual_values = city_daily_values(data, city_row.geometry)
                        annual_total = annual_precipitation(annual_values)

                        for threshold in THRESHOLDS_MM:
                            records.append(
                                {
                                    "model": model,
                                    "scenario": scenario,
                                    "year": year,
                                    "city": city,
                                    "threshold": threshold,
                                    "exceed_days": count_threshold_days(threshold_values, threshold),
                                    "annual_precip_mm": annual_total,
                                }
                            )
                finally:
                    source_dataset.close()

    output = pd.DataFrame.from_records(records)
    output.sort_values(["model", "scenario", "city", "threshold", "year"], inplace=True)
    output.to_csv(OUTPUT_FILE, index=False, encoding="utf-8-sig")
    print(f"Saved {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
