#!/bin/bash

# ==============================================================================
# MOM6 Curvilinear to Rectilinear Regridding Script (Admin / Sudo Version)
# ==============================================================================

set -e

CONDA_DIR="/opt/miniconda3"
ENV_DIR="$CONDA_DIR/envs/ocean_env"

echo "=== 1. Setting up System-Wide Conda Environment ==="
if [ ! -d "$CONDA_DIR" ]; then
    echo "Conda not found. Installing Miniconda to $CONDA_DIR..."
    sudo wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O miniconda.sh
    sudo bash miniconda.sh -b -p "$CONDA_DIR"
    sudo rm miniconda.sh
else
    echo "Conda is already installed at $CONDA_DIR."
fi

echo "=== 2. Accepting Anaconda Terms of Service ==="
sudo "$CONDA_DIR/bin/conda" tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main || true
sudo "$CONDA_DIR/bin/conda" tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r || true

echo "=== 3. Installing NCO and CDO via Conda-Forge ==="
if [ ! -d "$ENV_DIR" ]; then
    sudo "$CONDA_DIR/bin/conda" create -y -n ocean_env -c conda-forge cdo nco
else
    echo "ocean_env already exists."
fi

# ==============================================================================
# 4. Define File Paths
# ==============================================================================
STATIC_FILE="/atlantisarchive/Joseph.Caracappa/MOM6/MOM6_hindcast_ocean_static.nc"
RAW_FILE="/atlantisarchive/Joseph.Caracappa/MOM6/tob.nwa.full.ss_fcast.daily.raw.r20250710.enss.i202604.nc"
OUTPUT_FILE="/atlantisarchive/Joseph.Caracappa/MOM6/tob.nwa.full.ss_fcast.daily.regrid.r20250710.enss.i202604.nc"

# FIX: Move temporary processing to the local high-speed /tmp directory. 
# This bypasses the "Operation not supported" errors from the network archive drive.
TEMP_FILE="/tmp/mom6_temp_working_file.nc"
CHECKPOINT_FILE="/tmp/mom6_prepped_checkpoint_file.nc"
GRID_DEF="/tmp/mom6_nwa_grid.txt"

# ==============================================================================
# 5 & 6. Prepping Data (With Checkpointing)
# ==============================================================================
if sudo test -f "$CHECKPOINT_FILE"; then
    echo "=== 5 & 6. Found existing prepped checkpoint! Skipping NCO prep... ==="
    # Use -f so cp overwrites without prompting
    sudo cp -f "$CHECKPOINT_FILE" "$TEMP_FILE"
else
    echo "=== 5. Prepping Data & Injecting True Coordinates ==="
    # FIX: Use '-O' to silently overwrite existing files, skipping the (e/o/a) prompt.
    # FIX: Use '-5' (CDF5). It allows >4GB variables AND avoids HDF5 dimscale bugs!
    sudo "$ENV_DIR/bin/ncks" -O -5 "$RAW_FILE" "$TEMP_FILE"

    # Append geolon and geolat from static file
    sudo "$ENV_DIR/bin/ncks" -A -v geolon,geolat "$STATIC_FILE" "$TEMP_FILE"

    # Point the coordinates attribute ONLY to the true spatial grid so CDO knows where to look
    sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,tob,o,c,"geolon geolat" "$TEMP_FILE"
    sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,tob_anom,o,c,"geolon geolat" "$TEMP_FILE"

    echo "=== 6. Bypassing CDO Strict CF-Compliance Rules ==="
    # 6a. Strip out confusing metadata so CDO doesn't try to validate non-spatial scalars
    sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,valid_time,d,, "$TEMP_FILE"
    sudo "$ENV_DIR/bin/ncks" -O -x -v init,month "$TEMP_FILE" "$TEMP_FILE"

    # 6b. Trick CDO by renaming custom dimensions AND your valid_time variable
    sudo "$ENV_DIR/bin/ncrename" -d lead,time -v valid_time,time "$TEMP_FILE"
    sudo "$ENV_DIR/bin/ncrename" -d member,lev "$TEMP_FILE"
    
    # Save checkpoint so you don't have to do this again if CDO fails
    sudo cp -f "$TEMP_FILE" "$CHECKPOINT_FILE"
fi

echo "=== 7. Defining the Target Rectilinear Grid ==="
sudo tee "$GRID_DEF" > /dev/null << EOF
gridtype  = lonlat
xsize     = 620
ysize     = 470
xfirst    = -98.0
xinc      = 0.1
yfirst    = 5.0
yinc      = 0.1
EOF

echo "=== 8. Regridding with CDO ==="
# FIX 1: Add '-v' for verbose output to see exactly where it fails if it crashes again.
# FIX 2: Change '-f nc4' to '-f nc5'. CDO will output an unchunked CDF5 file, bypassing the HDF error entirely.
sudo "$ENV_DIR/bin/cdo" -v -O -f nc5 remapbil,"$GRID_DEF" "$TEMP_FILE" "$OUTPUT_FILE"

echo "=== 9. Restoring Original Metadata ==="
# 9a. Rename the CDO standard dimensions and variables back to your custom names
sudo "$ENV_DIR/bin/ncrename" -d time,lead -v time,valid_time "$OUTPUT_FILE"
sudo "$ENV_DIR/bin/ncrename" -d lev,member "$OUTPUT_FILE"

# 9b. Append the omitted scalar variables back into the new file exactly as they were
sudo "$ENV_DIR/bin/ncks" -A -v init,month "$RAW_FILE" "$OUTPUT_FILE"

# 9c. Restore original 'coordinates' attributes for downstream tools
sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,valid_time,o,c,"init month" "$OUTPUT_FILE"
sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,tob,o,c,"member" "$OUTPUT_FILE"
sudo "$ENV_DIR/bin/ncatted" -O -a coordinates,tob_anom,o,c,"init month" "$OUTPUT_FILE"

echo "=== 10. Finalizing and Compressing to NetCDF4 ==="
# FIX 3: Let NCO do the heavy lifting of converting the final unchunked file into a compressed NetCDF4 file.
# '-4' makes it NetCDF4, '-L 1' applies level-1 deflation (compression) to save disk space.
sudo "$ENV_DIR/bin/ncks" -O -4 -L 1 "$OUTPUT_FILE" "$OUTPUT_FILE"

echo "=== 11. Cleaning Up ==="
sudo rm -f "$TEMP_FILE" "$GRID_DEF"
# We deliberately do NOT remove the CHECKPOINT_FILE here so you can reuse it. 
# Delete it manually when you move on to a completely different MOM6 file.

echo "=== Done! Successfully created $OUTPUT_FILE ==="