#!/usr/bin/env python
# coding: utf-8

# In[2]:


pip install pyubx2 pandas


# In[1]:


from pyubx2 import UBXReader
import pandas as pd
from datetime import datetime


# In[2]:


# Open UBX file
ubx_path = "/home/kellebea/public/Evaluation/Experiments/2025/FPSE009/LT_SE009/GPS/COM5___9600_250524_144714.ubx"  # Replace with your UBX file path



# In[3]:


records = []

with open(ubx_path, 'rb') as stream:
    ubr = UBXReader(stream)
    for raw, msg in ubr:
        if msg.identity == "NAV-PVT":
            # Ensure valid GPS fix
            if msg.fixType >= 2 and msg.gnssFixOk == 1:
                time_str = f"{msg.hour:02}:{msg.min:02}:{msg.second:02}.{int(msg.nano / 1e6):03}"
                date_str = f"{msg.month:02}/{msg.day:02}/{msg.year}"
                utc = f"{time_str} {date_str}"

                lat = round(msg.lat, 8) 
                lon = round(msg.lon, 8) 
                alt = round(msg.hMSL / 1000, 1)  # hMSL in mm

                records.append({
                    "Fix type (GPS) valid": "Yes",
                    "UTC": utc,
                    "Lat": round(lat, 8),
                    "Lon": round(lon, 8),
                    "Alt (MSL)": alt
                })

# Create and display DataFrame
df = pd.DataFrame(records)
print(df.to_string(index=False))


# In[4]:


from pyubx2 import UBXReader


with open(ubx_path, 'rb') as stream:
    ubr = UBXReader(stream)
    for i, (raw, msg) in enumerate(ubr):
        print(f"{i}: {msg.identity}")
        if i > 50:  # Limit to first 50 messages
            break


# In[5]:


with open(ubx_path, 'rb') as stream:
    ubr = UBXReader(stream)
    for (raw, msg) in ubr:
        if msg.identity == "NAV-PVT":
            print(msg)
            break


# In[1]:


import os
import pandas as pd
from pyubx2 import UBXReader

def extract_gps_from_ubx(ubx_path, output_dir):
    records = []

    with open(ubx_path, 'rb') as stream:
        ubr = UBXReader(stream)
        for raw, msg in ubr:
            if msg.identity == "NAV-PVT":
                if msg.fixType >= 2 and msg.gnssFixOk == 1:
                    time_str = f"{msg.hour:02}:{msg.min:02}:{msg.second:02}.{int(msg.nano / 1e6):03}"
                    date_str = f"{msg.month:02}/{msg.day:02}/{msg.year}"
                    utc = f"{time_str} {date_str}"

                    lat = round(msg.lat, 8)
                    lon = round(msg.lon, 8)
                    alt = round(msg.hMSL / 1000, 1)

                    records.append({
                        "Fix type (GPS) valid": "Yes",
                        "UTC": utc,
                        "Lat": lat,
                        "Lon": lon,
                        "Alt (MSL)": alt
                    })

    # If data exists, write to CSV
    if records:
        df = pd.DataFrame(records)
        base_name = os.path.splitext(os.path.basename(ubx_path))[0]
        output_path = os.path.join(output_dir, f"{base_name}.csv")
        df.to_csv(output_path, index=False)
        print(f"✅ Extracted: {output_path}")
    else:
        print(f"⚠️ No valid NAV-PVT data found in {ubx_path}")


# In[7]:


import glob

input_folder = "/home/kellebea/public/Evaluation/Experiments/2025/FPSE009/LT_SE009/GPS/"
output_folder = "/home/kellebea/public/Evaluation/Experiments/2025/FPSE009/LT_SE009/GPS/Processed"

os.makedirs(output_folder, exist_ok=True)

for ubx_file in glob.glob(os.path.join(input_folder, "*.ubx")):
    extract_gps_from_ubx(ubx_file, output_folder)


# In[2]:


import glob

input_folder = "/home/kellebea/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/GPS/"
output_folder = "/home/kellebea/public/Evaluation/Experiments/2025/ESSB019/LFT_SB019/GPS/Processed"

os.makedirs(output_folder, exist_ok=True)

for ubx_file in glob.glob(os.path.join(input_folder, "*.ubx")):
    extract_gps_from_ubx(ubx_file, output_folder)


# In[ ]:




