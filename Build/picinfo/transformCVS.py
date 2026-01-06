import requests
import csv
import json
import time
from datetime import datetime
import re
# Array of parts to include (edit this list as needed)
parts = ['PIC16', 'PIC10', 'PIC18', 'PIC12', 'PIC14'] # Examples; add more as required
# Array of excluded part substrings (lowercase for key matching; catches CR/LR/LC/16C anywhere, extend as needed)
excludedparts = ['cr', 'lr', 'lc', '16c']
# URL for the CSV
csv_url = 'https://www.microchipdirect.com/feed/masterfeed/Microchip-Technology-Part-Data-Feed.csv'
# Download the CSV
print("Downloading CSV...")
try:
    response = requests.get(csv_url, timeout=30)
    response.raise_for_status()
    print("CSV downloaded successfully.")
except Exception as e:
    print(f"Error downloading CSV: {e}")
    exit(1)
# Parse CSV
lines = response.text.splitlines()
reader = csv.DictReader(lines)
print(f"CSV has {len(lines) - 1} data rows.") # Total rows minus header
print("CSV Columns:", reader.fieldnames)
# Output data structure
data = {}
# Counters
total_rows = 0
matched_mpu = 0
has_link = 0
processed = 0
updated = 0  # New counter for updates due to better links
# Set to track unique (key, link) to avoid duplicates
seen_docs = set()
# Excluded array for variants
excluded = []
# Process rows (no limit now)
for row_num, row in enumerate(reader, start=2):
    total_rows += 1
    try:
        mpu = row.get('mpu', '').upper().strip()
        if not mpu:
            continue
       
        # Check if mpu matches any part in the array
        match = any(part.upper() in mpu for part in parts)
        if match:
            matched_mpu += 1
            # Optional: print first few matches for debug
            if matched_mpu <= 5:
                print(f"Row {row_num} MATCHED mpu: {mpu}")
       
        if not match:
            continue
       
        # Get base part number: use 'base partnumber'
        base_part = row.get('base partnumber', '').strip()
        if not base_part:
            continue
       
        # Slugify base part number for key
        part_slug = re.sub(r'[^\w\s-]', '', base_part.lower()).strip()
        part_slug = re.sub(r'\s+', '-', part_slug).strip('-')
        if not part_slug:
            continue
       
        # Key is just the slugged base partnumber
        key = part_slug
       
        # Datasheet title: use 'description', fallback
        title = row.get('description', f"{base_part} Datasheet").strip()
        if not title or title == 'N/A':
            title = f"{base_part} Datasheet"
       
        # Datasheet link: use 'datasheet-url', fallback to 'datasheet-url 2'
        link = row.get('datasheet-url', row.get('datasheet-url 2', '')).strip()
        if not link:
            continue
        has_link += 1
       
        # Check for duplicate doc for this key (same link), but since only one per key, not needed now
       
        # Process date: use 'INTRO DATE' as proxy for process date
        date_str = row.get('INTRO DATE', '').strip()
        date = False
        if date_str:
            try:
                # Assume format like 'YYYY-MM-DD' or similar; adjust based on actual format
                dt = datetime.strptime(date_str, '%Y-%m-%d')
                date = int(time.mktime(dt.timetuple()))
            except ValueError:
                try:
                    # Try alternative format if needed, e.g., 'MM/DD/YYYY'
                    dt = datetime.strptime(date_str, '%m/%d/%Y')
                    date = int(time.mktime(dt.timetuple()))
                except ValueError:
                    print(f"Warning: Invalid date format in row {row_num}: {date_str}")
                    date = False
       
        # Entry object
        entry = {
            "date": date,
            "title": title,
            "link": link
        }
       
        # Check for exclusion: Substrings OR hyphen suffix OR specific title OR "Not Available" link
        is_excluded = any(excl in key for excl in excludedparts) or '-' in key or title == "FOR INTERNAL USE ONLY." or link == "Not Available"
        if is_excluded:
            excluded.append({key: entry})
            reason = next((excl for excl in excludedparts if excl in key), 'hyphen variant' if '-' in key else 'internal title' if title == "FOR INTERNAL USE ONLY." else 'not available link')
            print(f"Excluded {key} ({reason})")
            continue
       
        # Handle duplicates: Prefer PDF links, then non-"Not Available"
        if key in data:
            existing_entry = data[key]
            existing_link = existing_entry["link"]
            new_link = link
            
            # Update if new is PDF and existing isn't, or new is valid and existing is "Not Available"
            update = False
            if new_link.lower().endswith('.pdf') and not existing_link.lower().endswith('.pdf'):
                update = True
            elif existing_link == "Not Available" and new_link != "Not Available":
                update = True
            elif date and existing_entry["date"] and date > existing_entry["date"]:
                # Bonus: Update to newer date if links are equal/valid
                update = True
            
            if update:
                data[key] = entry
                updated += 1
                print(f"Updated {key}: Better link '{new_link}' (from '{existing_link}')")
            # Else keep existing
        else:
            data[key] = entry
            processed += 1
       
        # Progress indicator every 10000 rows
        if total_rows % 10000 == 0:
            print(f"Processed {total_rows} rows, {processed} new + {updated} updates so far...")
       
    except KeyError as e:
        if total_rows <= 5: # Only warn for first few
            print(f"Warning: Missing column in row {row_num}: {e}")
        continue
    except Exception as e:
        if total_rows <= 5:
            print(f"Warning: Error processing row {row_num}: {e}")
        continue

# Post-process: Ensure LF versions use F datasheets if missing or "Not Available"
lf_mappings = []
for key in list(data.keys()):
    if 'f' in key.lower() and 'lf' not in key.lower():
        lf_key = key.lower().replace('f', 'lf')
        f_entry = data[key]
        if lf_key not in data or data.get(lf_key, {}).get('link', '') == "Not Available":
            # Copy F entry to LF
            data[lf_key] = f_entry.copy()
            # Adjust title: replace first 'F' with 'LF'
            orig_title = data[lf_key]['title']
            data[lf_key]['title'] = orig_title.replace('F', 'LF', 1)
            lf_mappings.append(lf_key)
            print(f"Ensured LF {lf_key} from F {key} (title: {data[lf_key]['title'][:50]}...)")

print(f"LF mappings created: {len(lf_mappings)}")

print(f"\nSummary:")
print(f"Total rows processed: {total_rows}")
print(f"Matched mpu: {matched_mpu}")
print(f"Has link: {has_link}")
print(f"Processed entries: {processed}")
print(f"Updated entries: {updated}")
print(f"Unique keys in output: {len(data)}")
print(f"Excluded entries: {len(excluded)}")
# Dump excluded entries (print as JSON-like for inspection)
print("\nExcluded entries dump:")
print(json.dumps(excluded, indent=4, default=str))
# Write only filtered data to JSON file
output_file = 'disti-datasheets.json'
with open(output_file, 'w') as f:
    json.dump(data, f, indent=4)
print(f"\nTransformation complete. Filtered output written to {output_file}")
if not data:
    print("No data processed. Verify if PIC parts exist in the 'mpu' column by checking later rows.")
else:
    print("Sample key from output:", list(data.keys())[0] if data else "N/A")
    print("Sample entry:", data[list(data.keys())[0]])