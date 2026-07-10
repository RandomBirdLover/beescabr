#!/usr/bin/env python3
"""
parse_beeple_calendars.py

Parses the annual Cabrillo Bee Survey Calendar PDFs and writes
data/project_info/beeple_calendar_windows.csv.

Each PDF page is a table with columns: Date | [OT] | TP1 | TP2 | UPMON | BST
Each row is a 4-day survey window with a beeple first name in each transect column.

Run from the beescabr project root:
    python3 scripts/utils/parse_beeple_calendars.py

Requirements:
    pip install pdfplumber

To add a new year:
    1. Drop the new PDF into data/cabr_surveys/nonlethal_inat_beeple/ or anywhere
       accessible, then update CALENDAR_FILES below with its path and year.
    2. Re-run this script -- it rewrites the full CSV from scratch.
"""

import csv
import re
import sys
from datetime import date
from pathlib import Path

try:
    import pdfplumber
except ImportError:
    sys.exit("pdfplumber not installed. Run: pip install pdfplumber")

# ---------------------------------------------------------------------------
# Config: add new years here
# ---------------------------------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parents[2]

CALENDAR_FILES = [
    (PROJECT_ROOT / "data/cabr_surveys/nonlethal_inat_beeple/2022 Cabrillo Bee Survey Calendar.pdf", 2022),
    (PROJECT_ROOT / "data/cabr_surveys/nonlethal_inat_beeple/2023 Cabrillo Bee Survey Calendar.pdf", 2023),
    (PROJECT_ROOT / "data/cabr_surveys/nonlethal_inat_beeple/2024 Cabrillo Bee Survey Calendar.pdf", 2024),
    (PROJECT_ROOT / "data/cabr_surveys/nonlethal_inat_beeple/2025 Cabrillo Bee Survey Calendar.pdf", 2025),
    (PROJECT_ROOT / "data/cabr_surveys/nonlethal_inat_beeple/2026 Cabrillo Bee Survey Calendar.pdf", 2026),
]

OUTPUT_FILE = PROJECT_ROOT / "data/project_info/beeple_calendar_windows.csv"

# ---------------------------------------------------------------------------

MONTH_NUMS = {
    "January":1,"February":2,"March":3,"April":4,"May":5,"June":6,
    "July":7,"August":8,"September":9,"October":10,"November":11,"December":12
}

# Also accept 3-letter abbreviations
MONTH_ABBR = {k[:3]: v for k, v in MONTH_NUMS.items()}


def month_num(name: str) -> int | None:
    name = name.strip()
    return MONTH_NUMS.get(name) or MONTH_ABBR.get(name[:3])


def parse_date_range(raw: str, current_month: int, current_year: int):
    """
    Parse strings like: '19-22', 'Apr 30-3', '26-Mar 1', '30-Aug 2'
    Returns (start_date, end_date) or (None, None) on failure.
    """
    raw = raw.strip()

    # "MonthAbbr D-D" e.g. "Mar 30-2" (start month differs from current)
    m = re.match(r"([A-Za-z]+)\s+(\d+)-(\d+)$", raw)
    if m:
        start_m = month_num(m.group(1))
        start_d, end_d = int(m.group(2)), int(m.group(3))
        if start_m is None:
            return None, None
        # start month < current month -> start is in previous year
        start_y = current_year - 1 if start_m > current_month else current_year
        return date(start_y, start_m, start_d), date(current_year, current_month, end_d)

    # "D-MonthAbbr D" e.g. "26-Mar 1", "30-Aug 2"
    m = re.match(r"(\d+)-([A-Za-z]+)\s+(\d+)$", raw)
    if m:
        start_d = int(m.group(1))
        end_m = month_num(m.group(2))
        end_d = int(m.group(3))
        if end_m is None:
            return None, None
        end_y = current_year + 1 if end_m < current_month else current_year
        return date(current_year, current_month, start_d), date(end_y, end_m, end_d)

    # Simple "D-D"
    m = re.match(r"(\d+)-(\d+)$", raw)
    if m:
        return (date(current_year, current_month, int(m.group(1))),
                date(current_year, current_month, int(m.group(2))))

    return None, None


def split_names(cell: str) -> list[str]:
    """
    Split a cell that may contain multiple names.
    Handles:
      - slash-separated: "Patricia/Juliet" -> ["Patricia", "Juliet"]
      - CamelCase-merged: "MarkJorge" -> ["Mark", "Jorge"]
      - line-break fragments from PDF extraction (already stripped by caller)
    """
    if not cell:
        return []
    cell = cell.strip().replace("\n", "")
    if "/" in cell:
        return [n.strip() for n in cell.split("/") if n.strip()]
    # Split CamelCase (two or more capitalised words merged without space)
    parts = re.findall(r"[A-Z][a-z]+", cell)
    if len(parts) > 1:
        return parts
    return [cell] if cell else []


def parse_calendar(pdf_path: Path, year: int) -> list[dict]:
    rows = []
    with pdfplumber.open(pdf_path) as pdf:
        current_month = None
        transect_headers = []
        for page in pdf.pages:
            for table in page.extract_tables():
                for row in table:
                    if not row or not row[0]:
                        continue
                    cell0 = str(row[0]).strip().replace("\n", "")

                    # Month header row: first cell is a month name, rest are transect headers
                    if cell0 in MONTH_NUMS:
                        current_month = MONTH_NUMS[cell0]
                        transect_headers = [
                            c.strip() for c in row[1:] if c and c.strip()
                        ]
                        continue

                    # Skip stray rows: no digits, or purely a transect name
                    if not re.search(r"\d", cell0):
                        continue
                    if current_month is None:
                        continue

                    start, end = parse_date_range(cell0, current_month, year)
                    if start is None:
                        continue

                    # Assign each non-date cell to its transect column
                    for i, cell in enumerate(row[1:]):
                        if i >= len(transect_headers):
                            break
                        transect = transect_headers[i]
                        for name in split_names(str(cell) if cell else ""):
                            rows.append({
                                "year":         year,
                                "window_start": start.isoformat(),
                                "window_end":   end.isoformat(),
                                "first_name":   name,
                                "transect":     transect,
                            })
    return rows


def main():
    all_rows = []
    for pdf_path, year in CALENDAR_FILES:
        if not pdf_path.exists():
            print(f"WARNING: not found, skipping: {pdf_path}")
            continue
        rows = parse_calendar(pdf_path, year)
        print(f"{year}: {len(rows)} windows parsed from {pdf_path.name}")
        all_rows.extend(rows)

    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_FILE, "w", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=["year","window_start","window_end","first_name","transect"]
        )
        writer.writeheader()
        writer.writerows(all_rows)

    print(f"\nWritten {len(all_rows)} rows to {OUTPUT_FILE}")


if __name__ == "__main__":
    main()
