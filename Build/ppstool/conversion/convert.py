# Example usage: python convert.py pic16f17156 PPStoModule.txt 16F171xxPPSInputSelectionTable.txt 16F171xxPPSOuput.txt pic16f_28pinTemplate_description.xml
# python convert.py pic16f17156 PPStoModule.txt 16F175xxPPSInputSelectionTable.txt 16F175xxPPSOuput.txt pic16f_28pinTemplate_description.xml


import sys
import os

def trim(s):
    return s.strip()


# ---------------------------------------------------------------------------
# Normalize PPS values: supports 'bxxxxx, 0bxxxxx, 0xNN, decimal
# ---------------------------------------------------------------------------
def normalize_pps_value(value):
    v = value.lower().strip()

    # Microchip style: 'b000101 or ‘b000101 (curly apostrophe)
    if v.startswith("'b") or v.startswith("‘b"):
        bits = v[2:]
        try:
            return f"0x{int(bits, 2):02X}"
        except ValueError:
            return value

    # Python style: 0b000101
    if v.startswith("0b"):
        bits = v[2:]
        try:
            return f"0x{int(bits, 2):02X}"
        except ValueError:
            return value

    # Already hex
    if v.startswith("0x"):
        return value.upper()

    # Decimal → convert to hex
    if v.isdigit():
        return f"0x{int(v):02X}"

    # Unknown format → return unchanged
    return value


# ---------------------------------------------------------------------------
# Load PPS → Module mapping
# ---------------------------------------------------------------------------
def load_pps_to_module(filename):
    print(f"Loading PPS→Module map from {filename}")
    mapping = {}
    with open(filename) as f:
        for line in f:
            line = trim(line)
            if not line or line.startswith("'"):
                continue
            parts = [trim(x) for x in line.split(",")]
            if len(parts) < 2:
                continue
            pps, module = parts[:2]
            mapping[pps] = module
    print(f"  Loaded {len(mapping)} mappings")
    return mapping


# ---------------------------------------------------------------------------
# Load PPS Inputs (AWK loads them but does not output them)
# ---------------------------------------------------------------------------
def load_pps_inputs(filename):
    print(f"Loading PPS inputs from {filename}")
    inputs = []
    with open(filename) as f:
        for line in f:
            line = trim(line)
            if not line or line.startswith("'"):
                continue
            parts = [trim(x) for x in line.split(",")]
            if len(parts) < 2:
                continue
            module, pps = parts[:2]
            inputs.append((pps, module))
    print(f"  Loaded {len(inputs)} PPS inputs")
    return inputs


# ---------------------------------------------------------------------------
# Load PPS Outputs (space OR comma separated)
# ---------------------------------------------------------------------------
def load_pps_outputs(filename, pps_to_module):
    print(f"Loading PPS outputs from {filename}")
    outputs = []
    line_number = 0

    with open(filename) as f:
        for raw in f:
            line_number += 1
            line = trim(raw)

            # Skip blank or comment lines
            if not line or line.startswith("'"):
                continue

            # Skip RESERVED
            if "RESERVED" in line.upper():
                continue

            # Accept comma OR space separated formats
            if "," in line:
                parts = [trim(x) for x in line.split(",")]
            else:
                parts = [trim(x) for x in line.split()]

            if len(parts) < 2:
                print(f"Skipping malformed line {line_number}: '{raw.rstrip()}'")
                continue

            value_raw, alias = parts[:2]
            value = normalize_pps_value(value_raw)
            module = pps_to_module.get(alias, "Error")

            xml = (
                f'            <pinAlias alias="{alias}" '
                f'module="{module}" value="{value}" direction="output"/>'
            )
            outputs.append(xml)

    print(f"  Loaded {len(outputs)} PPS outputs")
    return outputs


# ---------------------------------------------------------------------------
# Load template and replace CHIPNAME
# ---------------------------------------------------------------------------
def load_template(filename, chipname):
    print(f"Loading template from {filename}")
    lines = []
    with open(filename) as f:
        for line in f:
            if "CHIPNAME" in line:
                line = line.replace('"CHIPNAME"', f'"{chipname}"')
            lines.append(line.rstrip("\n"))
    print(f"  Loaded {len(lines)} template lines")
    return lines


# ---------------------------------------------------------------------------
# Marker detection for PPS insertion
# ---------------------------------------------------------------------------
MARKERS = [
    "<PPSOUTPUTS>",
    "<PPSOutputs>",
    "<ppsoutputs>",
    "<PPS_OUTPUTS>",
]

def is_pps_marker(line):
    clean = line.replace(" ", "").lower()
    for m in MARKERS:
        if m.replace(" ", "").lower() in clean:
            return True
    return False


# ---------------------------------------------------------------------------
# Write output XML (insert PPSOUTPUTS at marker)
# ---------------------------------------------------------------------------
def write_output(template, pps_outputs, outfile):
    print(f"Writing output to {outfile}")
    found_marker = False

    with open(outfile, "w") as f:
        for line in template:
            if is_pps_marker(line):
                found_marker = True
                f.write(line + "\n")
                for entry in pps_outputs:
                    f.write(entry + "\n")
            else:
                f.write(line + "\n")

    print("PPS outputs inserted successfully.")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    if len(sys.argv) != 6:
        print("Usage: python convert.py CHIP PPStoModule.txt PPSInputs.txt PPSOutputs.txt template.xml")
        sys.exit(1)

    chip = sys.argv[1]
    pps_to_module_file = sys.argv[2]
    pps_inputs_file = sys.argv[3]
    pps_outputs_file = sys.argv[4]
    template_file = sys.argv[5]

    print(f"Processing chip: {chip}")

    pps_to_module = load_pps_to_module(pps_to_module_file)
    pps_inputs = load_pps_inputs(pps_inputs_file)
    pps_outputs = load_pps_outputs(pps_outputs_file, pps_to_module)
    template = load_template(template_file, chip)

    outfile = f"{chip.lower()}_description.xml"
    write_output(template, pps_outputs, outfile)

    print(f"Done. Output file: {outfile}")


if __name__ == "__main__":
    main()
