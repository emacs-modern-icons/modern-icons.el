import glob
import sys
import xml.etree.ElementTree as ET


# Add the width and height information of the SVG file if they are missing
def fix_width_height_header(file_name):
    ET.register_namespace("", "http://www.w3.org/2000/svg")
    tree = ET.parse(file_name)
    root = tree.getroot()
    # check if root has a width and height attribute
    if "viewBox" in root.attrib:
        viewbox = root.attrib["viewBox"]
        width, height = viewbox.split(" ")[2:]
        if (
            "width" not in root.attrib
            or "height" not in root.attrib
            or root.attrib["width"] != width
            or root.attrib["height"] != height
        ):
            width, height = viewbox.split(" ")[2:]
            root.attrib["width"] = width
            root.attrib["height"] = height
            tree.write(file_name)
            print(f"Fixed SVG file: {file_name}")


# main
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python fix-svg-header.py <icon-dir>")
        sys.exit(1)
    for file in glob.glob(sys.argv[1] + "/**/*.svg", recursive=True):
        fix_width_height_header(file)
