import os
import csv
import subprocess 
import sys

def format_file_name(file_name):
    """Format file name to capitalize first letter and replace underscores with spaces."""
    file_name = file_name.replace('_', ' ')
    return file_name.capitalize()

def count_lines(file_path):
    """Count the number of lines in a file."""
    with open(file_path, 'r', encoding='utf-8') as file:
        return sum(1 for line in file)

def illum_size(file_path):
    """Get the size of the compiled file."""
    try:
        # print ("dune exec illum hllc", file_path)
        output = subprocess.check_output(["dune", "exec", "illum", "hllc", file_path], stderr=subprocess.STDOUT, text=True)
        return sum(1 for _ in output.splitlines()) 
    except subprocess.CalledProcessError:
        return -1

def main(folder_path, extension):
    """Scan files in a folder with a given extension and output a CSV with file names and their line counts."""
    csv_writer = csv.writer(sys.stdout)
    csv_writer.writerow(['Contract', 'LoC (HeLLUM)', 'LoC (ILLUM)'])

    for file_name in os.listdir(folder_path):
        if file_name.endswith(extension) and os.path.isfile(os.path.join(folder_path, file_name)):
            file_path = os.path.join(folder_path, file_name)
            formatted_file_name = format_file_name(os.path.splitext(file_name)[0])
            hll_lines = count_lines(file_path)
            ill_lines = illum_size(file_path)
            csv_writer.writerow([formatted_file_name, hll_lines, ill_lines])

if __name__ == "__main__":
    folder_path = "test"
    extension = ".hll"

    main(folder_path, extension)