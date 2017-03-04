
import os

def file_ext(filename):
    filename = os.path.basename(filename)
    if "." in filename:
        return filename.split(".")[-1].lower()


