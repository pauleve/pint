
import os

def file_ext(filename):
    """
    Returns the extension of `filename` in lower case; or None if there is no
    extension
    """
    filename = os.path.basename(filename)
    if "." in filename:
        return filename.split(".")[-1].lower()


