
import abc
import os

class Model(object):
    __metaclass__ = abc.ABCMeta

    @abs.asbtractmethod
    def is_file(self):
        return


def file_ext(filename):
    filename = os.path.basename(filename)
    if "." in filename:
        return filename.split(".")[-1].lower()

class FileModel(Model):
    def __init__(self, filename):
        self.filename = filename
    def is_file(self):
        return True
    def src_file(self):
        return self.filename

class InMemoryModel(Model):
    def __init__(self, data):
        self.data = data
    def is_file(self):
        return False
    def src_data(self):
        return self.data

ext2format = {
    "an": "an",
}
def load(filename, format=None):
    ext = file_ext(filename)
    if format is None:
        assert ext in ext2format, "Unknown format '%s'" % ext
        format = ext2format[ext]
    if format == "an":
        return FileModel(filename)
    else:
        assert "Unknown format '%s'" % format


