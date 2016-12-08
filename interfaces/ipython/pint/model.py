
import json
import os
import subprocess
import tempfile
from urllib.parse import urlparse
from urllib.request import urlretrieve

from IPython.display import display, FileLink

from .cfg import *
from .tools import *
from .ui import *

class InitialState(dict):
    def __init__(self, info):
        self.defaults = info["initial_state"]
        super(InitialState, self).__init__(self.defaults)
        self.domain = {}
        for a in info["automata"]:
            self.domain[a] = set(info["local_states"][a] +
                                info["named_local_states"][a])
            if type(self.defaults[a]) is list:
                self.defaults[a] = tuple(self.defaults[a])
        self.__override = {}

    def is_custom(self):
        return len(self.__override) > 0

    def __delitem__(self, key):
        del self.__override[key]
        super(InitialState, self).__setitem__(key, self.defaults[key])

    def __getitem__(self, key):
        return self.__override.get(key, self.defaults[key])

    def __setitem__(self, key, value):
        if type(value) not in [int, str, list, set, tuple]:
            raise TypeError("Invalid type value")
        if key not in self.defaults:
            raise TypeError("Unknown automaton '%s'" % key)
        if type(value) in [int, str] and value in self.domain[key] \
                or self.domain[key].issuperset(set(value)):
            if type(value) not in [int, str]:
                value = tuple(value)
            if value == self.defaults[key]:
                del self.__override[key]
            else:
                self.__override[key] = value
                super(InitialState, self).__setitem__(key, value)
        else:
            raise TypeError("Invalid value for '%s' (allowed: %s)" \
                                % (key, ", ".join(map(str, self.domain[key]))))

    def reset(self):
        """Restore to default initial state"""
        self.__override.clear()
        super(InitialState, self).update(self.defaults)

    def to_pint(self):
        def fmt_values(i):
            if type(i) is int:
                return [str(i)]
            elif type(i) is str:
                return ["\"%s\"" % i]
            else:
                return [fmt_values(j) for j in i]
        def pint_of_keyvalue(a,i):
            return ["\"%s\"=%s" % (a,i) for i in fmt_values(i)]
        lss = []
        for a, i in self.__override.items():
            lss += pint_of_keyvalue(a,i)
        return ",".join(lss)


@EquipTools
class Model(object):
    def load(self):
        args = ["pint-export", "-l", "nbjson"]
        kwargs = {}
        self.initial_state = None
        self.populate_popen_args(args, kwargs)
        self.info = json.loads(subprocess.check_output(args, **kwargs).decode())
        self.initial_state = InitialState(self.info)

    @property
    def automata(self):
        return self.info["automata"]
    @property
    def local_states(self):
        return self.info["local_states"]
    @property
    def named_local_states(self):
        return self.info["named_local_states"]
    @property
    def features(self):
        return self.info["features"]

    def populate_popen_args(self, args, kwargs):
        if self.initial_state is not None and self.initial_state.is_custom():
            args += ["--initial-context", self.initial_state.to_pint()]


def file_ext(filename):
    filename = os.path.basename(filename)
    if "." in filename:
        return filename.split(".")[-1].lower()

class FileModel(Model):

    def __init__(self, filename):
        self.filename = filename
        self.load()

    def populate_popen_args(self, args, kwargs):
        args += ["-i", self.filename]
        super(FileModel, self).populate_popen_args(args, kwargs)

class InMemoryModel(Model):

    def __init__(self, data):
        self.data = data
        self.load()

    def populate_popen_args(self, args, kwargs):
        kwargs["input"] = self.data
        super(FileModel, self).populate_popen_args(args, kwargs)


def import_using_logicalmodel(fmt, inputfile, outputfile, simplify=True):
    subprocess.check_call(["logicalmodel", "%s:an" % fmt,
                                inputfile, outputfile])
    if simplify:
        info("Simplifying model...")
        subprocess.check_call(["pint-export", "--simplify", "-i", outputfile,
                                "-o", outputfile])
    return outputfile


ext2format = {
    "an": "an",
    "bn": "boolfunctions",
    "ginml": "ginml",
    "sbml": "sbml",
}

def load(filename, format=None, simplify=True):
    bname = os.path.basename(filename)
    ext = file_ext(filename)
    name = bname[:-len(ext)-1]
    if format is None:
        assert ext in ext2format, "Unknown extension '%s'" % ext
        format = ext2format[ext]

    uri = urlparse(filename)
    if uri.netloc:
        filename = new_output_file(suffix=bname)
        url = uri.geturl()
        info("Downloading '%s' to '%s'" % url, filename)
        filename, _ = urlretrieve(url, filename=filename)
    else:
        assert os.path.exists(filename)

    if format == "an":
        info("Source file is in Automata Network (an) format")
        return FileModel(filename)
    elif format in ["boolfunctions", "boolsim", "booleannet",
                        "ginml", "sbml"]:
        info("Source file is in %s format, importing using logicalmodel" \
                    % format)
        anfile = new_output_file(suffix="%s.an"%name)
        anfile = import_using_logicalmodel(format, filename, anfile,
                    simplify=simplify)
        display(FileLink(anfile))
        return FileModel(anfile)
    else:
        raise ValueError("Unknown format '%s'" % format)


__all__ = ["load", "FileModel", "InMemoryModel"]

