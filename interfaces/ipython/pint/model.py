
import copy
import json
import operator
import os
import re
import subprocess
import tempfile
from urllib.parse import urlparse
from urllib.request import urlretrieve
from xml.dom.minidom import parse as xml_parse_dom
from zipfile import ZipFile

from .cfg import *
from .tools import *
from .ui import *

if IN_IPYTHON:
    from IPython.display import display, FileLink

class InitialState(dict):
    def __init__(self, info):
        if isinstance(info, InitialState):
            # copy constructor
            s = info
            for field in ["info", "defaults", "domain"]:
                setattr(self, field, getattr(s, field))
            self.__override = s.__override.copy()
            super(InitialState, self).__init__(self.defaults)
            return
        self.info = info
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
                if key in self.__override:
                    del self.__override[key]
            else:
                self.__override[key] = value
                super(InitialState, self).__setitem__(key, value)
        else:
            raise TypeError("Invalid value for '%s' (allowed: %s)" \
                                % (key, ", ".join(map(str, self.domain[key]))))

    def copy(self):
        return InitialState(self)

    def having(self, *args, **kwargs):
        s = self.copy()
        s.update(*args, **kwargs)
        return s

    def update(self, *args, **F):
        for E in args:
            if hasattr(E, "keys"):
                for k in E:
                    self[k] = E[k]
            else:
                for k,v in E:
                    self[k] = v
        for k,v in F.items():
            self[k] = v

    def reset(self):
        """Restore to default initial state"""
        self.__override.clear()
        super(InitialState, self).update(self.defaults)

    def changes(self):
        return self.__override.copy()

    def nonzeros(self):
        return dict([(a,i) for (a,i) in self.items() if i > 0])

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


def InfoFields(*fields):
    def plug(cls):
        for field in fields:
            setattr(cls, field, property(lambda self, field=field: self.info[field]))
        return cls
    return plug

@EquipTools
@InfoFields("automata", "local_states", "named_local_states", "features")
class Model(object):
    def __init__(self):
        self.named_states = {}

    def load(self):
        args = ["pint-export", "-l", "nbjson"]
        kwargs = {}
        self.__initial_state = None
        self.populate_popen_args(args, kwargs)
        self.info = json.loads(subprocess.check_output(args, **kwargs).decode())
        self.__initial_state = InitialState(self.info)

    def set_initial_state(self, state):
        assert state.info == self.info
        self.__initial_state = state
    initial_state = property(lambda self: self.__initial_state, set_initial_state)

    def populate_popen_args(self, args, kwargs):
        if self.initial_state is not None and self.initial_state.is_custom():
            args += ["--initial-context", self.initial_state.to_pint()]

    def register_state(self, name, state):
        self.named_states[name] = InitialState(self.info)
        self.named_states[name].update(state)

    def having(self, initial_state=None, **kwargs):
        """
        Returns a copy of the model with supplied modifications

        initial_state: InitialState
          replaces the initial state

        If ``kwargs`` are present, the initial state is replaced by a copy
        updated with kwargs (see `InitialState.having`)

        Exemples:
        >>> m.having(m.named_states["ProT1"]).reachability("Tf1=1")
        >>> m.having(HR=1).reachability("Tf1=1")
        """
        m = copy.copy(self)
        if initial_state:
            m.initial_state = initial_state
        if kwargs:
            m.initial_state = m.initial_state.having(**kwargs)
        return m


class FileModel(Model):
    def __init__(self, filename):
        super(FileModel, self).__init__()
        self.filename = filename
        self.load()

    def populate_popen_args(self, args, kwargs):
        args += ["-i", self.filename]
        super(FileModel, self).populate_popen_args(args, kwargs)

class InMemoryModel(Model):
    def __init__(self, data):
        super(InMemoryModel, self).__init__()
        self.data = data
        self.load()

    def populate_popen_args(self, args, kwargs):
        kwargs["input"] = self.data
        super(FileModel, self).populate_popen_args(args, kwargs)





def import_using_logicalmodel(fmt, inputfile, anfile, simplify=True):

    # some file formats, such as zginml, can define named states
    states = {}

    cleanup_files = []

    if fmt == "zginml":
        def parse_localstate(value):
            a,i = value.split(";")
            return (a,int(i))

        def parse_state(value):
            return dict(map(parse_localstate, value.strip().split()))

        with ZipFile(inputfile) as z:
            with z.open("GINsim-data/initialState") as f:
                dom = xml_parse_dom(f)
                for state in dom.getElementsByTagName("initialState"):
                    name = state.getAttribute("name")
                    states[name] = parse_state(state.getAttribute("value"))

    if fmt in ["zginml", "ginml"]:
        # use GINsim to export to SBML-qual
        info("Invoking GINsim...")
        _, sbmlfile = tempfile.mkstemp(suffix=".sbml")
        _, tmps = tempfile.mkstemp(suffix=".py")
        fd = open(tmps, "w")
        fd.write("""gs.service("SBML").export(gs.open("%s"), "%s")""" % \
                        (os.path.abspath(inputfile), os.path.abspath(sbmlfile)))
        fd.close()
        subprocess.check_call(["GINsim", "-s", os.path.abspath(tmps)])
        os.unlink(tmps)
        inputfile = sbmlfile
        cleanup_files.append(sbmlfile)
        fmt = "sbml"

    subprocess.check_call(["logicalmodel", "%s:an" % fmt,
                                inputfile, anfile])
    if simplify:
        info("Simplifying model...")
        subprocess.check_call(["pint-export", "--simplify", "-i", anfile,
                                "-o", anfile])

    if IN_IPYTHON:
        display(FileLink(anfile))

    model = FileModel(anfile)

    if states:
        for name, state in states.items():
            model.register_state(name, state)
        info("%d state(s) have been registered: %s" \
            % (len(states), ", ".join(states.keys())))

    for filename in cleanup_files:
        os.unlink(filename)

    return model


def download(url, suffix=None):
    filename = new_output_file(suffix=suffix)
    info("Downloading '%s' to '%s'" % (url, filename))
    filename, _ = urlretrieve(url, filename=filename)
    return filename


def sbml_from_cellcollective(modelid):
    url = "http://api.cellcollective.org/model/export/%s?type=SBML" % modelid
    filename = download(url, suffix="%s.zip" % modelid)
    sbmlfile = new_output_file(suffix="%s.sbml" % modelid)
    with ZipFile(filename) as z:
        with z.open("sbml/%s.sbml" % modelid) as z:
            with open(sbmlfile, "w") as s:
                s.write(z.read().decode())
    os.unlink(filename)
    return sbmlfile


def file_ext(filename):
    filename = os.path.basename(filename)
    if "." in filename:
        return filename.split(".")[-1].lower()
ext2format = {
    "an": "an",
    "bn": "boolfunctions",
    "booleannet": "booleannet",
    "boolfunctions": "boolfunctions",
    "boolsim": "boolsim",
    "ginml": "ginml",
    "sbml": "sbml",
    "zginml": "zginml",
}

def load(filename, format=None, simplify=True):
    """
    Load a Pint model from given filename.
    The format is guessed from the filename extension, but can be enforced with
    the `format` parameter.
    Except when loading directly from a '.an' file (native format for Pint), the
    model will be converted to .an and a simplification step is performed.
    This latter stage can be deactivated with `simplify=False`

    `filename` can be either a path to local file, or an URL. In the latter
    case, it will be downloaded locally first, and then processed as a local
    file.

    Supported formats:
    - an (automata network), native Pint file format
    - ginml, imported using GINsim+logicalmodel (use intermediate SBML
          conversion)
    - zginml, like ginml, with in addition the support of named initial states
    - sbml (SBML-qual), imported using logicalmodel
    - boolsim, booleannet, boolfunctions: Boolean network formats, imported
          using logical model. Files with '.bn' extensions are assumed to be in
          boolfunctions format.

    Returns a `Model` instance.
    If the model results from an importation, IPython displays the link to the
    generated .an file.
    """

    match_cellcollective = re.search("https?://[^/]*\\bcellcollective\.org/#(\\d+)\\b", filename)
    if match_cellcollective:
        modelid = match_cellcollective.group(1)
        filename = sbml_from_cellcollective(modelid)
        name = modelid
        format = "sbml"

    else:
        bname = os.path.basename(filename)
        ext = file_ext(filename)
        name = bname[:-len(ext)-1]
        if format is None:
            assert ext in ext2format, "Unknown extension '%s'" % ext
            format = ext2format[ext]

    uri = urlparse(filename)
    if uri.netloc:
        filename = download(uri.geturl(), suffix=bname)
    else:
        assert os.path.exists(filename)

    if format == "an":
        info("Source file is in Automata Network (an) format")
        return FileModel(filename)

    elif format in ["boolfunctions", "boolsim", "booleannet", "sbml",
                    "ginml", "zginml"]:
        info("Source file is in %s format, importing with logicalmodel" \
                    % format)
        anfile = new_output_file(suffix="%s.an"%name)
        return import_using_logicalmodel(format, filename, anfile,
                    simplify=simplify)

    else:
        raise ValueError("Format '%s' is not supported." % format)


__all__ = ["load", "FileModel", "InMemoryModel"]

