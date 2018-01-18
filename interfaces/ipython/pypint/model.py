
import base64
import copy
import json
import operator
import os
import random
import re
import subprocess
import tempfile
from urllib.parse import urlparse
from urllib.request import urlretrieve
from xml.dom.minidom import parse as xml_parse_dom
from zipfile import ZipFile

from colomoto_jupyter.io import download, ensure_localfile

from .cfg import *
from .tools import *
from .types import *
from .ui import *
from .utils import *
from .converters import CONVERTERS

if IN_IPYTHON:
    from IPython.display import display, FileLink, HTML
    from colomoto_jupyter.upload import jupyter_upload

class InitialState(dict):
    """
    A state is represented with a ``dict``-like python object, which associates
    each automaton to a local state, or to a set of initial local states.

    The automata, their value domain, as well as their initial *default* value,
    are tied to a :py:class:`.Model` object.

    A new `InitialState` should be instanciated only from an existing
    `InitialState` instance, typically :py:attr:`.Model.initial_state`,
    either using the copy constructor, or using :py:meth:`.InitialState.copy`,
    or using :py:meth:`.InitialState.having` method (recommended).
    """
    def __init__(self, state):
        if isinstance(state, InitialState):
            # copy constructor
            s = state
            for field in ["info", "defaults", "domain"]:
                setattr(self, field, getattr(s, field))
            self.__override = s.__override.copy()
            super(InitialState, self).__init__(self.defaults)
            return
        self.info = state
        self.defaults = self.info["initial_state"]
        super(InitialState, self).__init__(self.defaults)
        self.domain = {}
        for a in self.info["automata"]:
            self.domain[a] = set(self.info["local_states"][a])
            if type(self.defaults[a]) is list:
                self.defaults[a] = tuple(self.defaults[a])
        self.__override = {}

    def is_custom(self):
        """
        :returns: `True` iff the state is different from the default initial state of the AN.
        """
        return len(self.__override) > 0

    def __delitem__(self, a):
        """
        Restore the default initial value of automaton `a`.

        >>> del m.initial_state["a"]
        """
        del self.__override[a]
        super(InitialState, self).__setitem__(a, self.defaults[a])

    def __getitem__(self, key):
        return self.__override.get(key, self.defaults[key])

    def __setitem__(self, a, i):
        """
        Sets the initial state of automaton `a` to `i`.
        `i` can be either a single local state, or a set of local states.
        """
        key = a
        value = i
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
        """
        :returns: a copy of the object.
        """
        return InitialState(self)

    def having(self, *args, **kwargs):
        """
        :returns: a copy of the object modified with :py:meth:`.update` method.
        """
        s = self.copy()
        s.update(*args, **kwargs)
        return s

    def update(self, *args, **F):
        """
        Sets the initial local states of specified automata. Arguments can be
        either `dict`-like objects, lists of pairs or keywords.

        Examples:

        >>> s.update(a=1, b=0)  # updates automaton 'a' and 'b'
        >>> s.update({"a": 1, "b": 0})  # equivalent to above
        """
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
        """
        Restore to default initial state.
        """
        self.__override.clear()
        super(InitialState, self).update(self.defaults)

    def changes(self):
        """
        :returns: A `dict` object of the changes to the default initial state.
        """
        return self.__override.copy()

    def nonzeros(self):
        """
        :returns: A `dict` object associating automata to their initial local
            states when it is different from ``0``.
        """
        return dict([(a,i) for (a,i) in self.items() if isinstance(i, str) or i > 0])

    def to_pint(self, full=False):
        """
        :returns: Pint text representation of the state
        :keyword full: if `True`, also output initial states being ``0``.
        """
        items = self.items() if full else self.__override.items()
        return pint_of_localstates(items)


def InfoFields(*fields):
    def plug(cls):
        for field in fields:
            setattr(cls, field, property(lambda self, field=field: self.info[field]))
        return cls
    return plug

@EquipTools
@InfoFields("automata", "local_states", "features", "local_transitions")
class Model(object):
    """
    Abstract class for manipulating and analyzing an AN (Automata Network) model.

    A `Model` is typically instanciated using the :py:func:`.load` function, or
    using :py:class:`.InMemoryModel` for small in-memory AN specifications.

    .. py:attribute:: automata

        The list of automata names

    .. py:attribute:: local_states

        Dictionnary associating to each automaton its list of local states
        (`str` or `int` list).

    .. py:attribute:: local_transitions

        The list of local transitions (:py:class:`.LocalTransition` or
        :py:class:`.SynchronizedLocalTransitions`) in the AN specification.

    .. py:attribute:: initial_state

        The current initial state of the model (:py:class:`.InitialState`
        instance).

    .. py:attribute:: named_states

        A dictionnary of named :py:class:`.InitialState` instances.

    .. py:attribute:: features

        A list of features the AN model is using, if any, among:

        * `"synced_transitions"`: the AN uses synchronized local transitions.
    """
    def __init__(self):
        self.named_states = {}

    def load(self):
        args = ["pint-export", "-l", "nbjson"]
        kwargs = {}
        self.__initial_state = None
        self.populate_popen_args(args, kwargs)
        kwargs["stderr"]= subprocess.PIPE
        try:
            self.info = json.loads(subprocess.check_output(args, **kwargs).decode())
        except subprocess.CalledProcessError as e:
            # backward compatible 'raise e from None'
            e = PintProcessError(e.returncode, e.cmd, e.output, e.stderr)
            e.__cause__ = None
            raise e

        key = "local_transitions"
        if key in self.info:
            self.info[key] = [local_transition_from_json(d) \
                                    for d in self.info[key]]
        self.__initial_state = InitialState(self.info)

    def set_initial_state(self, state):
        assert state.info == self.info
        self.__initial_state = state
    initial_state = property(lambda self: self.__initial_state, set_initial_state)

    def populate_popen_args(self, args, kwargs):
        if self.initial_state is not None and self.initial_state.is_custom():
            args += ["--initial-context", self.initial_state.to_pint()]

    def register_state(self, name, state):
        """
        Register a named state in :py:attr:`named_states` attribute.

        :param str name: name of the state
        :param state: state to register
        :type state: dict or .InitialState
        """
        self.named_states[name] = InitialState(self.info)
        self.named_states[name].update(state)

    def having(self, initial_state=None, **kwargs):
        """
        Returns a copy of the model with supplied modifications

        :param initial_state: new initial state
        :type initial_state: .InitialState or str
        :keyword kwargs: if non-empty, defines a new initial state from
            :py:attr:`.Model.initial_state` by calling
            :py:meth:`.InitialState.having`.
        :rtype: :py:class:`.Model`

        Exemples:

        >>> m.having(m.named_states["ProT1"]).reachability("Tf1=1")
        >>> m.having(HR=1).reachability("Tf1=1")
        """
        m = copy.copy(self)
        if isinstance(initial_state, str):
            m.initial_state = m.named_states[initial_state]
            return m
        m.initial_state = self.initial_state.copy()
        args = []
        if initial_state:
            args.append(initial_state)
        m.initial_state.update(*args, **kwargs)
        return m


class FileModel(Model):
    """
    Concrete class of :py:class:`.Model` for AN model stored in a local file.
    """
    def __init__(self, filename):
        super(FileModel, self).__init__()
        self.filename = filename
        self.load()

    def populate_popen_args(self, args, kwargs):
        args += ["-i", self.filename]
        super(FileModel, self).populate_popen_args(args, kwargs)

    def source(self):
        """
        Returns the text source in Pint native format
        """
        with open(self.filename) as f:
            data = f.read()
        return data

class InMemoryModel(Model):
    """
    Concrete class of :py:class:`.Model` for an AN model stored in memory.
    """
    def __init__(self, data):
        super(InMemoryModel, self).__init__()
        self.data = data
        self.load()

    def populate_popen_args(self, args, kwargs):
        kwargs["input"] = self.data
        super(FileModel, self).populate_popen_args(args, kwargs)

    def source(self):
        """
        Returns the text source in Pint native format
        """
        return self.data



def import_with_ginsim(fmt, inputfile, anfile, simplify=True):

    # some file formats, such as zginml, can define named states
    states = {}

    cleanup_files = []

    if fmt == "zginml":
        def parse_localstate(value):
            a,i = value.split(";")
            return (a,int(i))

        def parse_state(value):
            return dict(map(parse_localstate, value.strip().split()))

        initialState_e = "GINsim-data/initialState"
        with ZipFile(inputfile) as z:
            if initialState_e in z.namelist():
                with z.open("GINsim-data/initialState") as f:
                    dom = xml_parse_dom(f)
                    for state in dom.getElementsByTagName("initialState"):
                        name = state.getAttribute("name")
                        states[name] = parse_state(state.getAttribute("value"))

    args = ["GINsim", "-lqm", "-if", fmt, inputfile, "-of", "an", anfile]
    subprocess.check_call(args)

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


ext2format = {
    "an": "an",
    "bc": "biocham",
    "bcx": "cadbiom",
    "bn": "boolfunctions",
    "booleannet": "booleannet",
    "boolfunctions": "boolfunctions",
    "boolsim": "boolsim",
    "ginml": "ginml",
    "sbgn": "sbgnpd",
    "sbgnml": "sbgnpd",
    "sbml": "sbml",
    "zginml": "zginml",
}

LOAD_SUPPORTED_FORMATS = list(sorted(set(ext2format.values())))
"""
Formats supported by :py:func:`.load` method
"""


def load(filename=None, format=None, simplify=True, **opts):
    """
    Load a Pint model from given filename.
    The format is guessed from the filename extension, but can be enforced with
    the `format` parameter.
    Except when loading directly from a ``.an`` file (native format for Pint), the
    model will be converted to .an and a simplification step is performed.
    This latter stage can be deactivated with `simplify=False`

    `filename` can be either a path to local file, or an URL. In the latter
    case, it will be downloaded locally first, and then processed as a local
    file.

    Within IPython/Jupyter web notebook, the `filename` argument can be omited:
    in that case, a file upload form will be displayed.

    Supported file extensions:

    * ``.an`` (automata network), native Pint file format;
    * ``.ginml``, imported using GINsim
    * ``.zginml``, like ginml, with in addition the support of named initial states;
    * ``.sbml`` (SBML-qual), imported using GINsim (with bioLQM);
    * ``.boolsim``, ``.booleannet``, ``.boolfunctions`` (or ``.bn``): Boolean network formats, imported
      using GINsim (with bioLQM)`.
    * ``.bc``: Biocham model - the importation implements its Boolean semantics
      (experimental feature)

    Supported URL schemes:

    * `CellCollective <htts://cellcollective.org>`_ models can be imported by
      supplying the URL of the model main page. The SBML file location is
      determined automatically;
    * any other URL is treated as a link to a file with supported extension. It
      will be downloaded and then loaded as a local file.

    Returns a :py:class:`.Model` instance.
    If the model results from an importation, IPython displays the link to the
    generated .an file.

    Examples:

    >>> m1 = pypint.load("mylocalfile.an")
    >>> m2 = pypint.load("http://ginsim.org/sites/default/files/Frontiers-Th-Full-model-annotated.zginml")
    >>> m3 = pypint.load("https://cellcollective.org/#4705/septation-initiation-network")

    .. seealso:: :py:data:`.LOAD_SUPPORTED_FORMATS` for supported formats.
    """

    if filename is None:
        if IN_IPYTHON:
            return jupyter_upload("pypint.load", "pypint.load")
        else:
            raise TypeError("missing filename argument")

    filename = ensure_localfile(filename)

    bname = os.path.basename(filename)
    ext = file_ext(filename)
    name = bname[:-len(ext)-1][-15:]
    if format is None:
        assert ext in ext2format, "Unknown extension '%s'" % ext
        format = ext2format[ext]

    def make_anfile():
        return new_output_file(suffix="%s.an"%name)

    if format == "an":
        info("Source file is in Automata Network (an) format")
        return FileModel(filename, **opts)

    elif format in ["boolfunctions", "boolsim", "booleannet", "sbml",
                    "ginml", "zginml"]:
        info("Source file is in %s format, importing with GINsim" \
                    % format)
        anfile = make_anfile()
        return import_with_ginsim(format, filename, anfile,
                    simplify=simplify, **opts)

    elif format in CONVERTERS:
        info("Source file is in %s format" % format)
        anfile = make_anfile()
        with open(anfile, "w") as outfd:
            CONVERTERS[format](filename, outfd, **opts)
        return FileModel(anfile)
    else:
        raise ValueError("Format '%s' is not supported." % format)


__all__ = [
    "LOAD_SUPPORTED_FORMATS",
    "load",
    "Model", "FileModel", "InMemoryModel",
    "InitialState"]

