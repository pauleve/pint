"""
This code is from
https://github.com/cesaro/cunf/blob/master/tools/ptnet/net.py
Author: César Rodríguez - https://github.com/cesaro
"""

import sys
import xml.parsers.expat

class Transition :
    def __init__ (self, name) :
        self.ident = None
        self.name = name
        self.pre = set ()
        self.cont = set ()
        self.post = set ()
        self.weight_pre = {}
        self.weight_cont = {}
        self.weight_post = {}
        self.m = 0
        self.c = 0

    def __repr__ (self) :
        return str (self.name)

    def __str__ (self) :
        return "'%s' Pre %s;  Cont %s;  Post %s" \
                % (self.__repr__ (), self.weight_pre, self.weight_cont, self.weight_post)

    def pre_add (self, p, w=1) :
        if p in self.pre : return
        assert (w >= 1)
        self.pre.add (p)
        self.weight_pre[p] = w
        p.post_add (self, w)

    def cont_add (self, p, w=1) :
        if p in self.cont : return
        assert (w >= 1)
        self.cont.add (p)
        self.weight_cont[p] = w
        p.cont_add (self, w)

    def post_add (self, p, w=1) :
        if p in self.post : return
        assert (w >= 1)
        self.post.add (p)
        self.weight_post[p] = w
        p.pre_add (self, w)

    def pre_rem (self, p) :
        if p not in self.pre : return
        self.pre.remove (p)
        del self.weight_pre[p]
        p.post_rem (self)

    def cont_rem (self, p) :
        if p not in self.cont : return
        self.cont.remove (p)
        del self.weight_cont[p]
        p.cont_rem (self)

    def post_rem (self, p) :
        if p not in self.post : return
        self.post.remove (p)
        del self.weight_post[p]
        p.pre_rem (self)

class Place :
    def __init__ (self, name) :
        self.ident = None
        self.name = name
        self.pre = set ()
        self.cont = set ()
        self.post = set ()
        self.weight_pre = {}
        self.weight_cont = {}
        self.weight_post = {}
        self.m = 0
        self.c = 0
        self.marking_eq_mark = 0

    def __repr__ (self) :
        return str (self.name)

    def __str__ (self) :
        return "'%s' Pre %s;  Cont %s;  Post %s" \
                % (self.__repr__ (), self.weight_pre, self.weight_cont, self.weight_post)
                #% (self.__repr__ (), self.pre, self.cont, self.post)

    def pre_add (self, t, w=1) :
        if t in self.pre : return
        assert (w >= 1)
        self.pre.add (t)
        self.weight_pre[t] = w
        t.post_add (self, w)

    def cont_add (self, t, w=1) :
        if t in self.cont : return
        assert (w >= 1)
        self.cont.add (t)
        self.weight_cont[t] = w
        t.cont_add (self, w)

    def post_add (self, t, w=1) :
        if t in self.post : return
        assert (w >= 1)
        self.post.add (t)
        self.weight_post[t] = w
        t.pre_add (self, w)

    def pre_rem (self, t) :
        if t not in self.pre : return
        self.pre.remove (t)
        del self.weight_pre[t]
        t.post_rem (self)

    def cont_rem (self, t) :
        if t not in self.cont : return
        self.cont.remove (t)
        del self.weight_cont[t]
        t.cont_rem (self)

    def post_rem (self, t) :
        if t not in self.post : return
        self.post.remove (t)
        del self.weight_post[t]
        t.pre_rem (self)

class Marking :
    frozen_eq = False
    def __init__ (self) :
        self.__marking = {}
        self.formulas_sat = set ()
        self.formulas_undef = set ()
        self.fully_expanded = False
        self.m = 0
        self.__hash = 0

    def __getitem__ (self, place) :
        try :
            return self.__marking[place];
        except KeyError :
            return 0

    def __setitem__ (self, place, value) :
        #assert (self.__hash == hash (self))
        self.__update_hash (place, value)
        self.__marking[place] = value
        if (value == 0) : del self.__marking[place]
        #assert (self.__hash == hash (self))
        for p in self.__marking :
            assert (self.__marking[p] > 0)
        return value

    def __iter__ (self) :
        for p in self.__marking :
            yield p

    def __repr__ (self) :
        s = " ".join ("%s=%d" % (repr (p), self.__marking[p]) for p in self.__marking)
        return "[%s, %s]" % (str (id (self)), s)

    def __str__ (self) :
        s = "===============\n"
        s += "marking:"
        for p in sorted (self.__marking) :
            s += "\n\t%8s = %d" % (repr (p), self.__marking[p])
        s += "\nfully_expanded:\n\t" + str (self.fully_expanded)
        s += "\nformulas_sat:\n\t" + str (self.formulas_sat)
        s += "\nformulas_undef:\n\t" + str (self.formulas_undef)
        s += "\n===============\n"
        return s

    def __update_hash (self, place, value) :
        if place in self.__marking :
            self.__hash -= id (place) + self.__marking[place]
        if value != 0 :
            self.__hash += id (place) + value

    def __hash__ (self) :
#        i = 0
#        for k,v in self.__marking.items () :
#            i += id (k) + v
#        print "__hash__: i", i, "__hash", self.__hash
#        assert (self.__hash == i)
        return self.__hash

    def __eq__ (self, other) :
        if Marking.frozen_eq :
            return id (self) == id (other)
        else :
            return self.__marking == other.__marking

    def clone (self) :
        new = Marking () 
        for place in self.__marking :
            new.__marking[place] = self.__marking[place]
        new.__hash = self.__hash
        return new

    def is_sat (self, formula) :
        return formula in self.formulas_sat

    def is_unsat (self, formula) :
        return formula not in self.formulas_sat and \
                formula not in self.formulas_undef

    def is_undef (self, formula) :
        return formula in self.formulas_undef

class Net :
    def __init__ (self, sanity_check=True) :
        self.places = []
        self.trans = []
        self.m0 = Marking ()
        self.sanity_check = sanity_check
        self.m = 1

        self.__trans_lookup_table = None
        self.__places_lookup_table = None

        self.author = ''
        self.title = ''
        self.date = ''
        self.note = ''
        self.version = ''

    def trans_lookup (self, ident) :

        if self.__trans_lookup_table == None :
            self.__trans_lookup_table = {}
            for t in self.trans :
                self.__trans_lookup_table[t.ident] = t

        if ident in self.__trans_lookup_table :
            return self.__trans_lookup_table[ident]
        return None

    def place_lookup (self, ident) :

        if self.__places_lookup_table == None :
            self.__places_lookup_table = {}
            for t in self.places :
                self.__places_lookup_table[t.ident] = t

        if ident in self.__places_lookup_table :
            return self.__places_lookup_table[ident]
        return None

    def new_mark (self) :
        self.m += 1
        return self.m

    def place_add (self, name, m0=0) :
        p = Place (name)
        self.places.append (p)
        if m0 : self.m0[p] = m0
        # print 'place_add', name, m0
        return p

    def trans_add (self, name) :
        t = Transition (name)
        self.trans.append (t)
        # print 'trans_add', name
        return t

    def enables (self, m, t) :
        for p in t.pre :
            if m[p] < t.weight_pre[p] : return False
        for p in t.cont :
            if m[p] < t.weight_cont[p] : return False
        return True

    def enabled (self, marking) :
        #result = set ()
        candidates = set ()
        m = self.new_mark ()
        for p in marking :
            candidates |= p.post | p.cont
            p.m = m
        for t in candidates :
            found = False
            for p in t.pre :
                assert ((p.m != m or marking[p] < t.weight_pre[p]) == (marking[p] < t.weight_pre[p]))
                if marking[p] < t.weight_pre[p] :
                    found = True
                    break
            if found :
                continue
            found = False
            for p in t.cont :
                if marking[p] < t.weight_cont[p] :
                    found = True
                    break
            if not found :
                yield t
                #result.add (t)
        #assert result == self.enabled2 (marking)
        #return result

    def enabled2 (self, marking) :
        result = set ()
        for t in self.trans :
            if self.enables (marking, t) : result.add (t)
        return result

    def fire (self, marking, t) :
        assert (self.enables (marking, t))
        new_marking = marking.clone ()
        for p in t.pre :
            new_marking[p] -= t.weight_pre[p]
        for p in t.post :
            new_marking[p] += t.weight_post[p]
        return new_marking

    def fire_run (self, run, m=None) :
        if m == None : m = self.m0.clone ()
        for t in run :
#            db ('at', list (m))
#            db ('firing', t)
            if not self.enables (m, t) :
                raise Exception, 'Cannot fire, transition not enabled'
            m -= set (t.pre)
            m |= set (t.post)
#        db ('reached', list (m), 'enables', self.enabled (m))
        return m

    def plain2cont (self) :
        for t in self.trans :
            s = t.pre & t.post
            for p in s :
                t.pre_rem (p)
                t.post_rem (p)
                t.cont_add (p)

    def cont2plain (self) :
        for t in self.trans :
            for p in t.cont :
                t.pre_add (p)
                t.post_add (p)
            for p in list (t.cont) :
                t.cont_rem (p)

    def cont2pr (self) :
        pass

    def __stubbornify_make_fork (self, pin, pleft, pright, idx) :
        # print 'make_fork pin', pin, 'pleft', pleft, 'pright', pright, 'idx', idx
        pfork = self.place_add ('aux_%d_fork' % idx)

        ponl = self.place_add ('aux%d_onl' % idx, 1)
        ponr = self.place_add ('aux%d_onr' % idx, 1)
        poffl = self.place_add ('aux%d_offl' % idx)
        poffr = self.place_add ('aux%d_offr' % idx)

        tonl_onr = self.trans_add ('aux%d_init__onl_onr' % idx)
        tonl_offr = self.trans_add ('aux%d_init__onl_offr' % idx)
        toffl_onr = self.trans_add ('aux%d_init__offl_onr' % idx)
        # this transition is dead, actually, we can never have both offl and offr
        # toffl_offr = self.trans_add ('aux%d_init__offl_offr' % idx)

        tl = self.trans_add ('aux%d_l__go' % idx)
        tlwait = self.trans_add ('aux%d_l__wait' % idx)
        tr = self.trans_add ('aux%d_r__go' % idx)
        trwait = self.trans_add ('aux%d_r__wait' % idx)

        # setup this mess
        pin.post_add (tonl_onr)
        pin.post_add (tonl_offr)
        pin.post_add (toffl_onr)
        #pin.post_add (toffl_offr)

        pfork.pre_add (tonl_onr)
        pfork.pre_add (tonl_offr)
        pfork.pre_add (toffl_onr)
        #pfork.pre_add (toffl_offr)

        # on left, on right; nothing to do
        tonl_onr.cont_add (ponl)
        tonl_onr.cont_add (ponr)

        # on left, off right; switch right on
        tonl_offr.cont_add (ponl)
        tonl_offr.pre_add (poffr)
        tonl_offr.post_add (ponr)

        # off left, on right; switch left on
        toffl_onr.cont_add (ponr)
        toffl_onr.pre_add (poffl)
        toffl_onr.post_add (ponl)

        # off left, off right; switch both on
        # toffl_offr.pre_add (poffl)
        # toffl_offr.pre_add (poffr)
        # toffl_offr.post_add (ponl)
        # toffl_offr.post_add (ponr)

        # at this point pfork, ponl, ponr are marked, regardless of the initial
        # state

        # choose left
        tl.pre_add (pfork)
        tl.pre_add (ponr)
        tl.post_add (poffr)
        tl.post_add (pleft)

        # choose right
        tr.pre_add (pfork)
        tr.pre_add (ponl)
        tr.post_add (poffl)
        tr.post_add (pright)

        # wait left
        tlwait.pre_add (poffl)
        tlwait.post_add (ponl)

        # wait right
        trwait.pre_add (poffr)
        trwait.post_add (ponr)

    def __stubbornify_make_fork_more (self, pin, outplaces, idx) :
        #print 'make_fork_more, pin', pin, 'outplaces', outplaces, 'idx', idx
        if len (outplaces) == 2 :
            self.make_fork (pin, outplaces[0], outplaces[1], idx)
            return idx + 1
        pright = self.place_add ('aux%d_partialfork' % idx)
        self.__stubbornify_make_fork (pin, outplaces[0], pright, idx)
        return self.__stubbornify_make_fork_more (pright, outplaces[1:], idx + 1)

    def __stubbornify_make_race (self, p, idx) :
        assert len (p.post) >= 2
        #print 'make_race', p, idx

        # make a new place for each transition in the postset
        place_choices = []
        for t in list (p.post) :
            pnew = self.place_add ('aux%d_priv_%s_%s' % (idx, p.name, t.name))
            place_choices.append (pnew)
            t.pre_rem (p)
            t.pre_add (pnew)
        return self.__stubbornify_make_fork_more (p, place_choices, idx + 1) 

    def stubbornify (self) :
        idx = 0
        for p in list (self.places) :
            #print 'stubbornify loop', p, id (p)
            if len (p.post) >= 2 :
                idx = self.__stubbornify_make_race (p, idx)

    def make_unsafe (self, places) :
        t1 = self.trans_add ('uns_t1')
        t2a = self.trans_add ('uns_t2a')
        t2b = self.trans_add ('uns_t2b')
        p2a = self.place_add ('uns_p2a')
        p2b = self.place_add ('uns_p2b')
        p3 = self.place_add ('uns_p3')
        t3 = self.trans_add ('uns_t3')

        for p in places :
            t1.pre_add (p)
        t1.post_add (p2a)
        t1.post_add (p2b)
        p2a.post_add (t2a)
        p2b.post_add (t2b)
        t2a.post_add (p3)
        t2b.post_add (p3)
        p3.post_add (t3)

    def write (self, f, fmt='pep', m=0) :
        if fmt == 'pep' : return self.__write_pep (f, m)
        if fmt == 'dot' : return self.__write_dot (f, m)
        if fmt == 'grml' : return self.__write_grml (f, m)
        if fmt == 'pnml' : return self.__write_pnml (f, m)
        if fmt == 'ctxdot' : return self.__write_ctxdot (f, 5, 1)
        if fmt == 'pt1' : return self.__write_pt1 (f, m)
        raise Exception, "'%s': unknown output format" % fmt

    def __write_pep (self, f, m=0) :
        f.write ('PEP\nPetriBox\nFORMAT_N2\n')
        s = ''
        for att in ['author', 'title', 'date', 'note', 'version'] :
            if att in self.__dict__ :
                s += '% ' + att + ' "' + self.__dict__[att].strip () + '"\n'
        f.write (s + 'PL\n');

        tab = {'first' : 'element'}
        for p in self.places :
            if m != 0 and p.m != m : continue
            m1 = 'M%d' % self.m0[p] if self.m0[p] > 0 else ''
            f.write ('%d"%s"9@9%s\n' % (len (tab), repr (p), m1))
            tab[p] = len (tab)
        f.write ('TR\n')
        for t in self.trans :
            if m != 0 and t.m != m : continue
            f.write ('%d"%s"9@9\n' % (len (tab), repr (t)))
            tab[t] = len (tab)

        out = ''
        f.write ('TP\n')
        for t in self.trans :
            for p in t.post :
                if m != 0 and (t.m != m or p.m != m) : continue
                f.write ('%d<%d\n' % (tab[t], tab[p]))
            for p in t.cont :
                if m != 0 and (t.m != m or p.m != m) : continue
                out += '%d<%d\n' % (tab[t], tab[p])

        f.write ('PT\n')
        for t in self.trans :
            for p in t.pre :
                if m != 0 and (t.m != m or p.m != m) : continue
                f.write ('%d>%d\n' % (tab[p], tab[t]))

        if out : f.write ('RA\n' + out)

    def __write_pt1 (self, f, m=0) :
        # we ignore m !!
        f.write ('PT1\n%d\n%d\n' % (len (self.places), len (self.trans)))
        s = ''

        tab = {}
        for p in self.places :
            f.write ('"%s" %d\n' % (repr (p), self.m0[p]))
            tab[p] = len (tab)

        for t in self.trans :
            assert len (t.cont) == 0
            f.write ('"%s" %d %d' % (repr (t), len (t.pre), len (t.post)))
            for p in t.pre : f.write (' %d' % tab[p])
            for p in t.post : f.write (' %d' % tab[p])
            f.write ('\n')

    # FIXME -- this method has never been tested !!
    def __write_ctxdot (self, f, items, n) :
        if len (items) != 0 : return self.__write_ctxdot_items (f, items, n)

        f.write ('digraph {\n')
        for t in self.trans :
            self.__write_ctxdot_items (f, set ([t]), n, repr (t), False)
        f.write ('}\n')

    # FIXME -- this method has never been tested !!
    # FIXME -- the line type (x.pre) will return Instance, not Event!
    def __write_ctxdot_items (self, f, items, n, prefx='', full=True) :
        self.m += 1
        m = self.m

        t = set (items)
        for x in t :
            x.m = m
            x.count = n
        while len (t) :
            s = t
            t = set ()
            for x in s :
                assert (x.m == m)
                if x.count <= 0 : continue

                for y in x.post | x.cont :
                    if y.m == m : continue
                    y.m = m
                    y.count = x.count - 1
                    t.add (y)
                if type (x.pre) == Event :
                    if x.pre.m != m :
                        x.pre.m = m
                        x.pre.count = x.count - 1
                        t.add (x.pre)
                elif type (x.pre) == set :
                    for y in x.pre :
                        if y.m == m : continue
                        y.m = m
                        y.count = x.count - 1
                        t.add (y)

        self.__write_dot (f, m, prefx, full)

    def __write_dot (self, f, m=0, prefx='', full=True) :
        if full : f.write ('digraph {\n')
        f.write ('\t/* transitions */\n')
        f.write ('\tnode\t[shape=box style=filled fillcolor=gray80];\n')
        for t in self.trans :
            if m != 0 and t.m != m : continue
            s = '\t%st%d [label="%s"' % (prefx, id (t), repr (t))
#            if e.isblack or e.isgray : s += ' shape=Msquare'
#            if (e.m) : s += ' fillcolor=blue'
            f.write (s + '];\n')

        f.write ('\n\t/* places, flow and context relations */\n')
        f.write ('\tnode\t[shape=circle fillcolor=gray95];')
        for p in self.places :
            if m != 0 and p.m != m: continue
            s = ' (%d)' % self.m0[p] if self.m0[p] > 0 else ''
            s = '\n\t%sp%d [label="%s%s"];\n' % (prefx, id (p), repr (p), s)
            for t in p.pre :
                if m == 0 or t.m == m :
                    s += '\t%st%d -> %sp%d;\n' % (prefx, id (t), prefx, id (p))

            for t in p.post :
                if m == 0 or t.m == m :
                    s += '\t%sp%d -> %st%d;\n' % (prefx, id (p), prefx, id (t))
            for t in p.cont :
                if m == 0 or t.m == m :
                    s += '\t%sp%d -> %st%d [arrowhead=none color=red];\n' \
                            % (prefx, id (p), prefx, id (t))
            f.write (s)

        if full :
            f.write ('\n\tgraph [label="%d transitions\\n%d places"];\n}\n' %
                    (len (self.trans), len (self.places)))

    def __write_pnml (self, f, m=0) :
        s = '<?xml version="1.0" encoding="UTF-8"?>\n'
        s += '<pnml xmlns="http://www.pnml.org/version-2009/grammar/pnml">\n'
        s += '<net id="n1" type="http://www.pnml.org/version-2009/grammar/ptnet">\n'
        s += '<name> <text>"%s" version "%s", by "%s"</text> </name>\n' % \
            (self.title, self.version, self.author)
        s += '<page id="page">\n'

        f.write (s + '\n<!-- places -->\n')
        tab = {}
        for p in self.places :
            if m != 0 and c.m != m : continue
            s = '<place id="p%d">\n' % len (tab)
            s += '<name><text>%s</text></name>\n' % repr (p)
            s += '<initialMarking> <text>%d</text> </initialMarking>\n' % self.m0[p]
            s += '</place>\n'
            f.write (s)
            tab[p] = len (tab)

        f.write ('\n<!-- transitions -->\n')
        for t in self.trans :
            if m != 0 and t.m != m : continue
            s = '<transition id="t%d">\n' % len (tab)
            s += '<name><text>%s</text></name>\n' % repr (t)
            s += '</transition>\n'
            f.write (s)
            tab[t] = len (tab)

        f.write ('\n<!-- flow relation -->\n')
        for p in self.places :
            if m != 0 and c.m != m : continue
            s = ''
            for t in p.pre :
                if m == 0 or t.m == m :
                    s += '<arc id="a%d" source="t%d" target="p%d" />\n' \
                            % (len (tab), tab[t], tab[p])
                    tab[t, p, 'a'] = len (tab)
            for t in p.post :
                if m == 0 or t.m == m :
                    s += '<arc id="a%d" source="p%d" target="t%d" />\n' \
                            % (len (tab), tab[p], tab[t])
                    tab[p, t, 'a'] = len (tab)
            for t in p.cont :
                if m == 0 or t.m == m :
                    s += '<!-- read arc: -->\n'
                    s += ' <arc id="ra%d" source="p%d" target="t%d" />\n' \
                            % (len (tab), tab[p], tab[t])
                    tab[p, t, 'ra'] = len (tab)
                    s += ' <arc id="ra%d" source="t%d" target="p%d" />\n' \
                            % (len (tab), tab[t], tab[p])
                    tab[t, p, 'ra'] = len (tab)
            f.write (s)

        f.write ('\n</page>\n</net>\n</pnml>\n')


    def __write_grml (self, f, m=0) :
        s = '<?xml version="1.0" encoding="UTF-8"?>\n'
        s += '<model formalismUrl="http://formalisms.cosyverif.org/pt-net.fml"\n'
        s += '  xmlns="http://cosyverif.org/ns/model">\n\n'

        s += '<attribute name="authors">%s</attribute>\n' % self.author
        s += '<attribute name="title">%s</attribute>\n' % self.title
        s += '<attribute name="date">%s</attribute>\n' % self.date
        s += '<attribute name="version">%s</attribute>\n' % self.version
        s += '<attribute name="note">%s</attribute>\n\n' % self.note

        tab = {}
        f.write (s + '<!-- transitions -->\n')
        for t in self.trans :
            if m != 0 and t.m != m : continue
            s = '<node id="%d" nodeType="transition">\n' % len (tab)
            s += '<attribute name="name">%s</attribute>\n' % repr (t)
            s += '</node>\n'
            f.write (s)
            tab[t] = len (tab)

        f.write ('\n<!-- places, flow and context relations -->')
        for p in self.places :
            if m != 0 and c.m != m : continue
            s = '\n<node id="%d" nodeType="place">\n' % len (tab)
            s += '<attribute name="name">%s</attribute>\n' % repr (p)
            s += '<attribute name="marking">%d</attribute>\n' % self.m0[p]
            s += '</node>\n'
            tab[p] = len (tab)

            for t in p.pre :
                if m == 0 or t.m == m :
                    s += '<arc id="%d" arcType="arc" source="%d" target="%d">\n' \
                            % (len (tab), tab[t], tab[p])
                    s += '<attribute name="valuation">1</attribute>\n'
                    s += '</arc>\n'
                    tab[t, p] = len (tab)
            for t in p.post :
                if m == 0 or t.m == m :
                    s += '<arc id="%d" arcType="arc" source="%d" target="%d">\n' \
                            % (len (tab), tab[p], tab[t])
                    s += '<attribute name="valuation">1</attribute>\n'
                    s += '</arc>\n'
                    tab[p, t] = len (tab)
            for t in p.cont :
                if m == 0 or t.m == m :
                    s += '<arc id="%d" arcType="readarc" source="%d" target="%d">\n' \
                            % (len (tab), tab[p], tab[t])
                    s += '<attribute name="valuation">1</attribute>\n'
                    s += '</arc>\n'
                    tab[p, t] = len (tab)
            f.write (s)
        f.write ('\n</model>\n')

    def read (self, f, fmt='pep') :
        if fmt == 'pep' : return self.__read_pep (f)
        if fmt == 'pt1' : return self.__read_pt1 (f)
        if fmt == 'grml' : return self.__read_grml (f)
        if fmt == 'pnml' : return self.__read_pnml (f)
        if fmt == 'stg' : return self.__read_stg (f)
        raise Exception, "'%s': unknown input format" % fmt

    def __read_pep (self, f) :
        raise Exception, 'reading PEP files is not yet implemented'

    def __read_pt1 (self, f) :
        l = f.readline ()
        l = l[:-1]
        if l != 'PT1' :
            raise Exception, "Expected 'PT1' as first line, found '%s'" % l
        try :
            pnr = int (f.readline ())
            tnr = int (f.readline ())
        except IOError :
            raise
        except :
            raise Exception, "Expected number of places and/or number of transitions"

        # read pnr place names and initial marking
        tab = {}
        i = 3 # line number
        for l in f :
            i += 1
            l = l.rstrip ()
            #print "line %d: '%s'" % (i, l)
            if not l or l[0] != '"' :
                raise Exception, "line %d: syntax error" % i

            # split the line in three parts, separator is "quote space"
            name, sep, end = l[1:].rpartition ('" ')
            if i - 4 < pnr :
                # if it is a place line
                try :
                    end = int (end)
                except :
                    raise Exception, "line %d: expected number, found '%s'" % (i, end)
                tab[i - 4] = self.place_add (name, end)
            else :
                # if it is a transition line
                try :
                    nums = [int (x) for x in end.split ()]
                except :
                    raise Exception, "line %d: expected number" % i
                if len (nums) < 2 or len (nums) != 2 + nums[0] + nums[1] :
                    raise Exception, "line %d: expected different number of indexes" % i
                t = self.trans_add (name)
                for idx in nums[2 : 2 + nums[0]] :
                    #print "pre idx %d, place %s" % (idx, tab[idx])
                    t.pre_add (tab[idx])
                for idx in nums[2 + nums[0] : ] :
                    #print "post idx %d, place %s" % (idx, tab[idx])
                    t.post_add (tab[idx])

    def __read_grml (self, f) :
        par = xml.parsers.expat.ParserCreate ()
        par.StartElementHandler = self.__grml_start
        par.EndElementHandler = self.__grml_end
        par.CharacterDataHandler = self.__grml_data

        self.__grmlq = []
        self.__grmlidx = {}
        par.ParseFile (f)
        del self.__grmlq
        del self.__grmlidx

    def __read_stg (self, f) :
        # TODO: signals of the form a+/4 ...
        nr = 0
        idx = {}
        for l in f :
            nr += 1
            l = l.strip ()
            if len (l) == 0 : continue
            if l[0] == '#' : continue
            if l[0:7] == '.inputs' :
                for s in l[7:].split () : idx[s] = None
            if l[0:8] == '.outputs' :
                for s in l[8:].split () : idx[s] = None
            if l[0:9] == '.internal' :
                for s in l[9:].split () : idx[s] = None
            if l[0:6] == '.dummy' :
                for s in l[6:].split () : idx[s] = self.trans_add (s)
            if l == '.graph' :
                for s in list (idx) :
                    idx[s + '+'] = self.trans_add (s + '+')
                    idx[s + '-'] = self.trans_add (s + '-')
            if l[0:8] == '.marking' :
                l = l[8:].strip (' {}')
                if '<' in l or '>' in l :
                    raise Exception, 'line %d: implicit places disallowed' % nr
                for s in l.split () :
                    if s not in idx :
                        raise Exception, 'line %d: place "%s" not found' % \
                                (nr, s)
                    if idx[s].__class__ != Place :
                        raise Exception, 'line %d: "%s" is not a place' % \
                                (nr, s)
                    idx[s].m0 = 1
                    self.m0.add (idx[s])
                continue
            if l[0] == '.' : continue

            ll = l.split ()
            if len (ll) < 2 :
                raise Exception, 'line %d: not a valid arc definition' % nr
            if ll[0] not in idx : idx[ll[0]] = self.place_add (ll[0])
            for n in ll[1:] :
                if n not in idx : idx[n] = self.place_add (n)
                if idx[ll[0]].__class__ == idx[n].__class__ :
                    raise Exception, 'line %d: not a p-t or t-p arc' % nr
                idx[ll[0]].post_add (idx[n])
                # print 'arc', idx[ll[0]], '->', idx[n]

    def __grml_start (self, tag, attr):
        # print "START", repr (tag), attr

        attr['__data'] = ''
        self.__grmlq.append (attr)
        if tag == 'model' :
            if not 'formalismUrl' in attr :
                raise Exception, "no 'formalismUrl' defined"
            if attr['formalismUrl'] != 'http://formalisms.cosyverif.org/pt-net.fml' :
                raise Exception, "unknown value for 'formalismUrl'"
        elif tag == 'arc' :
            attr['__type'] = attr['arcType']
        elif tag == 'node' :
            attr['__type'] = attr['nodeType']
        elif tag != 'attribute' :
                raise Exception, "'%s': unknown tag" % tag
        # print self.__grmlq

    def __grml_end (self, tag):
        # print "END", repr (tag)

        if tag == 'attribute' :
            if len (self.__grmlq) < 2 : raise Exception, 'Misplaced attribute'
            d = self.__grmlq.pop ()
            self.__grmlq[-1][d['name']] = d['__data']

        elif tag == 'node' :
            d = self.__grmlq.pop ()
            if 'id' not in d : raise Exception, 'id missing in node tag'
            if 'marking' not in d : d['marking'] = 0
            if 'name' not in d : d['name'] = ''
            if d['__type'] == 'place' :
                p = Place (d['name'], int (d['marking']))
                self.places.append (p)
                self.__grmlidx[d['id']] = p
                if p.m0 > 0 : self.m0.add (p)
            elif d['__type'] == 'transition' :
                t = Transition (d['name'])
                self.trans.append (t)
                self.__grmlidx[d['id']] = t
            else :
                raise Exception, "'%s': unknown nodeType" % d['__type']

        elif tag == 'arc' :
            d = self.__grmlq.pop ()
            if 'id' not in d or 'source' not in d or 'target' not in d :
                raise Exception, 'id, source or target missing in arc tag'
            if 'valuation' not in d : d['valuation'] = 1
            if d['valuation'] != '1' :
                raise Exception, 'Arc valuations different than 1 not supported'
            if d['source'] not in self.__grmlidx :
                raise Exception, 'node with id %d has not yet been seen' \
                        % d['source']
            if d['target'] not in self.__grmlidx :
                raise Exception, 'node with id %d has not yet been seen' \
                        % d['target']
            if d['__type'] == 'arc' :
                self.__grmlidx[d['source']].post_add (
                        self.__grmlidx[d['target']])
            elif d['__type'] == 'readarc' :
                self.__grmlidx[d['source']].cont_add (
                        self.__grmlidx[d['target']])
            else :
                raise Exception, "'%s': unknown arcType" % d['__type']

        elif tag == 'model' :
            d = self.__grmlq.pop ()
            if 'authors' in d : self.author = d['__data']
            if 'title' in d : self.title = d['__data']
            if 'date' in d : self.date = d['__data']
            if 'note' in d : self.note = d['__data']
            if 'version' in d : self.version = d['__data']
        else :
            raise Exception, 'Not a pt-net'

    def __grml_data (self, data):
        # print "DATA", repr (data)
        if len (self.__grmlq) : self.__grmlq[-1]['__data'] = data

    def __read_pnml (self, f) :
        # documentation:
        # http://www.pnml.org/papers/PNML-Tutorial.pdf
        par = xml.parsers.expat.ParserCreate ()
        par.StartElementHandler = self.__pnml_start
        par.EndElementHandler = self.__pnml_end
        par.CharacterDataHandler = self.__pnml_data

        self.__pnmlitm = {}
        self.__pnmlq = []
        self.__pnmldepth = 0
        self.__pnmlskipdepth = sys.maxint
        par.ParseFile (f)
        if len (self.__pnmlitm) == 0 :
            raise Exception, 'missplaced "%s" entity' % tag
        self.__pnmlq.append (self.__pnmlitm)

        idx = {}
        for d in self.__pnmlq :
            if 'id' not in d : d['id'] = 'xxx'
            if 'name' not in d : d['name'] = d['id']
            if d['type'] == 'place' :
                if 'm0' not in d : d['m0'] = 0
                idx[d['id']] = self.place_add (d['name'], int (d['m0']))
                idx[d['id']].ident = d['id']
            elif d['type'] == 'transition' :
                idx[d['id']] = self.trans_add (d['name'])
                idx[d['id']].ident = d['id']
            elif d['type'] == 'net' :
                self.title = d['name']
        for d in self.__pnmlq :
            if d['type'] != 'arc' : continue
            if d['source'] not in idx or d['target'] not in idx :
                raise Exception, 'Arc with id "%s" has unknown source or target' % d['id']
            weight = 1
            if 'weight' in d :
                weight = int (d['weight'])
                #print "arc id '%s' with weight %d" % (d["id"], weight)
            idx[d['source']].post_add (idx[d['target']], weight)

        del self.__pnmlitm
        del self.__pnmlq

    def __pnml_start (self, tag, attr):
        self.__pnmldepth += 1
        #print "START", repr (tag), attr, "depth", self.__pnmldepth, "skip depth", self.__pnmlskipdepth
        if self.__pnmldepth >= self.__pnmlskipdepth : return

        if tag == 'net' :
            if len (self.__pnmlitm) != 0 :
                raise Exception, 'Missplaced XML tag "net"'
            self.__pnmlitm = {}
            self.__pnmlitm['type'] = 'net'

        elif tag in ['place', 'transition', 'arc'] :
            if len (self.__pnmlitm) == 0 :
                raise Exception, 'Missplaced XML tag "%s"' % tag
            #print 'new! ', repr (self.__pnmlitm)
            for k in ['name', 'm0'] :
                if k in self.__pnmlitm :
                    self.__pnmlitm[k] = self.__pnmlitm[k].strip(' \n\t')
            self.__pnmlq.append (self.__pnmlitm)
            self.__pnmlitm = {}
            self.__pnmlitm['type'] = tag
            self.__pnmlitm['id'] = attr['id']
            for k in ['source', 'target'] :
                if k in attr :
                    self.__pnmlitm[k] = attr[k]

        elif tag == 'name' :
            self.__pnmlitm['data'] = 'name'
            self.__pnmlitm['name'] = ''
        elif tag == 'initialMarking' :
            self.__pnmlitm['data'] = 'm0'
            self.__pnmlitm['m0'] = ''
        elif tag == 'inscription' :
            self.__pnmlitm['data'] = 'weight'
            self.__pnmlitm['weight'] = ''

        # bug if inscription is an arc weight !!!!
        elif tag in ['toolspecific', 'graphics'] :
            self.__pnmlskipdepth = self.__pnmldepth
            return
        elif tag in ['page', 'pnml', 'text'] :
            return
        elif tag in ['delay', 'interval',  'cn']:
            self.__pnmlskipdepth = self.__pnmldepth
            print "WARNING: found '%s' tag, is this a timed net? Ignoring time information" % tag
            return
        # 'offset', 'position', 'dimension', 'fill', 'line', 'size', 'structure', 'unit', 'subunits', 'places'
        else :
            # this else clause is just to be on the safe side
            raise Exception, 'Unexpected XML tag "%s", probably I cannot handle this model. Is this a P/T model?' % tag

    def __pnml_end (self, tag):
        #print "END  ", repr (tag)
        self.__pnmldepth -= 1
        if self.__pnmldepth < self.__pnmlskipdepth :
            self.__pnmlskipdepth = sys.maxint

    def __pnml_data (self, data):
        #data = data.strip(' \n\t') <- dangerous here, data can be split!!
        if len (data) == 0 : return

        #print "DATA ", repr (data)
        if 'data' not in self.__pnmlitm : return
        k = self.__pnmlitm['data']
        self.__pnmlitm[k] += data

def test1 () :
    n = Net (True)
    n.read (sys.stdin, 'grml')
    n.write (sys.stdout, 'grml')

def test2 () :
    n = Net (True)
    n.read (sys.stdin, 'pt1')
    n.stubbornify ()
    #n.write (sys.stdout, 'pt1')

def test3 () :

    # two transitions in conflict
    n = Net (True)
    p0 = n.place_add ('p0', 1)
    p1 = n.place_add ('p1')
    p2 = n.place_add ('p2')

    t1 = n.trans_add ('t1')
    t2 = n.trans_add ('t2')

    t1.pre_add (p0)
    t1.post_add (p1)

    t2.pre_add (p0)
    t2.post_add (p2)

    print "Before stubbornifying !!"
    n.write (sys.stdout, 'pt1')
    n.write (sys.stdout, 'dot')
    n.stubbornify ()
    print "After stubbornifying !!"
    n.write (sys.stdout, 'dot')
    n.cont2plain ()
    n.write (sys.stdout, 'pt1')

    f = open ('./out.ll_net', 'w')
    n.write (f, 'pep')

def test4 () :

    # three transitions in conflict
    n = Net (True)
    p0 = n.place_add ('p0', 1)
    p1 = n.place_add ('p1')
    p2 = n.place_add ('p2')
    p3 = n.place_add ('p3')

    t1 = n.trans_add ('t1')
    t2 = n.trans_add ('t2')
    t3 = n.trans_add ('t3')

    t1.pre_add (p0)
    t1.post_add (p1)

    t2.pre_add (p0)
    t2.post_add (p2)

    t3.pre_add (p0)
    t3.post_add (p3)

    print "Before stubbornifying !!"
    n.write (sys.stdout, 'dot')
    n.stubbornify ()
    print "After stubbornifying !!"
    n.write (sys.stdout, 'dot')

    f = open ('./out.ll_net', 'w')
    n.write (f, 'pep')

def test5 () :

    # two transitions in conflict, and return
    n = Net (True)
    p0 = n.place_add ('p0', 1)
    p1 = n.place_add ('p1')

    t1 = n.trans_add ('t1')
    t2 = n.trans_add ('t2')
    t3 = n.trans_add ('t3')

    t1.pre_add (p0)
    t1.post_add (p1)

    t2.pre_add (p0)
    t2.post_add (p1)

    t3.pre_add (p1)
    t3.post_add (p0)

    n.write (sys.stdout, 'dot')
    n.stubbornify ()
    n.write (sys.stdout, 'dot')

    f = open ('./out.ll_net', 'w')
    n.write (f, 'pep')

def test6 () :

    n = Net (True)
    n.read (sys.stdin, 'pt1')
    n.stubbornify ()
    n.cont2plain ()
    n.write (sys.stdout, 'pep')

if __name__ == '__main__' :
    test3 ()

# vi:ts=4:sw=4:et:
