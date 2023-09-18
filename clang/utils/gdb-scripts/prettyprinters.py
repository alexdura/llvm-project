from __future__ import print_function
import struct
import sys
import time

import gdb.printing
import gdb.types


def call_method(base, method):
    return gdb.parse_and_eval("(* (" + str(base.type) + "*)(" + str(base.address) + "))." + method + "()")


clade_to_type = {
    "NKI_Stmt" : "Stmt",
    "NKI_Decl" : "Decl"
    }


class DynTypedNode:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        nk = self.val["NodeKind"]
        v = call_method(nk, "getCladeKind")
        clade = str(v["KindId"]).split("::")[-1]

        if clade in clade_to_type:
            raw = call_method(self.val, "getMemoizationData")
            t = clade_to_type[clade]
            raw_c = raw.cast(gdb.lookup_type("clang::" + t).pointer())
            call_method(raw_c.dereference(), "dump")
            return

        return str(self.val)

    def display_hint(self):
        return 'string'


# pp = gdb.printing.RegexpCollectionPrettyPrinter("DynTypedNode" + str(int(time.time())))
pp = gdb.printing.RegexpCollectionPrettyPrinter("DynTypedNode")
pp.add_printer('clang::DynTypedNode', '^clang::DynTypedNode$', DynTypedNode)
gdb.printing.register_pretty_printer(gdb.current_objfile(), pp)
