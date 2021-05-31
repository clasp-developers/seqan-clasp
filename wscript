

from wscript_utils import *

def analyze_clasp(cfg):
    pass
    
def options(cfg):
    pass

def configure(cfg):
#    cfg.check_cxx(stlib='seqan3', cflags='-Wall', uselib_store='SEQAN')
#    cfg.extensions_stdlib += cfg.env.STLIB_SEQAN
    pass

def build(bld):
    bld.recurse("src")
    
def build3(bld):
    pass

def build4(bld):
    pass

