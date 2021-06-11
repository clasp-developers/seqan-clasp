import os
from wscript_utils import *

def analyze_clasp(cfg):
    pass
    
def options(cfg):
    pass

def configure(cfg):
#    cfg.check_cxx(stlib='seqan3', cflags='-Wall', uselib_store='SEQAN')
#    cfg.extensions_stdlib += cfg.env.STLIB_SEQAN
    pass

def update_dependencies(cfg):
    print( "In update_dependencies for seqan")
    fetch_git_revision("extensions/seqan-clasp/seqan",
                       "https://github.com/seqan/seqan.git",
                       label = "master")
    
def build(bld):
    bld.extensions_include_dirs.append("extensions/seqan-clasp/seqan/include")
    bld.recurse("src")
    
def build3(bld):
    pass

def build4(bld):
    pass

