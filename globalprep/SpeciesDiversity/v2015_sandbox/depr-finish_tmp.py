# move local databases to remote
import os, shutil

local = r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a'
remote = r'N:\model\GL-NCEAS-SpeciesDiversity_v2013a\tmp'

for path in ('spp.db', 'geodb.gdb'):
    l = os.path.join(local, path)
    r = os.path.join(remote, path)
    if os.path.exists(l):
        if os.path.exists(r):
            if os.path.isdir(r):
                shutil.rmtree(r)
            else:
                os.unlink(r)
        shutil.move(l, r)
    #shutil.rmtree(local)