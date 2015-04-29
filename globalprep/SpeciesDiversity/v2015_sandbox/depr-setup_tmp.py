# move remote databases to local
import os, shutil

local = r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a'
remote = r'N:\model\GL-NCEAS-SpeciesDiversity_v2013a\tmp'

if not os.path.exists(local):
    os.makedirs(local)

for path in ('spp.db', 'geodb.gdb'):
    l = os.path.join(local, path)
    r = os.path.join(remote, path)
    if os.path.exists(r):
        if os.path.exists(l):
            if os.path.isdir(l):
                shutil.rmtree(l)
            else:
                os.unlink(l)
        shutil.move(r, l)