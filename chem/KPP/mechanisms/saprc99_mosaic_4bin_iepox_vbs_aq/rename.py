from os import rename, listdir

fnames=listdir('.')
for fname in fnames:
    new_name=fname.replace('_8bin', '_4bin')
    rename(fname, new_name)
