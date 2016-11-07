
scriptfn <- sys.frame(1)$ofile

message('Filename: ', scriptfn)

parent_frames <- sys.parents()
message('parent frames: ', paste(parent_frames, sep = ', '))

parent_names = sapply(parent_frames, function(x) sys.frame(x)$ofile)

print(parent_names)