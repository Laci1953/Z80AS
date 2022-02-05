This folder contains source files that can be assembled either with ZAS or Z80AS.

Use make.sub and link.sub to build Z80AS from the sources.

If ZAS cannot assemble some of the source files, or you decide to use Z80AS to build Z80AS, you must modify the make.sub file, changing ZAS with Z80AS.

Also, the order of the object files in the link.sub is critical: start.obj must be the first one, and cpmio.obj the last one.

The Z80AS.COM executable file contents will differ, depending on your choice:

- if assembled with Z80AS, the uninitialized bytes (DEFS) will be set to 00H
- if assembled with ZAS, the uninitialized bytes (DEFS) contents cannot be guaranteed (usually, they will be set to 1AH)

In both cases, Z80AS will work fine.

