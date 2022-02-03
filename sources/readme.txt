Use make.sub and link.sub to build Z80AS from the sources.

If you decide to use ZAS to build Z80AS, you must modify the make.sub file, changing Z80AS with ZAS.

Also, the order of the object files in the link.sub is critical: start.obj must be the first one, and cpmio.obj the last one.

The Z80AS.COM executable file contents will differ, depending on your choice:

- if assembled with Z80AS, the uninitialized bytes (DEFS) will be set to 00H
- if assembled with ZAS, the uninitialized bytes (DEFS) contents cannot be guaranteed (usually, they will be set to 1AH)

In both cases, Z80AS will work fine.

