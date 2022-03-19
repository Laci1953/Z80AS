This folder contains source files that must be compiled ONLY with Z80AS

Use makezas.sub and linkzas.sub to build Z80AS from the sources.

The order of the object files in the link.sub is critical: start.obj must be the first one, and cpmio.obj the last one.

