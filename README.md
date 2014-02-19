`Polygonization.hs` has code to convert arbitrary implicitly defined
surfaces to triangle meshes.  It's slow---O(N^3) with bad constant
factors because it doesn't reuse calculations.  Visually satisfying
resolution tends to overflow the stack.  But I think it's not bad for
<100 lines of code.

There are some examples that add boolean CSG operations, and output to STL.

# TODO

* Use Repa to store the function values at vertices (~8xN^3)
* Use Repa to stencil the edge calculations, and reuse those too (~5x (# tris), O(N^2))
* Look into adaptive or continuation methods, which can be O(N^2)
