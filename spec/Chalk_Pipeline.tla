The Chalk Pipeline consists of 3 moving pieces:

1) A graph of nodes to be drawn, used for 4 things:
   * Preserve depth ordering
   * Preserve coordinates (x,y)
   * Allow for granular updates
   * Determine whether changes have been made to the graph

2) The frame composition process, where all of the nodes in 1) are
   pieced together into a single frame to be dispatched.

3) The frame rate dispatcher, that triggers the process as often as
   the frame rate cap allows it.

These 3 pieces working in harmony allow for:

* Incremental rendering
* Canvas composition
* Configurable frame rate

--------------------------- MODULE Chalk_Pipeline ---------------------------


=============================================================================
\* Modification History
\* Last modified Fri Mar 20 03:20:57 CET 2020 by ostera
\* Created Fri Mar 20 02:59:17 CET 2020 by ostera
