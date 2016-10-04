unit zyre_node;

interface

uses zyre_library;

type
  zyre_node_t = record end;            TZyreNode = zyre_node_t;            PZyreNode = ^TZyreNode;


//  This is the actor that runs a single node; it uses one thread, creates
//  a zyre_node object at start and destroys that when finishing.
procedure zyre_node_actor(pipe:TZyreSocket; args:Pointer);cdecl;external ZYRE_LIBRARY_NAME;

implementation

end.

