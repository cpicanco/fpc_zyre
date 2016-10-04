unit zyre_group;

interface

uses zyre_peer, zyre_library;

type
  zyre_group_t = record end;            TZyreGroup = zyre_group_t;            PZyreGroup = ^TZyreGroup;

  //  Constructor
  function zyre_group_new(name:PChar;container:TZHash):TZyreGroup;cdecl;external ZYRE_LIBRARY_NAME;

  //  Destructor
  procedure zyre_group_destroy(self_p:PZyreGroup);external ZYRE_LIBRARY_NAME;

  //  Add peer to group
  procedure zyre_group_join(self:TZyreGroup;peer:TZyrePeer);cdecl;external ZYRE_LIBRARY_NAME;

  //  Remove peer from group
  procedure zyre_group_leave(self:TZyreGroup;peer:TZyrePeer);cdecl;external ZYRE_LIBRARY_NAME;

  //  Send message to all peers in group
  procedure zyre_group_send(self:TZyreGroup;msg_p:PZyreMessage);cdecl;external ZYRE_LIBRARY_NAME;

implementation

end.
