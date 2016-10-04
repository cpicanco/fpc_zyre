unit zyre_peer;

interface

uses ctypes, zyre_library;

type
  zyre_peer_t = record end;            TZyrePeer = zyre_peer_t;            PZyrePeer = ^TZyrePeer;

  //  Constructor
  function zyre_peer_new(container:TZHash;uuid:TCZmqUUID):TZyrePeer;cdecl;external ZYRE_LIBRARY_NAME;

  //  Destructor
  procedure zyre_peer_destroy(self_p:PZyrePeer);external ZYRE_LIBRARY_NAME;

  //  Connect peer mailbox
  procedure zyre_peer_connect(self_p:PZyrePeer;from:TCZmqUUID; endpoint:PChar);cdecl;external ZYRE_LIBRARY_NAME;

  //  Connect peer mailbox
  procedure zyre_peer_disconnect(self_p:PZyrePeer);external ZYRE_LIBRARY_NAME;

  //  Return peer connected status
  function zyre_peer_connected(self_p:PZyrePeer):Boolean;external ZYRE_LIBRARY_NAME;

  //  Return peer connection endpoint
  function zyre_peer_endpoint(self_p:PZyrePeer):PChar;external ZYRE_LIBRARY_NAME;

  //  Send message to peer
  function zyre_peer_send(self_p:PZyrePeer;msg_p:PZyreMessage):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Return peer identity string
  function zyre_peer_identity(self_p:PZyrePeer):PChar;external ZYRE_LIBRARY_NAME;

  //  Register activity at peer
  procedure zyre_peer_refresh(self_p:PZyrePeer);external ZYRE_LIBRARY_NAME;

  //  Return peer future evasive time
  function zyre_peer_evasive_at(self_p:PZyrePeer):cint64;external ZYRE_LIBRARY_NAME;

  //  Return peer future expired time
  function zyre_peer_expired_at(self_p:PZyrePeer):cint64;external ZYRE_LIBRARY_NAME;

  //  Return peer name
  function zyre_peer_name(self_p:PZyrePeer):PChar;external ZYRE_LIBRARY_NAME;

  //  Set peer name
  procedure zyre_peer_set_name(self_p:PZyrePeer;name:PChar);cdecl;external ZYRE_LIBRARY_NAME;

  //  Set current node name, for logging
  procedure zyre_peer_set_origin(self_p:PZyrePeer;origin:PChar);cdecl;external ZYRE_LIBRARY_NAME;

  //  Return peer status
  function zyre_peer_status(self_p:PZyrePeer):byte;external ZYRE_LIBRARY_NAME;

  //  Set peer status
  procedure zyre_peer_set_status(self_p:PZyrePeer;status:byte);cdecl;external ZYRE_LIBRARY_NAME;

  //  Return peer ready state
  function zyre_peer_ready(self_p:PZyrePeer):byte;external ZYRE_LIBRARY_NAME;

  //  Set peer ready
  procedure zyre_peer_set_ready(self_p:PZyrePeer; ready:Boolean);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get peer header value
  function zyre_peer_header(self_p:PZyrePeer; key, default_value:cchar):PChar;cdecl;external ZYRE_LIBRARY_NAME;

  //  Get peer headers table
  function zyre_peer_headers(self_p:PZyrePeer):TZHash;external ZYRE_LIBRARY_NAME;

  //  Set peer headers from provided dictionary
  procedure zyre_peer_set_headers(self_p:PZyrePeer;headers:TZHash);cdecl;external ZYRE_LIBRARY_NAME;

  //  Check if messages were lost from peer, returns true if they were.
  function zyre_peer_messages_lost(self_p:PZyrePeer;msg:TZyreMessage):Boolean;cdecl;external ZYRE_LIBRARY_NAME;

  //  Ask peer to log all traffic via zsys
  procedure zyre_peer_set_verbose(self_p:PZyrePeer;verbose:Boolean);cdecl;external ZYRE_LIBRARY_NAME;

implementation

end.

