unit zyre;

interface

uses ctypes, zyre_library;

  //  Constructor, creates a new Zyre node. Note that until you start the
  //  node it is silent and invisible to other nodes on the network.
  //  The node name is provided to other nodes during discovery. If you
  //  specify NULL, Zyre generates a randomized node name from the UUID.
  function zyre_new(name:PChar):PZyre;external ZYRE_LIBRARY_NAME;

  //  Destructor, destroys a Zyre node. When you destroy a node, any
  //  messages it is sending or receiving will be discarded.
  procedure zyre_destroy(self_p:PZyre);external ZYRE_LIBRARY_NAME;

  //  Return our node UUID string, after successful initialization
  function zyre_uuid(self:TZyre):PChar;external ZYRE_LIBRARY_NAME;

  //  Return our node name, after successful initialization
  function zyre_name(self:TZyre):PChar;external ZYRE_LIBRARY_NAME;

  //  Set node header;these are provided to other nodes during discovery
  //  and come in each ENTER message.
  procedure zyre_set_header(self:TZyre;name:PChar;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Set verbose mode;this tells the node to log all traffic as well as
  //  all major events.
  procedure zyre_set_verbose(self:TZyre);external ZYRE_LIBRARY_NAME;

  //  Set UDP beacon discovery port;defaults to 5670, this call overrides
  //  that so you can create independent clusters on the same network, for
  //  e.g. development vs. production. Has no effect after zyre_start().
  procedure zyre_set_port(self:TZyre;port_nbr:cint);cdecl;external ZYRE_LIBRARY_NAME;

  //  Set UDP beacon discovery interval, in milliseconds. Default is instant
  //  beacon exploration followed by pinging every 1,000 msecs.
  procedure zyre_set_interval(self:TZyre;interval:size_t);cdecl;external ZYRE_LIBRARY_NAME;

  //  Set network interface for UDP beacons. If you do not set this, CZMQ will
  //  choose an interface for you. On boxes with several interfaces you should
  //  specify which one you want to use, or strange things can happen.
  procedure zyre_set_interface(self:TZyre;value:PChar);cdecl;external ZYRE_LIBRARY_NAME;

  //  By default, Zyre binds to an ephemeral TCP port and broadcasts the local
  //  host name using UDP beaconing. When you call this method, Zyre will use
  //  gossip discovery instead of UDP beaconing. You MUST set-up the gossip
  //  service separately using zyre_gossip_bind() and _connect(). Note that the
  //  endpoint MUST be valid for both bind and connect operations. You can use
  //  inproc://, ipc://, or tcp:// transports (for tcp://, use an IP address
  //  that is meaningful to remote as well as local nodes). Returns 0 if
  //  the bind was successful, else -1.
  function zyre_set_endpoint(self:TZyre;format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Set-up gossip discovery of other nodes. At least one node in the cluster
  //  must bind to a well-known gossip endpoint, so other nodes can connect to
  //  it. Note that gossip endpoints are completely distinct from Zyre node
  //  endpoints, and should not overlap (they can use the same transport).
  procedure zyre_gossip_bind(self:TZyre;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Set-up gossip discovery of other nodes. A node may connect to multiple
  //  other nodes, for redundancy paths. For details of the gossip network
  //  design, see the CZMQ zgossip class.
  procedure zyre_gossip_connect(self:TZyre;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Start node, after setting header values. When you start a node it
  //  begins discovery and connection. Returns 0 if OK, -1 if it wasn't
  //  possible to start the node.
  function zyre_start(self:TZyre):cint;external ZYRE_LIBRARY_NAME;

  //  Stop node;this signals to other peers that this node will go away.
  //  This is polite;however you can also just destroy the node without
  //  stopping it.
  procedure zyre_stop(self:TZyre);external ZYRE_LIBRARY_NAME;

  //  Join a named group;after joining a group you can send messages to
  //  the group and all Zyre nodes in that group will receive them.
  function zyre_join (self:TZyre;group:PChar):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Leave a group
  function zyre_leave (self:TZyre;group:PChar):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Receive next message from network;the message may be a control
  //  message (ENTER, EXIT, JOIN, LEAVE) or data (WHISPER, SHOUT).
  //  Returns zmsg_t object, or NULL if interrupted
  function zyre_recv(self:TZyre): TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Send message to single peer, specified as a UUID string
  //  Destroys message after sending
  function zyre_whisper(self:TZyre; peer : PChar; PZyreMessage : zmsg_t):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send message to a named group
  //  Destroys message after sending
  function zyre_shout(self:TZyre; group:PChar;msg_p:PZyreMessage):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send formatted string to a single peer specified as UUID string
  function zyre_whispers(self:TZyre; peer:PChar;format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send formatted string to a named group
  function zyre_shouts(self:TZyre; group:PChar;format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Return zlist of current peer ids.
  //  Caller owns return value and must destroy it when done.
  function zyre_peers(self:TZyre):TZyreList;external ZYRE_LIBRARY_NAME;

  //  Return zlist of currently joined groups.
  //  Caller owns return value and must destroy it when done.
  function zyre_own_groups(self:TZyre):TZyreList;external ZYRE_LIBRARY_NAME;

  //  Return zlist of groups known through connected peers.
  //  Caller owns return value and must destroy it when done.
  function zyre_peer_groups(self:TZyre):TZyreList;external ZYRE_LIBRARY_NAME;

  //  Return the endpoint of a connected peer.
  //  Caller owns return value and must destroy it when done.
  function zyre_peer_address(self:TZyre;peer:PChar):PChar;cdecl;external ZYRE_LIBRARY_NAME;

  //  Return the value of a header of a conected peer.
  //  Returns null if peer or key doesn't exits.
  //  Caller owns return value and must destroy it when done.
  function zyre_peer_header_value(self:TZyre;peer:PChar;name:PChar):PChar;cdecl;external ZYRE_LIBRARY_NAME;

  //  Return socket for talking to the Zyre node, for polling
  function zyre_socket(self:TZyre):TZyreSocket;external ZYRE_LIBRARY_NAME;

  //  Print zyre node information to stdout
  procedure zyre_print(self:TZyre);external ZYRE_LIBRARY_NAME;

  //  Return the Zyre version for run-time API detection
  procedure zyre_version(var major, minor, patch:cint);cdecl;external ZYRE_LIBRARY_NAME;

  //  Self test of this class.
  procedure zyre_test(verbose:Boolean); external ZYRE_LIBRARY_NAME;

{$IFDEF ADD_ZYRE_IGNORED_FUNCTIONS}
  procedure zyre_set_header(self:TZyre;name,format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;
  function zyre_set_endpoint (self:TZyre;format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;
  procedure zyre_gossip_bind(self:TZyre;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;
  procedure zyre_gossip_connect(self:TZyre;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;
  function zyre_whispers(self:TZyre;peer,format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;
  function zyre_shouts(self:TZyre;group,format:PChar;KArgs:array of const):cint;cdecl;external ZYRE_LIBRARY_NAME;
{$ENDIF}

  // alias
  procedure zyre_dump(Z:TZyre);external ZYRE_LIBRARY_NAME name 'zyre_print';

implementation


end.

