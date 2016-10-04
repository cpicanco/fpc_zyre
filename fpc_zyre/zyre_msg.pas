unit zyre_msg;

interface

uses ctypes, zyre_library;

{  These are the zre_msg messages:

    HELLO - Greet a peer so it can connect back to us
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
        endpoint            string      Sender connect endpoint
        groups              strings     List of groups sender is in
        status              number 1    Sender groups status value
        name                string      Sender public name
        headers             hash        Sender header properties

    WHISPER - Send a multi-part message to a peer
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
        content             msg         Wrapped message content

    SHOUT - Send a multi-part message to a group
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
        group               string      Group to send to
        content             msg         Wrapped message content

    JOIN - Join a group
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
        group               string      Name of group
        status              number 1    Sender groups status value

    LEAVE - Leave a group
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
        group               string      Name of group
        status              number 1    Sender groups status value

    PING - Ping a peer that has gone silent
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number

    PING_OK - Reply to a peer's ping
        version             number 1    Version number (2)
        sequence            number 2    Cyclic sequence number
}

const
  ZRE_MSG_HELLO   = 1;
  ZRE_MSG_WHISPER = 2;
  ZRE_MSG_SHOUT   = 3;
  ZRE_MSG_JOIN    = 4;
  ZRE_MSG_LEAVE   = 5;
  ZRE_MSG_PING    = 6;
  ZRE_MSG_PING_OK = 7;

//type
//  zre_msg_t = record end;         TZyreMessage = zre_msg_t;       PZyreMessage = ^TZyreMessage;
//  zmsg_t = record end;            TZMessage = zmsg_t;             PZMessage = ^TZMessage;

  //  @interface
  //  Create a new zre_msg
    function zre_msg_new(id:cint):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Destroy the zre_msg
    procedure zre_msg_destroy(msg_p:PZyreMessage);external ZYRE_LIBRARY_NAME;

  //  Parse a TZMessage and decides whether it is zre_msg. Returns
  //  true if it is, false otherwise. Doesn't destroy or modify the
  //  original message.
    function is_zre_msg(msg_p:TZMessage):Boolean;external ZYRE_LIBRARY_NAME;

  //  Parse a zre_msg from TZMessage. Returns a new object, or NULL if
  //  the message could not be parsed, or was NULL. Destroys msg and
  //  nullifies the msg reference.
    function zre_msg_decode(msg_p:PZMessage):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Encode zre_msg into zmsg and destroy it. Returns a newly created
  //  object or NULL if error. Use when not in control of sending the message.
    function zre_msg_encode(msg_p:PZyreMessage):TZMessage;external ZYRE_LIBRARY_NAME;

  //  Receive and parse a zre_msg from the socket. Returns new object,
  //  or NULL if error. Will block if there's no message waiting.
    function zre_msg_recv(input:Pointer):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Receive and parse a zre_msg from the socket. Returns new object,
  //  or NULL either if there was no input waiting, or the recv was interrupted.
    function zre_msg_recv_nowait(input:Pointer):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Send the zre_msg to the output, and destroy it
    function zre_msg_send (msg_p:PZyreMessage; output:Pointer):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the zre_msg to the output, and do not destroy it
    function zre_msg_send_again(self:TZyreMessage; output:Pointer):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the HELLO
    function zre_msg_encode_hello(
          sequence: cuint16;
          endpoint:PChar;
          groups:TZList;
          status:byte;
          name:PChar;
          headers:TZHash):TZMessage;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the WHISPER
    function zre_msg_encode_whisper(
          sequence:cuint16;
          content:TZMessage):TZMessage;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the SHOUT
    function zre_msg_encode_shout(
          sequence:cuint16;
          group:PChar;
          content:TZMessage):TZMessage;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the JOIN
    function zre_msg_encode_join(
          sequence:cuint16;
          group:PChar;
          status:byte):TZMessage;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the LEAVE
    function zre_msg_encode_leave(
          sequence:cuint16;
          group:PChar;
          status:byte):TZMessage;cdecl;external ZYRE_LIBRARY_NAME;

  //  Encode the PING
    function zre_msg_encode_ping(
          sequence:cuint16):TZMessage;external ZYRE_LIBRARY_NAME;

  //  Encode the PING_OK
    function zre_msg_encode_ping_ok(
          sequence:cuint16):TZMessage;external ZYRE_LIBRARY_NAME;


  //  Send the HELLO to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_hello(output:Pointer;
          sequence:cuint16;
          endpoint:PChar;
          groups:TZList;
          status:byte;
          name:PChar;
          headers:TZHash):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the WHISPER to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_whisper(output:Pointer;
          sequence:cuint16;
          content:TZMessage):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the SHOUT to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_shout(output:Pointer;
          sequence:cuint16;
          group:PChar;
          content:TZMessage):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the JOIN to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_join(output:Pointer;
          sequence:cuint16;
          group:PChar;
          status:byte):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the LEAVE to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_leave(output:Pointer;
          sequence:cuint16;
          group:PChar;
          status:byte):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the PING to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_ping(output:Pointer; sequence:cuint16):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Send the PING_OK to the output in one step
  //  WARNING, this call will fail if output is of type ZMQ_ROUTER.
    function zre_msg_send_ping_ok(output:Pointer; sequence:cuint16):cint;cdecl;external ZYRE_LIBRARY_NAME;

  //  Duplicate the zre_msg message
    function zre_msg_dup(self:TZyreMessage):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Print contents of message to stdout
    procedure zre_msg_print(self:TZyreMessage);external ZYRE_LIBRARY_NAME;

  //  Get/set the message routing id
    function zre_msg_routing_id(self:TZyreMessage):TZFrame;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_routing_id(self:TZyreMessage;routing_id:TZFrame);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get the zre_msg id and printable command
    function zre_msg_id(self:TZyreMessage):cint;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_id(self:TZyreMessage; id:cint);external ZYRE_LIBRARY_NAME;
    function zre_msg_command(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;

  //  Get/set the sequence field
    function zre_msg_sequence(self:TZyreMessage):cuint16;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_sequence(self:TZyreMessage;sequence:cuint16);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set the endpoint field
    function zre_msg_endpoint(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_endpoint(
          self:TZyreMessage;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set the groups field
    function zre_msg_groups(self:TZyreMessage):TZList;external ZYRE_LIBRARY_NAME;
  //  Get the groups field and transfer ownership to caller
    function zre_msg_get_groups(self:TZyreMessage):TZList;external ZYRE_LIBRARY_NAME;
  //  Set the groups field, transferring ownership from caller
    procedure zre_msg_set_groups(self:TZyreMessage;groups_p:PZList);cdecl;external ZYRE_LIBRARY_NAME;

  //  Iterate through the groups field, and append a groups value
    function zre_msg_groups_first(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;
    function zre_msg_groups_next(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_groups_append(
          self:TZyreMessage;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

    function zre_msg_groups_size(self:TZyreMessage):csize_t;external ZYRE_LIBRARY_NAME;

  //  Get/set the status field
    function zre_msg_status(self:TZyreMessage):byte;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_status(self:TZyreMessage;status:byte);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set the name field
    function zre_msg_name(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_name(
          self:TZyreMessage;format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set the headers field
    function zre_msg_headers(self:TZyreMessage):TZHash;external ZYRE_LIBRARY_NAME;
  //  Get the headers field and transfer ownership to caller
    function zre_msg_get_headers(self:TZyreMessage):TZHash;external ZYRE_LIBRARY_NAME;
  //  Set the headers field, transferring ownership from caller
    procedure zre_msg_set_headers(self:TZyreMessage; headers_p:PZHash);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set a value in the headers dictionary
    function zre_msg_headers_string(
          self:TZyreMessage;
          key, default_value:PChar):PChar;cdecl;external ZYRE_LIBRARY_NAME;

    function zre_msg_headers_number(
          self:TZyreMessage;key:PChar;
          default_value:cuint64):cuint64;cdecl;external ZYRE_LIBRARY_NAME;

    procedure zre_msg_headers_insert(self:TZyreMessage;
          key, format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

    function zre_msg_headers_size(self:TZyreMessage):csize_t;external ZYRE_LIBRARY_NAME;

  //  Get a copy of the content field
    function zre_msg_content(self:TZyreMessage):TZMessage;external ZYRE_LIBRARY_NAME;
  //  Get the content field and transfer ownership to caller
    function zre_msg_get_content(self:TZyreMessage):TZMessage;external ZYRE_LIBRARY_NAME;
  //  Set the content field, transferring ownership from caller
    procedure zre_msg_set_content(self:TZyreMessage;msg_p:PZMessage);cdecl;external ZYRE_LIBRARY_NAME;

  //  Get/set the group field
    function zre_msg_group(self:TZyreMessage):PChar;external ZYRE_LIBRARY_NAME;
    procedure zre_msg_set_group(
          self:TZyreMessage; format:PChar;KArgs:array of const);cdecl;external ZYRE_LIBRARY_NAME;

  //  Self test of this class
    procedure zre_msg_test(verbose:Boolean);external ZYRE_LIBRARY_NAME;
  //  @end

  //  For backwards compatibility with old codecs
   procedure zre_msg_dump(M:TZyreMessage);external ZYRE_LIBRARY_NAME name 'zre_msg_print';

implementation

end.

