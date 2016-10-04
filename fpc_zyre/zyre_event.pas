
unit zyre_event;

interface

uses zyre_library;

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

type
  zyre_event_type_t = (
    ZYRE_NONE,
    ZYRE_EVENT_ENTER,
    ZYRE_EVENT_JOIN,
    ZYRE_EVENT_LEAVE,
    ZYRE_EVENT_EXIT,
    ZYRE_EVENT_WHISPER,
    ZYRE_EVENT_SHOUT,
    ZYRE_EVENT_STOP,
    ZYRE_EVENT_EVASIVE
  );

  TZyreEventEnum = zyre_event_type_t;

  PZyreEventEnum  = ^TZyreEventEnum;

  //  Constructor: receive an event from the zyre node, wraps zyre_recv.
  //  The event may be a control message (ENTER, EXIT, JOIN, LEAVE) or
  //  data (WHISPER, SHOUT).
  function zyre_event_new(node:TZyre):TZyreEvent;external ZYRE_LIBRARY_NAME;

  //  Destructor; destroys an event instance
  procedure zyre_event_destroy(self_p:PZyreEvent);external ZYRE_LIBRARY_NAME;

  //  Returns event type, which is a zyre_event_type_t
  function zyre_event_type(self:PZyreEvent):TZyreEventEnum;external ZYRE_LIBRARY_NAME;

  //  Return the sending peer's uuid as a string
  function zyre_event_peer_uuid(self:TZyreEvent):PChar;external ZYRE_LIBRARY_NAME;

  //  Return the sending peer's public name as a string
  function zyre_event_peer_name(self:TZyreEvent):PChar;external ZYRE_LIBRARY_NAME;

  //  Return the sending peer's ipaddress as a string
  function zyre_event_peer_addr(self:TZyreEvent):PChar;external ZYRE_LIBRARY_NAME;

  //  Returns the event headers, or NULL if there are none
  function zyre_event_headers(self:TZyreEvent):TZyreHash;external ZYRE_LIBRARY_NAME;

  //  Returns value of a header from the message headers
  //  obtained by ENTER. Return NULL if no value was found.
  function zyre_event_header(self:TZyreEvent;name:PChar):PChar;cdecl;external ZYRE_LIBRARY_NAME;

  //  Returns the group name that a SHOUT event was sent to
  function zyre_event_group(self:TZyreEvent):PChar;external ZYRE_LIBRARY_NAME;

  //  Returns the incoming message payload
  function zyre_event_msg(self:TZyreEvent):TZyreMessage;external ZYRE_LIBRARY_NAME;

  //  Print event to zsys log
  procedure zyre_event_print(self:PZyreEvent);external ZYRE_LIBRARY_NAME;

  //  Self test of this class.
  procedure zyre_event_test (verbose:Boolean);external ZYRE_LIBRARY_NAME;

implementation

end.
