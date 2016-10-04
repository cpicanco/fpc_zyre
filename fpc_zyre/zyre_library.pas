
unit zyre_library;

interface

const
{$ifdef WINDOWS}
  ZYRE_LIBRARY_NAME = 'libzyre.dll';
{$endif}

{$ifdef LINUX}
  ZYRE_LIBRARY_NAME = 'libzyre.so.1';
{$endif}

{$ifdef DARWIN}
  ZYRE_LIBRARY_NAME = 'libzyre.1.dylib';
{$endif}

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

//  ZYRE version macros for compile-time API detection

type
  zyre_t       = record end;    TZyre        = zyre_t;          PZyre        = ^TZyre;
  zyre_event_t = record end;    TZyreEvent   = zyre_event_t;    PZyreEvent   = ^TZyreEvent;
  zre_msg_t    = record end;    TZyreMessage = zre_msg_t;       PZyreMessage = ^TZyreMessage;

  zframe_t     = record end;    TZFrame      = zframe_t;        PZFrame      = ^TZFrame;
  zmsg_t       = record end;    TZMessage    = zmsg_t;          PZMessage    = ^TZMessage;
  zlist_t      = record end;    TZyreList    = zlist_t;         PZyreList    = ^TZyreList;
  zsock_t      = record end;    TZyreSocket  = zsock_t;         PZyreSocket  = ^TZyreSocket;
  zhash_t      = record end;    TZyreHash    = zhash_t;         PZyreHash    = ^TZyreHash;
  zuuid_t      = record end;    TCZmqUUID    = zuuid_t;         PCZmqUUID    = ^TCZmqUUID;

  size_t = Int64;
const
  ZRE_DISCOVERY_PORT = 5670;
implementation

end.
