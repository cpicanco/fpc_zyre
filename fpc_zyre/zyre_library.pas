unit zyre_library;

// static on linux
//{$LINKLIB stdc++}
//{$LINKLIB gcc_s}
//{$LINK /usr/local/lib/libsodium.a}
//{$LINK /usr/local/lib/libzmq.a}
//{$LINK /usr/local/lib/libczmq.a}
//{$LINK /usr/local/lib/libzyre.a}

interface

const
{$ifdef WINDOWS}
  ZYRE_LIBRARY_NAME = 'libzyre.dll';
{$endif}

{$ifdef LINUX}
  ZYRE_LIBRARY_NAME = 'zyre';
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
  zlist_t      = record end;    TZList       = zlist_t;         PZList       = ^TZList;
  zsock_t      = record end;    TZSocket     = zsock_t;         PZSocket     = ^TZSocket;
  zhash_t      = record end;    TZHash       = zhash_t;         PZHash       = ^TZHash;
  zuuid_t      = record end;    TCZmqUUID    = zuuid_t;         PCZmqUUID    = ^TCZmqUUID;

const
  ZRE_DISCOVERY_PORT = 5670;

  // use zyre_version instead
  //ZYRE_VERSION_MAJOR = 2;
  //ZYRE_VERSION_MINOR = 0;
  //ZYRE_VERSION_PATCH = 1;

implementation

end.
