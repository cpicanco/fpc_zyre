
unit zyre_classes;

interface

uses zyre_library;

const
  PEER_EVASIVE = 10000;      { 10 seconds' silence is evasive }
  PEER_EXPIRED = 30000;      { 30 seconds' silence is expired }
  REAP_INTERVAL = 1000;      { Once per second }

//  Self test of this class.
procedure zyre_peer_test(verbose : Boolean); external ZYRE_LIBRARY_NAME;

//  Self test of this class.
procedure zyre_group_test(verbose : Boolean); external ZYRE_LIBRARY_NAME;

//  Self test of this class.
procedure zyre_node_test(verbose : Boolean); external ZYRE_LIBRARY_NAME;

implementation

end.
