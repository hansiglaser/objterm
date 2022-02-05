(**
 * ObjTerm - Terminal with more than Text
 *
 * Main Client-Side Unit
 *
 * Implements a simple WebSocket server, see https://github.com/Warfley/LazWebsockets
 *
 * (c) 2021 by Johann Glaser
 *)
Unit ObjTerm;

{$mode objfpc}
{$H+}

Interface

Uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  classes, SysUtils,
  fpjson, jsonparser,
  OTUtils,
  wsutils,
  wsmessages,
  wsstream,
  websocketserver,
  Generics.Collections;

Type

  TObjTerm = class;

  { TOTWSHandler }

  TOTWSHandler = class(TThreadedWebsocketHandler)
  private
    FObjTerm : TObjTerm;
    Procedure OnClose(Sender: TObject);
    Procedure OnReceiveMessage(Sender: TObject);
  public
    Constructor Create(AObjTerm : TObjTerm);
    Function Accept(const ARequest: TRequestData; const ResponseHeaders: TStrings): boolean; override;
    Procedure DoHandleCommunication(ACommunication: TWebsocketCommunicator); override;
    Procedure CloseAll;
    Function IsConnected : Boolean;
  End;

  { TOTWSThread }

  TOTWSThread = class(TThread)
  private
    FObjTerm : TObjTerm;
  public
    Constructor Create(AObjTerm : TObjTerm);
  protected
    Procedure Execute; override;
  End;

  { TOTObject }

  TOTObject = class
    FObjTerm    : TObjTerm;
    FUniqueID   : String;      // different from the DOM element ID
    FCSSClasses : Array of String;
    FCSSStyle   : String;
    Constructor Create;
    Procedure CmdAppend(AMsg : TJSONObject); virtual;
    class Function GetOTType : String; virtual; abstract;
  End;

  TOnConnect        = Procedure(AAddress:String);
  TOnDisconnect     = Procedure(AAddress:String);
  TOnInvalidMessage = Procedure(AMsg : TJSONObject; AError  :String);
  TOnHello          = Procedure(AHelloMsg, AHelloReplyMsg : TJSONObject);
  TOnCmdStatus      = Function (AMsg : TJSONObject; ACmdId, AStatus, AMessage : String; AData : Pointer) : Boolean;
  TOnInput          = Function (AMsg : TJSONObject; AText : String) : Boolean;
  TOnNotify         = Function (AMsg : TJSONObject) : Boolean;

  TMsgSeverity = (msNote,msWarning,msError,msFatal);

  { TObjTerm }

  TObjTerm = class
    // no object store because the objects should be kept by the creators
    FPort    : Word;
    FSendBuf : TStringRingBuffer;
    FRecvBuf : TStringRingBuffer;
    FWST     : TOTWSThread;
    FWSS     : TWebSocketServer;
    FWSH     : TOTWSHandler;
  private
  const
    FObjTermVersion   = '0.1';
  type
    { TCmdListEntry }
    TCmdListEntry = class
      FCmdId           : String;
      FRTLEvent        : PRTLEvent;
      FOnCmdStatus     : TOnCmdStatus;
      FOnCmdStatusData : Pointer;
      Constructor Create(ACmdId : String;ARTLEvent : PRTLEvent; AOnCmdStatus : TOnCmdStatus; AOnCmdStatusData : Pointer);
    End;
    TCmdList       = specialize TObjectList<TCmdListEntry>;
    TCmdThreadList = specialize TThreadedObject<TCmdList>;
  var
    FVerbose          : Boolean;            // defaults to false
    FOutput           : Text;               // defaults to Output, used for our own WriteLn
    FOnConnect        : TOnConnect;
    FConnectEvent     : PRTLEvent;
    FOnDisconnect     : TOnDisconnect;
    FHelloIdent       : String;
    FServerVersion    : String;
    FServerFeatures   : TStringArray;
    FOnInvalidMessage : TOnInvalidMessage;
    FOnHello          : TOnHello;
    FCmdList          : TCmdThreadList;
    FIgnoreCmdStatus  : Boolean;
    FOnCmdStatus      : TOnCmdStatus;
    FOnCmdStatusData  : Pointer;
    FIgnoreInput      : Boolean;
    FOnInput          : TOnInput;
    FIgnoreNotify     : Boolean;
    FOnNotify         : TOnNotify;
  public
    Constructor Create(APort : Word);
    Destructor Destroy; override;
    Procedure StartWebSocketServer;
    Procedure StartWebSocketServerThread;

    Function AppendObj(AOTObject : TOTObject) : TOTObject;
    Function AppendOTString(ASt : String) : TOTObject;
    Function AppendOTHTML(AHTML : String) : TOTObject;
    Function AppendOTDygraph(AWidth : Integer; AHeight : Integer; AData : TJSONArray; AOptions : TJSONData) : TOTObject;
    Function AppendMsg(Const ASeverity:TMsgSeverity;Const AMessage:String) : TOTObject;

    Procedure Assign(Var F : Text);

    Function WaitForConnect(ATimeout : LongInt = 0) : Boolean;
    Function IsConnected : Boolean;

  protected
    Function HandleMessage  (AMsg : TJSONObject) : Boolean;
    Function HandleHello    (AMsg : TJSONObject) : Boolean;
    Function HandleCmdStatus(AMsg : TJSONObject) : Boolean;
    Function HandleInput    (AMsg : TJSONObject) : Boolean;
    Function HandleNotify   (AMsg : TJSONObject) : Boolean;
    Function CreateMessage(AMsgType : String) : TJSONObject;
    Function CreateMsgHelloReply : TJSONObject;
  public
    Function CreateMsgCmd(ACmd : String; ARTLEvent : PRTLEvent = Nil; AOnCmdStatus : TOnCmdStatus = Nil; AOnCmdStatusData : Pointer = Nil) : TJSONObject;
    Function SendMessage(AMsg : TJSONObject) : Boolean;

    property Verbose          : Boolean           read FVerbose          write FVerbose;
    property Output           : Text              read FOutput           write FOutput;
    property HelloIdent       : String            read FHelloIdent       write FHelloIdent;
    property ServerVersion    : String            read FServerVersion;
    property ServerFeatures   : TStringArray      read FServerFeatures;
    property OnConnect        : TOnConnect        read FOnConnect        write FOnConnect;
    property OnDisconnect     : TOnDisconnect     read FOnDisconnect     write FOnDisconnect;
    property OnInvalidMessage : TOnInvalidMessage read FOnInvalidMessage write FOnInvalidMessage;
    property OnHello          : TOnHello          read FOnHello          write FOnHello;
    property IgnoreCmdStatus  : Boolean           read FIgnoreCmdStatus  write FIgnoreCmdStatus;
    property OnCmdStatus      : TOnCmdStatus      read FOnCmdStatus      write FOnCmdStatus;
    property OnCmdStatusData  : Pointer           read FOnCmdStatusData  write FOnCmdStatusData;
    property IgnoreInput      : Boolean           read FIgnoreInput      write FIgnoreInput;
    property OnInput          : TOnInput          read FOnInput          write FOnInput;
    property IgnoreNotify     : Boolean           read FIgnoreNotify     write FIgnoreNotify;
    property OnNotify         : TOnNotify         read FOnNotify         write FOnNotify;
  private
    Procedure CmdListAdd(AEntry:TCmdListEntry);
    Function  CmdListFind(ACmdId:String):TCmdListEntry;
  End;

Implementation

Uses OTOBase;

Const
  CMsgSeverityCSSClass : Array[TMsgSeverity] of String = ('otmsgnote', 'otmsgwarning', 'otmsgerror', 'otmsgfatal');

{ TOTWSHandler }

Constructor TOTWSHandler.Create(AObjTerm : TObjTerm);
Begin
  inherited Create;
  FObjTerm := AObjTerm;
End;

Function TOTWSHandler.Accept(Const ARequest: TRequestData; Const ResponseHeaders: TStrings): boolean;
Begin
  if FObjTerm.FVerbose then
    WriteLn(FObjTerm.FOutput, 'Accept at ', ARequest.Host,' ',ARequest.Path);
  Result := True;
End;

(**
 * Handle the communication with a WebSocket client (browser)
 *
 * This method is executed as soon as a WebSocket client (i.e., a browser) has
 * connected to the WebSocket server.
 *
 * This method first sets up the communicaton, and then only handles the
 * sending part. Receiving is done in OnReceiveMessage, closing is done in
 * OnClose.
 *)
Procedure TOTWSHandler.DoHandleCommunication(ACommunication : TWebsocketCommunicator);
Var St : String;
Begin
  ACommunication.OnReceiveMessage := @OnReceiveMessage;
  ACommunication.OnClose          := @OnClose;
  FObjTerm.FSendBuf.EventAdd := RTLEventCreate;
  // notify about established connection
  if assigned(FObjTerm.FOnConnect) then
     FObjTerm.FOnConnect(ACommunication.SocketStream.RemoteAddress.Address);
  RTLEventSetEvent(FObjTerm.FConnectEvent);
  // keep active while communicating
  while ACommunication.Open do
    Begin
      RTLEventWaitFor(FObjTerm.FSendBuf.EventAdd, 100);  // use timeout so that we don't overlook when the connection is closed
      while not FObjTerm.FSendBuf.IsEmpty do
        Begin
          St := FObjTerm.FSendBuf.Get;
          if FObjTerm.FVerbose then
            WriteLn(FObjTerm.FOutput, 'Message to ', ACommunication.SocketStream.RemoteAddress.Address, ': ''', St, '''');
          ACommunication.WriteStringMessage(St);
        End;
    End;
  RTLEventDestroy(FObjTerm.FSendBuf.EventAdd);
  FObjTerm.FSendBuf.EventAdd := Nil;
  if FObjTerm.FVerbose then
    WriteLn(FObjTerm.FOutput, 'Disconnected from ', ACommunication.SocketStream.RemoteAddress.Address);
End;

Procedure TOTWSHandler.CloseAll;
Var ConnectionList : TConnectionList;
    Connection     : TWebsocketCommunicator;
Begin
  ConnectionList := Connections.Lock;
  try
    for Connection in ConnectionList do
      FinalizeCommunication(Connection);
  finally
    Connections.Unlock;
  end;
End;

Function TOTWSHandler.IsConnected : Boolean;
Var ConnectionList : TConnectionList;
    Connection     : TWebsocketCommunicator;
Begin
  Result := False;
  ConnectionList := Connections.Lock;
  try
    for Connection in ConnectionList do
      Result := Result or Connection.Open;
  finally
    Connections.Unlock;
  end;
End;

Procedure TOTWSHandler.OnReceiveMessage(Sender : TObject);
Var Comm     : TWebsocketCommunicator;
    Messages : TWebsocketMessageOwnerList;
    Message  : TWebsocketMessage;
Begin
  Comm := TWebsocketCommunicator(Sender);
  Messages := TWebsocketMessageOwnerList.Create(True);
  try
    Comm.GetUnprocessedMessages(Messages);
    for Message in Messages do
      Begin
        if not (Message is TWebsocketStringMessage) then
          Begin
            raise Exception.Create('Unknown message type '+Message.ClassName);
          End;
        if FObjTerm.FVerbose then
          WriteLn(FObjTerm.FOutput, 'Message from ', Comm.SocketStream.RemoteAddress.Address, ': ''', TWebsocketStringMessage(Message).Data,'''');
        if FObjTerm.HandleMessage(GetJSON(TWebsocketStringMessage(Message).Data) as TJSONObject) then
          FObjTerm.FRecvBuf.Add(TWebsocketStringMessage(Message).Data);
      End;
  finally
    Messages.Free;
  End;
End;

Procedure TOTWSHandler.OnClose(Sender : TObject);
Var Comm: TWebsocketCommunicator;
Begin
  Comm := TWebsocketCommunicator(Sender);
  // notify about closed connection
  if assigned(FObjTerm.FOnDisconnect) then
     FObjTerm.FOnDisconnect(Comm.SocketStream.RemoteAddress.Address);
End;


{ TOTWSThread }

(**
 * Background thread for WebSocket server
 *)
Constructor TOTWSThread.Create(AObjTerm : TObjTerm);
Begin
  inherited Create(False);
  FreeOnTerminate := False;
  FObjTerm := AObjTerm;
End;

Procedure TOTWSThread.Execute;
Begin
  FObjTerm.StartWebSocketServer;
  if FObjTerm.FVerbose then
    WriteLn(FObjTerm.FOutput, 'Exiting Web Socket Thread');
End;


{ TOTObject }

Constructor TOTObject.Create;
Begin
  FUniqueID := GetGUIDStr;
End;

Procedure TOTObject.CmdAppend(AMsg : TJSONObject);
Begin
  AMsg.Add('uniqueid',   FUniqueID);
  AMsg.Add('type',       GetOTType);
  AMsg.Add('cssclasses', StrArr2JSONArray(FCSSClasses));
  AMsg.Add('cssstyle',   FCSSStyle);
End;


{ TObjTerm.TCmdListEntry }

Constructor TObjTerm.TCmdListEntry.Create(ACmdId : String; ARTLEvent : PRTLEvent; AOnCmdStatus : TOnCmdStatus; AOnCmdStatusData : Pointer);
Begin
  inherited Create;
  FCmdId           := ACmdId;
  FRTLEvent        := ARTLEvent;
  FOnCmdStatus     := AOnCmdStatus;
  FOnCmdStatusData := AOnCmdStatusData;
End;


{ TObjTerm }

Constructor TObjTerm.Create(APort:Word);
Begin
  FPort    := APort;
  FOutput  := System.Output;   // store a copy to StdOut for our own WriteLn
  FSendBuf := TStringRingBuffer.Create(16);
  FRecvBuf := TStringRingBuffer.Create(16);
  FCmdList := TCmdThreadList.Create(TCmdList.Create);
  FConnectEvent := RTLEventCreate;
End;

Procedure ObjTermWrite(Var F : TextRec); forward;

Destructor TObjTerm.Destroy;
Begin
  if TextRec(System.Output).InOutFunc = Pointer(@ObjTermWrite) then
    Begin
      WriteLn(FOutput, 'Error: System.Output is still assigned to this object in Destroy. Resetting to our internal copy. You should Assign() something else before Free()ing this object.');
      System.Output := FOutput;
    End;
  if assigned(FWST) then
    Begin
      FWST.Terminate;
      FWST.WaitFor;
    End;
  FSendBuf.Free;
  FRecvBuf.Free;
  FCmdList.Free;
  Inherited Destroy;
End;

(**
 * Run the WebSocket server
 *
 * This method executes the WebSocket server. It is blocking, i.e., it only
 * returns after the server is stopped.
 *
 * Use the method StartWebSocketServerThread to start a background thread for
 * the WebSocket server.
 *
 *)
Procedure TObjTerm.StartWebSocketServer;
Begin
  if FVerbose then
    WriteLn(FOutput, 'Starting WebSocket server on port ',FPort);
  FWSS := TWebSocketServer.Create(FPort);
  try
    FWSS.Socket.ReuseAddress := True;    // allow using the same port immediately if the program restarts
    FWSS.FreeHandlers := True;
    //FWSS.AcceptingMethod:=samThreadPool;
    FWSH := TOTWSHandler.Create(Self);
    FWSS.RegisterHandler('*', '*', FWSH, True, True);
    if FVerbose then
      WriteLn(FOutput, 'Web Socket Server started');
    FWSS.Start;
  finally
    FWSS.Free;
  End;
End;

(**
 * Create a background thread with the WebSocket server
 *
 * This method creates a background thread and immediately returns.
 *
 * In the background thread, the method StartWebSocketServer is executed.
 *)
Procedure TObjTerm.StartWebSocketServerThread;
Begin
  FWST := TOTWSThread.Create(Self);
End;

Function TObjTerm.AppendObj(AOTObject:TOTObject) : TOTObject;
Var Msg : TJSONObject;
Begin
  AOTObject.FObjTerm := Self;
  Msg := CreateMsgCmd('append');
  AOTObject.CmdAppend(Msg);
  SendMessage(Msg);
  Msg.Free;

  Result := AOTObject;
End;

Function TObjTerm.AppendOTString(ASt : String) : TOTObject;
Begin
  Result := AppendObj(TOTOString.Create(ASt));
End;

Function TObjTerm.AppendOTHTML(AHTML : String) : TOTObject;
Begin
  Result := AppendObj(TOTOHTML.Create(AHTML));
End;

Function TObjTerm.AppendOTDygraph(AWidth : Integer; AHeight : Integer; AData : TJSONArray; AOptions : TJSONData) : TOTObject;
Begin
  Result := AppendObj(TOTODygraph.Create(AWidth, AHeight, AData, AOptions));
End;

Function TObjTerm.AppendMsg(Const ASeverity : TMsgSeverity; Const AMessage : String) : TOTObject;
Var OS : TOTObject;
Begin
  OS := TOTOString.Create(AMessage);
  OS.FCSSClasses := [CMsgSeverityCSSClass[ASeverity]];
  Result :=  AppendObj(OS);
End;

// Store reference to TObjTerm object to textrec
Procedure SetObjTerm(Var F : TextRec; OT : TObjTerm);
Begin
  Move(OT, TextRec(F).UserData[1], SizeOf(OT));
End;

// Extract TObjTerm object reference from textrec
Function GetObjTerm(Var F : TextRec) : TObjTerm;
Begin
  Move(F.UserData[1], Result, SizeOf(Result));
End;

// Store reference to TOTOString object to textrec
Procedure SetOTOString(Var F : TextRec; OS : TOTOString);
Begin
  Move(OS, TextRec(F).UserData[1+8], SizeOf(OS));
End;

// Extract last TOTOString object reference from textrec
Function GetLastOTOString(Var F : TextRec) : TOTOString;
Begin
  Move(F.UserData[1+8], Result, SizeOf(Result));
End;

// Write function for ObjTerm associated file
Procedure ObjTermWrite(Var F : TextRec);
Var StartIdx : Integer;
    Idx      : Integer;
    OT       : TObjTerm;
    OS       : TOTOString;

  Procedure AppendFromBuf;
  Var St : String;
  Begin
    SetLength(St, Idx - StartIdx);
    Move(F.BufPtr^[StartIdx], St[1], Idx - StartIdx);
    //WriteLn(StdErr, 'Appending string '''+St+''' with OT = $',IntToHex(PtrUInt(OT), 8));
    if assigned(OS) then
      OS.Append(St)
    else
      OS := TOTOString(OT.AppendOTString(St));
  End;

Begin
  StartIdx := 0;
  Idx := StartIdx;
  OT := GetObjTerm(F);
  OS := GetLastOTOString(F);
  if not Assigned(OT) then
    Exit;   // ignore if no object (should not happen)
  if not OT.IsConnected then         // this will deadlock if this object is still assigned to System.Output in Destroy
    Exit;   // ignore if not connected
  While Idx < F.BufPos do
    Begin
      if F.BufPtr^[Idx] = #10 then
        Begin
          AppendFromBuf;
          OS := Nil;   // start a new line with the next text
          StartIdx := Idx+1;   // +1 to skip #10
        End;
      Inc(Idx);
    End;
  if Idx > StartIdx then
    Begin
      // more text without a #10
      AppendFromBuf;
    End;
  SetOTOString(F, OS);
  F.BufPos := 0;
End;


// Close ObjTerm associated file
Procedure ObjTermClose(Var F : TextRec);
Begin
  F.Mode := fmClosed;
End;

/// Open ObjTerm associated file
Procedure ObjTermOpen(Var F : TextRec);
Begin
  if F.Mode = fmOutput Then
    Begin
      TextRec(F).InOutFunc := @ObjTermWrite;
      TextRec(F).FlushFunc := @ObjTermWrite;
    End
  else
    Begin
      F.Mode := fmInput;
      WriteLn(StdErr, 'Error: ObjTerm associated file openend for mode ', F.Mode);
    End;
  TextRec(F).CloseFunc := @ObjTermClose;
End;

(**
 * Assign a file for writing to ObjTerm. All output to that file goes to ObjTerm instead
 *
 * see also unit Crt and https://forum.lazarus.freepascal.org/index.php?topic=34621.0
 *)
Procedure TObjTerm.Assign(Var F : Text);
Begin
  System.Assign(F, '');    // initialize
  TextRec(F).OpenFunc := @ObjTermOpen;
  // store reference to ObjTerm object and clear the rest to store more information
  SetObjTerm  (TextRec(F), Self);
  SetOTOString(TextRec(F), Nil);
End;

Function TObjTerm.WaitForConnect(ATimeout:LongInt = 0) : Boolean;
Begin
  if ATimeOut = 0 then
    RTLEventWaitFor(FConnectEvent)
  else
    RTLEventWaitFor(FConnectEvent, ATimeOut);
End;

Function TObjTerm.IsConnected : Boolean;
Begin
  Result := FWSH.IsConnected;
End;

(**
 * Handle a messages received via WebSocket
 *
 * This function runs in the Receive Thread.
 *
 * @returns  true  if the message was not handled and should    be added to the RecvBuf,
 *           false if the message was     handled and hould not be added to the RecvBuf
 *)
Function TObjTerm.HandleMessage(AMsg : TJSONObject) : Boolean;
Var MsgType : String;
Begin
  if AMsg.IndexOfName('msgtype') < 0 then
    Begin
      if assigned(FOnInvalidMessage) then
        FOnInvalidMessage(AMsg, 'Error: Received message without a message type ("'+AMsg.AsJSON+'")');
      Exit(false); // message is fully handled, therefore don't queue to FRecvBuf
    End;
  MsgType := AMsg.Strings['msgtype'];
  Case MsgType of
    'hello'     : Result := HandleHello(AMsg);
    'cmdstatus' : Result := HandleCmdStatus(AMsg);
    'input'     : Result := HandleInput(AMsg);
    'notify'    : Result := HandleNotify(AMsg);
  else
    if assigned(FOnInvalidMessage) then
      FOnInvalidMessage(AMsg, 'Error: Received message with invalid message type "'+MsgType+'" ("'+AMsg.AsJSON+'")');
    Exit(false); // message is fully handled, therefore don't queue to FRecvBuf
  End;
End;

Function TObjTerm.HandleHello(AMsg : TJSONObject) : Boolean;
Var HelloReply : TJSONObject;
Begin
  // store info from server
  FServerVersion  := AMsg.Strings['version'];
  FServerFeatures := JSONArray2StrArr(AMsg.Arrays['features']);
  // create helloreply message
  HelloReply := CreateMsgHelloReply;
  // notify event OnHello
  if assigned(FOnHello) then
    FOnHello(AMsg, HelloReply);
  // send
  SendMessage(HelloReply);
  Result := false; // message is fully handled, therefore don't queue to FRecvBuf
End;

Function TObjTerm.HandleCmdStatus(AMsg : TJSONObject) : Boolean;
Var CmdId   : String;
    Status  : String;
    Message : String;
    CmdListEntry : TCmdListEntry;
    B            : Boolean;
Begin
  CmdId   := AMsg.Strings['cmdid'];
  Status  := AMsg.Strings['status'];
  Message := AMsg.Strings['message'];

  Result := true; // message is not handled, therefore queue to FRecvBuf

  // look in command list
  CmdListEntry := CmdListFind(CmdId);
  if assigned(CmdListEntry) then
    Begin
      // first the OnCmdStatus, then the RTLEvent, so that it can be used to wait for data created by OnCmdStatus
      if assigned(CmdListEntry.FOnCmdStatus) then
        Result := CmdListEntry.FOnCmdStatus(AMsg, CmdId, Status, Message, CmdListEntry.FOnCmdStatusData);
      if CmdListEntry.FRTLEvent <> Nil then
        RTLEventSetEvent(CmdListEntry.FRTLEvent);
    End;

  if FIgnoreCmdStatus then
    Exit(false); // message is fully handled, therefore don't queue to FRecvBuf

  // catch-all event notification
  if assigned(FOnCmdStatus) then
    Begin
      B := FOnCmdStatus(AMsg, CmdId, Status, Message, FOnCmdStatusData);
      Result := B and Result;  // always execute FOnCmdStatus
    End;
End;

Function TObjTerm.HandleInput(AMsg : TJSONObject) : Boolean;
Var TheText : String;
Begin
  if FIgnoreInput then
    Exit(false); // message is fully handled, therefore don't queue to FRecvBuf
  TheText := AMsg.Strings['text'];
  Result := true; // message is not handled, therefore queue to FRecvBuf
  if assigned(FOnInput) then
    Result := FOnInput(AMsg, TheText);
End;

Function TObjTerm.HandleNotify(AMsg : TJSONObject) : Boolean;
Begin
  if FIgnoreNotify then
    Exit(false); // message is fully handled, therefore don't queue to FRecvBuf
  Result := true; // message is not handled, therefore queue to FRecvBuf
  if assigned(FOnNotify) then
    Result := FOnNotify(AMsg);
End;

Function TObjTerm.CreateMessage(AMsgType:String) : TJSONObject;
Begin
  Result := TJSONObject.Create(['msgtype',AMsgType]);
End;

Function TObjTerm.CreateMsgHelloReply : TJSONObject;
Begin
  Result := CreateMessage('helloreply');
  Result.Add('version',     FObjTermVersion);
  Result.Add('ident',       FHelloIdent);
  Result.Add('inputenable', 'false');
  Result.Add('inputprompt', '');
  Result.Add('promptstyle', '');
  Result.Add('inputstyle',  '');
End;

Function TObjTerm.CreateMsgCmd(ACmd:String;ARTLEvent:PRTLEvent=Nil;AOnCmdStatus:TOnCmdStatus=Nil; AOnCmdStatusData : Pointer=Nil) : TJSONObject;
Var CmdId        : String;
    CmdListEntry : TCmdListEntry;
Begin
  CmdId  := GetGUIDStr;
  Result := CreateMessage('cmd');
  Result.Add('cmd',   ACmd);
  Result.Add('cmdid', CmdId);
  // if notification for cmdstatus message required, add to FCmdList
  if (ARTLEvent <> Nil) or (AOnCmdStatus <> Nil) then
    Begin
      CmdListEntry := TCmdListEntry.Create(CmdID, ARTLEvent, AOnCmdStatus, AOnCmdStatusData);
      CmdListAdd(CmdListEntry);
    End;
End;

Function TObjTerm.SendMessage(AMsg : TJSONObject) : Boolean;
Begin
  //if FVerbose then
  //  Begin
  //    WriteLn(FOutput, 'Sending JSON:');
  //    WriteLn(FOutput, AMsg.FormatJSON);
  //  End;
  Result := FSendBuf.Add(AMsg.AsJSON);
End;

Procedure TObjTerm.CmdListAdd(AEntry : TCmdListEntry);
Var CmdList : TCmdList;
Begin
  CmdList := FCmdList.Lock;
  try
    CmdList.Add(AEntry);
  finally
    FCmdList.Unlock;
  End;
End;

Function TObjTerm.CmdListFind(ACmdId : String) : TCmdListEntry;
Var CmdList : TCmdList;
    I       : SizeInt;
Begin
  CmdList := FCmdList.Lock;
  try
    for I := 0 to CmdList.Count-1 do
      Begin
        Result := CmdList[I];
        if Result.FCmdId = ACmdId then
          Begin
            CmdList.Delete(I);
            Exit;
          End
      End;
  finally
    FCmdList.Unlock;
  End;
  Result := Nil;
End;

End.

