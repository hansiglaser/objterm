(**
* ObjTerm - Terminal with more than Text
*
* Main Server-Side Unit
*
* (c) 2021 by Johann Glaser
*)
Unit OTObjTerm;

{$mode objfpc}

Interface

Uses
  JS, Classes, SysUtils, Web, OTUtils, Generics.Collections;

Type

  TObjTerm = class;

  { TOTObject }

  /// Base class for ObjTerm classes
  TOTObject = class
  strict protected    // these variables are only allowed to be set in the constructor
    FObjTerm    : TObjTerm;
    FUniqueID   : String;            // different from the DOM element ID
  protected           // can be set by TObjTerm
    FCSSClasses : Array of String;
    FCSSStyle   : String;
    FElement    : TJSHTMLElement;
  public
    Constructor Create(AObjTerm:TObjTerm;AUniqueID:String);
    Procedure SetElementAndFill(AElement:TJSHTMLElement); virtual;   // called after the <div> was added, intended to add the HTML of the ObjTerm object (e.g., text, image, ...)
    Procedure ObjCmd(AMsg:TJSObject); virtual;                // called via WebSocket to implement custom ObjTerm class type specific callbacks, AMsg is a JSON data structure of the message
    class Function GetOTType : String; virtual; abstract;     // returns a ObjTerm class specific type string
    property UniqueID : String read FUniqueID;
    class Function CreateFromCmdAppend(AObjTerm:TObjTerm;AUniqueID:String;AMsg:TJSObject) : TOTObject; virtual;
    class Function CreateFromConstArr(AObjTerm:TObjTerm;AUniqueID:String;const AArgs:array of JSValue) : TOTObject; virtual;
  End;

  TOTObjectClass = class of TOTObject;


  TMsgSeverity = (msNote,msWarning,msError,msFatal);

  { TObjTerm }

  // main class for ObjTerm
  TObjTerm = class
    // web page elements
    FHead      : TJSElement;
    FTerm      : TJSElement;
    FFoot      : TJSElement;
    FWSURL     : TJSHTMLInputElement;
    FWSConn    : TJSHTMLInputElement;
    FShowBoxes : TJSHTMLInputElement;
    FInput     : TJSHTMLInputElement;
    // ObjTerm object store
    FObjects : TJSArray;    // no Pascal array because .push() is native JavaScript and more efficient
    // WebSocket
    FWS : TJSWebSocket;
    FWSConnecting : Boolean;
    FWSClosing    : Boolean;
    // methods
    Constructor Create;
    // object store methods
    Function  AppendObj(AOTObject : TOTObject) : TOTObject;
    Function  AppendOTString(ASt : String) : TOTObject;
    Function  AppendOTHTML(AHTML : String) : TOTObject;
    Function  AppendOTDygraph(AWidth : Integer; AHeight : Integer; AData : TJSArray; AOptions : JSValue) : TOTObject;
    Function  AppendMsg(Const ASeverity:TMsgSeverity;Const AMessage:String) : TOTObject;
    Function  GetObj(AIndex:Integer) : TOTObject;
    Function  FindObj(AUniqueID:String) : TOTObject;
    // HTML event callbacks
    Function  OnWSConnClick(Event : TJSMouseEvent) : Boolean;
    Function  OnWithBoxesClick(Event : TJSMouseEvent) : Boolean;
    Function  OnOTInputKeyDown(Event:TJSKeyBoardEvent) : Boolean;
    // HTML and DOM functions
    Procedure InputEnable(APrompt, APromptStyle, AInputStyle : String);
    Procedure InputDisable;
    Procedure BoxesShow;
    Procedure BoxesHide;
    // high-level WebSocket methods
    Procedure HandleMessage(AMsg:TJSObject);
    Procedure HandleHelloReply(AMsg:TJSObject);
    Procedure HandleCmd(AMsg:TJSObject);
    Procedure HandleCmdAppend(AMsg:TJSObject);
    Procedure HandleCmdObjCmd(AMsg:TJSObject);
    Function  CreateMessage(AMsgType : String) : TJSObject;
    Function  CreateMsgHello : TJSObject;
    Function  CreateMsgCmdStatus(ACmdId:String;AStatus:String;AMessage:String) : TJSObject;
    Function  CreateMsgInput(AText:String) : TJSObject;
    Function  CreateMsgNotify : TJSObject;
    Procedure SendMessage(AMsg : TJSObject);
    // low-level WebSocket methods
    Procedure Connect(AUrl:String);
    Procedure Disconnect;
    Function  OnWSMessage(Event : TEventListenerEvent) : Boolean;
    Function  OnWSClose(Event : TEventListenerEvent) : Boolean;
    Function  OnWSError(Event : TEventListenerEvent) : Boolean;
    Function  OnWSOpen(Event : TEventListenerEvent) : Boolean;
  End;


  { TOTObjFactory }

  // abstract/empty class to have a thin interface for the outside world, will be overwritten with a real implementatoin class
  TOTObjFactory = class
  private
  protected
    Constructor Create; reintroduce; virtual; abstract;
  public
    Procedure RegisterObjType(AObjType:String;AObjClass:TOTObjectClass); virtual; abstract;
    Function  GetObjType(AObjType:String) : TOTObjectClass; virtual; abstract;
  End;

Var ObjFactory : TOTObjFactory;

Implementation

Uses OTOBase;

Const
  CMsgSeverityCSSClass : Array[TMsgSeverity] of String = ('otmsgnote', 'otmsgwarning', 'otmsgerror', 'otmsgfatal');


{ TOTObject }

Constructor TOTObject.Create(AObjTerm : TObjTerm; AUniqueID : String);
Begin
  FObjTerm  := AObjTerm;
  FUniqueID := AUniqueID;
End;

Procedure TOTObject.SetElementAndFill(AElement : TJSHTMLElement);
Begin
  FElement := AElement;
End;

Procedure TOTObject.ObjCmd(AMsg : TJSObject);
Begin
  raise Exception.Create(self.ClassName+'.ObjCmd is not implemented');
End;

Class Function TOTObject.CreateFromCmdAppend(AObjTerm : TObjTerm; AUniqueID : String; AMsg : TJSObject) : TOTObject;
Begin
  raise Exception.Create(self.ClassName+'.CreateFromCmdAppend is not implemented');
End;

Class Function TOTObject.CreateFromConstArr(AObjTerm : TObjTerm; AUniqueID : String; const AArgs : array of JSValue) : TOTObject;
Begin
  raise Exception.Create(self.ClassName+'.CreateFromConstArr is not implemented');
End;


{ TObjTerm }

Constructor TObjTerm.Create;
Begin
  // get references to existing HTML elements
  FHead      := document.getElementById('othead');
  FTerm      := document.getElementById('otterm');
  FFoot      := document.getElementById('otfoot');
  FWSUrl     := TJSHTMLInputElement(document.getElementById('otwsurl'));
  FWSConn    := TJSHTMLInputElement(document.getElementById('otwsconn'));
  FShowBoxes := TJSHTMLInputElement(document.getElementById('otwithboxes'));
  FInput     := TJSHTMLInputElement(document.getElementById('otinput'));
  // setup constants
  FObjects := TJSArray.new;
  // install event handlers on HTML elements
  FWSConn.OnClick    := @OnWSConnClick;
  FShowBoxes.OnClick := @OnWithBoxesClick;
  FInput.OnKeyDown   := @OnOTInputKeyDown;
End;

// object store methods

Function TObjTerm.AppendObj(AOTObject:TOTObject) : TOTObject;
Var Obj  : TJSHTMLElement;
    Box  : TJSHTMLElement;
    Meta : TJSHTMLElement;
    St   : String;
Begin
  // create actual object
  Obj := TJSHTMLElement(document.createElement('div'));
  St := AOTObject.GetOTType;
  if Length(AOTObject.FCSSClasses) > 0 then
    St := St + ' ' + String.Join(' ', AOTObject.FCSSClasses);
  Obj.className := 'otobject ' + St;
  if AOTObject.FCSSStyle > '' then
    Obj.setAttribute('style', AOTObject.FCSSStyle);   // Obj.style wants a TJSCSSStyleDeclaration
  Obj.ID        := 'o' + IntToStr(FObjects.Length);
  // create enclosing box
  Box := TJSHTMLElement(document.createElement('div'));
  Box.className := 'otbox';
  Box.ID        := 'b' + IntToStr(FObjects.Length);
  // create meta info field
  Meta := TJSHTMLElement(document.createElement('div'));
  Meta.className := 'otmeta';
  Meta.InnerHtml := 'Entry ' + IntToStr(FObjects.Length) + ' ('+St+')';
  // insert in DOM tree
  Box.AppendChild(Meta);
  Box.AppendChild(Obj);
  FTerm.AppendChild(Box);
  // object store
  FObjects.push(AOTObject);
  // assign to ObjTerm object (and update with content)
  // at least for Dygraph this must be after it was inserted in the DOM tree
  AOTObject.SetElementAndFill(Obj);
  // scroll to the bottom
  FTerm.scrollTop := FTerm.scrollHeight - FTerm.clientHeight;
  Result := AOTObject;
End;

Function TObjTerm.AppendOTString(ASt : String) : TOTObject;
Begin
  Result := AppendObj(TOTOString.Create(Self, GetGUIDStr, ASt));
End;

Function TObjTerm.AppendOTHTML(AHTML : String) : TOTObject;
Begin
  Result := AppendObj(TOTOHTML.Create(Self, GetGUIDStr, AHTML));
End;

Function TObjTerm.AppendOTDygraph(AWidth : Integer; AHeight : Integer; AData : TJSArray; AOptions : JSValue) : TOTObject;
Begin
  Result := AppendObj(TOTODygraph.Create(Self, GetGUIDStr, AWidth, AHeight, AData, AOptions));
End;

Function TObjTerm.AppendMsg(Const ASeverity:TMsgSeverity;Const AMessage:String) : TOTObject;
Var OS : TOTObject;
Begin
  OS := TOTOString.Create(Self, GetGUIDStr, AMessage);
  OS.FCSSClasses := [CMsgSeverityCSSClass[ASeverity]];
  Result :=  AppendObj(OS);
End;

Function TObjTerm.GetObj(AIndex : Integer) : TOTObject;
Begin
  if (AIndex < 0) or (AIndex >= FObjects.Length) then
    Exit(Nil);
  Result := TOTObject(FObjects[AIndex]);
End;

Function TObjTerm.FindObj(AUniqueID : String) : TOTObject;
Var I : Integer;
Begin
  For I := 0 to FObjects.length-1 do
    if TOTObject(FObjects[I]).UniqueID = AUniqueID then
     Exit(TOTObject(FObjects[I]));
  Result := Nil;
End;

// HTML event callbacks

Function TObjTerm.OnWSConnClick(Event : TJSMouseEvent) : Boolean;
Begin
(*  WriteLn('Message:');
  WriteLn('  Type=', Event._type);
  WriteLn('  Target.ID=',Event.TargetElement.ID);*)
  if Event.TargetElement.ID <> 'otwsconn' then
    Begin
      AppendMsg(msFatal, 'ObjTerm: OnWSConnClick was called from invalid element id '''+Event.TargetElement.ID+'''');
      Exit(True);
    End;
  if FWSConn.checked then
    Begin
      Connect(FWSUrl.value);
    End
  else
    Begin
      Disconnect;
    End;
  Result := True;
End;

Function TObjTerm.OnWithBoxesClick(Event : TJSMouseEvent) : Boolean;
Begin
  if not FShowBoxes.checked then
    BoxesHide
  else
    BoxesShow;
  Result := True;
End;

Function TObjTerm.OnOTInputKeyDown(Event : TJSKeyBoardEvent) : Boolean;
Var E : TJSHTMLInputElement;
Begin
  if (Event.key <> 'Enter') then Exit(True);
  if Event.TargetElement.ID <> 'otinput' then
    Begin
      AppendMsg(msFatal, 'ObjTerm: OnOTInputKeyDown was called from invalid element id '''+Event.TargetElement.ID+'''');
      Exit(True);
    End;
  E := TJSHTMLInputElement(Event.TargetElement);   // TODO: why not FInput?
//  WriteLn(E.value);
  SendMessage(CreateMsgInput(E.value));
  E.value := '';
  Result := False;
End;

(* Show and Hide Boxes around Objects
 *
 * By adding the class "othidebox" to the element with id "otterm" (i.e.,
 * the enclosing terminal area), the CSS styles of the elements within
 * are changed.
 *)
Procedure TObjTerm.BoxesShow;
Begin
  // enable boxes with meta infos
  FTerm.classList.remove('othidebox');
End;

Procedure TObjTerm.BoxesHide;
Begin
  // disable boxes with meta infos
  FTerm.classList.add('othidebox');
End;

Procedure TObjTerm.InputEnable(APrompt, APromptStyle, AInputStyle : String);
Var E : TJSHTMLElement;
Begin
  WriteLn('Enabling input with ',APrompt,' ',APromptStyle,' ',AInputStyle);
  // update the prompt
  E := TJSHTMLElement(document.getElementById('otprompt'));
  E.innerHTML := APrompt;
  E.setAttribute('style', APromptStyle);   // E.style wants a TJSCSSStyleDeclaration
  // update the <input ...>
  E := TJSHTMLElement(document.getElementById('otinput'));
  E.setAttribute('style', AInputStyle);    // E.style wants a TJSCSSStyleDeclaration
  TJSHTMLInputElement(E).textContent := '';
  E.focus;
  // show the input bar
  E := TJSHTMLElement(document.getElementById('otinputbar'));
  E.style.setProperty('display', 'block');
End;

Procedure TObjTerm.InputDisable;
Var E : TJSHTMLElement;
Begin
  // hide the input bar
  E := TJSHTMLElement(document.getElementById('otinputbar'));
  E.style.setProperty('display', 'none');
End;

// high-level WebSocket methods

Procedure TObjTerm.HandleMessage(AMsg : TJSObject);
Var MsgType : String;
Begin
  if not isDefined(AMsg['msgtype']) then
    Begin
      AppendMsg(msError, 'ObjTerm: Received message without a message type ("'+TJSJSON.stringify(AMsg)+'")');
      Exit;
    End;
  MsgType := String(AMsg['msgtype']);
  Case MsgType of
    'helloreply' : HandleHelloReply(AMsg);
    'cmd'        : HandleCmd(AMsg);
  else
    AppendMsg(msError, 'ObjTerm: Received message with invalid message type "'+MsgType+'" ("'+TJSJSON.stringify(AMsg)+'")');
  End;
End;

Procedure TObjTerm.HandleHelloReply(AMsg : TJSObject);
Var ClientVersion : String;
    ClientIdent   : String;
    InputEnable   : String;
    InputPrompt   : String;
    PromptStyle   : String;
    InputStyle    : String;
Begin
  ClientVersion := String(AMsg['version']);
  ClientIdent   := String(AMsg['ident']);
  InputEnable   := String(AMsg['inputenable']);
  InputPrompt   := String(AMsg['inputprompt']);
  PromptStyle   := String(AMsg['promptstyle']);
  InputStyle    := String(AMsg['inputstyle']);
  if InputEnable = 'true' then
    Self.InputEnable(InputPrompt, PromptStyle, InputStyle)
  else
    Self.InputDisable;
  AppendMsg(msNote, 'ObjTerm: Client at '+FWS.url+' uses version '+ClientVersion+', ident = '+ClientIdent);
End;

Procedure TObjTerm.HandleCmd(AMsg : TJSObject);
Var Cmd   : String;
    CmdId : String;
Begin
  if not isDefined(AMsg['cmdid']) then
    Begin
      AppendMsg(msError, 'ObjTerm: Received command message without command ID ("'+TJSJSON.stringify(AMsg)+'")');
      Exit;
    End;
  Cmd   := String(AMsg['cmd']);
  CmdId := String(AMsg['cmdid']);
  Case Cmd of
    'append'           : HandleCmdAppend(AMsg);
    'objcmd'           : HandleCmdObjCmd(AMsg);
// TODO:    'eval'             : ;
// TODO:    'settitle'         : ;
// TODO:    'addeventlistener' : ;
  else
    AppendMsg(msError, 'ObjTerm: Received invalid command "'+Cmd+'" ("'+TJSJSON.stringify(AMsg)+'")');
    SendMessage(CreateMsgCmdStatus(CmdId, 'error', 'Invalid command "'+Cmd+'"'));
  End;
End;

Procedure TObjTerm.HandleCmdAppend(AMsg : TJSObject);
Var CmdId      : String;
    OTType     : String;
    UniqueID   : String;
    V          : JSValue;
    A          : TJSArray;
    CSSClasses : Array of String;
    CSSStyle   : String;
    OTC        : TOTObjectClass;
    OT         : TOTObject;
Begin
  CmdId    := String(AMsg['cmdid']);
  OTType   := String(AMsg['type']);
  UniqueID := String(AMsg['uniqueid']);
  // TODO: add more checking of the received JSON object
  // retrieval of the JSON fields 'cssclasses' and 'cssstyle' treates them as optional, but the client actually always sends them
  V := AMsg['cssclasses'];
  if isDefined(V) then
    Begin
      if not isArray(V) then
        raise Exception.Create('cssclasses is not an array');
      A := TJSArray(V);
      CSSClasses := JSArrayOfStr2Pas(A);
    End;
  V := AMsg['cssstyle'];
  if isDefined(V) then
    Begin
      CSSStyle := String(V);
    End;
  OTC := ObjFactory.GetObjType(OTType);
  if OTC = Nil then
    Begin
      AppendMsg(msError, 'ObjTerm: Received command "append" with invalid type "'+OTType+'" ("'+TJSJSON.stringify(AMsg)+'")');
      SendMessage(CreateMsgCmdStatus(CmdId, 'error', 'Command "append" with invalid type "'+OTType+'"'));
      Exit;
    End;
  OT  := OTC.CreateFromCmdAppend(Self, UniqueID, AMsg);
  OT.FCSSClasses := CSSClasses;
  OT.FCSSStyle   := CSSStyle;
  AppendObj(OT);
  SendMessage(CreateMsgCmdStatus(CmdId, 'ok', ''));
End;

Procedure TObjTerm.HandleCmdObjCmd(AMsg : TJSObject);
Var CmdId    : String;
    UniqueID : String;
    OT       : TOTObject;
Begin
  CmdId    := String(AMsg['cmdid']);
  UniqueID := String(AMsg['uniqueid']);
  OT := FindObj(UniqueID);
  if OT = Nil then
    Begin
      AppendMsg(msError, 'ObjTerm: Received command "objcmd" but couldn''t find unique ID="'+UniqueID+'" ("'+TJSJSON.stringify(AMsg)+'")');
      SendMessage(CreateMsgCmdStatus(CmdId, 'error', 'Command "objcmd" but couldn''t find unique ID="'+UniqueID+'"'));
      Exit;
    End;
  try
    OT.ObjCmd(AMsg);
  except
    on E : Exception do
      Begin
        AppendMsg(msError, 'ObjTerm: Received command "objcmd" and its execution resulted in the error: '+E.Message);
        SendMessage(CreateMsgCmdStatus(CmdId, 'error', 'Command "objcmd" execution resulted in the error: '+E.Message));
      End;
  End;
End;

Function TObjTerm.CreateMessage(AMsgType : String) : TJSObject;
Begin
  Result := TJSObject.new;
  Result['msgtype'] := AMsgType;
End;

Function TObjTerm.CreateMsgHello : TJSObject;
Begin
  Result := CreateMessage('hello');
  Result['version']  := 'myversion'; // TODO
  Result['features'] := TJSArray._of('myfeature1', 'myfeature2'); // TODO
End;

Function TObjTerm.CreateMsgCmdStatus(ACmdId : String; AStatus : String; AMessage : String) : TJSObject;
Begin
  Result := CreateMessage('cmdstatus');
  Result['cmdid']   := ACmdId;
  Result['status']  := AStatus;
  Result['message'] := AMessage;
End;

Function TObjTerm.CreateMsgInput(AText : String) : TJSObject;
Begin
  Result := CreateMessage('input');
  Result['text'] := AText;
End;

Function TObjTerm.CreateMsgNotify : TJSObject;
Begin
  Result := CreateMessage('notify');
End;

Procedure TObjTerm.SendMessage(AMsg : TJSObject);
Begin
  FWS.send(TJSJSON.stringify(AMsg));
End;


// low-level WebSocket methods

Procedure TObjTerm.Connect(AUrl : String);
Begin
  FWSConnecting := True;   // start the "connecting" procedure
  FWS := TJSWebSocket.new(AUrl);
  FWS.onOpen    := @OnWSOpen;
  FWS.onerror   := @OnWSError;
  FWS.onclose   := @OnWSClose;
  FWS.onmessage := @OnWSMessage;
  FWSUrl.disabled := True;
End;

Procedure TObjTerm.Disconnect;
Begin
  FWSClosing := True;   // start the "disconnecting" = "closing" procedure
  FWS.close;
  // Firefox creates an exception for close(code) and close(code, reason), that they are not defined
End;

(**
 * Callback when a connection is successfully established
 *)
Function TObjTerm.OnWSOpen(Event : TEventListenerEvent) : Boolean;
Begin
  // Event._type is always 'open'
  // Event.TargetElement.ID is always undefined
  // FWS.url is always the URL to which the connection is established
  // FWS.readyState is always 1 = OPEN
  // FWS.bufferedAmount is always 0
  // FWS.extensions is always empty
  // FWS.protocol is always empty
  FWSConn.checked := True;
  FWSConnecting   := False;   // successfully done with the "connecting" procedure
  AppendMsg(msNote, 'ObjTerm: Established connection to '+FWS.url);

  SendMessage(CreateMsgHello);

  Result := True;
End;

(**
 * Callback when an error happens.
 *
 * No reason is available.
 *)
Function TObjTerm.OnWSError(Event : TEventListenerEvent) : Boolean;
Begin
  // When an error occurs during connecting, then:
  //   Event is never a TJSErrorEvent
  //   Event._type is always 'error'
  //   Event.TargetElement.ID is always undefined
  //   FWS.url is always the URL to which the connection is or could not be established
  //   FWS.readyState is always 3 = CLOSED
  //   FWS.bufferedAmount is always 0
  //   FWS.extensions is always empty
  //   FWS.protocol is always empty
  // TODO: what is different for "normal" errors?
  FWSUrl.disabled := False;
  FWSConn.checked := False;
  if FWSConnecting then
    Begin
      // Error during connecting, will be treated in OnWSError
    End
  else
    Begin
      // normal error, but we don't get a reason from JS
      AppendMsg(msError, 'ObjTerm: An error occured with the WebSocket connection to '+FWS.url+'. No additional information is available.');
    End;
  Result := True;
End;

Function TObjTerm.OnWSClose(Event : TEventListenerEvent) : Boolean;
Var CE : TJSCloseEvent;
Begin
  // Event._type is always 'close'
  // Event.TargetElement.ID is always undefined
  // the Event is always a TJSCloseEvent
  // CE.wasClean is false and CE.code is 1006 if the client closed by itself without sending a close message, or if no connection could be established
  //  1006 = The connection was closed abnormally, e.g., without sending or receiving a Close control frame
  // CE.wasClean is true  and CE.code is 1005 if the client or browser closed it by itself (cleanly)
  //  1005 = No status code was actually present.
  // CE.reason is always empty
  if not (Event is TJSCloseEvent) then
    Begin
      AppendMsg(msFatal, 'ObjTerm: OnWSClose for '+FWS.url+' was called with an event which is not a CloseEvent: Type='+Event._type+', Target.ID='+Event.TargetElement.ID);
      Exit(True);
    End;
  CE := Event as TJSCloseEvent;
  if FWSConnecting then
    Begin
      // Closing during connecting
      FWSConnecting := False;   // unsuccessfully done with the "connecting" procedure
      AppendMsg(msError, 'ObjTerm: Could not connect to '+FWS.url+'. No additional information is available');
      if FWSClosing then
        AppendMsg(msFatal, 'ObjTerm: FWSConnecting = true and FWSClosing = true at the same time');
    End
  else
    Begin
      // normal disconnect
      if CE.wasClean then
        Begin
          if FWSClosing then
            AppendMsg(msNote, 'ObjTerm: Closed connection to '+FWS.url)
          else
            AppendMsg(msNote, 'ObjTerm: The client '+FWS.url+' closed the connection');
        End
      else
        Begin
          if FWSClosing then
            AppendMsg(msError, 'ObjTerm: Closed connection to '+FWS.url+' not clean. code = '+IntToStr(CE.code)+', reason = '+CE.reason)
          else
            AppendMsg(msError, 'ObjTerm: The client '+FWS.url+' closed the connection not clean. code = '+IntToStr(CE.code)+', reason = '+CE.reason);
        End;
    End;
  FWSClosing := False;          // successfully or unsuccessfully done with "closing" procedure
  FWSUrl.disabled := False;
  FWSConn.checked := False;
  InputDisable;   // hide input bar
  Result := True;
End;

Function TObjTerm.OnWSMessage(Event : TEventListenerEvent) : Boolean;
Var Msg : TJSMessageEvent absolute Event;
Begin
(*  WriteLn('Message:');
  WriteLn('  Type=', Event._type);
  WriteLn('  Target.ID=',Event.TargetElement.ID);
  WriteLn('  Origin=',Msg.Origin);
  WriteLn('  LastEventID=',Msg.LastEventID);
  WriteLn('  Data=',String(Msg.Data));*)
  // parse to JSValue and cast to TJSObject and don't use our fpjson unit, because this is
  // inefficient and I also couldn't find a solution to convert it back to
  // JSValue for Dygraph options
  HandleMessage(TJSObject(TJSJSON.Parse(String(Msg.Data))));
//  O := AppendString(String(Msg.Data));
//  FWS.send(O.FElement.ID);
  Result := True;
End;

Type

  { TOTObjFactoryImpl }

  TOTObjFactoryImpl = class(TOTObjFactory)
  private
    type TObjTypeMap = specialize TDictionary<String,TOTObjectClass>;
    var FMap : TObjTypeMap;
  protected
    Constructor Create; override;
    Destructor  Destroy; override;
  public
    Procedure RegisterObjType(AObjType:String;AObjClass:TOTObjectClass); override;
    Function  GetObjType(AObjType:String) : TOTObjectClass; override;
  End;

{ TOTObjFactoryImpl }

Constructor TOTObjFactoryImpl.Create;
Begin
  TObject.Create;
  FMap := TObjTypeMap.Create;
End;

Destructor TOTObjFactoryImpl.Destroy;
Begin
  FMap.Free;
  Inherited Destroy;
End;

Procedure TOTObjFactoryImpl.RegisterObjType(AObjType : String; AObjClass : TOTObjectClass);
Begin
  FMap.Add(AObjType, AObjClass);
End;

Function TOTObjFactoryImpl.GetObjType(AObjType : String) : TOTObjectClass;
Begin
  if not FMap.TryGetValue(AObjType, Result) then
    Result := Nil;
End;

Initialization
  ObjFactory := TOTObjFactoryImpl.Create;
// finalization not possible with pas2js
End.

