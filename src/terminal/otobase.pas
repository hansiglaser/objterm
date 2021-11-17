(**
* ObjTerm - Terminal with more than Text
*
* Main Server-Side Unit
*
* (c) 2021 by Johann Glaser
*)
Unit OTOBase;

{$mode objfpc}

Interface

Uses
  JS, Classes, SysUtils, Web, Dygraph, OTObjTerm, OTUtils;

Type

  { TOTOHTML }

  TOTOHTML = class(TOTObject)
    FHTML : String;             // it is the responsibility of the user (i.e., the client side), if "id" attributes are used, that they are valid and unique across the whole ObjTerm
    Constructor Create(AObjTerm : TObjTerm; AUniqueID : String; AHTML : String);
    Procedure SetElementAndFill(AElement:TJSHTMLElement); override;
    class Function GetOTType : String; override;
    class Function CreateFromCmdAppend(AObjTerm:TObjTerm;AUniqueID:String;AMsg:TJSObject) : TOTObject; override;
    class Function CreateFromConstArr(AObjTerm:TObjTerm;AUniqueID:String;const AArgs:array of JSValue) : TOTObject; override;
  End;

  { TOTOString }

  TOTOString = class(TOTOHTML)
    Constructor Create(AObjTerm : TObjTerm; AUniqueID : String; ASt : String);
    Procedure Append(ASt : String);                           // append text to string
    class Function GetOTType : String; override;
    Procedure ObjCmd(AMsg:TJSObject); override;
    Class Function Escape(ASt : String) : String;             // helper function to escape HTML special chars in string
    class Function CreateFromCmdAppend(AObjTerm:TObjTerm;AUniqueID:String;AMsg:TJSObject) : TOTObject; override;
    class Function CreateFromConstArr(AObjTerm:TObjTerm;AUniqueID:String;const AArgs:array of JSValue) : TOTObject; override;
  End;

  { TOTODygraph }

  TOTODygraph = class(TOTObject)
    FWidth   : Integer;
    FHeight  : Integer;
    FData    : TJSArray;
    FOptions : JSValue;
    FDiv     : TJSHTMLElement;
    FGraph   : TJSDygraph;
    Constructor Create(AObjTerm : TObjTerm; AUniqueID : String; AWidth : Integer; AHeight : Integer; AData : TJSArray; AOptions : JSValue);
    Procedure SetElementAndFill(AElement:TJSHTMLElement); override;
    Procedure SetData(AData : TJSArray; ABlockRedraw : Boolean = False);
    Procedure AppendPoint(APoint : TJSArray; ABlockRedraw : Boolean = False);
    Procedure AppendPoints(AData : TJSArray; ABlockRedraw : Boolean = False);
    class Function GetOTType : String; override;
    Procedure ObjCmd(AMsg:TJSObject); override;
    class Function CreateFromCmdAppend(AObjTerm:TObjTerm;AUniqueID:String;AMsg:TJSObject) : TOTObject; override;
    class Function CreateFromConstArr(AObjTerm:TObjTerm;AUniqueID:String;const AArgs:array of JSValue) : TOTObject; override;
  End;


Implementation

{ TOTOHTML }

Constructor TOTOHTML.Create(AObjTerm : TObjTerm; AUniqueID : String; AHTML : String);
Begin
  inherited Create(AObjTerm, AUniqueID);
  FHTML := AHTML;
End;

Procedure TOTOHTML.SetElementAndFill(AElement : TJSHTMLElement);
Begin
  inherited SetElementAndFill(AElement);
  FElement.InnerHtml := FHTML;
End;

Class Function TOTOHTML.GetOTType : String;
Begin
  Result := 'otohtml';
End;

class Function TOTOHTML.CreateFromCmdAppend(AObjTerm : TObjTerm; AUniqueID : String; AMsg : TJSObject) : TOTObject;
Begin
  Result := TOTOHTML.Create(AObjTerm, AUniqueID, String(AMsg['html']));
End;

class Function TOTOHTML.CreateFromConstArr(AObjTerm : TObjTerm; AUniqueID : String; const AArgs : array of JSValue) : TOTObject;
Begin
  Result := TOTOHTML.Create(AObjTerm, AUniqueID, String(AArgs[0]));
End;

{ TOTOString }

Constructor TOTOString.Create(AObjTerm : TObjTerm; AUniqueID : String; ASt : String);
Begin
  inherited Create(AObjTerm, AUniqueID, Escape(ASt));
End;

Procedure TOTOString.Append(ASt : String);
Begin
  FElement.InnerHtml := FElement.InnerHtml + Escape(ASt);
End;

Class Function TOTOString.GetOTType : String;
Begin
  Result := 'otostring';
End;

Procedure TOTOString.ObjCmd(AMsg : TJSObject);
Var CmdId  : String;
    SubCmd : String;
    St     : String;
    Status : TJSObject;
Begin
  CmdId  := String(AMsg['cmdid']);
  SubCmd := String(AMsg['subcmd']);
  if SubCmd = 'append' then
    Begin
      St := String(TJSObject(AMsg)['st']);
      Append(St);
      FObjTerm.SendMessage(FObjTerm.CreateMsgCmdStatus(CmdId, 'ok', ''));
    End
  else if SubCmd = 'getlength' then
    Begin
      Status := FObjTerm.CreateMsgCmdStatus(CmdId, 'ok', '');
      Status['length'] := Length(FElement.InnerHtml);  // TODO: this is not accurate, because the string would be escaped
      FObjTerm.SendMessage(Status);
    End
  else
    Begin
      raise Exception.Create('Invalid subcmd = "'+SubCmd+'"');
    End;
End;

Class Function TOTOString.Escape(ASt : String) : String;
Begin
  ASt := StringReplace(ASt,'<',   '&lt;',  [rfReplaceAll]);
  ASt := StringReplace(ASt,'>',   '&gt;',  [rfReplaceAll]);
  ASt := StringReplace(ASt,' ',   '&nbsp;',[rfReplaceAll]);
  ASt := StringReplace(ASt,#13#10,'<br>',  [rfReplaceAll]);
  ASt := StringReplace(ASt,#10,   '<br>',  [rfReplaceAll]);
  ASt := StringReplace(ASt,#13,   '<br>',  [rfReplaceAll]);
  Result := ASt;
End;

class Function TOTOString.CreateFromCmdAppend(AObjTerm : TObjTerm; AUniqueID : String; AMsg : TJSObject) : TOTObject;
Begin
  Result := TOTOString.Create(AObjTerm, AUniqueID, String(AMsg['st']));
End;

class Function TOTOString.CreateFromConstArr(AObjTerm : TObjTerm; AUniqueID : String; const AArgs : array of JSValue) : TOTObject;
Begin
  Result := TOTOString.Create(AObjTerm, AUniqueID, String(AArgs[0]));
End;

{ TOTODygraph }

Constructor TOTODygraph.Create(AObjTerm : TObjTerm; AUniqueID : String; AWidth : Integer; AHeight : Integer; AData : TJSArray; AOptions : JSValue);
Begin
  inherited Create(AObjTerm, AUniqueID);
  FWidth   := AWidth;
  FHeight  := AHeight;
  FData    := AData;
  FOptions := AOptions;
End;

Procedure TOTODygraph.SetElementAndFill(AElement : TJSHTMLElement);
Begin
  Inherited SetElementAndFill(AElement);
  // add another <div>, otherwise the background would default to the otbox,
  // even if boxes are not shown.
  FDiv := TJSHTMLElement(document.createElement('div'));
  FElement.appendChild(FDiv);
  // configure containing <div>
  FDiv.style.setProperty('width',      IntToStr(FWidth) +'px');
  FDiv.style.setProperty('height',     IntToStr(FHeight)+'px');
  FDiv.style.setProperty('margin',     '5px');
  FDiv.style.setProperty('padding',    '5px');
  FDiv.style.setProperty('color',      '#000000');
  FDiv.style.setProperty('background', '#FFFFFF');
  // Dygraph creates another <div> inside our own and sets its width and height
  // to the width and height of ours plus its padding. Inside the created <div>,
  // the canvas is placed with the same size.
  // Due to this behavior, if the padding >0px, then the content is shifted
  // right and down.
  FGraph := TJSDygraph.new(FDiv, FData, FOptions);
End;

Procedure TOTODygraph.SetData(AData : TJSArray; ABlockRedraw:Boolean=False);
Begin
  FData := AData;
  if not ABlockRedraw then   // don't even tell the Dygraph object yet
    FGraph.updateOptions(new(['file', FData]));
End;

Procedure TOTODygraph.AppendPoint(APoint : TJSArray; ABlockRedraw:Boolean=False);
Begin
(*
  // update data points (clean)
  FData.push(APoint);
  if not ABlockRedraw then   // don't even tell the Dygraph object yet
    FGraph.updateOptions(new(['file', FData]));
*)
  // append data points (dirty)
  FGraph.file_.push(APoint);
  if not ABlockRedraw then
    FGraph.start_();
End;

Procedure TOTODygraph.AppendPoints(AData : TJSArray; ABlockRedraw:Boolean=False);
Var Point : JSValue;
Begin
  For Point in AData do
    FGraph.file_.push(TJSArray(Point));
  if not ABlockRedraw then
    FGraph.start_();
End;

Class Function TOTODygraph.GetOTType : String;
Begin
  Result := 'otodygraph';
End;

Procedure TOTODygraph.ObjCmd(AMsg : TJSObject);
Var CmdId       : String;
    SubCmd      : String;
    Data        : TJSArray;
    BlockRedraw : Boolean;
Begin
  CmdId  := String(AMsg['cmdid']);
  SubCmd := String(AMsg['subcmd']);
  if SubCmd = 'appendpoints' then
    Begin
      Data        := TJSArray(AMsg['data']);
      BlockRedraw := Boolean(AMsg['blockredraw']);
      AppendPoints(Data, BlockRedraw);
      FObjTerm.SendMessage(FObjTerm.CreateMsgCmdStatus(CmdId, 'ok', ''));
    End
  else
    Begin
      raise Exception.Create('Invalid subcmd = "'+SubCmd+'"');
    End;
End;

class Function TOTODygraph.CreateFromCmdAppend(AObjTerm : TObjTerm; AUniqueID : String; AMsg : TJSObject) : TOTObject;
Begin
  Result := TOTODygraph.Create(AObjTerm, AUniqueID, Integer(AMsg['width']), Integer(AMsg['height']), TJSArray(AMsg['data']), AMsg['options']);
End;

class Function TOTODygraph.CreateFromConstArr(AObjTerm : TObjTerm; AUniqueID : String; const AArgs : array of JSValue) : TOTObject;
Begin
  Result := TOTODygraph.Create(AObjTerm, AUniqueID, Integer(AArgs[0]), Integer(AArgs[1]), TJSArray(AArgs[2]), TJSObject(AArgs[3]));
End;

Initialization
  // register object types
  if not assigned(ObjFactory) then
    raise Exception.Create('ObjFactory is not assigned. Did you use OTObjTerm before OTOBase');
  ObjFactory.RegisterObjType('otohtml',    TOTOHTML);
  ObjFactory.RegisterObjType('otostring',  TOTOString);
  ObjFactory.RegisterObjType('otodygraph', TOTODygraph);
End.

