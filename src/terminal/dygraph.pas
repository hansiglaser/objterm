(**
 * ObjTerm - Terminal with more than Text
 *
 * Pascal binding for Dygraph JavaScript Library
 *
 * https://dygraphs.com/
 *
 * (c) 2021 by Johann Glaser
 *)
Unit Dygraph;

{$mode objfpc}
{$modeswitch externalclass}

Interface

Uses JS, Web;

Type

  { TJSDygraph }

  TJSDygraph = class external name 'Dygraph'(TJSObject)
  private
    Ffile_ : TJSArray external name 'file_';
  public
    constructor new(ADiv:TJSElement; AData:TJSArray; AOpts:JSValue);
    procedure start_;
    procedure updateOptions(input_attrs : JSValue; block_redraw : Boolean = False);
    property file_ : TJSArray read Ffile_;
  End;

Implementation

End.

