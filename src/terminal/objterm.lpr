(**
 * ObjTerm - Terminal with more than Text
 *
 * Main Terminal Program (server-side, executed in browser)
 *
 * (c) 2021 by Johann Glaser
 *)
Program ObjTerm;

{$mode objfpc}

Uses OTOBase, OTObjTerm;
(* Due to the circular unit dependence, it is important to use OTOBase _before_
 * OTObjTerm. The compiler first reads OTOBase, and since it depends on
 * OTObjTerm, the compiler reads that before completing OTOBase. The generated
 * code first executes the initialization section of OTObjTerm. And only after
 * that, the initialization section of OTOBase is executed. This is important
 * because the latter depends on the former.
 * If first OTObjTerm would be used, then OTOBase would be read second,
 * however, the generated code would execute the initialization section of
 * OTOBase first.
 *)

Var OT : TObjTerm;

Begin
  OT := TObjTerm.Create;
  OT.AppendOTHTML('Welcome to <b>ObjTerm</b>!');
End.

