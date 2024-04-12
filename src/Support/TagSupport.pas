Unit TagSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TTagSupport }

  TTagSupport = Class(TObject)
  Public
    Procedure AddBasicTags;

    Procedure ProcessGrid(ACallback: TNotifyEvent);
  End;

Function Tags: TTagSupport;

Implementation

Uses
  FormFileRenamer2, DB;

Var
  lTagSupport: TTagSupport;

Function Tags: TTagSupport;
Begin
  If Not assigned(lTagSupport) Then
    lTagSupport := TTagSupport.Create;

  Result := lTagSupport;
End;


Initialization
  lTagSupport := nil;

Finalization
  FreeAndNil(lTagSupport);

End.

