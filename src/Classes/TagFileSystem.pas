Unit TagFileSystem;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Tags, DB;

Type
  // This is a list of the File System metadata tags found in all files

  { TTagFileSystem }

  TTagFileSystem = Class(TFileTagBase)
  Protected
    Procedure SetTag(AName: String; AValue: Variant); Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name: String; Override;

    Function ParseFile(sFilename: String): Boolean; Override;
  End;

Implementation

{ TTagFileSystem }

Procedure TTagFileSystem.SetTag(AName: String; AValue: Variant);
Var
  oTag: TMetaTag;
Begin
  // Don't make these columns visible (which is what happens in the base class)
  oTag := TagByName(AName);
  If Assigned(oTag) Then
  Begin
    oTag.Value := AValue;
    FHasTags := True;
  End;
End;

Constructor TTagFileSystem.Create;
Begin
  Inherited Create;

  AddTag('Count', ftInteger);
  AddTag('Filename', ftString, 260);
  AddTag('FileExt', ftString, 52, True);
  AddTag('Tag', ftString, 52, True);
  AddTag('Temp', ftString, 4096);
  AddTag('Date', ftDateTime);
  AddTag('Size', ftLargeint, -1);

  AddTag('Path', ftString, 520, True);
  AddTag('Parent', ftString, 260, True);
  AddTag('Parent2', ftString, 260, True);
  AddTag('Original', ftString, 520, True);
End;

Destructor TTagFileSystem.Destroy;
Begin
  Inherited Destroy;
End;

Function TTagFileSystem.Name: String;
Begin
  Result := 'File System';
End;

Function TTagFileSystem.ParseFile(sFilename: String): Boolean;
Begin
  Result := Inherited ParseFile(sFilename);
  FHasTags := True;
End;

End.