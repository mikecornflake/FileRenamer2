Unit AppSettings;

Interface

Uses
  AdvObjects, AdvFilers, AdvXMLFilers, AdvPersistents, AdvStrings, AdvStringMatches,
  Actions;

Type
  TFileRenamerSettings=Class(TAdvPersistent)
  Private
    FSimpleSettings : TadvStringMatch;
    FSearches : TAdvStrings;
    FRenameExpressions : TAdvStrings;
    FCustomFields : TAdvStrings;

    FSearch: String;
    FCustomField: String;
    FRenameExpression: String;
    FFolder: String;
    FActions: TActions;

    Function GetCustomFields: TAdvStrings;
    Function GetRenameExpressions: TAdvStrings;
    Function GetSearches: TAdvStrings;
    Function GetSimpleSettings(sIndex: String): String;
    Function GetSimpleSettingsBoolean(sIndex: String): Boolean;
    Function GetSimpleSettingsInteger(sIndex: String): Integer;
    Procedure SetCustomFields(Const Value: TAdvStrings);
    Procedure SetRenameExpressions(Const Value: TAdvStrings);
    Procedure SetSearches(Const Value: TAdvStrings);
    Procedure SetSimpleSettings(sIndex: String; Const Value: String);
    Procedure SetSimpleSettingsBoolean(sIndex: String; Const Value: Boolean);
    Procedure SetSimpleSettingsInteger(sIndex: String; Const Value: Integer);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Assign(oObject : TAdvObject); Override;
    Procedure Define(oFiler : TAdvFiler); Override;

    Procedure Default;

    Procedure Apply;
    Procedure LoadFromUI;

    Procedure LoadFromFile(Const sFilename: String); Override;
    Procedure SaveToFile(Const sFilename: String);   Override;

    Property Folder : String           Read FFolder           Write FFolder;
    Property Search : String           Read FSearch           Write FSearch;
    Property RenameExpression : String Read FRenameExpression Write FRenameExpression;
    Property CustomField : String      Read FCustomField      Write FCustomField;

    Property Searches : TAdvStrings          Read GetSearches          Write SetSearches;
    Property RenameExpressions : TAdvStrings Read GetRenameExpressions Write SetRenameExpressions;
    Property CustomFields : TAdvStrings      Read GetCustomFields      Write SetCustomFields;

    Property StringSetting[sIndex : String] : String Read GetSimpleSettings Write SetSimpleSettings; Default;
    Property BooleanSetting[sIndex : String] : Boolean Read GetSimpleSettingsBoolean Write SetSimpleSettingsBoolean;
    Property IntegerSetting[sIndex : String] : Integer Read GetSimpleSettingsInteger Write SetSimpleSettingsInteger;

    Property Actions : TActions Read FActions;
  End;

Function Settings : TFileRenamerSettings;

Implementation

Uses
  FormMain,
  ShellSupport, StringSupport;

Var
  gSettings : TFileRenamerSettings;

{----------------------------------------------------------------------------------------}
Function Settings : TFileRenamerSettings;
Begin { AppSettings }
  Result := gSettings;
End; { AppSettings }

{ TFileRenamerSettings }

{----------------------------------------------------------------------------------------}
Constructor TFileRenamerSettings.Create;
Begin { Create }
  Inherited;

  FSearches := TAdvStrings.Create;
  FRenameExpressions := TAdvStrings.Create;
  FCustomFields := TAdvStrings.Create;

  FSimpleSettings := TAdvStringMatch.Create;
  FSimpleSettings.Force := True;
  FSimpleSettings.Default := '';

  FActions := TActions.Create;

  Default;

  If Not Assigned(gSettings) Then
    gSettings := Self;
End; { Create }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.Default;
Begin { Default }
  FFolder := SpecialFolder(sfMyComputer);
  FSearch := '*.*';
  FRenameExpression := '%Parent% + %Count% + %FileExt%';
  FCustomField := 'TextBetween(%Filename%, ''Start'', ''End''';

  FSearches.Clear;
  FSearches.Add('*.mp3;*.wma;*.avi;*.wmv;*.jpg;*.lit');
  FSearches.Add('*.avi;*.wmv;*.mpg;*.divx;*.rm;*.swf');
  FSearches.Add('*.mp3;*.wma');
  FSearches.Add('*.jpg;*.bmp;*.gif');
  FSearches.Add('*.lit;*.txt;*.rtf;*.wri;*.doc;*.pdf');
  FSearches.Add('*.*');

  FRenameExpressions.Clear;

  FSimpleSettings.Clear;
  BooleanSetting['RefreshGridAfterOperation'] := True;
End; { Default }

{----------------------------------------------------------------------------------------}
Destructor TFileRenamerSettings.Destroy;
Begin { Destroy }
  FSimpleSettings.Free;
  FSimpleSettings := Nil;

  FSearches.Free;
  FSearches := Nil;

  FRenameExpressions.Free;
  FRenameExpressions := Nil;

  FCustomFields.Free;
  FCustomFields := Nil;

  FActions.Free;
  FActions := Nil;

  Inherited;
End; { Destroy }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.Define(oFiler: TAdvFiler);
Begin { Define }
  Inherited;

  oFiler['FFolder'].DefineString(FFolder);
  oFiler['FSearch'].DefineString(FSearch);
  oFiler['FRenameExpression'].DefineString(FRenameExpression);
  oFiler['FCustomField'].DefineString(FCustomField);

  oFiler['FSearches'].DefineObject(FSearches);
  oFiler['FRenameExpressions'].DefineObject(FRenameExpressions);
  oFiler['FCustomFields'].DefineObject(FCustomFields);

  oFiler['FSimpleSettings'].DefineObject(FSimpleSettings);

  oFiler['FActions'].DefineObject(FActions);
End; { Define }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.Assign(oObject: TAdvObject);
Begin { Assign }
  Inherited;

  FFolder := TFileRenamerSettings(oObject).FFolder;
  FSearch := TFileRenamerSettings(oObject).FSearch;
  FRenameExpression := TFileRenamerSettings(oObject).FRenameExpression;
  FCustomField := TFileRenamerSettings(oObject).FCustomField;

  FSearches.         Assign(TFileRenamerSettings(oObject).FSearches);
  FRenameExpressions.Assign(TFileRenamerSettings(oObject).FRenameExpressions);
  FCustomFields.     Assign(TFileRenamerSettings(oObject).FCustomFields);
  FSimpleSettings.   Assign(TFileRenamerSettings(oObject).FSimpleSettings);
  FActions.            Assign(TFileRenamerSettings(oObject).FActions);
End; { Assign }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.LoadFromFile(Const sFilename: String);
Begin { LoadFromFile }
  LoadFromFile(sFilename, TAdvXMLReader);
End; { LoadFromFile }


{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SaveToFile(Const sFilename: String);
Begin { SaveToFile }
  SaveToFile(sFilename, TAdvXMLWriter);
End; { SaveToFile }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.Apply;
Var
  i : Integer;
Begin { Apply }
  frmMain.Folder := FFolder;

  frmMain.dlgExpression.edtExpression.Text := FCustomField;

  frmMain.cboSearch.Properties.Items.Clear;
  For i := 0 To FSearches.Count-1 Do
    frmMain.cboSearch.Properties.Items.Add(FSearches[i]);

  frmMain.cboSearch.Text := FSearch;
End; { Apply }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.LoadFromUI;
Begin { LoadFromUI }
  FFolder := frmMain.Folder;
  FCustomField := frmMain.dlgExpression.edtExpression.Text;
  FSearch := frmMain.cboSearch.Text;
End; { LoadFromUI }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetCustomFields: TAdvStrings;
Begin { GetCustomFields }
  Result := FCustomFields;
End; { GetCustomFields }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetRenameExpressions: TAdvStrings;
Begin { GetRenameExpressions }
  Result := FRenameExpressions;
End; { GetRenameExpressions }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetSearches: TAdvStrings;
Begin { GetSearches }
  Result := FSearches;
End; { GetSearches }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetCustomFields(Const Value: TAdvStrings);
Begin { SetCustomFields }
  FCustomFields.Clear;

  If Assigned(Value) Then
    FCustomFields.Assign(Value);
End; { SetCustomFields }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetRenameExpressions(Const Value: TAdvStrings);
Begin { SetRenameExpressions }
  FRenameExpressions.Clear;

  If Assigned(Value) Then
    FRenameExpressions.Assign(Value);
End; { SetRenameExpressions }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetSearches(Const Value: TAdvStrings);
Begin { SetSearches }
  FSearches.Clear;

  If Assigned(Value) Then
    FSearches.Assign(Value);
End; { SetSearches }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetSimpleSettings(sIndex: String): String;
Begin { GetSimpleSettings }
  Result := FSimpleSettings[sIndex];
End; { GetSimpleSettings }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetSimpleSettings(sIndex: String; Const Value: String);
Begin { SetSimpleSettings }
  FSimpleSettings[sIndex] := Trim(Value);
End; { SetSimpleSettings }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetSimpleSettingsBoolean(sIndex: String): Boolean;
Begin { GetSimpleSettingsBoolean }
  Result := Compare(FSimpleSettings[sIndex], 'True')=0;
End; { GetSimpleSettingsBoolean }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetSimpleSettingsBoolean(sIndex: String; Const Value: Boolean);
Begin { SetSimpleSettingsBoolean }
  If Value Then
    FSimpleSettings[sIndex] := 'True'
  Else
    FSimpleSettings[sIndex] := 'False';
End; { SetSimpleSettingsBoolean }

{----------------------------------------------------------------------------------------}
Function TFileRenamerSettings.GetSimpleSettingsInteger(sIndex: String): Integer;
Begin { GetSimpleSettingsInteger }
  Result := ToInteger(FSimpleSettings[sIndex], -1);
End; { GetSimpleSettingsInteger }

{----------------------------------------------------------------------------------------}
Procedure TFileRenamerSettings.SetSimpleSettingsInteger(sIndex: String; Const Value: Integer);
Begin { SetSimpleSettingsInteger }
  FSimpleSettings[sIndex] := ToString(Value);
End; { SetSimpleSettingsInteger }

Initialization
  gSettings := Nil;

End.
