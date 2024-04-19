Unit Tags;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, DB, DBSupport, BufDataset, Menus, fgl, Contnrs;

Const
  BLANK = '<blank>';

Type

  { TFileTag }

  TFileTag = Class(TObject)
  Public
    Name: String;
    FieldType: TFieldType;
    Size: Integer;
    ReadOnly: Boolean;
    Value: Variant;

    Constructor Create(AName: String; AFieldType: TFieldType; ASize: Integer = -1;
      AReadOnly: Boolean = False);
  End;

  TFileTagList = Specialize TFPGMapObject<String, TFileTag>;

  { TFileTagger }

  TFileTagger = Class(TObject)
  Protected
    FCommon: TFileTagger;
    FTags: TFileTagList;
    FFilter: String;
    FHasTags: Boolean;
    FFilename: String;

    Procedure ClearTags;

    Function GetTag(AName: String): Variant;
    Procedure SetTag(AName: String; AValue: Variant); Virtual;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure AddTag(AName: String; AFieldType: TFieldType; ASize: Integer = -1;
      AReadOnly: Boolean = False);

    Function Writeable: Boolean; Virtual;

    Function Name: String; Virtual;

    Function ParseFile(AFilename: String): Boolean; Virtual;
    Function Write: Boolean; Virtual;

    Function TagByName(AName: String): TFileTag;

    Property HasTags: Boolean read FHasTags;
    Property Filename: String read FFilename;
    Property Filter: String read FFilter write FFilter;

    Property Tags: TFileTagList read FTags;
    Property Tag[AName: String]: Variant read GetTag write SetTag;

    Property Common: TFileTagger read FCommon write FCommon;
  End;

  TFileTaggerClass = Class Of TFileTagger;
  TFileTaggerList = Specialize TFPGObjectList<TFileTagger>;

  { TTagManager }

  TTagManager = Class(TObject)
  Private
    FFileTaggerClassByExt: TFPHashList; // The Factory
    FFileTaggers: TFileTaggerList; // Contains a unique instance of each created FileTagger
    FVisibleFields: TStringList;
    FFiles: TMemTable;
    FCommonTags: TFileTagger;
    FDBLock: TRTLCriticalSection;
    Procedure AddTagDef(AName: String; AFieldType: TFieldType; ASize: Integer = -1;
      AReadOnly: Boolean = False);
    Function GetDataset: TBufDataset;

    Function GetFileTagger(AIndex: Integer): TFileTagger;
    Function GetTag(AName: String): Variant;
    Procedure SetTag(AName: String; AValue: Variant);

    Procedure DoGetMemo(Sender: TField; Var aText: String; DisplayText: Boolean);
    Procedure DoSetText(Sender: TField; Const aText: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure BeginUpdate;
    Procedure EndUpdate;

    Procedure DefineTags;
    Function TagDefByName(AName: String): TFileTag;

    Procedure ParseFile(ACommon: TFileTagger; ATags: TFileTaggerList);
    Procedure AppendFile(ATags: TFileTaggerList);
    Procedure ClearFiles;

    Procedure Register(AFileTagger: TFileTaggerClass; AFileExt: Array Of String);
    Function Build(AFileExt: String): TFileTagger;

    // TODO - Implement
    Function Update(AFilename, ATag, AValue: String): Boolean; Overload;
    Function Update(AFilename: String; ATag, AValue: Array Of String): Boolean; Overload;

    Property Dataset: TBufDataset read GetDataset;
    Property FileTaggers: TFileTaggerList read FFileTaggers;
    Property Tag[AName: String]: Variant read GetTag write SetTag;
    Property VisibleFields: TStringList read FVisibleFields;
  End;

Function TagManager: TTagManager;

Implementation

Uses
  StringSupport, TagCommon;

Var
  lTagManager: TTagManager;
  lVisibleFieldLock: TRTLCriticalSection;

Function TagManager: TTagManager;
Begin
  If Not assigned(lTagManager) Then
    lTagManager := TTagManager.Create;

  Result := lTagManager;
End;

{ TFileTag }

Constructor TFileTag.Create(AName: String; AFieldType: TFieldType; ASize: Integer;
  AReadOnly: Boolean);
Begin
  Name := AName;
  FieldType := AFieldType;
  Size := ASize;
  ReadOnly := AReadOnly;
  Value := Null;
End;

{ TFileTagger }

Constructor TFileTagger.Create;
Begin
  FTags := TFileTagList.Create(True);

  FHasTags := False;

  FFilter := '';
  FCommon := nil;
End;

Destructor TFileTagger.Destroy;
Begin
  FreeAndNil(FTags);

  Inherited Destroy;
End;

Function TFileTagger.Writeable: Boolean;
Begin
  Result := False;
End;

Function TFileTagger.ParseFile(AFilename: String): Boolean;
Begin
  Result := False;
  FFilename := AFilename;

  ClearTags;
End;

Function TFileTagger.Write: Boolean;
Begin
  Result := False;
End;

Function TFileTagger.TagByName(AName: String): TFileTag;
Var
  oTemp: TFileTag;
Begin
  Result := nil;

  If FTags.TryGetData(AName, oTemp) Then
    Result := oTemp;
End;

Function TFileTagger.Name: String;
Begin
  Result := BLANK;
End;

Function TFileTagger.GetTag(AName: String): Variant;
Var
  oTag: TFileTag;
Begin
  oTag := TagByName(AName);
  If Assigned(oTag) Then
    Result := oTag.Value
  Else
    Result := Null;
End;

Procedure TFileTagger.SetTag(AName: String; AValue: Variant);
Var
  oTag: TFileTag;
Begin
  oTag := TagByName(AName);
  If Assigned(oTag) Then
  Begin
    oTag.Value := AValue;

    If AValue <> Null Then
    Begin
      FHasTags := True;
      EnterCriticalsection(lVisibleFieldLock);
      Try
        TagManager.VisibleFields.Add(AName);
      Finally
        LeaveCriticalsection(lVisibleFieldLock);
      End;
    End;
  End;
End;

Procedure TFileTagger.ClearTags;
Var
  i: Integer;
Begin
  FHasTags := False;

  For i := 0 To FTags.Count - 1 Do
    FTags.Data[i].Value := Null;
End;

Procedure TFileTagger.AddTag(AName: String; AFieldType: TFieldType; ASize: Integer;
  AReadOnly: Boolean);
Var
  oFileTagDef: TFileTag;
Begin
  oFileTagDef := TFileTag.Create(AName, AFieldType, ASize, AReadOnly);
  FTags.Add(AName, oFileTagDef);
End;

{ TTagManager }

Procedure TTagManager.AddTagDef(AName: String; AFieldType: TFieldType;
  ASize: Integer; AReadOnly: Boolean);
Var
  oFileTagDef: TFileTag;
Begin
  oFileTagDef := TFileTag.Create(AName, AFieldType, ASize, AReadOnly);
  FCommonTags.Tags.Add(AName, oFileTagDef);
End;

Function TTagManager.GetDataset: TBufDataset;
Begin
  Result := FFiles.Table;
End;

Constructor TTagManager.Create;
Begin
  // The TFileTagger Factory
  FFileTaggerClassByExt := TFPHashList.Create;

  // Unique list of FileTaggers
  FFileTaggers := TFileTaggerList.Create(True);

  FFiles := TMemTable.Create;

  FCommonTags := TTagCommon.Create;

  FVisibleFields := TStringList.Create;
  FVisibleFields.Sorted := True;
  FVisibleFields.Duplicates := dupIgnore;

  InitCriticalSection(FDBLock);
End;

Destructor TTagManager.Destroy;
Begin
  DoneCriticalSection(FDBLock);

  FreeAndNil(FVisibleFields);
  FreeAndNil(FCommonTags);
  FreeAndNil(FFiles);
  FreeAndNil(FFileTaggers);  // OwnsObjects := True
  FreeAndNil(FFileTaggerClassByExt);

  Inherited Destroy;
End;

Procedure TTagManager.BeginUpdate;
Begin
  FVisibleFields.Clear;
  FFiles.Table.DisableControls;
End;

Procedure TTagManager.EndUpdate;
Begin
  // Add these at the end to stop some pointless searches during the processing
  FVisibleFields.Add('Count');
  FVisibleFields.Add('Filename');
  FVisibleFields.Add('FileExt');
  FVisibleFields.Add('Tag');
  FVisibleFields.Add('Temp');
  FVisibleFields.Add('Date');
  FVisibleFields.Add('Size');
  FVisibleFields.Add('Path');

  FFiles.Table.EnableControls;
End;

Procedure TTagManager.Register(AFileTagger: TFileTaggerClass; AFileExt: Array Of String);
Var
  oFileTagger: TFileTagger;
  sExt, sFilter: String;
Begin
  oFileTagger := AFileTagger.Create;
  FFileTaggers.Add(oFileTagger);

  sFilter := '';
  For sExt In AFileExt Do
  Begin
    If sFilter = '' Then
      sFilter := Format('*%s', [sExt])
    Else
      sFilter := Format('%s;*%s', [sFilter, sExt]);

    FFileTaggerClassByExt.Add(Lowercase(sExt), Pointer(AFileTagger));
  End;

  oFileTagger.Filter := sFilter;
End;

Function TTagManager.Build(AFileExt: String): TFileTagger;
Var
  i: Integer;
  oClass: TFileTaggerClass;
Begin
  Result := nil;

  i := FFileTaggerClassByExt.FindIndexOf(AFileExt);
  If i >= 0 Then
  Begin
    oClass := TFileTaggerClass(FFileTaggerClassByExt.Items[i]);
    Result := oClass.Create;
  End;
End;

Procedure TTagManager.DefineTags;
Var
  i: Integer;
  oField: TField;
  oTag: TFileTag;
  oFileTagger: TFileTagger;
Begin
  If FFiles.Active Then
    FFiles.Close;

  For i := 0 To FCommonTags.Tags.Count - 1 Do
  Begin
    oTag := FCommonTags.Tags.Data[i];
    FFiles.AddField(oTag.Name, oTag.FieldType, oTag.Size);
  End;

  For oFileTagger In FFileTaggers Do
    For i := 0 To oFileTagger.Tags.Count - 1 Do
    Begin
      oTag := oFileTagger.Tags.Data[i];
      Try
        If Not FFiles.HasField(oTag.Name) Then
          FFiles.AddField(oTag.Name, oTag.FieldType, oTag.Size);
      Except
      End;
    End;

  FFiles.Open;

  FFiles.FieldByName['Filename'].OnSetText := @DoSetText;

  For oField In FFiles.Table.Fields Do
    If oField.DataType = ftMemo Then
      oField.OnGetText := @DoGetMemo;
End;

Function TTagManager.TagDefByName(AName: String): TFileTag;
Var
  oTemp: TFileTag;
  oFileTagger: TFileTagger;
Begin
  Result := nil;

  If FCommonTags.Tags.TryGetData(AName, oTemp) Then
    Result := oTemp
  Else
    For oFileTagger In FFileTaggers Do
      If oFileTagger.Tags.TryGetData(AName, oTemp) Then
      Begin
        Result := oTemp;
        Break;
      End;
End;

Function TTagManager.GetFileTagger(AIndex: Integer): TFileTagger;
Begin
  Result := FFileTaggers[AIndex];
End;

Function TTagManager.GetTag(AName: String): Variant;
Var
  oFileTagDef: TFileTag;
Begin
  oFileTagDef := TagDefByName(AName);
  If Assigned(oFileTagDef) Then
    Result := oFileTagDef.Value
  Else
    Result := Null;
End;

Procedure TTagManager.SetTag(AName: String; AValue: Variant);
Var
  oFileTagDef: TFileTag;
Begin
  oFileTagDef := TagDefByName(AName);
  If Assigned(oFileTagDef) Then
    oFileTagDef.Value := AValue;
End;

Procedure TTagManager.DoGetMemo(Sender: TField; Var aText: String; DisplayText: Boolean);
Begin
  aText := Sender.AsString;
End;

Procedure TTagManager.DoSetText(Sender: TField; Const aText: String);
Begin
  If Assigned(Sender) And FFiles.Table.Active Then
    FFiles.Table.FieldByName('Temp').AsString := Sender.Fieldname + '=' + aText;
End;

Procedure TTagManager.ParseFile(ACommon: TFileTagger; ATags: TFileTaggerList);
Var
  oFileTagger: TFileTagger;
  sExt: String;
  sFilename: String;
Begin
  // This routine needs to be threadsafe - use only local or passed variables

  sExt := ACommon.Tag['FileExt'];
  sFilename := ACommon.Tag['Original'];

  ACommon.Tag['Temp'] := '';

  oFileTagger := Build(sExt);
  If Assigned(oFileTagger) Then
  Begin
    ATags.Add(oFileTagger);

    oFileTagger.Common := ACommon;
    oFileTagger.ParseFile(sFilename);

    If oFileTagger.HasTags Then
      If ACommon.Tag['Tag'] = '' Then
        ACommon.Tag['Tag'] := oFileTagger.Name
      Else
        ACommon.Tag['Tag'] := ACommon.Tag['Tag'] + ', ' + oFileTagger.Name;
  End;
End;

Procedure TTagManager.AppendFile(ATags: TFileTaggerList);
Var
  oFileTagger: TFileTagger;
  oTag: TFileTag;
  i: Integer;
Begin
  EnterCriticalsection(FDBLock);
  Try
    FFiles.Table.DisableControls;
    Try
      If Not (FFiles.Table.State In [dsEdit, dsInsert]) Then
      Begin
        FFiles.Table.Append;
        Try
          For oFileTagger In ATags Do
            If oFileTagger.HasTags Then
              For i := 0 To oFileTagger.Tags.Count - 1 Do
              Begin
                oTag := oFileTagger.Tags.Data[i];
                If oTag.Value <> Null Then
                  FFiles.Table[oTag.Name] := oTag.Value;
              End;
        Finally
          FFiles.Table.Post;
        End;
      End
      Else
        Raise Exception.Create('TTagManager.AppendFile: Unable to append new record');
    Finally
      FFiles.Table.EnableControls;
    End;
  Finally
    LeaveCriticalsection(FDBLock);
  End;
End;

Procedure TTagManager.ClearFiles;
Begin
  FFiles.ClearAllRecords;
  FVisibleFields.Clear;
End;

Function TTagManager.Update(AFilename, ATag, AValue: String): Boolean;
(*
Var
  sPath, sExt: String;
  oFileTagger: TFileTagger;
*)
Begin
  Result := False;
(*
  sPath := IncludeSlash(Value(FFiles.Table, 'Path'));
  ATag := TrimChars(ATag, [' ', '%']);

  If AnsiCompareText(ATag, 'Filename') = 0 Then
    Result := FileSupport.FileRename(sPath + AFilename, sPath + AValue)
  Else
  Begin
    sExt := Lowercase(Value(FFiles.Table, 'Ext'));
    oFileTagger := FileTaggerByExt(sExt);

    If Assigned(oFileTagger) And (oFileTagger.Writeable) Then
    Begin
      Result := True;
      oFileTagger.Filename := AFilename;
      oFileTagger.Value[ATag] := AValue;
    End;
  End;
*)
End;

Function TTagManager.Update(AFilename: String; ATag, AValue: Array Of String): Boolean;
(*
Var
  sPath, sExt: String;
  oFileTagger: TFileTagger;
  i: Integer;
*)
Begin
  // TODO COMPLETE THE WRITING OF TAGS
  Result := False;

(*
  sPath := IncludeSlash(Value(FFiles.Table, 'Path'));

  sExt := Lowercase(Value(FFiles.Table, 'Ext'));
  oFileTagger := FileTaggerByExt(sExt);

  If Assigned(oFileTagger) And (oFileTagger.Writeable) Then
  Begin
    Result := True;
    oFileTagger.Filename := AFilename;
    For i := Low(AValue) To High(AValue) Do
      oFileTagger.Value[ATag[i]] := AValue[i];
  End;
*)
End;

Initialization
  lTagManager := nil;
  InitCriticalSection(lVisibleFieldLock);

Finalization
  FreeAndNil(lTagManager);
  DoneCriticalsection(lVisibleFieldLock);
End.
