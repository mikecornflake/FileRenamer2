Unit Tags;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, DB, DBSupport, BufDataset, Menus, fgl, Contnrs;

Const
  BLANK = '<blank>';

Type

  { TMetaTag }

  TMetaTag = Class(TObject)
  Public
    Name: String;
    FieldType: TFieldType;
    Size: Integer;
    ReadOnly: Boolean;
    Value: Variant;

    Constructor Create(AName: String; AFieldType: TFieldType; ASize: Integer = -1;
      AReadOnly: Boolean = False);
  End;

  TMetaTagList = Specialize TFPGMapObject<String, TMetaTag>;

  { TMetaFileHandler }

  TMetaFileHandler = Class(TObject)
  Protected
    FCommon: TMetaFileHandler;
    FTags: TMetaTagList;
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

    Function TagByName(AName: String): TMetaTag;

    Property HasTags: Boolean read FHasTags;
    Property Filename: String read FFilename;
    Property Filter: String read FFilter write FFilter;

    Property Tags: TMetaTagList read FTags;
    Property Tag[AName: String]: Variant read GetTag write SetTag;

    Property Common: TMetaFileHandler read FCommon write FCommon;
  End;

  TMetaFileHandlerClass = Class Of TMetaFileHandler;
  TMetaFileHandlerList = Specialize TFPGObjectList<TMetaFileHandler>;

  { TTagManager }

  TTagManager = Class(TObject)
  Private
    FMetaFileHandlerClassByExt: TFPHashList; // The Factory

    FFiles: TMemTable;
    FFileSystemTags: TMetaFileHandler;
    FMetaFileHandlers: TMetaFileHandlerList; // Contains a unique instance of each created MetaFileHandler
    FVisibleFields: TStringList;

    FDBLock: TRTLCriticalSection;

    Function GetDataset: TBufDataset;

    Function GetMetaFileHandler(AIndex: Integer): TMetaFileHandler;
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
    Function TagDefByName(AName: String): TMetaTag;

    Procedure ParseFile(ACommon: TMetaFileHandler; ATags: TMetaFileHandlerList);
    Procedure AppendFile(ATags: TMetaFileHandlerList);
    Procedure ClearFiles;

    Procedure Register(AMetaFileHandler: TMetaFileHandlerClass; AFileExt: Array Of String);
    Function Build(AFileExt: String): TMetaFileHandler;

    // TODO - Implement
    Function Update(AFilename, ATag, AValue: String): Boolean; Overload;
    Function Update(AFilename: String; ATag, AValue: Array Of String): Boolean; Overload;

    Property Dataset: TBufDataset read GetDataset;
    Property MetaFileHandlers: TMetaFileHandlerList read FMetaFileHandlers;
    Property Tag[AName: String]: Variant read GetTag write SetTag;
    Property VisibleFields: TStringList read FVisibleFields;
  End;

Function TagManager: TTagManager;

Implementation

Uses
  StringSupport, TagFileSystem;

Var
  lTagManager: TTagManager;
  lVisibleFieldLock: TRTLCriticalSection;

Function TagManager: TTagManager;
Begin
  If Not assigned(lTagManager) Then
    lTagManager := TTagManager.Create;

  Result := lTagManager;
End;

{ TMetaTag }

Constructor TMetaTag.Create(AName: String; AFieldType: TFieldType; ASize: Integer;
  AReadOnly: Boolean);
Begin
  Name := AName;
  FieldType := AFieldType;
  Size := ASize;
  ReadOnly := AReadOnly;
  Value := Null;
End;

{ TMetaFileHandler }

Constructor TMetaFileHandler.Create;
Begin
  FTags := TMetaTagList.Create(True);

  FHasTags := False;

  FFilter := '';
  FCommon := nil;
End;

Destructor TMetaFileHandler.Destroy;
Begin
  FreeAndNil(FTags);

  Inherited Destroy;
End;

Function TMetaFileHandler.Writeable: Boolean;
Begin
  Result := False;
End;

Function TMetaFileHandler.ParseFile(AFilename: String): Boolean;
Begin
  Result := False;
  FFilename := AFilename;

  ClearTags;
End;

Function TMetaFileHandler.Write: Boolean;
Begin
  Result := False;
End;

Function TMetaFileHandler.TagByName(AName: String): TMetaTag;
Var
  oTemp: TMetaTag;
Begin
  Result := nil;

  If FTags.TryGetData(AName, oTemp) Then
    Result := oTemp;
End;

Function TMetaFileHandler.Name: String;
Begin
  Result := BLANK;
End;

Function TMetaFileHandler.GetTag(AName: String): Variant;
Var
  oTag: TMetaTag;
Begin
  oTag := TagByName(AName);
  If Assigned(oTag) Then
    Result := oTag.Value
  Else
    Result := Null;
End;

Procedure TMetaFileHandler.SetTag(AName: String; AValue: Variant);
Var
  oTag: TMetaTag;
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

Procedure TMetaFileHandler.ClearTags;
Var
  i: Integer;
Begin
  FHasTags := False;

  For i := 0 To FTags.Count - 1 Do
    FTags.Data[i].Value := Null;
End;

Procedure TMetaFileHandler.AddTag(AName: String; AFieldType: TFieldType;
  ASize: Integer; AReadOnly: Boolean);
Var
  oFileTagDef: TMetaTag;
Begin
  oFileTagDef := TMetaTag.Create(AName, AFieldType, ASize, AReadOnly);
  FTags.Add(AName, oFileTagDef);
End;

{ TTagManager }

Function TTagManager.GetDataset: TBufDataset;
Begin
  Result := FFiles.Table;
End;

Constructor TTagManager.Create;
Begin
  // The TMetaFileHandler Factory
  FMetaFileHandlerClassByExt := TFPHashList.Create;

  // Unique list of MetaFileHandlers
  FMetaFileHandlers := TMetaFileHandlerList.Create(True);

  FFiles := TMemTable.Create;

  FFileSystemTags := TTagFileSystem.Create;

  FVisibleFields := TStringList.Create;
  FVisibleFields.Sorted := True;
  FVisibleFields.Duplicates := dupIgnore;

  InitCriticalSection(FDBLock);
End;

Destructor TTagManager.Destroy;
Begin
  DoneCriticalSection(FDBLock);

  FreeAndNil(FVisibleFields);
  FreeAndNil(FFileSystemTags);
  FreeAndNil(FFiles);
  FreeAndNil(FMetaFileHandlers);  // OwnsObjects := True
  FreeAndNil(FMetaFileHandlerClassByExt);

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

Procedure TTagManager.Register(AMetaFileHandler: TMetaFileHandlerClass; AFileExt: Array Of String);
Var
  oMetaFileHandler: TMetaFileHandler;
  sExt, sFilter: String;
Begin
  oMetaFileHandler := AMetaFileHandler.Create;
  FMetaFileHandlers.Add(oMetaFileHandler);

  sFilter := '';
  For sExt In AFileExt Do
  Begin
    If sFilter = '' Then
      sFilter := Format('*%s', [sExt])
    Else
      sFilter := Format('%s;*%s', [sFilter, sExt]);

    FMetaFileHandlerClassByExt.Add(Lowercase(sExt), Pointer(AMetaFileHandler));
  End;

  oMetaFileHandler.Filter := sFilter;
End;

Function TTagManager.Build(AFileExt: String): TMetaFileHandler;
Var
  i: Integer;
  oClass: TMetaFileHandlerClass;
Begin
  Result := nil;

  i := FMetaFileHandlerClassByExt.FindIndexOf(AFileExt);
  If i >= 0 Then
  Begin
    oClass := TMetaFileHandlerClass(FMetaFileHandlerClassByExt.Items[i]);
    Result := oClass.Create;
  End;
End;

Procedure TTagManager.DefineTags;
Var
  i: Integer;
  oField: TField;
  oTag: TMetaTag;
  oMetaFileHandler: TMetaFileHandler;
Begin
  If FFiles.Active Then
    FFiles.Close;

  For i := 0 To FFileSystemTags.Tags.Count - 1 Do
  Begin
    oTag := FFileSystemTags.Tags.Data[i];
    FFiles.AddField(oTag.Name, oTag.FieldType, oTag.Size);
  End;

  For oMetaFileHandler In FMetaFileHandlers Do
    For i := 0 To oMetaFileHandler.Tags.Count - 1 Do
    Begin
      oTag := oMetaFileHandler.Tags.Data[i];
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

Function TTagManager.TagDefByName(AName: String): TMetaTag;
Var
  oTemp: TMetaTag;
  oMetaFileHandler: TMetaFileHandler;
Begin
  Result := nil;

  If FFileSystemTags.Tags.TryGetData(AName, oTemp) Then
    Result := oTemp
  Else
    For oMetaFileHandler In FMetaFileHandlers Do
      If oMetaFileHandler.Tags.TryGetData(AName, oTemp) Then
      Begin
        Result := oTemp;
        Break;
      End;
End;

Function TTagManager.GetMetaFileHandler(AIndex: Integer): TMetaFileHandler;
Begin
  Result := FMetaFileHandlers[AIndex];
End;

Function TTagManager.GetTag(AName: String): Variant;
Var
  oFileTagDef: TMetaTag;
Begin
  oFileTagDef := TagDefByName(AName);
  If Assigned(oFileTagDef) Then
    Result := oFileTagDef.Value
  Else
    Result := Null;
End;

Procedure TTagManager.SetTag(AName: String; AValue: Variant);
Var
  oFileTagDef: TMetaTag;
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

Procedure TTagManager.ParseFile(ACommon: TMetaFileHandler; ATags: TMetaFileHandlerList);
Var
  oMetaFileHandler: TMetaFileHandler;
  sExt: String;
  sFilename, sTemp: String;
  oClass: TMetaFileHandlerClass;
  i: Integer;
Begin
  // This routine needs to be threadsafe - use only local or passed variables

  sExt := ACommon.Tag['FileExt'];
  sFilename := ACommon.Tag['Original'];

  // TODO, Do I need to clear all Tags first?
  ACommon.Tag['Temp'] := '';
  ACommon.Tag['Tag'] := '';

  // Iterate over all TMetaFileHandler's, apply each one that applies
  For i := 0 To FMetaFileHandlerClassByExt.Count-1 Do
  Begin
    sTemp := FMetaFileHandlerClassByExt.NameOfIndex(i);
    If sTemp=sExt Then
    Begin
      oClass := TMetaFileHandlerClass(FMetaFileHandlerClassByExt.Items[i]);
      oMetaFileHandler := oClass.Create;
      ATags.Add(oMetaFileHandler);

      oMetaFileHandler.Common := ACommon;
      oMetaFileHandler.ParseFile(sFilename);

      If oMetaFileHandler.HasTags Then
        If ACommon.Tag['Tag']='' Then
          ACommon.Tag['Tag'] := oMetaFileHandler.Name
        Else
          ACommon.Tag['Tag'] := ACommon.Tag['Tag'] + ', ' + oMetaFileHandler.Name
    End;
  End;

  //oMetaFileHandler := Build(sExt);
  //If Assigned(oMetaFileHandler) Then
  //Begin
  //  ATags.Add(oMetaFileHandler);
  //
  //  oMetaFileHandler.Common := ACommon;
  //  oMetaFileHandler.ParseFile(sFilename);
  //
  //  If oMetaFileHandler.HasTags Then
  //    If ACommon.Tag['Tag']='' Then
  //      ACommon.Tag['Tag'] := oMetaFileHandler.Name
  //    Else
  //      ACommon.Tag['Tag'] := ACommon.Tag['Tag'] + ', ' + oMetaFileHandler.Name
  //End;
End;

Procedure TTagManager.AppendFile(ATags: TMetaFileHandlerList);
Var
  oMetaFileHandler: TMetaFileHandler;
  oTag: TMetaTag;
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
          For oMetaFileHandler In ATags Do
            If oMetaFileHandler.HasTags Then
              For i := 0 To oMetaFileHandler.Tags.Count - 1 Do
              Begin
                oTag := oMetaFileHandler.Tags.Data[i];
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
  oMetaFileHandler: TMetaFileHandler;
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
    oMetaFileHandler := MetaFileHandlerByExt(sExt);

    If Assigned(oMetaFileHandler) And (oMetaFileHandler.Writeable) Then
    Begin
      Result := True;
      oMetaFileHandler.Filename := AFilename;
      oMetaFileHandler.Value[ATag] := AValue;
    End;
  End;
*)
End;

Function TTagManager.Update(AFilename: String; ATag, AValue: Array Of String): Boolean;
(*
Var
  sPath, sExt: String;
  oMetaFileHandler: TMetaFileHandler;
  i: Integer;
*)
Begin
  // TODO COMPLETE THE WRITING OF TAGS
  Result := False;

(*
  sPath := IncludeSlash(Value(FFiles.Table, 'Path'));

  sExt := Lowercase(Value(FFiles.Table, 'Ext'));
  oMetaFileHandler := MetaFileHandlerByExt(sExt);

  If Assigned(oMetaFileHandler) And (oMetaFileHandler.Writeable) Then
  Begin
    Result := True;
    oMetaFileHandler.Filename := AFilename;
    For i := Low(AValue) To High(AValue) Do
      oMetaFileHandler.Value[ATag[i]] := AValue[i];
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
