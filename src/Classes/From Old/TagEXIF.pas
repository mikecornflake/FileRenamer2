Unit TagEXIF;

Interface

Uses
  AdvObjects,
  TagSupport, FileSupport, SysUtils, StringSupport,
  dEXIF, dIPTC, msData,
  Classes, Windows;

Type
  TTagEXIF = Class(TFileTag)
  Protected
    FName : String;
    FVisibleColumns : TStringList;
    FReadOnlyColumns : TStringList;
    FEditableColumns : TStringList;
    FIPTCColumns : TStringList;
    FEXIFColumns : TStringList;

    FImgData : TImgData;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;

Implementation

{ TTagEXIF }

{----------------------------------------------------------------------------------------}
Constructor TTagEXIF.Create;
Begin { Create }
  Inherited;

  FName := '';

  FImgData := TImgData.Create;
  ExifTrace := 0;

  FVisibleColumns := TStringList.Create;
  FVisibleColumns.Sorted := True;
  FVisibleColumns.Add('%Author%');
  FVisibleColumns.Add('%Comments%');
  FVisibleColumns.Add('%Date Time Original%');
  FVisibleColumns.Add('%Exif Image Length%');
  FVisibleColumns.Add('%Exif Image Width%');
  FVisibleColumns.Add('%Keywords%');
  FVisibleColumns.Add('%Subject%');
  FVisibleColumns.Add('%Title%');
  FVisibleColumns.Add('%Make%');
  FVisibleColumns.Add('%Model%');

  FReadOnlyColumns := TStringList.Create;
  FReadOnlyColumns.Sorted := True;
  FReadOnlyColumns.Add('%Date Time Original%');
  FReadOnlyColumns.Add('%Exif Image Length%');
  FReadOnlyColumns.Add('%Exif Image Width%');

  FEditableColumns := TStringList.Create;
  FEditableColumns.Sorted := True;
  FEditableColumns.Add('%Author%');   // Although I currently can't edit EXIF tags, I need these columns editable for the other tags
  FEditableColumns.Add('%Comments%');
  FEditableColumns.Add('%Keywords%');
  FEditableColumns.Add('%Subject%');
  FEditableColumns.Add('%Title%');

  FIPTCColumns := TStringList.Create;
  FEXIFColumns := TStringList.Create;
End; { Create }
{----------------------------------------------------------------------------------------}
Destructor TTagEXIF.Destroy;
Begin { Destroy }
  FEXIFColumns.Free;
  FEXIFColumns := Nil;

  FIPTCColumns.Free;
  FIPTCColumns := Nil;

  FVisibleColumns.Free;
  FVisibleColumns := Nil;

  FImgData.Free;
  FImgData := Nil;

  FEditableColumns.Free;
  FEditableColumns := Nil;

  FReadOnlyColumns.Free;
  FReadOnlyColumns := Nil;

  Inherited;
End; { Destroy }
{----------------------------------------------------------------------------------------}
Function TTagEXIF.Name: String;
Begin { Name }
  Result := FName;
End; { Name }
{----------------------------------------------------------------------------------------}
Function LookupIPTCTable(sDesc : String): integer;
Var
  i : integer;
Begin { LookupIPTCTable }
  i := 0;

  While (i<IPTCTAGCNT) And (Lower(sDesc)<>Lower(IPTCTable[i].Desc)) Do
    Inc(i);

  If i<IPTCTAGCNT Then
    Result := i
  Else
    Result := -1;
End; { LookupIPTCTable }
{----------------------------------------------------------------------------------------}
Function TTagEXIF.ParseFile(sFilename: String): Boolean;
Var
  oItem : TTagEntry;
  sTag : String;
  sTemp : String;
  i, iMid, iIndex : Integer;
  oList : TStringList;

Begin { ParseFile }
  FEXIFColumns.Clear;
  FIPTCColumns.Clear;

  Result := Inherited ParseFile(sFilename);

  If (FileExists(sFilename)) And (FImgData.ProcessFile(sFilename)) Then
  Begin { If }
    Result := True;
    FHasTag := FImgData.HasExif;

    If FHasTag Then
    Begin { If }
      FName := 'EXIF';

      FImgData.ExifObj.ResetIterator;

      While FImgData.ExifObj.IterateFoundTags(GenericEXIF, oItem) Do
      Begin { While }
        sTag := '%'+oItem.Desc+'%';
        sTemp := oItem.Data;

        If (sTag='%Date Time Original%') Then
        Begin { If }
          iMid := Pos(' ', sTemp);

          sTemp := StringReplace(Copy(sTemp, 1, iMid-1), ':', '-', [rfReplaceAll, rfIgnoreCase])+' '+
                   StringReplace(Copy(sTemp, iMid+1, Length(sTemp)), ':', '', [rfReplaceAll, rfIgnoreCase]);

          AddValue(sTag, sTemp, True, True);
        End { If }
        Else
          AddValue(sTag, sTemp, (FEditableColumns.IndexOf(sTag)=-1), (FVisibleColumns.IndexOf(sTag)<>-1));

        FEXIFColumns.Add(sTag);
      End; { While }
    End; { If }

    If (FImgData.HasIPTC) Then
    Begin { If }
      If FHasTag Then
        FName := FName + ', IPTC'
      Else
        FName := 'IPTC';

      FHasTag := True;

      oList := FImgData.IPTCObj.ParseIPTCStrings(FImgData.IPTCSegment^.Data);
      If oList.Count > 0 Then
      Begin { If }
        For i := 0 To oList.Count-1 Do
        Begin { For }
          Split(oList[i], '=', sTag, sTemp);

          iIndex := LookupIPTCTable(Trim(sTag));

          sTag := '%'+Trim(sTag)+'%';
          sTemp := Trim(sTemp);

          If (iIndex<>-1) Then
            AddValue(sTag, sTemp, (FReadOnlyColumns.IndexOf(sTag)<>-1), (IPTCTable[iIndex].Size>5), IPTCTable[iIndex].Size)
          Else
            AddValue(sTag, sTemp, (FReadOnlyColumns.IndexOf(sTag)<>-1), True);

          FIPTCColumns.Add(sTag);
        End; { For }
      End; { If }
      oList.Free;
    End; { If }

    If FHasTag Then
      FName := FName + ' Tag';
  End; { If }
End; { ParseFile }
{----------------------------------------------------------------------------------------}
Function TTagEXIF.Write: Boolean;
Var
  bHasIPTC : Boolean;
  i : Integer;
  iIndex : Integer;
  oTag : TTagEntry;
  sDesc : String;
  sName : String;
  sTag : String;
  sValue : String;

Begin { Write }
  Result := Inherited Write;

  If FileExists(FFilename) Then
  Begin { If }
    For i := 0 To FEXIFColumns.Count-1 Do
    Begin { For }
      sTag := FEXIFColumns[i];
      sDesc := Copy(sTag, 2, Length(sTag)-2);
      sName := StringReplace(sDesc, ' ', '', [rfReplaceAll]); // Remove all spaces from Desc
      sValue := Value[sTag];
      If sValue=BLANK Then
        sValue := '';

      If Not (Info[sTag].ReadOnly) Then
      Begin
        // Load any existing information on this tag
        oTag := FImgData.ExifObj.GetTagByDesc(sDesc);

        // Update or add tag
        oTag.Data := sValue;
        oTag.Raw := sValue;
        oTag.Size := length(sValue);
        If (oTag.Name='') Then
        Begin { If }
          oTag.Name := sName;
          oTag.Desc := sDesc;
          oTag.TType := FMT_STRING;
        End; { If }

        FImgData.ExifObj.Data[oTag.Name] := oTag;  // should add if doesn't exist
      End;
    End; { For }

    bHasIPTC := FImgData.HasIPTC;

    For i := 0 to IPTCTAGCNT-1 Do
    Begin
      sDesc := IPTCTable[i].Desc;
      sTag := '%'+sDesc+'%';

      iIndex := FTag.IndexOf(sTag);

      If (iIndex<>-1) Then
      Begin
        sName := IPTCTable[i].Name;

        sValue := Value[sTag];
        If sValue=BLANK Then
          sValue := '';

        If Not bHasIPTC Then
        Begin
          FImgData.FillInIPTC;

          bHasIPTC := FImgData.HasIPTC;
        End;

        If bhasIPTC Then
          FImgData.IPTCObj.AddTag(sName, sValue);
      End;
    End;

    If bHasIPTC Then
      FImgData.IptcObj.WriteFile(FFilename)
    Else
      FImgData.WriteEXIFJPEG(FFilename);
  End; { If }
End; { Write }

Initialization
  TagManager.Register(TTagEXIF, ['.jpg', '.jpeg']);

End.
