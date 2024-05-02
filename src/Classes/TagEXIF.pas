Unit TagEXIF;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Tags, DB, fpeMetadata, fpeTags, fpeGlobal;

Type

  { TTagEXIF }

  TTagEXIF = Class(TMetaFileHandler)
  Protected
    FFields: TStringList;
    FImgInfo: TImgInfo;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name: String; Override;
    Function Writeable: Boolean; Override;

    Function ParseFile(sFilename: String): Boolean; Override;
  End;

Implementation

{ TTagEXIF }

Var
  LCreateEXIFLock: TRTLCriticalSection;

Constructor TTagEXIF.Create;
Var
  sTag, sMetaTag, sExifTag: String;
  i: Integer;
Begin
  Inherited Create;

  FImgInfo := TImgInfo.Create;

  FFields := TStringList.Create;
  FFields.Add('EXIF_Author=XPAuthor');
  FFields.Add('EXIF_Comments=XPComment');
  FFields.Add('EXIF_DateTimeOriginal=DateTimeOriginal');
  FFields.Add('EXIF_Keywords=XPKeywords');
  FFields.Add('EXIF_Make=Make');
  FFields.Add('EXIF_Model=Model');
  FFields.Add('EXIF_Software=Software');
  FFields.Add('EXIF_Subject=XPSubject');
  FFields.Add('EXIF_Title=XPTitle');
  FFields.Add('EXIF_Width=EXIFImageWidth');
  FFields.Add('EXIF_Height=EXIFImageHeight');

  // Add the special cases
  AddTag('EXIF_DateTimeOriginal', ftString, 25, True);
  AddTag('EXIF_Width', ftInteger, -1, True);
  AddTag('EXIF_Height', ftInteger, -1, True);

  // Add the simple strings
  For i := 0 To FFields.Count - 1 Do
  Begin
    sMetaTag := FFields.Names[i];
    sExifTag := FFields.ValueFromIndex[i];

    If FTags.IndexOf(sMetaTag) = -1 Then
      AddTag(sMetaTag, ftString, 100);
  End;

  // Finally the summary fields
  AddTag('EXIF_ID', ftString, 4096, True);
  AddTag('EXIF_IPTC_ID', ftString, 4096, True);
End;

Destructor TTagEXIF.Destroy;
Begin
  FreeAndNil(FImgInfo);
  FreeAndNil(FFields);

  Inherited Destroy;
End;

Function TTagEXIF.Name: String;
Begin
  Result := 'EXIF';
End;

Function TTagEXIF.Writeable: Boolean;
Begin
  Result := True;
End;

Function TTagEXIF.ParseFile(sFilename: String): Boolean;

  Procedure SetMetaFromExif(AMetaTag, AExifTag: String);
  Var
    oTag: fpeTags.TTag;
  Begin
    oTag := FImgInfo.ExifData.TagByName[AExifTag];
    If assigned(oTag) Then
      Tag[AMetaTag] := oTag.AsString;
  End;

  Function ExifAsNameValue(AIndex: Integer): String;
  Var
    oTag: TTag;
  Begin
    oTag := FImgInfo.ExifData.TagByIndex[AIndex];
    Try
      Result := Format('%s=%s', [oTag.Name, oTag.AsString]) + LineEnding;
    Except
      On E: Exception Do
        Result := Format('%s=%s', [oTag.Name, E.Message]) + LineEnding;
    End;
  End;

Var
  sTag, sField, sTemp, sMetaTag, sExifTag: String;
  i: Integer;
  oTemp: TStringList;
Begin
  Result := Inherited ParseFile(sFilename);

  If Not FileExists(sFilename) Then
    Exit;

  //fpExif is not threadsafe.  Ensure we only read one EXIF file at a time
  EnterCriticalSection(LCreateEXIFLock);
  Try
    FImgInfo.LoadFromFile(sFilename);

    If FImgInfo.HasExif Then
    Begin
      For i := 0 To FFields.Count - 1 Do
      Begin
        sMetaTag := FFields.Names[i];
        sExifTag := FFields.ValueFromIndex[i];

        SetMetaFromExif(sMetaTag, sExifTag);
      End;

      oTemp := TStringList.Create;
      Try
        FImgInfo.ExifData.ExportOptions :=
          [eoShowTagName, eoDecodeValue, eoTruncateBinary, eoBinaryAsASCII];
        FImgInfo.ExifData.ExportToStrings(oTemp, '=');

        Tag['EXIF_ID'] := oTemp.Text;
      Finally
        oTemp.Free;
      End;

      FHasTags := True;
    End;

    If FImgInfo.HasIptc Then
    Begin
      oTemp := TStringList.Create;
      Try
        FImgInfo.IptcData.ExportToStrings(oTemp, [eoShowTagName, eoDecodeValue,
          eoTruncateBinary, eoBinaryAsASCII], '=');

        Tag['EXIF_IPTC_ID'] := oTemp.Text;
      Finally
        oTemp.Free;
      End;
    End;
  Finally
    LeaveCriticalSection(LCreateEXIFLock);
  End;
End;

Initialization
  InitCriticalSection(LCreateEXIFLock);
  TagManager.Register(TTagEXIF, ['.jpg', '.jpeg', '.tiff']);

Finalization
  DoneCriticalsection(LCreateEXIFLock);

End.
