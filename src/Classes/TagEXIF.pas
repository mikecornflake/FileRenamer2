Unit TagEXIF;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Tags, DB, fpeMetadata, fpeTags;

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
  sTag: String;
Begin
  Inherited Create;

  FImgInfo := TImgInfo.Create;

  FFields := TStringList.Create;
  FFields.Add('EXIF_Author');
  FFields.Add('EXIF_Comments');
  FFields.Add('EXIF_Keywords');
  FFields.Add('EXIF_Subject');
  FFields.Add('EXIF_Title');
  FFields.Add('EXIF_Make');
  FFields.Add('EXIF_Model');
  FFields.Add('EXIF_Software');

  For sTag In FFields Do
    AddTag(sTag, ftString, 100);

  AddTag('EXIF_DateTimeOriginal', ftString, 25, True);
  AddTag('EXIF_Width', ftInteger, -1, True);
  AddTag('EXIF_Height', ftInteger, -1, True);
  AddTag('EXIF', ftString, 4096, True);
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

  Procedure SetTag(AMetaTag, AExifTag: String);
  Var
    oTag: fpeTags.TTag;
  Begin
    oTag := FImgInfo.ExifData.TagByName[AExifTag];
    If assigned(oTag) Then
      Tag[AMetaTag] := oTag.AsString;
  End;

  Function TagNameValue(AIndex: Integer): String;
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
  sTag, sField, sTemp: String;
  i: Integer;
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
      For sField In FFields Do
      Begin
        // Strip the EXIF prefix...
        sTag := Copy(sField, 6, Length(sField) - 5);

        SetTag(sField, sTag);
      End;

      sTemp := '';
      For i := 0 To FImgInfo.ExifData.TagCount - 1 Do
        sTemp := sTemp + TagNameValue(i);

      If Assigned(FCommon) Then
        Tag['EXIF'] := sTemp;

      SetTag('EXIF_DateTimeOriginal', 'DateTimeOriginal');
      SetTag('EXIF_Width', 'EXIFImageWidth');
      SetTag('EXIF_Height', 'EXIFImageHeight');
      SetTag('EXIF_Software', 'Software');
      SetTag('EXIF_Author', 'XPAuthor');
      SetTag('EXIF_Comments', 'XPComment');
      SetTag('EXIF_Keywords', 'XPKeywords');
      SetTag('EXIF_Subject', 'XPSubject');
      SetTag('EXIF_Title', 'XPTitle');

      FHasTags := True;
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
