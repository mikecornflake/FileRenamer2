Unit TagMultimedia;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Tags, DB, TagVideoNFO;

Type
  TTagVideo = Class(TMetaFileHandler)
  Private
    FHasNFO: Boolean;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name: String; Override;

    Function ParseFile(sFilename: String): Boolean; Override;
  End;

Implementation

Uses
  ffmpegSupport;

  { TTagVideo }

Constructor TTagVideo.Create;
Begin
  Inherited Create;

  AddTag('MM_File_Format', ftString, 100, True);
  AddTag('MM_Width', ftInteger, -1, True);
  AddTag('MM_Height', ftInteger, -1, True);
  AddTag('MM_Duration', ftFloat, -1, True);
  AddTag('MM_Streams', ftInteger, -1, True);
  AddTag('MM_VID_Codec', ftString, 100, True);
  AddTag('MM_VID_Stream', ftInteger, -1, True);
  AddTag('MM_AUD_Codec', ftString, 100, True);
  AddTag('MM_AUD_Stream', ftInteger, -1, True);
  AddTag('MM_SUB_Codec', ftString, 100, True);
  AddTag('MM_SUB_Stream', ftInteger, -1, True);
  AddTag('MM', ftString, 4096, True);

  FHasNFO := False;
End;

Destructor TTagVideo.Destroy;
Begin
  Inherited Destroy;
End;

Function TTagVideo.Name: String;
Begin
  If FHasNFO Then
    Result := 'Video, Video NFO'
  Else
    Result := 'Video';
End;

Function TTagVideo.ParseFile(sFilename: String): Boolean;
Var
  oNFO: TTagVideoNFO;
  oMediaInfo: TMediaInfo;
  sFile_nfo: String;
  oTag: TMetaTag;
  i: Integer;
Begin
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin
    If (FFmpegAvailable) Then
    Begin
      oMediaInfo := ffmpegSupport.MediaInfo(sFilename);

      If oMediaInfo.Filename = sFilename Then
      Begin
        Tag['MM'] := oMediaInfo.RAW;

        Tag['MM_File_Format'] := oMediaInfo.Format;
        Tag['MM_Streams'] := oMediaInfo.StreamCount;
        Tag['MM_Width'] := oMediaInfo.Width;
        Tag['MM_Height'] := oMediaInfo.Height;
        Tag['MM_Duration'] := oMediaInfo.Duration;

        If oMediaInfo.V_Stream <> -1 Then
        Begin
          Tag['MM_VID_Codec'] := oMediaInfo.V_Codec;
          Tag['MM_VID_Stream'] := oMediaInfo.V_Stream;
        End;

        If oMediaInfo.A_Stream <> -1 Then
        Begin
          Tag['MM_AUD_Codec'] := oMediaInfo.A_Codec;
          Tag['MM_AUD_Stream'] := oMediaInfo.A_Stream;
        End;

        If oMediaInfo.S_Stream <> -1 Then
        Begin
          Tag['MM_SUB_Codec'] := oMediaInfo.S_Codec;
          Tag['MM_SUB_Stream'] := oMediaInfo.S_Stream;
        End;

        FHasTags := True;
      End;
    End;

    sFile_nfo := ChangeFileExt(sFilename, '.nfo');

    If FileExists(sFile_nfo) Then
    Begin
      oNFO := TTagVideoNFO.Create;
      Try
        FHasNFO := True;
        oNFO.AddTags(self);

        oNFO.ParseFile(sFile_nfo);

        If oNFO.HasTags Then
        Begin
          For i := 0 To oNFO.Tags.Count - 1 Do
          Begin
            oTag := oNFO.Tags.Data[i];

            Tag[oTag.Name] := oTag.Value;
          End;
        End;
      Finally
        FreeAndNil(oNFO);
      End;
    End;
  End;
End;

Initialization
  InitializeFFmpeg;

  TagManager.Register(TTagVideo, ['.pkt', '.mpg', '.mp4', '.mkv', '.avi', '.wmv',
    '.asf', '.mov', '.flv', '.m4v']);

End.
