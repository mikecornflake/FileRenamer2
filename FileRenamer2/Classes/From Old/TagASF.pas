Unit TagASF;

Interface

Uses
  AdvObjects, WMF9,
  TagSupport, Windows;

Type
  TTagASF = Class(TFileTag)
  Protected
    FEditor : IWMMetadataEditor;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;


Implementation


Uses
  SysUtils,
  StringSupport, MathSupport, DateSupport,
  ActiveX, DirectShowBase,
  Dialogs;

{ TID3V1Tag }

{----------------------------------------------------------------------------------------}
Constructor TTagASF.Create;
Begin { Create }
  Inherited;

  WMCreateEditor(FEditor);
End; { Create }

{----------------------------------------------------------------------------------------}
Destructor TTagASF.Destroy;
Begin { Destroy }
  FEditor := Nil;

  Inherited;
End; { Destroy }

{----------------------------------------------------------------------------------------}
Function TTagASF.Name: String;
Begin { Name }
  Result := 'ASF Metadata';
End; { Name }

{----------------------------------------------------------------------------------------}
Function TTagASF.ParseFile(sFilename: String): Boolean;
Var
  i: Integer;
  iAttribStream, iAttributeCount, iAttribLen: Word;
  iDataLen: Word;
  iDuration: Int64;
  iVal: Integer;
  iVal64 : Int64;
  oHeaderInfo3 : IWMHeaderInfo3;
  oType: TWMTAttrDataType;
  pAttribName: PWideChar;
  pBuffer: Array[0..255] Of WideChar;
  pValue: PBYTE;
  pwcTemp : PWideChar;
  sValue : String;

  {--------------------------------------------------------------------------------------}
  Procedure LoadTag(sAttribute : PWideChar; sTag : String);
  Begin { LoadTag }
    If CompareText(pAttribName, sAttribute)=0 Then
      Value[sTag] := sValue;
  End; { LoadTag }

  {--------------------------------------------------------------------------------------}
  Procedure LoadTagEx(sAttribute : PWideChar; sTag : String; bVisible : Boolean);
  Begin { LoadTagEx }
    If CompareText(pAttribName, sAttribute)=0 Then
      AddValue(sTag, sValue, Not bVisible, bVisible);
  End; { LoadTagEx }
Begin { ParseFile }
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) And Assigned(FEditor) Then
  Begin { If }
    pwcTemp := StringToWideChar(sFilename, pBuffer, SizeOf(pBuffer) Div 2);
    If SUCCEEDED(FEditor.Open(pwcTemp)) Then
    Begin { If }
      FHasTag := True;

      FEditor.QueryInterface( IID_IWMHeaderInfo3, oHeaderInfo3);

      Result := True;

      iAttribStream := 0;

      oHeaderInfo3.GetAttributeCount(iAttribStream, iAttributeCount);

      For i := 0 To iAttributeCount -1 Do
      Begin { For }
        oHeaderInfo3.GetAttributeByIndex(i, iAttribStream, Nil, iAttribLen, oType, Nil, iDataLen);

        pAttribName := AllocMem(iAttribLen *2);
        pValue := AllocMem(iDataLen);

        oHeaderInfo3.GetAttributeByIndex(i, iAttribStream, pAttribName, iAttribLen, oType, pValue, iDataLen);

        If oType = WMT_TYPE_STRING Then
        Begin { If }
          sValue := WideCharLenToString(PWideChar(pValue), iDataLen Div 2);

          LoadTag('Author',        '%Artist%');
          LoadTag('COPYRIGHT',     '%Copyright%');
          LoadTag('DESCRIPTION',   '%Comments%');
          LoadTag('Title',         '%Title%');
          LoadTag('WM/ALBUMTITLE', '%Album%');
          LoadTag('WM/COMPOSER',   '%Composer%');
          LoadTag('WM/ENCODEDBY',  '%Encoded By%');
          LoadTag('WM/GENRE',      '%Genre%');
          LoadTag('WM/LYRICS',     '%Lyrics%');
          LoadTag('WM/TRACKNUMBER','%Track%');
          LoadTag('WM/URL',        '%URL%');
          LoadTag('WM/YEAR',       '%Year%');
          LoadTagEx('WMFSDKVERSION', '%SDK Version%', False);
        End; { If }

        If oType = WMT_TYPE_DWORD Then
        Begin { If }
          iVal := PInteger(pValue)^;

          If UpperCase(pAttribName) = 'BITRATE' Then
            AddValue('%Bitrate%', ToString(iVal Div 1000)+'kbps', True, False);
        End; { If }

        If oType = WMT_TYPE_QWORD Then
        Begin { If }
          iVal64 := PLargeInteger(pValue)^;

          If UpperCase(pAttribName) = 'DURATION' Then
          Begin { If }
            iDuration := iVal64 Div 10000000;
            AddValue('%Duration%', Format('HH:mm:ss', SecondsToTime(iDuration)), True, False);
          End; { If }
        End; { If }

        FreeMem(pAttribName);
        FreeMem(pValue);
      End; { For }

      FEditor.Close;
    End; { ShowTags }
  End; { If }
End; { ParseFile }

{----------------------------------------------------------------------------------------}
Function TTagASF.Write: Boolean;
Var
  pwcTemp : PWideChar;
  Buffer: Array[0..255] Of WideChar;
  oHeaderInfo3 : IWMHeaderInfo3;

  {--------------------------------------------------------------------------------------}
  Function GetAttribIndex(Attrib: String) : Integer;
  Var
    AttribName: PWideChar;
    P, Lang: PWord;
    AttribLen: Word;

  Begin { GetAttribIndex }
    Result := -1;

    AttribName := StringToOleStr(Attrib);
    Lang := PWord(0);

    oHeaderInfo3.GetAttributeIndices($FFFF, AttribName, Lang, Nil, AttribLen);

    If AttribLen <> 0 Then
    Begin { If }
      P := AllocMem(AttribLen);

      Try { Try }
        oHeaderInfo3.GetAttributeIndices($FFFF, AttribName, Lang, P, AttribLen);
        Result := PWord(P)^;
      Finally
        FreeMem(P);
      End; { Try }
    End; { If }
  End; { GetAttribIndex }

  {--------------------------------------------------------------------------------------}
  Procedure SaveTag(AttribName : PWideChar; sTag : String);
  Var
    iIndex, iLength : Integer;
    pValue: PWideChar;
    pwIndex: Word;
    sValue : String;

  Begin { SaveTag }
    iIndex := GetAttribIndex(AttribName);

    sValue := Value[sTag];
    pValue := StringToOleStr(sValue);
    iLength := Length(pValue) * 2;

    If iIndex >= 0 Then
    Begin { If }
      If sValue <> '' Then
        oHeaderInfo3.ModifyAttribute(0, iIndex, WMT_TYPE_STRING, 0, PByte(pValue), iLength)
      Else
        oHeaderInfo3.DeleteAttribute(0, iIndex);
    End { If }
    Else If iLength <> 0 Then
      oHeaderInfo3.AddAttribute(0, AttribName, pwIndex, WMT_TYPE_STRING, 0, PByte(pValue), iLength);
  End; { SaveTag }

Begin { Write }
  Result := Inherited Write;

  If FileExists(FFilename) And Assigned(FEditor) Then
  Begin { If }
    pwcTemp := StringToWideChar(FFilename, Buffer, SizeOf(Buffer) Div 2);
    If SUCCEEDED(FEditor.Open(pwcTemp)) Then
    Begin { If }
      FEditor.QueryInterface( IID_IWMHeaderInfo3, oHeaderInfo3);

      Result := True;

      SaveTag('Author',        '%Artist%');
      SaveTag('Copyright',     '%Copyright%');
      SaveTag('Description',   '%Comments%');
      SaveTag('Title',         '%Title%');
      SaveTag('WM/ALBUMTITLE', '%Album%');
      SaveTag('WM/COMPOSER',   '%Composer%');
      SaveTag('WM/ENCODEDBY',  '%Encoded By%');
      SaveTag('WM/GENRE',      '%Genre%');
      SaveTag('WM/LYRICS',     '%Lyrics%');
      SaveTag('WM/TRACKNUMBER','%Track%');
      SaveTag('WM/URL',        '%URL%');
      SaveTag('WM/YEAR',       '%Year%');

      FEditor.Flush;
      FEditor.Close;
    End; { If }
  End; { If }
End; { Write }

Initialization
  TagManager.Register(TTagASF, ['.wma', '.asf', '.wmv']);

End.
