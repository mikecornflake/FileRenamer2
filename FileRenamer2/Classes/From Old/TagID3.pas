Unit TagID3;

Interface

Uses
  AdvObjects,
  TagSupport;

Type
  TTagID3V1 = Class(TFileTag)
  Public
    Function Writeable : Boolean; Override;
    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;

  TTagID3V2 = Class(TFileTag)
  Public
    Function Writeable : Boolean; Override;
    Function Name : String; Override;

    Function ParseFile(sFilename : String) : Boolean; Override;
    Function Write : Boolean; Override;
  End;


Implementation

Uses
  SysUtils,
  StringSupport, MathSupport,
  ID3v2;

{ TID3V1Tag }

{----------------------------------------------------------------------------------------}
Function TTagID3V1.Name: String;
Begin { Name }
  Result := 'ID3 v1';
End; { Name }

{----------------------------------------------------------------------------------------}
Function TTagID3V1.ParseFile(sFilename: String): Boolean;
Var
  f : File;
  Buf : Array[1..128] Of Char;
  iLen : Integer;
  iGenre : Integer;

Const
  arGenre : Array[0..146] Of String = ('Blues', 'Classic Rock', 'Country', 'Dance', 'Disco', 'Funk',
              'Grunge', 'Hip Hop', 'Jazz', 'Metal', 'New Age', 'Oldies', 'Other', 'Pop', 'R&B',
              'Rap', 'Reggae', 'Rock', 'Techno', 'Industrial', 'Alternative', 'Ska', 'Death Metal',
              'Pranks', 'Soundtrack', 'Euro - Techno', 'Ambient', 'Trip Hop', 'Vocal', 'Jazz - Funk',
              'Fusion', 'Trance', 'Classical', 'Instrumental', 'Acid', 'House', 'Game', 'Sound Clip',
              'Gospel', 'Noise', 'Alt.Rock', 'Bass', 'Soul', 'Punk', 'Space', 'Meditative', 'Instrumental Pop',
              'Instrumental Rock', 'Ethnic', 'Gothic', 'Darkwave', 'Techno - Industrial', 'Electronic',
              'Pop / Folk', 'Eurodance', 'Dream', 'Southern Rock', 'Comedy', 'Cult', 'Gangsta Rap',
              'Top 40', 'Christian Rap', 'Pop / Funk', 'Jungle', 'Native American', 'Cabaret', 'New Wave',
              'Psychedelic', 'Rave', 'Showtunes', 'Trailer', 'Lo - fi', 'Tribal', 'Acid Punk', 'Acid Jazz',
              'Polka', 'Retro', 'Musical', 'Rock ''n''Roll', 'Hard Rock', 'Folk', 'Folk / Rock',
              'National Folk', 'Swing', 'Fast Fusion', 'Bebob', 'Latin', 'Revival', 'Celtic', 'Blue Grass',
              'Avant Garde', 'Gothic Rock', 'Progressive Rock', 'Psychedelic Rock', 'Symphonic Rock',
              'Slow Rock', 'Big Band', 'Chorus', 'Easy Listening', 'Acoustic', 'Humour', 'Speech',
              'Chanson', 'Opera', 'Chamber Music', 'Sonata', 'Symphony', 'Booty Bass', 'Primus',
              'Pr0n Groove', 'Satire', 'Slow Jam', 'Club', 'Tango', 'Samba', 'Folklore', 'Ballad',
              'Power Ballad', 'Rhythmic Soul', 'Freestyle', 'Duet', 'Punk Rock', 'Drum Solo', 'Euro - House',
              'Dance Hall', 'Goa', 'Drum & Bass', 'Club - House', 'Hardcore', 'Terror', 'Indie', 'Brit Pop',
              'Negerpunk', 'Polsk Punk', 'Beat', 'Christian Gangsta Rap', 'Heavy Metal', 'Black Metal',
              'Crossover', 'Contemporary Christian', 'Christian Rock', 'Merengue', 'Salsa', 'Thrash Metal',
              'Anime', 'JPop', 'Synth Pop');


Begin { ParseFile }
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin { If }
    AssignFile(f, sFilename);
    Try { Try }
      Reset(f, 1);
      iLen := System.FileSize(f);

      If iLen>128 Then
      Begin { If }
        Seek(f, iLen-128);                                  
        BlockRead(f, Buf, 128);

        If (Copy(Buf, 1, 3)='TAG') Then
        Begin { If }
          Value['%Title%'] := Copy(Buf, 4,  30);
          Value['%Artist%'] := Copy(Buf, 34, 30);
          Value['%Album%'] := Copy(Buf, 64, 30);
          Value['%Year%'] := Copy(Buf, 94, 4 );
          Value['%Comments%'] := Copy(Buf, 98, 30);

          iGenre := Byte(Buf[128]);
          If (iGenre>=0) And (iGenre<=146) Then
            Value['%Genre%'] := arGenre[iGenre]
          Else
            Value['%Genre%'] := 'Unknown';

          FHasTag := True;
        End; { If }
      End; { If }
    Finally
      CloseFile(f);
      Result := True;
    End; { Try }
  End; { If }
End; { ParseFile }

{----------------------------------------------------------------------------------------}
Function TTagID3V1.Writeable: Boolean;
Begin { Writeable }
  Result := False;
End; { Writeable }

{----------------------------------------------------------------------------------------}
Function TTagID3V1.Write: Boolean;
Var
  f    : File;
  Buf  : Array[1..128] Of Char;
  iLen : Integer;

  {--------------------------------------------------------------------------------------}
  Procedure MoveString(sValue : String; iIndex, iLen : Integer);
  Var
    i : Integer;
  Begin { MoveString }
    For i := 1 To Min(iLen, Length(sValue)) Do
      Buf[iIndex+i-1] := sValue[i];
  End; { MoveString }

Begin { Write }
  Result := Inherited Write;

  If FileExists(FFilename) Then
  Begin { If }
    FillChar(Buf, Sizeof(Buf), #0);

    AssignFile(f, FFilename);
    Try { Try }
      Reset(f, 1);
      iLen := System.FileSize(f);

      If FHasTag Then
      Begin { If }
        Seek(f, iLen-128);
      End { If }
      Else
      Begin { Else }
        Seek(f, iLen);
      End; { Else }

      MoveString('TAG', 1, 3);
      MoveString(Value['%Title%']  , 4  , 30);
      MoveString(Value['%Artist%'] , 34 , 30);
      MoveString(Value['%Album%']  , 64 , 30);
      MoveString(Value['%Year%']   , 94 , 4 );
      MoveString(Value['%Comments%'], 98 , 30);
      MoveString(Value['%Genre%']  , 128, 1 );

      BlockWrite(f, Buf, 128);
    Finally
      CloseFile(f);
      Result := True;
    End; { Try }
  End; { If }
End; { Write }

{ TTagID3V2 }

{----------------------------------------------------------------------------------------}
Function TTagID3V2.Name: String;
Begin { Name }
  Result := 'ID3 v2';
End; { Name }

{----------------------------------------------------------------------------------------}
Function TTagID3V2.ParseFile(sFilename: String): Boolean;
Var
  oTag: Tid3v2Tag;
  oComm : Comm;

  {--------------------------------------------------------------------------------------}
  Procedure LocalGetTag(sTag, sID3vsTag : String);
  Var
    sTemp : String;
  Begin { LocalGetTag }
    oTag.GetAsciiText(sID3vsTag, sTemp);
    Value[sTag] := Trim(sTemp);
  End; { LocalGetTag }

Begin { ParseFile }
  Result := Inherited ParseFile(sFilename);

  oTag := Tid3v2tag.create;
  Try { Try }
    If (oTag.LoadFromFile(sFilename, 0)<=255) Then
    Begin { If }
      Result := True;
      FHasTag := True;

      LocalGetTag('%Track%',        'TRCK');
      LocalGetTag('%Title%',        'TIT2');
      LocalGetTag('%Artist%',       'TPE1');
      LocalGetTag('%Album%',        'TALB');
      LocalGetTag('%Year%',         'TYER');
      LocalGetTag('%Genre%',        'TCON');
      LocalGetTag('%Composer%',     'TCOM');
      LocalGetTag('%Copyright%',    'TCOP');
      LocalGetTag('%Content%',      'TIT1');
      LocalGetTag('%OriginalYear%', 'TORY');
      LocalGetTag('%Band%',         'TPE2');

      oTag.getCOMM(oCOMM, '');
      Value['%Comments%'] := oCOMM.body;
    End; { If }
  Finally
    oTag.Free;
  End; { Try }
End; { ParseFile }

{----------------------------------------------------------------------------------------}
Function TTagID3V2.Writeable: Boolean;
Begin { Writeable }
  Result := False;
End; { Writeable }

{----------------------------------------------------------------------------------------}
Function TTagID3V2.Write: Boolean;
Var
  oTag: Tid3v2Tag;
  oComm : Comm;

  {--------------------------------------------------------------------------------------}
  Procedure LocalSetTag(sTag, sID3vsTag : String);
  Begin { LocalSetTag }
    If Value[sTag]<>BLANK Then
      oTag.SetAsciiText(sID3vsTag, Value[sTag]);
  End; { LocalSetTag }
Begin { Write }
  Result := Inherited Write;

  If FileExists(FFilename) Then
  Begin { If }
    oTag := Tid3v2tag.create;
    Try { Try }
      LocalSetTag('%Title%',        'TIT2');
      LocalSetTag('%Track%',        'TRCK');
      LocalSetTag('%Artist%',       'TPE1');
      LocalSetTag('%Album%',        'TALB');
      LocalSetTag('%Year%',         'TYER');
      LocalSetTag('%Genre%',        'TCON');
      LocalSetTag('%Composer%',     'TCOM');
      LocalSetTag('%Copyright%',    'TCOP');
      LocalSetTag('%Content%',      'TIT1');
      LocalSetTag('%OriginalYear%', 'TORY');
      LocalSetTag('%Band%',         'TPE2');

      oCOMM.Encoding := etASCII;
      oCOMM.Language := 'ENG';
      oCOMM.Description := '';
      oCOMM.Body := Value['%Comments%'];
      oTag.SetCOMM(oCOMM, oCOMM.description);

      Result := (oTag.SaveToFile(FFilename)<=255);
    Finally
      oTag.Free;
    End; { Try }
  End; { If }
End; { Write }

Initialization
  TagManager.Register(TTagID3V2, ['.mp3']);
  TagManager.Register(TTagID3V1, ['.mp3']);

End.


