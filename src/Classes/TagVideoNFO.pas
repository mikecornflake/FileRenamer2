Unit TagVideoNFO;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Tags, DB;

Type
  TTagVideoNFO = Class(TFileTagger)
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name: String; Override;

    Function ParseFile(sFilename: String): Boolean; Override;
  End;

Implementation

Uses
  xmlread, DOM;

  { TTagVideo }

Constructor TTagVideoNFO.Create;
Begin
  Inherited Create;

  AddTag('NFO_Tag', ftString, 25, True);

  AddTag('NFO_Show', ftString, 100, True);
  AddTag('NFO_Season', ftInteger, -1, True);
  AddTag('NFO_Episode', ftInteger, -1, True);
  AddTag('NFO_Title', ftString, 100, True);
  AddTag('NFO_Aired', ftDateTime, -1, True);
  AddTag('NFO_Added', ftDateTime, -1, True);

  AddTag('NFO_tmdb', ftString, 10, True);
  AddTag('NFO_imdb', ftString, 10, True);
  AddTag('NFO_tvdb', ftString, 10, True);

  AddTag('NFO_Plot', ftString, 4096, True);
End;

Destructor TTagVideoNFO.Destroy;
Begin
  Inherited Destroy;
End;

Function TTagVideoNFO.Name: String;
Begin
  Result := 'Video NFO';
End;

Function TTagVideoNFO.ParseFile(sFilename: String): Boolean;
Var
  oXML: TXMLDocument;
  sShow, sSeason, sEpisode, sTitle, sPlot, tmdbID, sAired, sAdded: String;
  oUniqueIDs: TDOMNodeList;
  oID, oNode: TDOMNode;
  i: Integer;
  imdbID, tvdbID, sNode: DOMString;

  Function Value(Anode: String): String;
  Var
    oNode: TDOMNode;
  Begin
    Result := '';
    oNode := oXML.DocumentElement.FindNode(Anode);
    If Assigned(oNode) Then
      Result := oNode.TextContent;
  End;

  Function StrToDateDef(ADate: String): TDateTime;
  Var
    iY, iM, iD, iH, iMin, iSec: Longint;
    sIn: String;
  Begin
    Result := -1;

    sIn := Trim(ADate);

    // 'yyyy-mm-dd HH:mm:ss'
    //  1234567890123456789
    If Length(sIn) >= 10 Then
    Begin
      iY := StrToIntDef(Copy(sIn, 1, 4), 0);
      iM := StrToIntDef(Copy(sIn, 6, 2), 0);
      iD := StrToIntDef(Copy(sIn, 9, 2), 0);

      Result := EncodeDate(iY, iM, iD);
    End;

    If Length(sIn) >= 19 Then
    Begin
      iH := StrToIntDef(Copy(sIn, 12, 2), 0);
      iMin := StrToIntDef(Copy(sIn, 15, 2), 0);
      iSec := StrToIntDef(Copy(sIn, 18, 2), 0);

      Result := Result + EncodeTime(iH, iMin, iSec, 0);
    End;
  End;

Begin
  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin
    ReadXMLFile(oXML, sFilename);
    If Assigned(oXML) Then
    Try
      sShow := Value('showtitle');

      sNode := oXML.DocumentElement.NodeName;

      if sNode='episodedetails' then
      Begin
        sSeason := Value('season');
        sTitle := Value('title');
        sEpisode := Value('episode');

        sAired := Value('aired');
      end
      Else if sNode='tvshow' then
      Begin
        sAired := Value('year')+'-01-01';
      End
      Else if sNode='movie' then
      Begin
        sTitle := Value('title');
        sAired := Value('premiered');
      end;

      sAdded := Value('dateadded');
      sPlot := Value('plot');

      oUniqueIDs := oXML.GetElementsByTagName('uniqueid');
      For i := 0 To oUniqueIDs.Count - 1 Do
      Begin
        oID := oUniqueIDs[i];

        oNode := oID.Attributes.GetNamedItem('type');

        Case oNode.TextContent Of
          'tmdb': tmdbID := oID.TextContent;
          'imdb': imdbID := oID.TextContent;
          'tvdb': tvdbID := oID.TextContent;
        End;
      End;

      Tag['NFO_Tag'] := sNode;

      Tag['NFO_Show'] := sShow;
      Tag['NFO_Season'] := StrToIntDef(sSeason, -1);
      Tag['NFO_Episode'] := StrToIntDef(sEpisode, -1);

      Tag['NFO_Title'] := sTitle;
      Tag['NFO_Aired'] := StrToDateDef(sAired);
      Tag['NFO_Added'] := StrToDateDef(sAdded);

      Tag['NFO_tmdb'] := tmdbID;
      Tag['NFO_imdb'] := imdbID;
      Tag['NFO_tvdb'] := tvdbID;

      Tag['NFO_Plot'] := sPlot;

      FHasTags := True;
    Finally
      oXML.Free;
    End;
  End;
End;

Initialization
  TagManager.Register(TTagVideoNFO, ['.nfo']);

End.
