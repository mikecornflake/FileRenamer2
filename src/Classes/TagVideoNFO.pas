Unit TagVideoNFO;

{$mode objfpc}{$H+}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
Interface

Uses
  Classes, SysUtils, Tags, DB;

Type

  { TTagVideoNFO }

  TTagVideoNFO = Class(TMetaFileHandler)
  Private
    FRoot: String;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Name: String; Override;

    Function ParseFile(sFilename: String): Boolean; Override;
  End;

Implementation

Uses
  xmlread, DOM, FileSupport;

  { TTagVideoNFO }

Constructor TTagVideoNFO.Create;
Begin
  Inherited Create;

  AddTag('NFO_Tag', ftString, 25, True);

  AddTag('NFO_Show', ftString, 100, False);
  AddTag('NFO_Season', ftInteger, -1, False);
  AddTag('NFO_Episode', ftInteger, -1, False);
  AddTag('NFO_Title', ftString, 100, False);
  AddTag('NFO_Aired', ftDateTime, -1, False);

  AddTag('NFO_Genre', ftString, 255, False);
  AddTag('NFO_Actors', ftString, 255, False);
  AddTag('NFO_Sets', ftString, 255, False);

  AddTag('NFO_Added', ftDateTime, -1, False);

  AddTag('NFO_IDs', ftString, 100, False);

  AddTag('NFO_Plot', ftString, 4096, False);
End;

Destructor TTagVideoNFO.Destroy;
Begin
  Inherited Destroy;
End;

Function TTagVideoNFO.Name: String;
Begin
  If FRoot = '' Then
    Result := 'nfo.xml'
  Else
    Result := 'nfo.xml.' + FRoot;

End;

Function TTagVideoNFO.ParseFile(sFilename: String): Boolean;
Var
  oXML: TXMLDocument;
  sShow, sSeason, sEpisode, sTitle, sPlot, sAired, sAdded, sIDs, sActors, sGenre,
  sSets, sExt: String;

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

  Function NodesToStr(ANode: String; ANamedItem: String = ''; ASubNode: String = ''): String;
  Var
    oElements: TDOMNodeList;
    i: Integer;
    oElement, oNode: TDOMNode;
    sTemp: Unicodestring;
  Begin
    Result := '';

    oElements := oXML.GetElementsByTagName(ANode);
    For i := 0 To oElements.Count - 1 Do
    Begin
      oElement := oElements[i];
      sTemp := Trim(oElement.TextContent);

      If (ANamedItem <> '') Then
      Begin
        oNode := oElement.Attributes.GetNamedItem(ANamedItem);
        sTemp := Format('%s: %s', [oNode.TextContent, sTemp]);
      End
      Else
      If (ASubNode <> '') Then
      Begin
        oNode := oElement.FindNode(ASubNode);
        If Assigned(oNode) Then
          sTemp := oNode.TextContent
        Else
          sTemp := '';
      End;

      If sTemp <> '' Then
      Begin
        If Result = '' Then
          Result := sTemp
        Else
          Result := Result + ', ' + LineEnding + sTemp;
      End;
    End;
  End;

Begin
  sExt := Lowercase(ExtractFileExt(sFilename));
  If sExt <> '.nfo' Then
    sFilename := ChangeFileExt(sFilename, '.nfo');

  Result := Inherited ParseFile(sFilename);

  If FileExists(sFilename) Then
  Begin
    Try
      ReadXMLFile(oXML, sFilename);
    Except
      Result := False;
    End;

    If Assigned(oXML) Then
    Try
      FRoot := oXML.DocumentElement.NodeName;

      If FRoot = 'episodedetails' Then
        sAired := Value('aired')
      Else If FRoot = 'tvshow' Then
        sAired := Value('year') + '-01-01'
      Else If FRoot = 'movie' Then
        sAired := Value('premiered')
      Else If FRoot = 'musicvideo' Then
        sAired := Value('premiered')
      Else
      Begin
        // Break, we only want to read Video NFOs
        Result := False;
        Exit;
      End;

      sSeason := Value('season');
      sTitle := Value('title');
      sEpisode := Value('episode');
      sShow := Value('showtitle');
      sAdded := Value('dateadded');
      sPlot := Value('outline');
      If sPlot = '' Then
        sPlot := Value('plot');

      sGenre := NodesToStr('genre', '', '');
      sIDs := NodesToStr('uniqueid', 'type', '');
      sActors := NodesToStr('actor', '', 'name');
      sSets := NodesToStr('set', '', 'name');

      Tag['NFO_Tag'] := FRoot;

      If sShow <> '' Then
        Tag['NFO_Show'] := sShow
      Else
        Tag['NFO_Show'] := Copy(sSets, 1, 255);

      Tag['NFO_Season'] := StrToIntDef(sSeason, -1);
      Tag['NFO_Episode'] := StrToIntDef(sEpisode, -1);

      Tag['NFO_Title'] := sTitle;
      Tag['NFO_Aired'] := StrToDateDef(sAired);
      Tag['NFO_Added'] := StrToDateDef(sAdded);

      Tag['NFO_IDs'] := Copy(sIDs, 1, 100);
      Tag['NFO_Actors'] := Copy(sActors, 1, 255);
      Tag['NFO_Genre'] := Copy(sGenre, 1, 255);
      Tag['NFO_Sets'] := Copy(sSets, 1, 255);

      Tag['NFO_Plot'] := sPlot;

      Result := True;
      FHasTags := True;
    Finally
      oXML.Free;
    End;
  End;
End;

Initialization
  TagManager.Register(TTagVideoNFO, ['.nfo']);
  TagManager.Register(TTagVideoNFO, FileExtVideo);

End.
