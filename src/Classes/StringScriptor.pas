Unit StringScriptor;

Interface

Function Interpret(sString: String): String;
Function ProcessOperator(sString: String): String;
Function CorrectRomanNumerals(sInput: String): String;
Function UpperFirst(Const sValue: String): String;
Function FindNext(sInput: String; setChars: Array Of Char; iStartPos: Integer = 0;
  bLookinString: Boolean = False): Integer;
Function Trim(oVar: Variant): String;

Const
  Tokens: Array [0..13] Of String =
    ('PADLEFT(', 'POS(', 'COPY(', 'RIGHT(', 'LEFT(', 'TOSTRING(', 'TOINTEGER(',
    'UPPER(', 'LOWER(', 'UPPERFIRST(', 'LENGTH(', 'TEXTBETWEEN(', 'MONTH(', 'TRIM(');

  Operators: Array[0..1] Of String = ('+', '-');

  RomanNumerals: Array[0..29] Of String =
    ('I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X', 'XI',
    'XII', 'XIII', 'XIV', 'XV', 'XVI', 'XVII', 'XVIII', 'XIX', 'XX', 'XXI',
    'XXII', 'XXIII', 'XXIV', 'XXV', 'XXVI', 'XXVII', 'XXVIII', 'XXIX', 'XXX');

  cQuote = '''';

Implementation

Uses
  StringSupport, SysUtils;

Function Trim(oVar: Variant): String;
Begin
  Result := StringSupport.Trim(VarToStr(oVar));
End;

Function CorrectRomanNumerals(sInput: String): String;
Var
  sNum: String;
  iNum: Integer;
  iNumLen: Integer;
  iPos: Integer;
  iLen: Integer;

Const
  setEndChars = [' ', '(', ')', '_', '-'];

Begin
  Result := sInput;

  // Despite all String replacements, Length won't change
  iLen := Length(Result);
  iNum := Low(RomanNumerals);

  // Cycle over each of the Roman Numerals to be tested.  Means the entire string is sampled 30 times!!
  While (iNum <= High(RomanNumerals)) Do
  Begin
    sNum := RomanNumerals[iNum];
    iNumLen := Length(sNum);
    iPos := 1;

    While (iPos > 0) And (iPos < iLen) Do
    Begin
      // Are the start and end conditions satisfied?
      If SameText(Copy(Result, iPos, iNumLen), sNum) Then
        If ((iPos = 1) Or (Result[iPos - 1] In setEndChars)) And
          ((iPos + iNumLen >= iLen) Or (Result[iPos + iNumLen] In setEndChars)) Then
          Result := Copy(Result, 1, iPos - 1) + sNum + Copy(Result, iPos + iNumLen, iLen);

      Inc(iPos);
    End;
    Inc(iNum);
  End;
End;

Function UpperFirst(Const sValue: String): String;
Var
  iLoop: Integer;
  sTemp: String;
  cLast: String;

  Function EndChar(cChar: Char): Boolean;
  Begin
    Result := ((cLast = ' ') And (cChar = cQuote)) Or
      (cChar In [' ', '(', ')', '_', '.', ',', '-', '[', ']']);
  End;

Begin
  sTemp := Sentence(Lower(sValue));
  cLast := ' ';

  For iLoop := 1 To Length(sTemp) Do
  Begin
    If iLoop > 1 Then
      cLast := sTemp[iLoop - 1];
    If EndChar(sTemp[iLoop]) And (iLoop < Length(sTemp)) Then
      sTemp[iLoop + 1] := Upper(sTemp[iLoop + 1]);
  End;

  sTemp := CorrectRomanNumerals(sTemp);
  Result := sTemp;
End;

Function ExtractField(Const sValue: String; cSeparator: Char; iIndex: Integer): String;
Var
  pValue, pMark: PChar;
  iCount: Integer;
  bInQuote: Boolean;
Begin
  pValue := PChar(sValue);
  pMark := pValue;
  iCount := 0;
  binQuote := False;

  While (pValue^ <> #0) Do
  Begin
    If (pValue^ = cQuote) Then
      bInQuote := Not bInQuote
    Else If (pValue^ = cSeparator) And Not bInQuote Then
    Begin
      Inc(iCount);
      If (iCount = iIndex) Then
      Begin
        pMark := pValue + 1;
        While (pMark^ = ' ') Do
          Inc(pMark);
      End
      Else If (iCount > iIndex) Then
      Begin
        pValue^ := #0;
        Result := pMark;
        pValue^ := cSeparator;
        Exit;
      End;
    End;

    Inc(pValue);
  End;
  If pMark = PChar(sValue) Then
    Result := ''
  Else
    Result := pMark;
End;

Function GetString(sValue: String): String;
Var
  iLen: Integer;
Begin
  iLen := Length(sValue);
  If (iLen >= 2) And (sValue[1] = cQuote) And (sValue[iLen] = cQuote) Then
    Result := Copy(sValue, 2, iLen - 2)
  Else
    Result := sValue;
End;

Function GetParam(sParams: String; iIndex: Integer): String;
Begin
  Result := ExtractField(sParams, ',', iIndex);
  Result := ProcessOperator(Result);
  Result := GetString(Result);
End;

Function FindNext(sInput: String; setChars: Array Of Char; iStartPos: Integer = 0;
  bLookinString: Boolean = False): Integer;
Var
  iPosition: Integer;
  bInQuotes: Boolean;
  iLen: Integer;
  cTemp: Char;
  i: Integer;
  iMaxChars: Integer;
Begin
  iPosition := iStartPos + 1;
  Result := -1;
  bInQuotes := False;
  iLen := Length(sInput);
  iMaxChars := High(setChars);

  While (iPosition <= iLen) And (Result = -1) Do
  Begin
    If Not (bLookinString) And (sInput[iPosition] = cQuote) Then
      bInQuotes := Not bInQuotes;

    If (Not bInQuotes) Then
    Begin
      cTemp := sInput[iPosition];
      i := Low(setChars);

      While (i <= iMaxChars) And (Result = -1) Do
      Begin
        If cTemp = setChars[i] Then
          Result := iPosition;
        Inc(i);
      End;
    End;
    Inc(iPosition);
  End;
  If (Result > iLen) Then
    Result := iLen;
End;

Function FindMatchingBrace(sInput: String): Integer;
Var
  iBraceCount, iPosition: Integer;
  bFirstBrace: Boolean;
  bInQuotes: Boolean;
Begin
  iPosition := 1;
  Result := -1;
  iBraceCount := 0;
  bFirstBrace := False;
  bInQuotes := False;

  While (iPosition <= Length(sInput)) And (Result = -1) Do
  Begin
    If (sInput[iPosition] = cQuote) Then
      bInQuotes := Not bInQuotes;

    If Not bInQuotes Then
      If (sInput[iPosition] = '(') Then
      Begin
        bFirstBrace := True;
        Inc(iBraceCount);
      End
      Else If (sInput[iPosition] = ')') And bFirstBrace Then
      Begin
        Dec(iBraceCount);
        If iBraceCount = 0 Then
          Result := iPosition;
      End;

    Inc(iPosition);
  End;
End;

Function FindNextQuote(sInput: String; iFromPos: Integer): Integer;
Begin
  Result := FindNext(sInput, [cQuote], iFromPos, True);
End;

Function HasToken(sString: String; Var iToken, iStart, iEnd: Integer;
  Var sParams: String): Boolean;
Var
  sInput: String;
  i: Integer;
  iEndBrace: Integer;
Begin
  iStart := -1;
  iEnd := -1;
  sInput := Upper(sString);
  Result := False;
  sParams := '';

  iToken := Low(Tokens);
  While (iToken <= High(Tokens)) And Not Result Do
  Begin
    iStart := Pos(Tokens[iToken], sInput);
    If (iStart > 0) Then
    Begin
      sInput := Copy(sString, iStart, Length(sInput));
      iEndBrace := FindMatchingBrace(sInput);
      If (iEndBrace <> -1) Then
      Begin
        i := Pos('(', sInput);
        iEnd := iStart + iEndBrace;
        sParams := Copy(sInput, i + 1, iEndBrace - i - 1);
        Result := True;
      End;
    End;
    Inc(iToken);
  End;

  If (iToken > High(Tokens) + 1) Then
    iToken := -1
  Else
    Dec(iToken);
End;

Function HasOperator(sString: String; Var iOperator: Integer;
  Var sLeft, sRight, sRemainder: String): Boolean;
Var
  sInput: String;
  i, iLen, iTemp: Integer;
  sOp: String;
  iLenOp: Integer;
Begin
  sLeft := '';
  sRight := '';
  sRemainder := '';
  sInput := Upper(sString);
  Result := False;
  iLen := Length(sInput);
  iOperator := Low(Operators);

  While (iOperator <= High(Operators)) And Not Result Do
  Begin
    sOp := Operators[iOperator];
    iLenOp := Length(sOp);
    i := 1;

    While (i < iLen) And Not Result Do
    Begin
      If sString[i] = cQuote Then
      Begin
        iTemp := FindNextQuote(sString, i);
        If iTemp <> -1 Then
          i := iTemp;
      End;

      If (i < iLen) Then
        If (SameText(Copy(sString, i, iLenOp), sOp)) Then
        Begin
          Result := True;

          // Everything to the left
          sLeft := Trim(Copy(sString, 1, i - 1));

          // Everything to the right, or to the first operator
          iTemp := FindNext(sString, ['+', '-', '(', ')'], i + iLenOp - 1);
          If iTemp = -1 Then
          Begin
            sRight := Trim(Copy(sString, i + iLenOp, iLen));
            sRemainder := '';
          End
          Else
          Begin
            sRight := Trim(Copy(sString, i + iLenOp, iTemp - i - iLenOp));
            sRemainder := Copy(sString, iTemp, iLen);
          End;
          sRight := Trim(sRight);
        End;
      Inc(i);
    End;

    Inc(iOperator);
  End;

  If (iOperator > High(Operators) + 1) Then
    iOperator := -1
  Else
    Dec(iOperator);
End;

Function IsNumber(sValue: String): Boolean;
Begin
  Result := ToInteger(sValue, MaxInt) <> MaxInt;
End;

Function ProcessOperator(sString: String): String;
Var
  sLeft, sRight, sRemainder: String;
  iOperator: Integer;

  Function DoAdd(sLeft, sRight: String): String;
  Begin
    If IsNumber(sLeft) And IsNumber(sRight) Then
      Result := ToString(ToInteger(sLeft) + ToInteger(sRight))
    Else
      Result := cQuote + GetString(sLeft) + GetString(sRight) + cQuote;
  End;

  Function DoSubtract(sLeft, sRight: String): String;
  Begin
    If IsNumber(sLeft) And IsNumber(sRight) Then
      Result := ToString(ToInteger(sLeft) - ToInteger(sRight))
    Else
      Result := '0';
  End;

Begin
  Result := sString;

  While HasOperator(Result, iOperator, sLeft, sRight, sRemainder) Do
    Case iOperator Of
      0: Result := DoAdd(sLeft, sRight) + sRemainder;
      1: Result := DoSubtract(sLeft, sRight) + sRemainder;
    End;
End;

Function DoInterpret(sString: String): String;

  Function DoTextBetween(sParams: String): String;
  Var
    sInput, sLeft, sRight: String;
    iLeft, iRight, iStart, iLen: Integer;
  Begin
    sInput := GetParam(sParams, 0);
    sLeft := GetParam(sParams, 1);
    sRight := GetParam(sParams, 2);
    iLen := Length(sInput);
    If (sLeft <> '') And (sLeft <> cQuote + cQuote) Then
    Begin
      iLeft := Pos(sLeft, sInput) + 1;
      If iLeft = 1 Then
        iLeft := iLen + 1;
    End
    Else
      iLeft := 0;
    If (sRight <> '') And (sRight <> cQuote + cQuote) Then
      iRight := Pos(sRight, sInput)
    Else
      iRight := iLen + 1;
    If iRight > iLeft Then
    Begin
      If iLeft <> 0 Then
        iStart := iLeft + Length(sLeft) - 1
      Else
        iStart := 1;
      Result := cQuote + Copy(sInput, iStart, iRight - iStart) + cQuote;
    End
    Else
      Result := cQuote + cQuote;
  End;

  Function DoMonth(sParams: String): String;
  Var
    iMonth: Integer;
  Begin
    iMonth := ToInteger(GetString(sParams), -1);

    Case iMonth Of
      1: Result := 'Jan';
      2: Result := 'Feb';
      3: Result := 'Mar';
      4: Result := 'Apr';
      5: Result := 'May';
      6: Result := 'Jun';
      7: Result := 'Jul';
      8: Result := 'Aug';
      9: Result := 'Sep';
      10: Result := 'Oct';
      11: Result := 'Nov';
      12: Result := 'Dec';
      Else
        Result := sParams
    End;
  End;

  Function DoLeft(sParams: String): String;
  Var
    iCount: Integer;
    sInput: String;
  Begin
    sInput := GetParam(sParams, 0);
    iCount := ToInteger(GetParam(sParams, 1), -1);
    If (iCount > 0) Then
      Result := cQuote + Copy(sInput, 1, iCount) + cQuote
    Else
      Result := cQuote + cQuote;
  End;

  Function DoRight(sParams: String): String;
  Var
    iCount: Integer;
    sInput: String;
  Begin
    sInput := GetParam(sParams, 0);
    iCount := ToInteger(GetParam(sParams, 1), -1);
    If (iCount > 0) Then
      Result := cQuote + Copy(sInput, Length(sInput) - iCount + 1, Length(sInput)) + cQuote
    Else
      Result := cQuote + cQuote;
  End;

  Function DoCopy(sParams: String): String;
  Var
    iStart, iLen: Integer;
    sInput: String;
  Begin
    sInput := GetParam(sParams, 0);
    iStart := ToInteger(GetParam(sParams, 1), -1);
    iLen := ToInteger(GetParam(sParams, 2), -1);
    If (iStart > 0) And (iLen > 0) Then
      Result := cQuote + Copy(sInput, iStart, iLen) + cQuote
    Else
      Result := cQuote + cQuote;
  End;

  Function DoPadleft(sParams: String): String;
  Var
    sValue, sChar: String;
    ilen: Integer;
  Begin
    sValue := GetParam(sParams, 0);
    sChar := GetParam(sParams, 1);
    iLen := ToInteger(GetParam(sParams, 2), -1);
    If Length(sChar) > 0 Then
      Result := cQuote + PadLeft(sValue, sChar[1], iLen) + cQuote
    Else
      Result := cQuote + cQuote;
  End;

  Function DoPos(sParams: String): String;
  Var
    sInput: String;
    sSubString: String;
  Begin
    sInput := GetParam(sParams, 0);
    sSubString := GetParam(sParams, 1);
    If (sInput <> '') And (sSubString <> '') Then
      Result := ToString(Pos(sSubString, sInput))
    Else
      Result := '0';
  End;

Var
  sParams: String;
  iToken: Integer;
  iStart, iEnd: Integer;
  sTemp: String;
Begin
  Result := sString;

  While HasToken(Result, iToken, iStart, iEnd, sParams) Do
  Begin
    sParams := DoInterpret(sParams);
    Case iToken Of
      0: sTemp := DoPadLeft(sParams);
      1: sTemp := DoPos(sParams);
      2: sTemp := DoCopy(sParams);
      3: sTemp := DoRight(sParams);
      4: sTemp := DoLeft(sParams);
      5: sTemp := cQuote + GetString(sParams) + cQuote;   // ToString(
      6: sTemp := GetString(sParams);                     // ToInteger(
      7: sTemp := Upper(sParams);                         // Upper(
      8: sTemp := Lower(sParams);                         // Lower(
      9: sTemp := UpperFirst(sParams);                    // UpperFirst(
      10: sTemp := ToString(Length(GetString(sParams)));  // Length(
      11: sTemp := DoTextBetween(sParams);
      12: sTemp := DoMonth(sParams);
      13: sTemp := cQuote + Trim(GetString(sParams)) + cQuote; // Trim(
      Else
        sTemp := '';
    End;

    sTemp := ProcessOperator(sTemp);
    Result := Copy(Result, 1, iStart - 1) + sTemp + Copy(Result, iEnd, Length(Result));
  End;

  Result := ProcessOperator(Result);
End;

Function Interpret(sString: String): String;
Begin
  Result := GetString(Trim(DoInterpret(sString)));
  Result := StringReplace(Result, cQuote + cQuote, cQuote, [rfReplaceAll, rfIgnoreCase]);
End;

End.
