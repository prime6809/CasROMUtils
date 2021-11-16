unit DragonCoCoBasicTokens;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

CONST   BasicFn   = $FF;  { Function codes are preceeded by a byte of $FF }

        BasCoCo   = 'C';  { Tandy Colour Computer Basic }
        BasDragon = 'D';  { Dragon Basic }
        BasMicro  = 'M';  { Tandy Mico Colour Computer / Alice Basic }

        TokInvalid      = $FE;  {Invalid token for destination}
        
TYPE    TToken  = class(TObject)
                  Token         : BYTE;
                  TokenType     : BYTE;
                  constructor Create;
                  constructor MakeToken(Tok     : BYTE;
                                        Last    : BYTE);
                  FUNCTION Clone : TToken;
                  FUNCTION Retokenize(SrcDialect : CHAR;                  { Source Dialect of basic }
                                      DstDialect : CHAR) : TToken;        { Destination Dialect }
                  PROCEDURE DragonToCoCo;
                  PROCEDURE MC10ToCoCo;
                  PROCEDURE CoCoToDragon;
                  PROCEDURE CoCoToMC10;
                  FUNCTION AsString : STRING;
                PRIVATE
                  FromDialect   : CHAR;
                  ToDialect     : CHAR;
                END;
                
FUNCTION TokenToText(Token      : BYTE;                 { Token to translate }
                     Dialect    : CHAR;                 { Dialect of basic }
                     Last       : BYTE) : STRING;       { Last char }

FUNCTION CommandTokenToText(Token      : BYTE) : STRING;
FUNCTION FunctionTokenToText(Token      : BYTE) : STRING;
FUNCTION MC10TokenToText(Token      : BYTE) : STRING;



Function DragonToCoCoCommand(Cmd	: BYTE) : BYTE;
Function DragonToCoCoFunction(Cmd	: BYTE) : BYTE;

Function CoCoToDragonCommand(Cmd	: BYTE) : BYTE;
Function CoCoToDragonFunction(Cmd	: BYTE) : BYTE;

FUNCTION DoMC10ToCoCo(Token       : TToken) : TToken;
FUNCTION DoCoCoToMC10(Token       : TToken) : TToken;

implementation

constructor TToken.Create;

BEGIN;
  INHERITED Create;
  Token:=$00;
  TokenType:=$00;
  FromDialect:=BasCoCo;
  ToDialect:=BasCoCo;
END;

constructor TToken.MakeToken(Tok     : BYTE;
                             Last    : BYTE);

BEGIN;
  Create;
  Token:=Tok;
  TokenType:=Last;
END;

FUNCTION TToken.Clone : TToken;

BEGIN;
  Result:=TToken.Create;
  Result.Token:=Self.Token;
  Result.TokenType:=Self.TokenType;
  Result.FromDialect:=Self.FromDialect;
  Result.ToDialect:=Self.ToDialect;
END;

FUNCTION CommandTokenToText(Token      : BYTE)  : STRING;

BEGIN;
  CASE Token OF
    $80 : Result:='FOR ';
    $81 : Result:='GO ';
    $82 : Result:='REM ';
    $83 : Result:=''' ';
    $84 : Result:='ELSE ';
    $85 : Result:='IF ';
    $86 : Result:='DATA ';
    $87 : Result:='PRINT ';
    $88 : Result:='ON ';
    $89 : Result:='INPUT ';
    $8a : Result:='END ';
    $8b : Result:='NEXT ';
    $8c : Result:='DIM ';
    $8d : Result:='READ ';
    $8e : Result:='RUN ';
    $8f : Result:='RESTORE ';
    $90 : Result:='RETURN ';
    $91 : Result:='STOP ';
    $92 : Result:='POKE ';
    $93 : Result:='CONT ';
    $94 : Result:='LIST ';
    $95 : Result:='CLEAR ';
    $96 : Result:='NEW ';
    $97 : Result:='CLOAD ';
    $98 : Result:='CSAVE ';
    $99 : Result:='OPEN ';
    $9a : Result:='CLOSE ';
    $9b : Result:='LLIST ';
    $9c : Result:='SET ';
    $9d : Result:='RESET ';
    $9e : Result:='CLS ';
    $9f : Result:='MOTOR ';
    $a0 : Result:='SOUND ';
    $a1 : Result:='AUDIO ';
    $a2 : Result:='EXEC ';
    $a3 : Result:='SKIPF ';
    $a4 : Result:='TAB ';
    $a5 : Result:='TO ';
    $a6 : Result:='SUB ';
    $a7 : Result:='THEN ';
    $a8 : Result:='NOT ';
    $a9 : Result:='STEP ';
    $aa : Result:='OFF ';
    $ab : Result:='+';
    $ac : Result:='-';
    $ad : Result:='*';
    $ae : Result:='/';
    $af : Result:='^ ';
    $b0 : Result:='AND ';
    $b1 : Result:='OR ';
    $b2 : Result:='>';
    $b3 : Result:='=';
    $b4 : Result:='<';
    $b5 : Result:='DEL ';
    $b6 : Result:='EDIT ';
    $b7 : Result:='TRON ';
    $b8 : Result:='TROFF ';
    $b9 : Result:='DEF ';
    $ba : Result:='LET ';
    $bb : Result:='LINE ';
    $bc : Result:='PCLS ';
    $bd : Result:='PSET ';
    $be : Result:='PRESET ';
    $bf : Result:='SCREEN ';
    $c0 : Result:='PCLEAR ';
    $c1 : Result:='COLOR ';
    $c2 : Result:='CIRCLE ';
    $c3 : Result:='PAINT ';
    $c4 : Result:='GET ';
    $c5 : Result:='PUT ';
    $c6 : Result:='DRAW ';
    $c7 : Result:='PCOPY ';
    $c8 : Result:='PMODE ';
    $c9 : Result:='PLAY ';
    $ca : Result:='DLOAD ';
    $cb : Result:='RENUM ';
    $cc : Result:='FN ';
    $cd : Result:='USING ';
  ELSE
    Result:='';
  END;
END;

FUNCTION FunctionTokenToText(Token      : BYTE) : STRING;
BEGIN;
  CASE Token OF
    $80 : Result:='SGN';
    $81 : Result:='INT';
    $82 : Result:='ABS';
    $83 : Result:='USR';
    $84 : Result:='RND';
    $85 : Result:='SIN';
    $86 : Result:='PEEK';
    $87 : Result:='LEN';
    $88 : Result:='STR$';
    $89 : Result:='VAL';
    $8a : Result:='ASC';
    $8b : Result:='CHR$';
    $8c : Result:='EOF';
    $8d : Result:='JOYSTK';
    $8e : Result:='LEFT$';
    $8f : Result:='RIGHT$';
    $90 : Result:='MID$';
    $91 : Result:='POINT';
    $92 : Result:='INKEY$';
    $93 : Result:='MEM';
    $94 : Result:='ATN';
    $95 : Result:='COS';
    $96 : Result:='TAN';
    $97 : Result:='EXP';
    $98 : Result:='FIX';
    $99 : Result:='LOG';
    $9a : Result:='POS';
    $9b : Result:='SQR';
    $9c : Result:='HEX$';
    $9d : Result:='VARPTR';
    $9e : Result:='INSTR';
    $9f : Result:='TIMER';
    $a0 : Result:='PPOINT';
    $a1 : Result:='STRING$';
  ELSE
    Result:='';
  END;
END;

FUNCTION MC10TokenToText(Token      : BYTE) : STRING;

BEGIN;
  CASE Token OF
    $80 : Result:='FOR ';
    $81 : Result:='GOTO ';
    $82 : Result:='GOSUB ';
    $83 : Result:='REM ';
    $84	: Result:='IF ';
    $85	: Result:='DATA ';
    $86	: Result:='PRINT ';
    $87	: Result:='ON ';
    $88	: Result:='INPUT ';
    $89	: Result:='END ';
    $8A	: Result:='NEXT ';
    $8B	: Result:='DIM ';
    $8C	: Result:='READ ';
    $8D	: Result:='LET ';
    $8E	: Result:='RUN ';
    $8F	: Result:='RESTORE ';
    $90	: Result:='RETURN ';
    $91	: Result:='STOP ';
    $92	: Result:='POKE ';
    $93	: Result:='CONT ';
    $94	: Result:='LIST ';
    $95	: Result:='CLEAR ';
    $96	: Result:='NEW ';
    $97	: Result:='CLOAD ';
    $98	: Result:='CSAVE ';
    $99	: Result:='LLIST ';
    $9A	: Result:='LPRINT ';
    $9B	: Result:='SET ';
    $9C	: Result:='RESET ';
    $9D	: Result:='CLS ';
    $9E	: Result:='SOUND ';
    $9F	: Result:='EXEC ';
    $A0	: Result:='SKIPF ';
    $A1	: Result:='TAB( ';
    $A2	: Result:='TO ';
    $A3	: Result:='THEN ';
    $A4	: Result:='NOT ';
    $A5	: Result:='STEP ';
    $A6	: Result:='OFF ';
    $A7	: Result:='+';
    $A8	: Result:='-';
    $A9	: Result:='*';
    $AA	: Result:='/';
    $AB	: Result:='^';
    $AC	: Result:='AND ';
    $AD	: Result:='OR ';
    $AE	: Result:='>';
    $AF	: Result:='=';
    $B0	: Result:='<';
    $B1	: Result:='SGN';
    $B2	: Result:='INT';
    $B3	: Result:='ABS';
    $B4	: Result:='USR';
    $B5	: Result:='RND';
    $B6	: Result:='SQR';
    $B7	: Result:='LOG';
    $B8	: Result:='EXP';
    $B9	: Result:='SIN';
    $BA	: Result:='COS';
    $BB	: Result:='TAN';
    $BC	: Result:='PEEK';
    $BD	: Result:='LEN';
    $BE	: Result:='STR$';
    $BF	: Result:='VAL';
    $C0	: Result:='ASC';
    $C1	: Result:='CHR$';
    $C2	: Result:='LEFT$';
    $C3	: Result:='RIGHT$';
    $C4	: Result:='MID$';
    $C5	: Result:='POINT';
    $C6	: Result:='VARPTR';
    $C7	: Result:='INKEY$';
    $C8	: Result:='MEM';
  ELSE
    Result:='';
  END;
END;

FUNCTION TokenToText(Token      : BYTE;         { Token to translate }
                     Dialect    : CHAR;         { Dialect of basic }
                     Last       : BYTE) : STRING;        { Last char }

BEGIN;
  IF (Last=BasicFn) THEN
  BEGIN;
    CASE Dialect OF
      BasCoCo           : Result:=FunctionTokenToText(Token);
      BasDragon         : Result:=FunctionTokenToText(DragonToCoCoFunction(Token));
      BasMicro          : Result:='';
    END;
  END
  ELSE
  BEGIN
    CASE Dialect OF
      BasCoCo           : Result:=CommandTokenToText(Token);
      BasDragon         : Result:=CommandTokenToText(DragonToCoCoCommand(Token));
      BasMicro          : Result:=MC10TokenToText(Token);
    END;
  END
END;


Function DragonToCoCoCommand(Cmd	: BYTE) : BYTE;

BEGIN;
  CASE Cmd OF
    $80 : Result:=$80; { FOR }
    $81 : Result:=$81; { GO }
    $82 : Result:=$82; { REM }
    $83 : Result:=$83; { ' }
    $84 : Result:=$84; { ELSE }
    $85 : Result:=$85; { IF }
    $86 : Result:=$86; { DATA }
    $87 : Result:=$87; { PRINT }
    $88 : Result:=$88; { ON }
    $89 : Result:=$89; { INPUT }
    $8a : Result:=$8a; { END }
    $8b : Result:=$8b; { NEXT }
    $8c : Result:=$8c; { DIM }
    $8d : Result:=$8d; { READ }
    $8e : Result:=$ba; { LET }
    $8f : Result:=$8e; { RUN }
    $90 : Result:=$8f; { RESTORE }
    $91 : Result:=$90; { RETURN }
    $92 : Result:=$91; { STOP }
    $93 : Result:=$92; { POKE }
    $94 : Result:=$93; { CONT }
    $95 : Result:=$94; { LIST }
    $96 : Result:=$95; { CLEAR }
    $97 : Result:=$96; { NEW }
    $98 : Result:=$b9; { DEF }
    $99 : Result:=$97; { CLOAD }
    $9a : Result:=$98; { CSAVE }
    $9b : Result:=$99; { OPEN }
    $9c : Result:=$9a; { CLOSE }
    $9d : Result:=$9b; { LLIST }
    $9e : Result:=$9c; { SET }
    $9f : Result:=$9d; { RESET }
    $a0 : Result:=$9e; { CLS }
    $a1 : Result:=$9f; { MOTOR }
    $a2 : Result:=$a0; { SOUND }
    $a3 : Result:=$a1; { AUDIO }
    $a4 : Result:=$a2; { EXEC }
    $a5 : Result:=$a3; { SKIPF }
    $a6 : Result:=$b5; { DEL }
    $a7 : Result:=$b6; { EDIT }
    $a8 : Result:=$b7; { TRON }
    $a9 : Result:=$b8; { TROFF }
    $aa : Result:=$bb; { LINE }
    $ab : Result:=$bc; { PCLS }
    $ac : Result:=$bd; { PSET }
    $ad : Result:=$be; { PRESET }
    $ae : Result:=$bf; { SCREEN }
    $af : Result:=$c0; { PCLEAR }
    $b0 : Result:=$c1; { COLOR }
    $b1 : Result:=$c2; { CIRCLE }
    $b2 : Result:=$c3; { PAINT }
    $b3 : Result:=$c4; { GET }
    $b4 : Result:=$c5; { PUT }
    $b5 : Result:=$c6; { DRAW }
    $b6 : Result:=$c7; { PCOPY }
    $b7 : Result:=$c8; { PMODE }
    $b8 : Result:=$c9; { PLAY }
    $b9 : Result:=$ca; { DLOAD }
    $ba : Result:=$cb; { RENUM }
    $bb : Result:=$a4; { TAB }
    $bc : Result:=$a5; { TO }
    $bd : Result:=$a6; { SUB }
    $be : Result:=$cc; { FN }
    $bf : Result:=$a7; { THEN }
    $c0 : Result:=$a8; { NOT }
    $c1 : Result:=$a9; { STEP }
    $c2 : Result:=$aa; { OFF }
    $c3 : Result:=$ab; { + }
    $c4 : Result:=$ac; { - }
    $c5 : Result:=$ad; { * }
    $c6 : Result:=$ae; { / }
    $c7 : Result:=$af; { ^ }
    $c8 : Result:=$b0; { AND }
    $c9 : Result:=$b1; { OR }
    $ca : Result:=$b2; { > }
    $cb : Result:=$b3; { = }
    $cc : Result:=$b4; { < }
    $cd : Result:=$cd; { USING }
  ELSE
    Result:=Cmd;
  END;
END;

Function DragonToCoCoFunction(Cmd	: BYTE) : BYTE;

BEGIN;
  CASE Cmd OF
    $80 : Result:=$80; { SGN }
    $81 : Result:=$81; { INT }
    $82 : Result:=$82; { ABS }
    $83 : Result:=$9a; { POS }
    $84 : Result:=$84; { RND }
    $85 : Result:=$9b; { SQR }
    $86 : Result:=$99; { LOG }
    $87 : Result:=$97; { EXP }
    $88 : Result:=$85; { SIN }
    $89 : Result:=$95; { COS }
    $8a : Result:=$96; { TAN }
    $8b : Result:=$94; { ATN }
    $8c : Result:=$86; { PEEK }
    $8d : Result:=$87; { LEN }
    $8e : Result:=$88; { STR$ }
    $8f : Result:=$89; { VAL }
    $90 : Result:=$8a; { ASC }
    $91 : Result:=$8b; { CHR$ }
    $92 : Result:=$8c; { EOF }
    $93 : Result:=$8d; { JOYSTK }
    $94 : Result:=$98; { FIX }
    $95 : Result:=$9c; { HEX$ }
    $96 : Result:=$8e; { LEFT$ }
    $97 : Result:=$8f; { RIGHT$ }
    $98 : Result:=$90; { MID$ }
    $99 : Result:=$91; { POINT }
    $9a : Result:=$92; { INKEY$ }
    $9b : Result:=$93; { MEM }
    $9c : Result:=$9d; { VARPTR }
    $9d : Result:=$9e; { INSTR }
    $9e : Result:=$9f; { TIMER }
    $9f : Result:=$a0; { PPOINT }
    $a0 : Result:=$a1; { STRING$ }
    $a1 : Result:=$83; { USR }
  ELSE
    Result:=Cmd;
  END;
END;

Function CoCoToDragonCommand(Cmd	: BYTE) : BYTE;

BEGIN;
  CASE Cmd OF
    $80 : Result:=$80; { FOR }
    $81 : Result:=$81; { GO }
    $82 : Result:=$82; { REM }
    $83 : Result:=$83; { ' }
    $84 : Result:=$84; { ELSE }
    $85 : Result:=$85; { IF }
    $86 : Result:=$86; { DATA }
    $87 : Result:=$87; { PRINT }
    $88 : Result:=$88; { ON }
    $89 : Result:=$89; { INPUT }
    $8a : Result:=$8a; { END }
    $8b : Result:=$8b; { NEXT }
    $8c : Result:=$8c; { DIM }
    $8d : Result:=$8d; { READ }
    $8e : Result:=$8f; { RUN }
    $8f : Result:=$90; { RESTORE }
    $90 : Result:=$91; { RETURN }
    $91 : Result:=$92; { STOP }
    $92 : Result:=$93; { POKE }
    $93 : Result:=$94; { CONT }
    $94 : Result:=$95; { LIST }
    $95 : Result:=$96; { CLEAR }
    $96 : Result:=$97; { NEW }
    $97 : Result:=$99; { CLOAD }
    $98 : Result:=$9a; { CSAVE }
    $99 : Result:=$9b; { OPEN }
    $9a : Result:=$9c; { CLOSE }
    $9b : Result:=$9d; { LLIST }
    $9c : Result:=$9e; { SET }
    $9d : Result:=$9f; { RESET }
    $9e : Result:=$a0; { CLS }
    $9f : Result:=$a1; { MOTOR }
    $a0 : Result:=$a2; { SOUND }
    $a1 : Result:=$a3; { AUDIO }
    $a2 : Result:=$a4; { EXEC }
    $a3 : Result:=$a5; { SKIPF }
    $a4 : Result:=$bb; { TAB }
    $a5 : Result:=$bc; { TO }
    $a6 : Result:=$bd; { SUB }
    $a7 : Result:=$bf; { THEN }
    $a8 : Result:=$c0; { NOT }
    $a9 : Result:=$c1; { STEP }
    $aa : Result:=$c2; { OFF }
    $ab : Result:=$c3; { + }
    $ac : Result:=$c4; { - }
    $ad : Result:=$c5; { * }
    $ae : Result:=$c6; { / }
    $af : Result:=$c7; { ^ }
    $b0 : Result:=$c8; { AND }
    $b1 : Result:=$c9; { OR }
    $b2 : Result:=$ca; { > }
    $b3 : Result:=$cb; { = }
    $b4 : Result:=$cc; { < }
    $b5 : Result:=$a6; { DEL }
    $b6 : Result:=$a7; { EDIT }
    $b7 : Result:=$a8; { TRON }
    $b8 : Result:=$a9; { TROFF }
    $b9 : Result:=$98; { DEF }
    $ba : Result:=$8e; { LET }
    $bb : Result:=$aa; { LINE }
    $bc : Result:=$ab; { PCLS }
    $bd : Result:=$ac; { PSET }
    $be : Result:=$ad; { PRESET }
    $bf : Result:=$ae; { SCREEN }
    $c0 : Result:=$af; { PCLEAR }
    $c1 : Result:=$b0; { COLOR }
    $c2 : Result:=$b1; { CIRCLE }
    $c3 : Result:=$b2; { PAINT }
    $c4 : Result:=$b3; { GET }
    $c5 : Result:=$b4; { PUT }
    $c6 : Result:=$b5; { DRAW }
    $c7 : Result:=$b6; { PCOPY }
    $c8 : Result:=$b7; { PMODE }
    $c9 : Result:=$b8; { PLAY }
    $ca : Result:=$b9; { DLOAD }
    $cb : Result:=$ba; { RENUM }
    $cc : Result:=$be; { FN }
    $cd : Result:=$cd; { USING }
  ELSE
    Result:=Cmd;
  END;
END;


Function CoCoToDragonFunction(Cmd	: BYTE) : BYTE;

BEGIN;
  CASE Cmd OF
    $80 : Result:=$80; { SGN }
    $81 : Result:=$81; { INT }
    $82 : Result:=$82; { ABS }
    $83 : Result:=$a1; { USR }
    $84 : Result:=$84; { RND }
    $85 : Result:=$88; { SIN }
    $86 : Result:=$8c; { PEEK }
    $87 : Result:=$8d; { LEN }
    $88 : Result:=$8e; { STR$ }
    $89 : Result:=$8f; { VAL }
    $8a : Result:=$90; { ASC }
    $8b : Result:=$91; { CHR$ }
    $8c : Result:=$92; { EOF }
    $8d : Result:=$93; { JOYSTK }
    $8e : Result:=$96; { LEFT$ }
    $8f : Result:=$97; { RIGHT$ }
    $90 : Result:=$98; { MID$ }
    $91 : Result:=$99; { POINT }
    $92 : Result:=$9a; { INKEY$ }
    $93 : Result:=$9b; { MEM }
    $94 : Result:=$8b; { ATN }
    $95 : Result:=$89; { COS }
    $96 : Result:=$8a; { TAN }
    $97 : Result:=$87; { EXP }
    $98 : Result:=$94; { FIX }
    $99 : Result:=$86; { LOG }
    $9a : Result:=$83; { POS }
    $9b : Result:=$85; { SQR }
    $9c : Result:=$95; { HEX$ }
    $9d : Result:=$9c; { VARPTR }
    $9e : Result:=$9d; { INSTR }
    $9f : Result:=$9e; { TIMER }
    $a0 : Result:=$9f; { PPOINT }
    $a1 : Result:=$a0; { STRING$ }
  ELSE
    Result:=Cmd;
  END;
END;

FUNCTION DoMC10ToCoCo(Token       : TToken) : TToken;

BEGIN;
  Result:=TToken.Create;
  
  IF ((Token.Token>=$B1) AND (Token.Token<=$C8)) THEN
    Token.TokenType:=BasicFn
  ELSE
    Token.TokenType:=$00;
    
  CASE Token.Token OF
    $80 : Result.Token:=$80; {FOR }
    $81 : BEGIN;
            Result.Token:=$81; {GOTO }
            Result.TokenType:=$A5;
          END;
    $82 : BEGIN;
            Result.Token:=$81; {GOSUB }
            Result.TokenType:=$A6;
          END;
    $83 : Result.Token:=$82; {REM }
    $84	: Result.Token:=$85; {IF }
    $85	: Result.Token:=$86; {DATA }
    $86	: Result.Token:=$87; {PRINT }
    $87	: Result.Token:=$88; {ON }
    $88	: Result.Token:=$89; {INPUT }
    $89	: Result.Token:=$8A; {END }
    $8A	: Result.Token:=$8B; {NEXT }
    $8B	: Result.Token:=$8C; {DIM }
    $8C	: Result.Token:=$8D; {READ }
    $8D	: Result.Token:=$BA; {LET }
    $8E	: Result.Token:=$8E; {RUN }
    $8F	: Result.Token:=$8F; {RESTORE }
    $90	: Result.Token:=$90; {RETURN }
    $91	: Result.Token:=$91; {STOP }
    $92	: Result.Token:=$92; {POKE }
    $93	: Result.Token:=$93; {CONT }
    $94	: Result.Token:=$94; {LIST }
    $95	: Result.Token:=$95; {CLEAR }
    $96	: Result.Token:=$96; {NEW }
    $97	: Result.Token:=$97; {CLOAD }
    $98	: Result.Token:=$98; {CSAVE }
    $99	: Result.Token:=$9B; {LLIST }
    $9A	: Result.Token:=$87; {LPRINT, MAPPED TO PRINT }
    $9B	: Result.Token:=$9C; {SET }
    $9C	: Result.Token:=$9D; {RESET }
    $9D	: Result.Token:=$9E; {CLS }
    $9E	: Result.Token:=$A0; {SOUND }
    $9F	: Result.Token:=$A2; {EXEC }
    $A0	: Result.Token:=$A3; {SKIPF }
    $A1	: Result.Token:=$A4; {TAB( }
    $A2	: Result.Token:=$A5; {TO }
    $A3	: Result.Token:=$A7; {THEN }
    $A4	: Result.Token:=$A8; {NOT }
    $A5	: Result.Token:=$A9; {STEP }
    $A6	: Result.Token:=$AA; {OFF }
    $A7	: Result.Token:=$AB; {+ }
    $A8	: Result.Token:=$AC; {- }
    $A9	: Result.Token:=$AD; {* }
    $AA	: Result.Token:=$AE; {/ }
    $AB	: Result.Token:=$AF; {^ }
    $AC	: Result.Token:=$B0; {AND }
    $AD	: Result.Token:=$B1; {OR }
    $AE	: Result.Token:=$B2; {> }
    $AF	: Result.Token:=$B3; {= }
    $B0	: Result.Token:=$B4; {< }
    $B1	: Result.Token:=$80; {SGN }
    $B2	: Result.Token:=$81; {INT }
    $B3	: Result.Token:=$82; {ABS }
    $B4	: Result.Token:=$83; {USR }
    $B5	: Result.Token:=$84; {RND }
    $B6	: Result.Token:=$9B; {SQR }
    $B7	: Result.Token:=$99; {LOG }
    $B8	: Result.Token:=$97; {EXP }
    $B9	: Result.Token:=$85; {SIN }
    $BA	: Result.Token:=$95; {COS }
    $BB	: Result.Token:=$96; {TAN }
    $BC	: Result.Token:=$86; {PEEK }
    $BD	: Result.Token:=$87; {LEN }
    $BE	: Result.Token:=$88; {STR$ }
    $BF	: Result.Token:=$89; {VAL }
    $C0	: Result.Token:=$8A; {ASC }
    $C1	: Result.Token:=$8B; {CHR$ }
    $C2	: Result.Token:=$8E; {LEFT$ }
    $C3	: Result.Token:=$8F; {RIGHT$ }
    $C4	: Result.Token:=$90; {MID$ }
    $C5	: Result.Token:=$91; {POINT }
    $C6	: Result.Token:=$9D; {VARPTR }
    $C7	: Result.Token:=$92; {INKEY$ }
    $C8	: Result.Token:=$93; {MEM }
  ELSE
    Result.Token:=TokInvalid;
  END;
END;

FUNCTION DoCoCoToMC10(Token       : TToken) : TToken;

BEGIN;
  Result:=TToken.Create;
  Result.Token:=TokInvalid;                     { Asume it will be invalid }

  IF (Token.TokenType=BasicFn) THEN
  BEGIN;
    CASE Token.Token OF
      $80 : Result.Token:=$B1;	{SGN}
      $81 : Result.Token:=$B2;	{INT}
      $82 : Result.Token:=$B3;	{ABS}
      $83 : Result.Token:=$B4;	{USR}
      $84 : Result.Token:=$B5;	{RND}
      $85 : Result.Token:=$B9;	{SIN}
      $86 : Result.Token:=$BC;	{PEEK}
      $87 : Result.Token:=$BD;	{LEN}
      $88 : Result.Token:=$BE;	{STR$}
      $89 : Result.Token:=$BF;	{VAL}
      $8a : Result.Token:=$C0;	{ASC}
      $8b : Result.Token:=$C1;	{CHR$}
      $8e : Result.Token:=$C2;	{LEFT$}
      $8f : Result.Token:=$C3;	{RIGHT$}
      $90 : Result.Token:=$C4;	{MID$}
      $91 : Result.Token:=$C5;	{POINT}
      $92 : Result.Token:=$C7;	{INKEY$}
      $93 : Result.Token:=$C8;	{MEM}
      $95 : Result.Token:=$BA;	{COS}
      $96 : Result.Token:=$BB;	{TAN}
      $97 : Result.Token:=$B8;	{EXP}
      $99 : Result.Token:=$B7;	{LOG}
      $9b : Result.Token:=$B6;	{SQR}
      $9d : Result.Token:=$C6;	{VARPTR}
    END;
  END
  ELSE
  BEGIN;
    CASE Token.Token OF
      $80	: Result.Token:=$80;	{FOR}

      {In CoCo/Dragon basic, GO, TO AND SUB are 3 seperate tokens, so therefore}
      {GOTO is token $81 GO followed by $A5 TO, in MC-10 basic GOTO and GOSUB}
      {are seperate tokens, we therefore encode GO as a space, and then check}
      {the last character for GO when we encounter a TO or SUB}
      $81 : Result.Token:=$00;	{GOTO/SUB}
      $82 : Result.Token:=$83;	{REM}
      $85 : Result.Token:=$84;	{IF}
      $86 : Result.Token:=$85;	{DATA}
      $87 : Result.Token:=$86;	{PRINT}
      $88 : Result.Token:=$87;	{ON}
      $89 : Result.Token:=$88;	{INPUT}
      $8a : Result.Token:=$89;	{END}
      $8b : Result.Token:=$8A;	{NEXT}
      $8c : Result.Token:=$8B;	{DIM}
      $8d : Result.Token:=$8C;	{READ}
      $8e : Result.Token:=$8E;	{RUN}
      $8f : Result.Token:=$8F;	{RESTORE}
      $90 : Result.Token:=$90;	{RETURN}
      $91 : Result.Token:=$91;	{STOP}
      $92 : Result.Token:=$92;	{POKE}
      $93 : Result.Token:=$93;	{CONT}
      $94 : Result.Token:=$94;	{LIST}
      $95 : Result.Token:=$95;	{CLEAR}
      $96 : Result.Token:=$96;	{NEW}
      $97 : Result.Token:=$97;	{CLOAD}
      $98 : Result.Token:=$98;	{CSAVE}
      $9b : Result.Token:=$99;	{LLIST}
      $9c : Result.Token:=$9B;	{SET}
      $9d : Result.Token:=$9C;	{RESET}
      $9e : Result.Token:=$9D;	{CLS}
      $a0 : Result.Token:=$9E;	{SOUND}
      $a2 : Result.Token:=$9F;	{EXEC}
      $a3 : Result.Token:=$A0;	{SKIPF}
      $a4 : Result.Token:=$A1;	{TAB(}
      $a5 : Result.Token:=$A2;	{TO}
      $a7 : Result.Token:=$A3;	{THEN}
      $a8 : Result.Token:=$A4;	{NOT}
      $a9 : Result.Token:=$A5;	{STEP}
      $aa : Result.Token:=$A6;	{OFF}
      $ab : Result.Token:=$A7;	{+}
      $ac : Result.Token:=$A8;	{-}
      $ad : Result.Token:=$A9;	{*}
      $ae : Result.Token:=$AA;	{/}
      $af : Result.Token:=$AB;	{^}
      $b0 : Result.Token:=$AC;	{AND}
      $b1 : Result.Token:=$AD;	{OR}
      $b2 : Result.Token:=$AE;	{>}
      $b3 : Result.Token:=$AF;	{=}
      $b4 : Result.Token:=$B0;	{<}
      $ba : Result.Token:=$8D;	{LET}
      $a6 : Result.Token:=$82;  {GOSUB}
      $FF : Result.Token:=$20;  {Function id becomes space}
    END;
  END;
  {GO TO detect}
  IF ((Token.Token=$A5) AND (Token.TokenType=$81)) THEN
    Result.Token:=$81;
END;

PROCEDURE TToken.DragonToCoCo;

BEGIN;
  IF (TokenType=BasicFn) THEN
    Token:=DragonToCoCoFunction(Token)
  ELSE
    Token:=DragonToCoCoCommand(Token);
END;

PROCEDURE TToken.MC10ToCoCo;

VAR     Temp    : TToken;

BEGIN;
  IF (Token>=$80) THEN
  BEGIN;
    Temp:=DoMC10ToCoCo(Self);
    Token:=Temp.Token;
    TokenType:=Temp.TokenType;
    Temp.Free;
  END;
END;

PROCEDURE TToken.CoCoToDragon;

BEGIN;
  IF (TokenType=BasicFn) THEN
    Token:=CoCoToDragonFunction(Token)
  ELSE
  BEGIN;
    Token:=CoCoToDragonCommand(Token);
    
    {We have to check for this incase we where translating from MC-10 basic}
    {As for GO TO/SUB, Token=GO, TokenType=TO or SUB}
    IF (TokenType<>0) THEN
      TokenType:=CoCoToDragonCommand(TokenType);
  END;
END;

PROCEDURE TToken.CoCoToMC10;

VAR     Temp    : TToken;

BEGIN;
  Temp:=DoCoCoToMC10(Self);
  Token:=Temp.Token;
  TokenType:=Temp.TokenType;
  Temp.Free;
END;

FUNCTION TToken.Retokenize(SrcDialect : CHAR;                  { Source Dialect of basic }
                           DstDialect : CHAR) : TToken;        { Destination Dialect }


BEGIN;
  FromDialect:=SrcDialect;                      { Set dialect vars }
  ToDialect:=DstDialect;
  Result:=Self.Clone;                           { Assume we will do nothing ! }

  {Only process if dialects are differnt, and it's a valid token, we ignore}
  IF ((SrcDialect<>DstDialect) AND (Token>=$80)) THEN
  BEGIN;
    {If this is a Dragon token, first map it to a coco one}
    IF (SrcDialect=BasDragon) THEN
      Result.DragonToCoCo;
      
    IF (SrcDialect=BasMicro) THEN
      Result.MC10toCoCo;
      
    CASE DstDialect OF
      BasDragon : Result.CoCoToDragon;
      BasMicro  : Result.CoCoToMC10;
    END;
  END;
END;

FUNCTION TToken.AsString : STRING;

BEGIN;
  IF ((FromDialect=BasMicro) AND (TokenType IN [$A5,$A6,$BC,$BD])) THEN
  BEGIN;
   {GO TO/SUB from MC-10 to CoCo OR Dragon}
    IF (((ToDialect=BasCoCo) AND (TokenType IN [$A5,$A6])) OR
        ((ToDialect=BasDragon) AND (TokenType IN [$BC,$BD]))) THEN
      Result:=CHR(Token)+CHR(TokenType);
  END
  ELSE
  BEGIN;
    IF ((ToDialect IN [BasCoCo,BasDragon]) AND (TokenType=BasicFn)) THEN
      Result:=CHR(TokenType)+CHR(Token)     { CoCo/Dragon function }
    ELSE
      Result:=CHR(Token);                   { CoCo/Dragon/MC-10 Command }
  END;
END;

end.

