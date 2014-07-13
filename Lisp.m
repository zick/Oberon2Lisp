MODULE Lisp;
IMPORT
  Conv, In, Out, Strings;

TYPE
  LObjDesc = RECORD END;
  LObj = POINTER TO LObjDesc;

  NilDesc = RECORD (LObjDesc) END;
  NumDesc = RECORD (LObjDesc) data: INTEGER END;
  SymDesc = RECORD (LObjDesc) data: ARRAY 256 OF CHAR END;
  ErrorDesc = RECORD (LObjDesc) data: ARRAY 256 OF CHAR END;
  ConsDesc = RECORD (LObjDesc) car, cdr: LObj END;
  Nil = POINTER TO NilDesc;
  Num = POINTER TO NumDesc;
  Sym = POINTER TO SymDesc;
  Error = POINTER TO ErrorDesc;
  Cons = POINTER TO ConsDesc;

VAR
  kLPar: CHAR;
  kRPar: CHAR;
  kQuote: CHAR;
  kNil: LObj;
  symTable: ARRAY 65536 OF Sym;
  symTableSize: INTEGER;

PROCEDURE MakeNum(n: INTEGER): LObj;
VAR
  num: Num;
BEGIN
  NEW(num);
  num.data := n;
  RETURN num
END MakeNum;

PROCEDURE MakeSym(s: ARRAY OF CHAR): LObj;
VAR
  i: INTEGER;
  sym: Sym;
BEGIN
  i := 0;
  LOOP
    IF i >= symTableSize THEN
      EXIT;
    END;
    IF s = symTable[i].data THEN
      RETURN symTable[i]
    END;
    i := i + 1
  END;
  NEW(sym);
  Strings.Append(s, sym.data);
  symTable[symTableSize] := sym;
  symTableSize := symTableSize + 1;
  RETURN sym
END MakeSym;

PROCEDURE MakeError(s: ARRAY OF CHAR): LObj;
VAR
  err: Error;
BEGIN
  NEW(err);
  Strings.Append(s, err.data);
  RETURN err
END MakeError;

PROCEDURE MakeCons(a, d: LObj): LObj;
VAR
  cons: Cons;
BEGIN
  NEW(cons);
  cons.car := a;
  cons.cdr := d;
  RETURN cons
END MakeCons;

PROCEDURE SafeCar(obj: LObj): LObj;
BEGIN
  WITH
    obj: Cons DO
      RETURN obj.car
  ELSE RETURN kNil
  END
END SafeCar;

PROCEDURE SafeCdr(obj: LObj): LObj;
BEGIN
  WITH
    obj: Cons DO
      RETURN obj.cdr
  ELSE RETURN kNil
  END
END SafeCdr;

PROCEDURE IsSpace(c: CHAR): BOOLEAN;
BEGIN
  IF (c = CHR(9)) OR (c = CHR(10)) OR (c = CHR(13)) OR (c = CHR(32)) THEN
    RETURN TRUE
  END;
  RETURN FALSE
END IsSpace;

PROCEDURE IsDelimiter(c: CHAR): BOOLEAN;
BEGIN
  IF (c = kLPar) OR (c = kRPar) OR (c = kQuote) OR (IsSpace(c)) THEN
    RETURN TRUE
  END;
  RETURN FALSE
END IsDelimiter;

PROCEDURE SkipSpaces(str: ARRAY OF CHAR; i: INTEGER): INTEGER;
BEGIN
  WHILE i < Strings.Length(str) DO
    IF ~IsSpace(str[i]) THEN
      RETURN i
    END;
    i := i + 1
  END;
  RETURN i
END SkipSpaces;

PROCEDURE MakeNumOrSym(str: ARRAY OF CHAR): LObj;
VAR
  i: INTEGER;
BEGIN
  IF (Strings.Length(str) = 1) & (str[0] = "0") THEN
    RETURN MakeNum(0)
  END;
  i := Conv.IntVal(str);
  IF i = 0 THEN
    RETURN MakeSym(str)
  END;
  RETURN MakeNum(i)
END MakeNumOrSym;

PROCEDURE ReadAtom(str: ARRAY OF CHAR; i: INTEGER; VAR obj: LObj): INTEGER;
VAR
  j: INTEGER;
  atom: ARRAY 256 OF CHAR;
BEGIN
  j := i;
  LOOP
    IF (j >= Strings.Length(str)) OR (IsDelimiter(str[j])) THEN
      EXIT;
    END;
    j := j + 1
  END;
  Strings.Extract(str, i, j - i, atom);
  obj := MakeNumOrSym(atom);
  RETURN j
END ReadAtom;

PROCEDURE Read(str: ARRAY OF CHAR; i: INTEGER; VAR obj: LObj): INTEGER;
BEGIN
  i := SkipSpaces(str, i);
  IF i >= Strings.Length(str) THEN
    obj:= MakeError("empty input");
    RETURN i
  ELSIF str[i] = kRPar THEN
    obj := MakeError("invalid syntax");
    RETURN Strings.Length(str)
  ELSIF str[i] = kLPar THEN
    obj := MakeError("noimpl");
    RETURN Strings.Length(str)
  ELSIF str[i] = kQuote THEN
    obj := MakeError("noimpl");
    RETURN Strings.Length(str)
  ELSE
    RETURN ReadAtom(str, i, obj)
  END;
END Read;

PROCEDURE PrintObj(obj: LObj; VAR str: ARRAY OF CHAR);
BEGIN
  WITH
    obj: Nil DO
      str := "nil"
  | obj: Num DO
      Conv.ConvInt(obj.data, str)
  | obj: Sym DO
      Strings.Append(obj.data, str)
  | obj: Error DO
      Strings.Append("<error: ", str);
      Strings.Append(obj.data, str);
      Strings.Append(">", str)
  END
END PrintObj;

PROCEDURE Init();
VAR
  nil: Nil;
BEGIN
  kLPar := "(";
  kRPar := ")";
  kQuote := "'";

  NEW(nil);
  kNil := nil;

  symTableSize := 0
END Init;

PROCEDURE Main();
VAR
  line: ARRAY 10240 OF CHAR;
  obj: LObj;
  i: INTEGER;
BEGIN
  Init();
  LOOP
    Out.String("> ");
    In.Line(line);
    IF Strings.Length(line) = 0 THEN EXIT END;
    i := Read(line, 0, obj);
    line := "";
    PrintObj(obj, line);
    Out.String(line);
    Out.Ln;
  END
END Main;

BEGIN
  Main();

END Lisp.