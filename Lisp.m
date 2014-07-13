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
  symQuote: LObj;

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

PROCEDURE Nreverse(lst: LObj): LObj;
VAR
  ret: LObj;
  tmp: LObj;
BEGIN
  ret := kNil;
  LOOP
    WITH
      lst: Cons DO
        tmp := lst.cdr;
        lst.cdr := ret;
        ret := lst;
    ELSE
      EXIT
    END;
    lst := tmp  (* This statement can't be in WITH due to type checking. *)
  END;
  RETURN ret;
END Nreverse;

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
VAR
  j: INTEGER;
  elm: LObj;
BEGIN
  i := SkipSpaces(str, i);
  IF i >= Strings.Length(str) THEN
    obj:= MakeError("empty input");
    RETURN i
  ELSIF str[i] = kRPar THEN
    obj := MakeError("invalid syntax");
    RETURN Strings.Length(str)
  ELSIF str[i] = kLPar THEN
    RETURN ReadList(str, i + 1, obj)
  ELSIF str[i] = kQuote THEN
    j := Read(str, i + 1, elm);
    obj := MakeCons(symQuote, MakeCons(elm, kNil));
    RETURN j
  ELSE
    RETURN ReadAtom(str, i, obj)
  END;
END Read;

PROCEDURE ReadList(str: ARRAY OF CHAR; i: INTEGER; VAR obj: LObj): INTEGER;
VAR
  elm: LObj;
BEGIN
  obj := kNil;
  LOOP
    i := SkipSpaces(str, i);
    IF i >= Strings.Length(str) THEN
      obj := MakeError("unfinished parenthesis");
      RETURN i
    ELSIF str[i] = kRPar THEN
      EXIT;
    END;
    i := Read(str, i, elm);
    IF elm IS Error THEN
      obj := elm;
      RETURN i
    END;
    obj := MakeCons(elm, obj);
  END;
  obj := Nreverse(obj);
  RETURN i + 1;
END ReadList;

PROCEDURE PrintObj(obj: LObj; VAR str: ARRAY OF CHAR);
VAR
  buf: ARRAY 32 OF CHAR;
BEGIN
  WITH
    obj: Nil DO
      Strings.Append("nil", str);
  | obj: Num DO
      Conv.ConvInt(obj.data, buf);
      Strings.Append(buf, str)
  | obj: Sym DO
      Strings.Append(obj.data, str)
  | obj: Error DO
      Strings.Append("<error: ", str);
      Strings.Append(obj.data, str);
      Strings.Append(">", str)
  | obj: Cons DO
      PrintList(obj, str);
  END
END PrintObj;

PROCEDURE PrintList(obj: LObj; VAR str: ARRAY OF CHAR);
VAR
  first: BOOLEAN;
BEGIN
  first := TRUE;
  Strings.Append("(", str);
  WHILE obj IS Cons DO
    IF first THEN
      first := FALSE
    ELSE
      Strings.Append(" ", str);
    END;
    PrintObj(SafeCar(obj), str);
    obj := SafeCdr(obj);
  END;
  IF ~(obj = kNil) THEN
    Strings.Append(" . ", str);
    PrintObj(obj, str)
  END;
  Strings.Append(")", str)
END PrintList;

PROCEDURE Init();
VAR
  nil: Nil;
BEGIN
  kLPar := "(";
  kRPar := ")";
  kQuote := "'";

  NEW(nil);
  kNil := nil;

  symTableSize := 0;
  symQuote := MakeSym("quote")
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