const idLen = 32;
  ident = 0;
  literal = 2;
  lparen = 3;
  lbrak = 4;
  lbrace = 5;
  bar = 6;
  eql = 7;
  rparen = 8;
  rbrak = 9;
  rbrace = 10;
  period = 11;
  other = 12;

type Identifier = array idLen of char;

var ch: char;
  sym: integer;
  id: Identifier;
  R: Texts.Reader;

procedure GetSym;
  var i: integer;
begin
  (* skip blanks *)
  while ~R.eot & (ch <= ' ') do
    Texts.Read(R, ch);
  end

  case ch
    of 'A' .. 'Z', 'a' .. 'z':
      sym := ident;
      i := 0;

      repeat
        id[i] := ch;
        inc(i);
        Texts.Read(R, ch);
      until ((cap(ch) < 'A') or (cap(ch) > 'Z'))

      id[i] := 0X

     | 22X: (* quote *)
      Texts.Read(R, ch);
      sym := literal;
      i := 0;

      while (ch # 22X) & (ch > ' ') do
        id[i] := ch;
        inc(i);
        Texts.Read(R, ch);
      end;

      if ch <= ' ' then
        error(1)
      end;

      id[i] := 0X;
      Texts.Read(R, ch);

     | '=': sym := eql; Texts.Read(R, ch);
     | '(': sym := lparen; Texts.Read(R, ch);
     | ')': sym := rparen; Texts.Read(R, ch);
     | '[': sym := lbrak; Texts.Read(R, ch);
     | ']': sym := rbrak; Texts.Read(R, ch);
     | '{': sym := lbrace; Texts.Read(R, ch);
     | '}': sym := rbrace; Texts.Read(R, ch);
     | '|': sym := bar; Texts.Read(R, ch);
     | '.': sym := period; Texts.Read(R, ch);

   else
      sym := other;
      Texts.Read(R, ch);
  end;
end GetSym;
