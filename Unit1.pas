unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, StrUtils, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    editInput: TEdit;
    btnGo: TButton;
    memOutput: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
  private
    { Private declarations }
    function Explode(s, d: string; n: integer): string;
    function CRCString(s: string): longword;
    function GetWord(a: integer): word;
    function GetDword(a: integer): longword;
    function GetString(a, maxlength: integer): string;
    function DoSum(s: string): int64;
    function Solve(s: string): int64;
    function Solve2(s, t: string): int64;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  crctable: array[0..255] of longword;
  filearray: array[0..5] of byte;

implementation

{$R *.dfm}

{ Replicate MediaWiki's "explode" string function. }
function TForm1.Explode(s, d: string; n: integer): string;
var n2: integer;
begin
  if (AnsiPos(d,s) = 0) and ((n = 0) or (n = -1)) then result := s // Output full string if delimiter not found.
  else
    begin
    if n > -1 then // Check for negative substring.
      begin
      s := s+d;
      n2 := n;
      end
    else
      begin
      d := AnsiReverseString(d);
      s := AnsiReverseString(s)+d; // Reverse string for negative.
      n2 := Abs(n)-1;
      end;
    while n2 > 0 do
      begin
      Delete(s,1,AnsiPos(d,s)+Length(d)-1); // Trim earlier substrings and delimiters.
      dec(n2);
      end;
    Delete(s,AnsiPos(d,s),Length(s)-AnsiPos(d,s)+1); // Trim later substrings and delimiters.
    if n < 0 then s := AnsiReverseString(s); // Un-reverse string if negative.
    result := s;
    end;
end;

{ Get CRC32 of string. }
function TForm1.CRCString(s: string): longword;
var i, x: integer;
  r: longint;
begin
  r := -1;
  for i := 0 to Length(s)-1 do
    begin
    x := (Ord(s[i+1]) xor r) and $FF;
    r := (r shr 8) xor crctable[x];
    end;
  Result := not r;
end;

{ Get word from file array. }
function TForm1.GetWord(a: integer): word;
begin
  result := (filearray[a]*$100)+filearray[a+1];
end;

{ Get longword from file array. }
function TForm1.GetDword(a: integer): longword;
begin
  result := (GetWord(a)*$10000)+GetWord(a+2);
end;

{ Get string from file array. }
function TForm1.GetString(a, maxlength: integer): string;
begin
  result := '';
  while maxlength > 0 do
    begin
    Dec(maxlength);
    if filearray[a] in [32..126] then result := result+Chr(filearray[a]) // Add character to string if valid.
    else maxlength := 0; // Otherwise end the string.
    Inc(a); // Next character.
    end;
end;

function TForm1.DoSum(s: string): int64; // Convert a string sum (e.g. '1+1') to integer.
var i, r: int64;
  sub: string;
begin
  s := ReplaceStr(s,' ',''); // Strip spaces.
  s := ReplaceStr(s,'<<','?L'); // Replace << to avoid clash with <.
  s := ReplaceStr(s,'>>','?R'); // Replace >> to avoid clash with >.
  if AnsiPos('>=',s) <> 0 then // Compare sides if string contains gte sign.
    if DoSum(Explode(s,'>=',0)) >= DoSum(Explode(s,'>=',1)) then s := '1' // 1 for greater than or equal.
    else s := '0'; // 0 for less.
  if AnsiPos('<=',s) <> 0 then // Compare sides if string contains lte sign.
    if DoSum(Explode(s,'<=',0)) <= DoSum(Explode(s,'<=',1)) then s := '1' // 1 for less than or equal.
    else s := '0'; // 0 for greater.
  if AnsiPos('=',s) <> 0 then // Compare sides if string contains equal sign.
    if DoSum(Explode(s,'=',0)) = DoSum(Explode(s,'=',1)) then s := '1' // 1 for equal.
    else s := '0'; // 0 for different.
  if AnsiPos('<>',s) <> 0 then // Compare sides if string contains inequal sign.
    if DoSum(Explode(s,'<>',0)) <> DoSum(Explode(s,'<>',1)) then s := '1' // 1 for inequal.
    else s := '0'; // 0 for same.
  if AnsiPos('>',s) <> 0 then // Compare sides if string contains greater than sign.
    if DoSum(Explode(s,'>',0)) > DoSum(Explode(s,'>',1)) then s := '1' // 1 for greater than.
    else s := '0'; // 0 for less.
  if AnsiPos('<',s) <> 0 then // Compare sides if string contains less than sign.
    if DoSum(Explode(s,'<',0)) < DoSum(Explode(s,'<',1)) then s := '1' // 1 for less than.
    else s := '0'; // 0 for greater.
  // Insert separator characters.
  s := ReplaceStr(s,'-','?-');
  s := ReplaceStr(s,'+','?+');
  s := ReplaceStr(s,'*','?*');
  s := ReplaceStr(s,'/','?/');
  s := ReplaceStr(s,'&','?&'); // AND
  s := ReplaceStr(s,'^','?^'); // XOR
  s := ReplaceStr(s,'|','?|'); // OR
  s := ReplaceStr(s,'%','?%'); // Modulo
  // Allow negative numbers.
  s := ReplaceStr(s,'+?-','+-');
  s := ReplaceStr(s,'*?-','*-');
  s := ReplaceStr(s,'/?-','/-');
  s := ReplaceStr(s,'&?-','&-');
  s := ReplaceStr(s,'^?-','^-');
  s := ReplaceStr(s,'|?-','|-');
  s := ReplaceStr(s,'%?-','%-');
  s := ReplaceStr(s,'L?-','L-');
  s := ReplaceStr(s,'R?-','R-');
  if Copy(s,1,2) = '?-' then Delete(s,1,1); // Allow sum to start with negative.
  i := 0;
  r := StrtoInt64(Explode(s,'?',0));
  while Explode(s,'?',i) <> '' do
    begin
    sub := Explode(s,'?',i); // Get substring.
    if Copy(sub,1,1) = '+' then r := r+StrtoInt64(Explode(sub,'+',1)) // If +, add it.
    else if Copy(sub,1,1) = '-' then r := r-StrtoInt64(Explode(sub,'-',1))
    else if Copy(sub,1,1) = '*' then r := r*StrtoInt64(Explode(sub,'*',1))
    else if Copy(sub,1,1) = '/' then r := r div StrtoInt64(Explode(sub,'/',1))
    else if Copy(sub,1,1) = '&' then r := r and StrtoInt64(Explode(sub,'&',1))
    else if Copy(sub,1,1) = '|' then r := r or StrtoInt64(Explode(sub,'|',1))
    else if Copy(sub,1,1) = '^' then r := r xor StrtoInt64(Explode(sub,'^',1))
    else if Copy(sub,1,1) = '%' then r := r mod StrtoInt64(Explode(sub,'%',1))
    else if Copy(sub,1,1) = 'L' then r := r shl StrtoInt64(Explode(sub,'L',1))
    else if Copy(sub,1,1) = 'R' then r := r shr StrtoInt64(Explode(sub,'R',1));
    inc(i);
    end;
  result := r;
end;

function TForm1.Solve(s: string): int64; // Convert sum with brackets to integer.
var sub, t: string;
begin
  while AnsiPos('"',s) <> 0 do
    begin
    sub := Explode(s,'"',1); // Get contents of quotes.
    s := ReplaceStr(s,'"'+sub+'"','$'+InttoHex(CRCString(sub),8)); // Replace string with CRC32.
    end;
  while AnsiPos(']',s) <> 0 do
    begin
    sub := Explode(Explode(s,']',0),'[',-1); // Get contents of square brackets.
    t := AnsiRightStr(Explode(Explode(s,']',0),'[',-2),1); // Get character before bracket (b/w/d/s).
    s := ReplaceStr(s,t+'['+sub+']',InttoStr(Solve2(sub,t))); // Solve & remove brackets.
    end;
  while AnsiPos(')',s) <> 0 do
    begin
    sub := Explode(Explode(s,')',0),'(',-1); // Get contents of brackets.
    s := ReplaceStr(s,'('+sub+')',InttoStr(DoSum(sub))); // Solve & remove brackets.
    end;
  result := DoSum(s); // Final sum after brackets are gone.
end;

function TForm1.Solve2(s, t: string): int64; // Get data from file array.
var stringaddr, stringmax: int64;
begin
  if t = 'b' then result := filearray[Solve(s)] // Return byte from file array.
  else if t = 'w' then result := GetWord(Solve(s)) // Return word.
  else if t = 'd' then result := GetDWord(Solve(s)) // Return longword.
  else if t = 's' then
    begin
    stringaddr := Solve(Explode(s,',',0));
    stringmax := Solve(Explode(s,',',1));
    result := CRCString(GetString(stringaddr,stringmax)); // Return CRC32 of string.
    end
  else result := 0; // Return nothing.
end;

procedure TForm1.FormCreate(Sender: TObject);
var i, j: integer;
begin
  { Create CRC32 lookup table. }
  for i := 0 to 255 do
    begin
    crctable[i] := i;
    for j := 0 to 7 do if Odd(crctable[i]) then
      crctable[i] := (crctable[i] shr 1) xor $EDB88320
      else crctable[i] := crctable[i] shr 1;
    end;
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
  memOutput.Clear;
  memOutput.Lines.Add(InttoStr(Solve(editInput.Text)));
end;

end.
