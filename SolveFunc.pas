unit SolveFunc;

interface
uses StrUtils, SysUtils, Math, ExplodeFunc, CRCFunc, FileFunc;

function DoSum(s: string): int64;
function DoSum2(s: string): int64;
function DoSum3(num1, num2: string; op: char): string;
function Solve(s: string): int64;
function Solve2(s: string): int64;
function SolveStr(s: string): string;
function SolveHex(s: string): string;

var
  val: int64;

implementation

{ Convert an expression (e.g. '1+1') to integer. }

function DoSum(s: string): int64;
label istrue;
begin
  s := AnsiUpperCase(ReplaceStr(s,' ','')); // Strip spaces and make it uppercase.
  s := ReplaceStr(s,'0X','$'); // Convert C++ hex prefix to Delphi/assembly.
  s := ReplaceStr(s,'<<','L'); // Replace << to avoid clash with <.
  s := ReplaceStr(s,'>>','R'); // Replace >> to avoid clash with >.
  if (AnsiPos('=',s) > 0) or (AnsiPos('<',s) > 0) or (AnsiPos('>',s) > 0) then // Check for conditional.
    begin
    result := 1; // Assume condition is satisfied.
    if AnsiPos('>=',s) > 0 then
      if DoSum2(Explode(s,'>=',0)) >= DoSum2(Explode(s,'>=',1)) then exit; // 1 for greater than or equal.
    if AnsiPos('<=',s) > 0 then
      if DoSum2(Explode(s,'<=',0)) <= DoSum2(Explode(s,'<=',1)) then exit; // 1 for less than or equal.
    if AnsiPos('=',s) > 0 then
      if DoSum2(Explode(s,'=',0)) = DoSum2(Explode(s,'=',1)) then exit; // 1 for equal.
    if AnsiPos('<>',s) > 0 then
      if DoSum2(Explode(s,'<>',0)) <> DoSum2(Explode(s,'<>',1)) then exit; // 1 for inequal.
    if AnsiPos('>',s) > 0 then
      if DoSum2(Explode(s,'>',0)) > DoSum2(Explode(s,'>',1)) then exit; // 1 for greater than.
    if AnsiPos('<',s) > 0 then
      if DoSum2(Explode(s,'<',0)) < DoSum2(Explode(s,'<',1)) then exit; // 1 for less than.
    result := 0; // Condition was not satisfied.
    end
  else result := DoSum2(s);
end;

function DoSum2(s: string): int64;
var i: integer;
  num1, num2, op: string;
begin
  num1 := '';
  num2 := '';
  op := '';
  for i := 1 to Length(s) do
    begin
    if AnsiPos(s[i],'0123456789ABCDEF$') > 0 then // Check if character is digit or operator.
      begin
      if op = '' then num1 := num1+s[i] // Add to first number.
      else num2 := num2+s[i]; // Add to second number if operator is set.
      end
    else
      begin
      if op <> '' then
        begin
        num1 := DoSum3(num1,num2,op[1]); // Perform operation and put result in num1.
        num2 := ''; // Clear second number.
        end;
      op := s[i]; // Next operator.
      end;
    end;
  if op <> '' then result := StrToInt64(DoSum3(num1,num2,op[1]))
  else result := StrToInt64(num1); // Return  single number if no operator is present.
end;

function DoSum3(num1, num2: string; op: char): string;
begin
  case Ord(op) of
  Ord('+'): result := IntToStr(StrToInt64(num1)+StrToInt64(num2));
  Ord('-'): result := IntToStr(StrToInt64(num1)-StrToInt64(num2));
  Ord('*'): result := IntToStr(StrToInt64(num1)*StrToInt64(num2));
  Ord('/'): result := IntToStr(StrToInt64(num1) div StrToInt64(num2));
  Ord('\'): result := IntToStr(Ceil(StrToInt64(num1)/StrToInt64(num2))); // Division rounding up.
  Ord('&'): result := IntToStr(StrToInt64(num1) and StrToInt64(num2));
  Ord('|'): result := IntToStr(StrToInt64(num1) or StrToInt64(num2));
  Ord('^'): result := IntToStr(StrToInt64(num1) xor StrToInt64(num2));
  Ord('%'): result := IntToStr(StrToInt64(num1) mod StrToInt64(num2));
  Ord('L'): result := IntToStr(StrToInt64(num1) shl StrToInt64(num2));
  Ord('R'): result := IntToStr(StrToInt64(num1) shr StrToInt64(num2));
  end;
end;

{ Convert sum with brackets to integer. }

function Solve(s: string): int64;
var sub: string;
begin
  s := ReplaceStr(s,'{filesize}',IntToStr(Length(filearray))); // Insert file size.
  s := ReplaceStr(s,'{val}',IntToStr(val)); // Insert predefined value.
  while AnsiPos('"',s) <> 0 do
    begin
    sub := Explode(s,'"',1); // Get contents of quotes.
    s := ReplaceStr(s,'"'+sub+'"','$'+CRCString(sub)); // Replace string with CRC32.
    end;
  while AnsiPos('}',s) <> 0 do
    begin
    sub := ExplodeFull(Explode(s,'}',0),'{',-1); // Get contents of curly brackets.
    s := ReplaceStr(s,'{'+sub+'}',InttoStr(Solve2(sub))); // Solve & remove brackets.
    end;
  while AnsiPos(')',s) <> 0 do
    begin
    sub := ExplodeFull(Explode(s,')',0),'(',-1); // Get contents of brackets.
    s := ReplaceStr(s,'('+sub+')',InttoStr(DoSum(sub))); // Solve & remove brackets.
    end;
  result := DoSum(s); // Final sum after brackets are gone.
end;

function Solve2(s: string): int64; // Get data from file array.
var t: string;
  param1, param2: int64;
begin
  t := Explode(s,':',0); // Get type (e.g. "b" for byte).
  Delete(s,1,Length(t)+1); // Remove type from input string.
  if t = 'b' then result := filearray[Solve(s)] // Return byte from file array.
  else if t = 'w' then result := GetWord(Solve(s)) // Return word.
  else if t = 'W' then result := GetWordRev(Solve(s)) // Return word (byteswapped).
  else if t = 'd' then result := GetDWord(Solve(s)) // Return longword.
  else if t = 'D' then result := GetDWordRev(Solve(s)) // Return longword (byteswapped).
  else if t = 's' then
    begin
    param1 := Solve(Explode(s,',',0)); // Get string address.
    param2 := Solve(Explode(s,',',1)); // Get max length.
    result := StrtoInt64('$'+CRCString(GetString(param1,param2))); // Return CRC32 of string.
    end
  else result := 0; // Return nothing.
end;

function SolveStr(s: string): string; // Solve with output as string.
begin
  result := IntToStr(Solve(s));
end;

function SolveHex(s: string): string; // Solve with output as (hex) string.
begin
  result := '$'+IntToHex(Solve(s),1);
end;

end.