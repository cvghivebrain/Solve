unit SolveFunc;

interface
uses StrUtils, SysUtils, ExplodeFunc, CRCFunc, FileFunc;

function DoSum(s: string): int64;
function Solve(s: string): int64;
function Solve2(s, t: string): int64;

var
  customarray: array[0..1000] of int64;

implementation

{ Convert a string sum (e.g. '1+1') to integer. }

function DoSum(s: string): int64;
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

{ Convert sum with brackets to integer. }

function Solve(s: string): int64;
var sub, t: string;
begin
  while AnsiPos('"',s) <> 0 do
    begin
    sub := Explode(s,'"',1); // Get contents of quotes.
    s := ReplaceStr(s,'"'+sub+'"','$'+CRCString(sub)); // Replace string with CRC32.
    end;
  while AnsiPos(']',s) <> 0 do
    begin
    sub := Explode(Explode(s,']',0),'[',-1); // Get contents of square brackets.
    t := AnsiRightStr(Explode(Explode(s,']',0),'[',-2),1); // Get character before bracket (b/w/d/s/a).
    s := ReplaceStr(s,t+'['+sub+']',InttoStr(Solve2(sub,t))); // Solve & remove brackets.
    end;
  while AnsiPos(')',s) <> 0 do
    begin
    sub := Explode(Explode(s,')',0),'(',-1); // Get contents of brackets.
    s := ReplaceStr(s,'('+sub+')',InttoStr(DoSum(sub))); // Solve & remove brackets.
    end;
  result := DoSum(s); // Final sum after brackets are gone.
end;

function Solve2(s, t: string): int64; // Get data from file array.
var stringaddr, stringmax: int64;
begin
  if t = 'b' then result := filearray[Solve(s)] // Return byte from file array.
  else if t = 'w' then result := GetWord(Solve(s)) // Return word.
  else if t = 'd' then result := GetDWord(Solve(s)) // Return longword.
  else if t = 's' then
    begin
    stringaddr := Solve(Explode(s,',',0));
    stringmax := Solve(Explode(s,',',1));
    result := StrtoInt64('$'+CRCString(GetString(stringaddr,stringmax))); // Return CRC32 of string.
    end
  else if t = 'a' then result := customarray[Solve(s)] // Return integer from array.
  else result := 0; // Return nothing.
end;

end.