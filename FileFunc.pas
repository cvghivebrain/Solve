unit FileFunc;

interface
uses SysUtils;

procedure LoadFile(openthis: string);
procedure SaveFile(savethis: string);
procedure ClipFile(a, len: integer; clipthis: string);
function GetByte(a: integer): byte;
function GetBit(a, b: integer): byte;
function Bit(i, b: integer): byte;
function GetWord(a: integer): word;
function GetDword(a: integer): longword;
function GetWordRev(a: integer): word;
function GetDwordRev(a: integer): longword;
function GetString(a, maxlength: integer): string;

var
  myfile: file;
  filearray: array of byte;

implementation

{ Copy file to memory. }

procedure LoadFile(openthis: string);
begin
  if FileExists(openthis) = true then
    begin
    AssignFile(myfile,openthis); // Get file.
    FileMode := fmOpenRead; // Read only.
    Reset(myfile,1);
    SetLength(filearray,FileSize(myfile)); // Match array size to file size.
    BlockRead(myfile,filearray[0],FileSize(myfile)); // Copy file to memory.
    CloseFile(myfile); // Close file.
    end;
end;

{ Save file from memory to file. }

procedure SaveFile(savethis: string);
begin
  AssignFile(myfile,savethis); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myfile,1);
  BlockWrite(myfile,filearray[0],Length(filearray)); // Copy contents of array to file.
  CloseFile(myfile); // Close file.
end;

{ Save section of file to another file. }

procedure ClipFile(a, len: integer; clipthis: string);
var myclipfile: file;
  clipfilearray: array of byte;
begin
  AssignFile(myclipfile,clipthis); // Open file.
  FileMode := fmOpenReadWrite;
  ReWrite(myclipfile,1);
  SetLength(clipfilearray,len);
  Move(filearray[a],clipfilearray[0],len); // Copy specified section.
  BlockWrite(myclipfile,clipfilearray[0],len); // Copy contents of array to file.
  CloseFile(myclipfile); // Close file.
end;

{ Get byte from file array. }

function GetByte(a: integer): byte;
begin
  result := filearray[a];
end;

{ Get bit from file array. }

function GetBit(a, b: integer): byte;
begin
  result := (filearray[a] and (1 shl b)) shr b;;
end;

{ Get bit from integer. }

function Bit(i, b: integer): byte;
begin
  result := (i and (1 shl b)) shr b;;
end;

{ Get word from file array. }

function GetWord(a: integer): word;
begin
  result := (filearray[a]*$100)+filearray[a+1];
end;

{ Get longword from file array. }

function GetDword(a: integer): longword;
begin
  result := (GetWord(a)*$10000)+GetWord(a+2);
end;

{ Get word (little endian) from file array. }

function GetWordRev(a: integer): word;
begin
  result := (filearray[a+1]*$100)+filearray[a];
end;

{ Get longword (little endian) from file array. }

function GetDwordRev(a: integer): longword;
begin
  result := (GetWordRev(a+2)*$10000)+GetWordRev(a);
end;

{ Get string from file array. }

function GetString(a, maxlength: integer): string;
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

end.
