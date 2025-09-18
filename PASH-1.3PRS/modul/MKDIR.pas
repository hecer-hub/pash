unit MKDIR;

interface

uses
  sysutils,
  trpo,
  glvar;

procedure RunMKDIR(const Dir: String);

implementation

procedure RunMKDIR(const Dir: String);
var
  fullPath: String;
begin
  if Dir = '' then
  begin
    trpoWriteLn('ERROR: name folder not found.');
    exit;
  end;

  fullPath := glvar.currentPath + '/' + Dir;

  if CreateDir(fullPath) then
  else
  begin
    trpoWriteLn('ERROR: create folder.');
  end;
end;

end.
