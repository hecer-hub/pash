unit trpo;
{$codepage UTF8}

interface
uses crt;

procedure trpoInit(const Prompt: String);
procedure trpoInput(const Prompt: String; var Command: String);
procedure trpoSetITV(Value: Boolean);
procedure trpoWriteLn(const Text: String);

implementation

var isInteractive: Boolean;

procedure trpoInit(const Prompt: String);
begin
  clrscr;
  isInteractive := True;
  if Prompt <> '' then WriteLn(Prompt);
end;

procedure trpoInput(const Prompt: String; var Command: String);
begin
  if wherex > 1 then WriteLn;
  Write(Prompt);
  ReadLn(Command);
end;

procedure trpoSetITV(Value: Boolean);
begin
  isInteractive := Value;
end;

procedure trpoWriteLn(const Text: String);
begin
  WriteLn(Text);
end;

end.