unit auth;

interface

uses
  sysutils,
  trpo,
  glvar;

procedure RunAuth;

implementation

procedure ShowLegalNotice;
begin
  trpoWriteLn('--------------------------------------------------');
  trpoWriteLn('                  LEGAL NOTICE');
  trpoWriteLn('--------------------------------------------------');
  trpoWriteLn('Pash is free software: you can redistribute it and/or');
  trpoWriteLn('modify it under the terms of the GNU General Public');
  trpoWriteLn('License as published by the Free Software Foundation.');
  trpoWriteLn('');
  trpoWriteLn('This program is distributed in the hope that it will be useful,');
  trpoWriteLn('but WITHOUT ANY WARRANTY; without even the implied warranty of');
  trpoWriteLn('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.');
  trpoWriteLn('');
  trpoWriteLn('You are using this software at your own risk.');
  trpoWriteLn('For more details, see the full GNU GPL license text.');
  trpoWriteLn('--------------------------------------------------');
  trpoWriteLn('');
end;

procedure RegisterUser;
var
  username, password: String;
  configFile: TextFile;
begin
  trpoWriteLn('-----------------------------');
  trpoWriteLn('   User Registration');
  trpoWriteLn('-----------------------------');

  trpoWriteLn('Enter username: ');
  ReadLn(username);

  trpoWriteLn('Enter password: ');
  ReadLn(password);

  Assign(configFile, '.pash_config');
  Rewrite(configFile);
  WriteLn(configFile, username);
  WriteLn(configFile, password);
  Close(configFile);

  trpoWriteLn('Registration successful. Data saved.');
end;

procedure LoginUser;
var
  username, password, savedUser, savedPass: String;
  configFile: TextFile;
begin
  trpoWriteLn('-----------------------------');
  trpoWriteLn('  Please log in.');
  trpoWriteLn('-----------------------------');

  Assign(configFile, '.pash_config');
  Reset(configFile);
  ReadLn(configFile, savedUser);
  ReadLn(configFile, savedPass);
  Close(configFile);

  repeat
    trpoWriteLn('Username: ');
    ReadLn(username);

    trpoWriteLn('Password: ');
    ReadLn(password);

    if (username = savedUser) and (password = savedPass) then
    begin
      trpoWriteLn('');
      trpoWriteLn('Login successful.');
      Break;
    end
    else
    begin
      trpoWriteLn('');
      trpoWriteLn('Error: invalid username or password.');
    end;
  until False;
end;

procedure RunAuth;
begin
  if not FileExists('.pash_config') then
  begin
    ShowLegalNotice;
    RegisterUser;
  end
  else
  begin
    LoginUser;
  end;
end;

end.
