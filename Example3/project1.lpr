{Sample of code for "UnTerminal", in a console program.
 This a very basic sample test. No interaction, no processing, no response.
 Just opening a CMD command.}

program project1;
uses Interfaces, UnTerminal, SysUtils;
type
  TProc=class
    proc: TConsoleProc;
    procedure procLineCompleted(const lin: string);
    constructor Create;
    destructor Destroy; override;
  end;
constructor TProc.Create;
begin
  proc := TConsoleProc.Create(nil);
  proc.LineDelimSend := LDS_CRLF;
  proc.OnLineCompleted:=@procLineCompleted;
end;

destructor TProc.Destroy;
begin
  proc.Destroy;
end;

procedure TProc.procLineCompleted(const lin: string);
begin
  //Line received
  writeln('--'+lin);
  //To respond, do something like: proc.SendLn('dir');
end;

var
  p: TProc;
  i: Integer;
begin
  writeln('Executing process');
  p := TProc.Create;
  p.proc.Open('cmd','');
  p.proc.Loop(3);  //Execute process until finished or pass 3 seconds
  p.proc.Close;
  writeln('Finished');
  p.Destroy;
end.

