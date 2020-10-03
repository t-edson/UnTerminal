{Sample of code implementing a Terminal, using the unit "UnTerminal".
 The output is captured using the Line by Line detection}
unit Unit1;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, UnTerminal;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    label2: TLabel;
    StatusBar1: TStatusBar;
    txtProcess: TEdit;
    label1: TLabel;
    Memo1: TMemo;
    txtCommand: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure procChangeState(State: string; pFinal: TPoint);
    procedure procLineCompleted(const lin: string);
    procedure procReadData(nDat: integer; const lastLin: string);
  private
    { private declarations }
  public
    LinPartial: boolean;
  end;

var
  Form1: TForm1;
  proc: TConsoleProc;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  proc.Open(txtProcess.Text,'');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  proc.Close;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  proc.SendLn(txtCommand.Text);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  proc.AutoConfigPrompt;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  proc:= TConsoleProc.Create(nil);
  proc.LineDelimSend := LDS_CRLF;
  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnReadData:=@procReadData;
  proc.OnChangeState:=@procChangeState;
  {$ifdef linux}
  txtProcess.Text:= 'bash';
  txtCommand.Text := 'ls';
  proc.LineDelimSend := LDS_LF;
  {$endif}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  proc.Destroy;
end;

procedure TForm1.procChangeState(State: string; pFinal: TPoint);
begin
  StatusBar1.Panels[0].Text:=State;
end;

procedure TForm1.procLineCompleted(const lin: string);
begin
  if LinPartial then begin
    //Estamos en la línea del prompt
    Memo1.Lines[Memo1.Lines.Count-1] := lin;  //reemplaza última línea
    LinPartial := false;
  end else begin  //caso común
    Memo1.Lines.Add(lin);
  end;
end;

procedure TForm1.procReadData(nDat: integer; const lastLin: string);
begin
  LinPartial := true;   //marca bandera
  Memo1.Lines.Add(lastLin);   //agrega la línea que contiene al prompt
end;


end.

