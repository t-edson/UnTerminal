unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, UnTerminal, TermVT;

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
    procedure procAddLine(HeightScr: integer);
    procedure procChangeState(State: string; pFinal: TPoint);
    procedure procInitLines(const grilla: TtsGrid; fIni, fFin: integer);
    procedure procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
    procedure procRefreshLines(const grilla: TtsGrid; fIni, fFin,
      HeightScr: integer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  proc: TConexProc;

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
  proc:= TConexProc.Create(nil);
  proc.sendCRLF:=true;
  proc.OnInitLines:=@procInitLines;
  proc.OnRefreshLine:=@procRefreshLine;
  proc.OnRefreshLines:=@procRefreshLines;
  proc.OnAddLine:=@procAddLine;
  proc.OnChangeState:=@procChangeState;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  proc.Destroy;
end;

procedure TForm1.procAddLine(HeightScr: integer);
begin
  Memo1.Lines.Add('');
end;

procedure TForm1.procChangeState(State: string; pFinal: TPoint);
begin
  StatusBar1.Panels[0].Text:=State;
end;

procedure TForm1.procInitLines(const grilla: TtsGrid; fIni, fFin: integer);
var
  i: Integer;
begin
  for i:=fIni to fFin do Memo1.Lines.Add(grilla[i]);
end;

procedure TForm1.procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer
  );
var
  yvt: Integer;
begin
  yvt := Memo1.Lines.Count-HeightScr-1;
  Memo1.Lines[yvt+fIni] := grilla[fIni];
end;

procedure TForm1.procRefreshLines(const grilla: TtsGrid; fIni, fFin,
  HeightScr: integer);
var
  yvt: Integer;
  f: Integer;
begin
  yvt := Memo1.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  for f:=fIni to fFin do Memo1.Lines[yvt+ f] := grilla[f];
end;

end.

