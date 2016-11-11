unit FrameCfgConex;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons,
  globales, Masks, UnTerminal, MisUtils, Graphics, Dialogs, ExtCtrls,
  ConfigFrame;

type
  //Tipos de conexiones
  TTipCon = (
     TCON_TELNET,    //Conexión telnet común
     TCON_SSH,       //Conexión ssh
     TCON_OTHER      //Otro proceso
  );

  { TfraConexion }

  TfraConexion = class(TCfgFrame)
    lblOtro: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    txtOtro: TEdit;
  private
    proc: TConsoleProc;
  public
    //parámetros
    Tipo      : TTipCon;  //tipo de conexión
    IP        : String;   //Direción IP (solo válido con el tipo TCON_TELNET Y TCON_SSH)
    Other     : String;   //Ruta del aplicativo (solo válido con el tipo TCON_OTHER)
    LineDelim : TTypLineDel;  //Tipo de delimitaodr de línea
    ConRecientes: TStringList;  //Lista de conexiones recientes
    procedure PropToWindow; override;
    procedure WindowToProp; override;
    procedure Iniciar(secINI0: string; proc0: TConsoleProc);
    procedure UpdateChanges;
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

{ TfraConexion }
procedure TfraConexion.Iniciar(secINI0: string; proc0: TConsoleProc);
begin
  secINI := secINI0;  //sección INI
  proc := proc0;
  OnUpdateChanges:=@UpdateChanges;
  //crea las relaciones variable-control
  Asoc_Str_TEdit(@Other, txtOtro, 'Other', '');
  Asoc_StrList(@ConRecientes, 'Recient');
  Asoc_Enum_TRadGroup(@LineDelim, SizeOf(LineDelim), RadioGroup1, 'LineDelim', 0);
//  Asoc_Bol_TChkB(@EjecMacro, chkEjecMacro, 'EjecMacro', false);
//  Asoc_Str_TEdit(@MacroIni, FileNameMacroIni,'MacroIni', '');
end;
constructor TfraConexion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ConRecientes := TStringList.Create;  //crea lista
  //valores por defecto de la conexión actual
  Tipo := TCON_TELNET;
end;
destructor TfraConexion.Destroy;
begin
  ConRecientes.Free;
  inherited Destroy;
end;
procedure TfraConexion.UpdateChanges;
//Configura el proceso de acuerdo a los parámetros de la conexión.
begin
  proc.LineDelim := LineDelim;   //configura salto de línea
end;

procedure TfraConexion.PropToWindow;
begin
  inherited;
end;

procedure TfraConexion.WindowToProp;
begin
  //solo si no hay errores
  inherited WindowToProp;
end;

end.

