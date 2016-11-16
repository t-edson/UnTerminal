unit frameCfgPantTerm;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, SynEdit,
  UnTerminal, globales
  ,ConfigFrame;
const MAX_LIN_TER = 32000;
type

  { TfraPantTerm }

  TfraPantTerm = class(TCfgFrame)
    edTpoMax: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    txtMaxColT: TEdit;
    txtMaxLinT: TEdit;
  private
    p: TConsoleProc;   //referencia a proceso telnet
    procedure ConfigTerminal;
  public
    maxLinTer : integer;  //máxima cantidad de líneas que se nantienen en el terminal
    maxColTer : integer;  //máxima cantidad de columnas que se muestran en el terminal
    TpoMax    : integer;
    procedure Iniciar(secINI0: string; p0: TConsoleProc ); //Inicia el frame
  end;

implementation

{$R *.lfm}

{ TfraPantTerm }
procedure TfraPantTerm.Iniciar(secINI0: string; p0: TConsoleProc);
begin
  secINI := secINI0;  //sección INI
  //asigna referencia necesarias
  p := p0;
  OnUpdateChanges := @ConfigTerminal;  //manejador de cambios
  //crea las relaciones variable-control
  Asoc_Int_TEdit(@maxLinTer ,txtMaxLinT,'maxLinTer',5000, 200,MAX_LIN_TER);  {menos de 200 líneas
                  puede causar problemas con la rutina de limitación de tamaño}
  Asoc_Int_TEdit(@maxColTer ,txtMaxColT,'maxColTer',1000, 80,10000);
  Asoc_Int_TEdit(@TpoMax, edTpoMax, 'TpoMax', 10, 1, 180);
end;

procedure TfraPantTerm.ConfigTerminal;
//Configura el terminal de acuerdo a las variables de estado
begin
  p.TerminalWidth:=maxColTer;
end;

end.

