program TTerm;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormPrincipal, FormConfig, globales,
  uResaltTerm, FormEditMacros;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.CreateForm(TConfig, Config);
  Application.CreateForm(TfrmEditMacros, frmEditMacros);
  Application.Run ;
end.

