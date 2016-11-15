{Unidad con formulario de configuración para manejar las propiedades de
 una aplicación. Está pensado para usarse con frames de la clase Tframe,
 definida en la unidad "PropertyFrame".
 }
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, SynEdit, Buttons, ComCtrls,
  UnTerminal, MisUtils, SynFacilCompletion,
  FrameCfgConex, frameCfgDetPrompt, frameCfgPantTerm,
  ConfigFrame;

type
  TEvCambiaProp = procedure of object;  //evento para indicar que hay cambio

  { TConfig }

  TConfig = class(TForm)
    bitAceptar: TBitBtn;
    bitAplicar: TBitBtn;
    bitCancel: TBitBtn;
    TreeView1: TTreeView;
    procedure bitAceptarClick(Sender: TObject);
    procedure bitAplicarClick(Sender: TObject);
    procedure bitCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    procedure LeerDeVentana;
    procedure MostEnVentana;
    { private declarations }
  public
    fraError: TCfgFrame;
    msjError: string;
    arIni   : String;      //Archivo de configuración
    edTerm  : TSynEdit;    //referencia al editor SynEdit
    edPCom  : TSynEdit;    //referencia al editor panel de comando
    edMacr  : TSynEdit;    //referencia al editor panel de comando
    edRemo  : TSynEdit;    //referencia al editor remoto
    prTel   : TConsoleProc;
    //frames de configuración
    fcConex   : TFraConexion;   //conexión
    fcPantTerm: TfraPantTerm;
    fcDetPrompt: TfraDetPrompt;
    procedure Iniciar(hl0: TSynFacilComplet);
    procedure LeerArchivoIni(iniFile: string='');
    procedure escribirArchivoIni(iniFile: string='');
    procedure Configurar(Id: string='');
    function ContienePrompt(const linAct, prIni, prFin: string): integer;
    function ContienePrompt(const cad: string): boolean;
    function EsPrompt(const cad: string): boolean;
    procedure SetLanguage(lang: string);
  end;

var
  Config: TConfig;

implementation
{$R *.lfm}

  { TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  //Crea frames de configuración
  fcConex:= TFraConexion.Create(Self);
  fcConex.parent := self;
  fcDetPrompt:= TfraDetPrompt.Create(Self);
  fcDetPrompt.parent := self;

  fcPantTerm:= TfraPantTerm.Create(Self);
  fcPantTerm.parent := self;

  //Obtiene nombre de archivo INI
  arIni := GetIniName;
  //selecciona primera opción
  TreeView1.Items[0].Selected:=true;
  TreeView1Click(self);
end;
procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;
procedure TConfig.FormShow(Sender: TObject);
begin
  MostEnVentana;   //carga las propiedades en el frame
end;
procedure TConfig.Iniciar(hl0: TSynFacilComplet);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //inicia Frames
  fcConex.Iniciar('conexion', prTel);
  fcPantTerm.Iniciar('panTerm',prTel);
  fcDetPrompt.Iniciar('detPrompt', edTerm, prTel);

  //lee parámetros del archivo de configuración.
  LeerArchivoIni;
end;
procedure TConfig.TreeView1Click(Sender: TObject);
begin
  if TreeView1.Selected = nil then exit;
  //hay ítem seleccionado
  Hide_AllConfigFrames(self);  //oculta todos
  case IdFromTTreeNode(TreeView1.Selected) of
  '1'  : fcConex.ShowPos(145,0) ;
  '2'  : fcDetPrompt.ShowPos(145,0);
  '3'    : fcPantTerm.ShowPos(145,0);
  end;
end;

procedure TConfig.bitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if fraError<>nil then exit;  //hubo error
  self.Close;   //porque es modal
end;
procedure TConfig.bitAplicarClick(Sender: TObject);
begin
  LeerDeVentana;       //Escribe propiedades de los frames
  if fraError<>nil then begin

    msgerr(fraError.MsjErr);
    exit;
  end;
  escribirArchivoIni;   //guarda propiedades en disco
  if edTerm<>nil then edTerm.Invalidate;     //para que refresque los cambios
  if edPCom<>nil then edPCom.Invalidate;     //para que refresque los cambios
end;
procedure TConfig.bitCancelClick(Sender: TObject);
begin
  self.Hide;
end;
procedure TConfig.Configurar(Id: string='');
//Muestra el formulario, de modo que permita configurar la sesión actual
var
  it: TTreeNode;
begin
  if Id<> '' then begin  /////se pide mostrar un Id en especial
    //oculta los demás
    it := TTreeNodeFromId(Id,TreeView1);
    if it <> nil then it.Selected:=true;
    TreeView1Click(self);
  end else begin ////////muestra todos
    for it in TreeView1.Items do begin
      it.Visible:=true;
    end;
  end;
  Showmodal;
end;

function TConfig.ContienePrompt(const linAct, prIni, prFin: string): integer;
//Verifica si una cadena contiene al prompt, usando los valroes de cadena inicial (prIni)
//y cadena final (prFin). La veriifcaión se hace siempre desde el inicio de la cadena.
//Si la cadena contiene al prompt, devuelve la longitud del prompt hallado, de otra forma
//devuelve cero.
//Se usa para el resaltador de sintaxis y el manejo de pantalla. Debería ser similar a
//la detección de prompt usada en el proceso.
var
  l: Integer;
  p: SizeInt;
begin
   Result := 0;   //valor por defecto
   l := length(prIni);
   if (l>0) and (copy(linAct,1,l) = prIni) then begin
     //puede ser
     if prFin = '' then begin
       //no hace falta validar más
       Result := l;  //el tamaño del prompt
       exit;    //no hace falta explorar más
     end;
     //hay que validar la existencia del fin del prompt
     p :=pos(prFin,linAct);
     if p>0 then begin  //encontró
       Result := p+length(prFin)-1;  //el tamaño del prompt
       exit;
     end;
   end;
end;
function TConfig.ContienePrompt(const cad: string): boolean;
//Forma siple de  ContienePrompt
begin
  Result := ContienePrompt(cad, fcDetPrompt.prIni, fcDetPrompt.prFin)>0;
end;
function TConfig.EsPrompt(const cad: string): boolean;
//Indica si la línea dada, es el prompt, de acuerdo a los parámetros dados. Esta función
//se pone aquí, porque aquí se tiene fácil acceso a las configuraciones del prompt.
var
  n: Integer;
begin
  if fcDetPrompt.DetecPrompt then begin  //si hay detección activa
    n := ContienePrompt(cad, fcDetPrompt.prIni, fcDetPrompt.prFin);
    Result := (n>0) and  (n = length(cad));
  end else begin
    Result := false;
  end;
end;

procedure TConfig.LeerDeVentana;
//Lee las propiedades de la ventana de configuración.
begin
  fraError := WindowToProp_AllFrames(self);
end;
procedure TConfig.MostEnVentana;
//Muestra las propiedades en la ventana de configuración.
begin
  fraError := PropToWindow_AllFrames(self);
end;
procedure TConfig.LeerArchivoIni(iniFile: string = '');
begin
  if iniFile = '' then
    msjError := ReadFileToProp_AllFrames(self, arINI)
  else
    msjError := ReadFileToProp_AllFrames(self, iniFile);
end;

procedure TConfig.escribirArchivoIni(iniFile: string='');
//Escribe el archivo de configuración
begin
  if iniFile ='' then
    msjError := SavePropToFile_AllFrames(self, arINI)
  else
    msjError := SavePropToFile_AllFrames(self, iniFile);
end;

procedure TConfig.SetLanguage(lang: string);
//Rutina de traducción
begin
  fcDetPrompt.SetLanguage(lang);

  case lowerCase(lang) of
  'es': begin
      TTreeNodeFromId('1',TreeView1).Text:='Conexión';
      TTreeNodeFromId('2',TreeView1).Text:='Detec.de Prompt';
      TTreeNodeFromId('3',TreeView1).Text:='Terminal';
    end;
  'en': begin
      TTreeNodeFromId('1',TreeView1).Text:='Connection';
      TTreeNodeFromId('2',TreeView1).Text:='Prompt detection';
      TTreeNodeFromId('3',TreeView1).Text:='Terminal';
    end;
  end;
end;

end.

