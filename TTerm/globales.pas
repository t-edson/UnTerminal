{
Unidad con declaraciones globales del proyecto
                 Creado por Tito Hinostroza - 01/08/2014
}
unit Globales; {$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Forms, SynEdit, SynEditKeyCmds, MisUtils,
      SynEditTypes, StrUtils, lclType, FileUtil,
      types, LazLogger, LazUTF8, Menus ;

const
  NOM_PROG = 'Tito''s Terminal';   //nombre de programa

var
   //Variables globales
   MsjError    : String;    //Bandera - Mensaje de error

   rutApp     : string;      //ruta de la aplicación
   rutMacros  : string;      //ruta de la carpeta de macros
   rutScripts : string;      //ruta de la carpeta de scripts
   rutLenguajes: string;     //ruta para guardar las sintaxis

   archivoEnt  : string;    //archivo de entrada
   MostrarError: Boolean;   //Bandera para mostrar mensajesde error.
   ActConsSeg  : Boolean;   //Activa consultas en segundo plano

//Funciones para control del editor
function NombDifArc(nomBase: String): String;
procedure LeeArchEnMenu(arc: string; mn: TMenuItem; accion: TNotifyEvent);
procedure CopiarMemu(menOrig, menDest: TMenuItem);

implementation

//Funciones para control del editor
function NombDifArc(nomBase: String): String;
{Genera un nombre diferente de archivo, tomando el nombre dado como raiz.}
const MAX_ARCH = 10;
var i : Integer;    //Número de intentos con el nombre de archivo de salida
    cadBase : String;   //Cadena base del nombre base
    extArc: string;    //extensión

  function NombArchivo(i: integer): string;
  begin
    Result := cadBase + '-' + IntToStr(i) + extArc;
  end;

begin
   Result := nomBase;  //nombre por defecto
   extArc := ExtractFileExt(nomBase);
   if ExtractFilePath(nomBase) = '' then exit;  //protección
   //quita ruta y cambia extensión
   cadBase := ChangeFileExt(nomBase,'');
   //busca archivo libre
   for i := 0 to MAX_ARCH-1 do begin
      If not FileExists(NombArchivo(i)) then begin
        //Se encontró nombre libre
        Exit(NombArchivo(i));  //Sale con nombre
      end;
   end;
   //todos los nombres estaban ocupados. Sale con el mismo nombre
End;
procedure LeeArchEnMenu(arc: string; mn: TMenuItem; accion: TNotifyEvent);
//Lee la carpeta de macros y actualiza un menú con el nombre de los archivos
//Devuelve la cantidad de ítems leidos
var
    Hay: Boolean;
    SR: TSearchRec;
    item: TMenuItem;
    n : integer;
begin
//  mn.Clear;
  // Crear la lista de ficheos en el dir. StartDir (no directorios!)
  n := 0;  //contador
  Hay := FindFirst(arc,faAnyFile - faDirectory, SR) = 0;
  while Hay do begin
     //encontró. Crea entrada
     item := TMenuItem.Create(nil);
     item.Caption:= SysToUTF8(SR.Name);  //nombre
     item.OnClick:=accion;
     mn.Add(item);
     //busca siguiente
     Hay := FindNext(SR) = 0;
     inc(n);
  end;
  if n = 0 then begin  //no encontró
     //encontró. Crea entrada
     item := TMenuItem.Create(nil);
     item.Caption:= 'vacío';  //nombre
     item.Enabled := false;
     mn.Add(item);
  end;
//  Result := n;
end;
procedure CopiarMemu(menOrig, menDest: TMenuItem);
//Copìa los ítems de un menú a otro
var
  it: TMenuItem;
  i: Integer;
begin
  menDest.Caption:=menOrig.Caption;
  menDest.Clear;
  for i := 0 To menOrig.Count - 1 do begin
    it := TMenuItem.Create(nil);
    it.Caption:= menOrig[i].Caption;
    it.OnClick:=menOrig[i].OnClick;
    it.Checked:=menOrig[i].Checked;
    menDest.Add(it);
  end;

end;

initialization
  //inicia directorios de la aplicación
  rutApp :=  ExtractFilePath(Application.ExeName);  //incluye el '\' final
  rutMacros := rutApp + 'macros';
  rutScripts := rutApp + 'scripts';
  rutLenguajes := rutApp + 'lenguajes';
  archivoEnt := '';    //archivo de entrada
  //verifica existencia de carpetas de trabajo
  try
    if not DirectoryExists(rutScripts) then begin
      msgexc('No se encuentra carpeta /scripts. Se creará.');
      CreateDir(rutScripts);
    end;
    if not DirectoryExists(rutMacros) then begin
      msgexc('No se encuentra carpeta /macros. Se creará.');
      CreateDir(rutMacros);
    end;
    if not DirectoryExists(rutLenguajes) then begin
      msgexc('No se encuentra carpeta /lenguajes. Se creará.');
      CreateDir(rutLenguajes);
    end;
  except
    msgErr('Error. No se puede leer o crear directorios.');
  end;

finalization
  //Por algún motivo, la unidad HeapTrc indica que hay gotera de memoria si no se liberan
  //estas cadenas:
  rutApp :=  '';
  rutMacros := '';
  rutScripts := '';
  rutLenguajes := '';
end.

