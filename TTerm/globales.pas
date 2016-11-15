{
Unidad con declaraciones globales del proyecto
                 Creado por Tito Hinostroza - 01/08/2014
}
unit Globales; {$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Forms;

var
   //Variables globales
   rutApp     : string;      //ruta de la aplicación

implementation


initialization
  //inicia directorios de la aplicación
  rutApp :=  ExtractFilePath(Application.ExeName);  //incluye el '\' final

finalization
  //Por algún motivo, la unidad HeapTrc indica que hay gotera de memoria si no se liberan
  //estas cadenas:
  rutApp :=  '';
end.

