{
CfgFrame 0.8b
=============
Por Tito Hinostroza 09/03/2016

* Se agrega el enumerado tp_StrList_TStringGrid, para poder asociar StringList a Grillas.

Descripción
===========
Unidad con Frame para servir de base en la creación de Frames de configuración que
faciliten la administración de propiedades. Incluye el manejo de entrada y salida a
archivos INI.

Por Tito Hinostroza 10/07/2014
}
unit ConfigFrame;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Spin, IniFiles, Dialogs,
  ComCtrls, Graphics, EditBtn, Grids;

const
{  MSG_NO_INI_FOUND = 'No se encuentra archivo de configuración: ';
  MSG_ERR_WRIT_INI = 'Error leyendo de archivo de configuración: ';
  MSG_ERR_READ_INI = 'Error escribiendo en archivo de configuración: ';
  MSG_INI_ONLY_READ = 'Error. Archivo de configuración es de solo lectura';
  MSG_FLD_HAV_VAL = 'Campo debe contener un valor.';
  MSG_ONLY_NUM_VAL ='Solo se permiten valores numéricos.';
  MSG_NUM_TOO_LONG = 'Valor numérico muy grande.';
  MSG_MAX_VAL_IS  = 'El mayor valor permitido es: ';
  MSG_MIN_VAL_IS  = 'El menor valor permitido es: ';
  MSG_DESIGN_ERROR = 'Error de diseño.';
  MSG_NO_IMP_ENUM_T = 'Tipo enumerado no manejable.';}

  MSG_NO_INI_FOUND = 'No INI file found: ';
  MSG_ERR_WRIT_INI = 'Error writing to INI file: ';
  MSG_ERR_READ_INI = 'Error reading from INI file: ';
  MSG_INI_ONLY_READ = 'Error. INI file is only read';
  MSG_FLD_HAV_VAL = 'Filed must contain a value.';
  MSG_ONLY_NUM_VAL ='Only numeric values are allowed.';
  MSG_NUM_TOO_LONG = 'Numeric value is too large.';
  MSG_WRG_FLT_NUMB = 'Wrong float number.';
  MSG_MAX_VAL_IS  = 'The maximun allowed value is: ';
  MSG_MIN_VAL_IS  = 'The minimun allowed value is: ';
  MSG_DESIGN_ERROR = 'Design error.';
  MSG_NO_IMP_ENUM_T = 'Enumerated type no handled.';

type
  //Tipos de asociaciones
  TTipPar = (
   tp_Int_TEdit       //entero asociado a TEdit
  ,tp_Dbl_TEdit       //Double asociado a TEdit
  ,tp_Dbl_TFloatSpinEdit   //Double asociado a TFloatSpinEdit
  ,tp_Int_TSpinEdit   //entero asociado a TSpinEdit
  ,tp_Str_TEdit       //string asociado a TEdit
  ,tp_Str_TEditButton //string asociado a TEditButton (ancestro de TFileNameEdit, TDirectoryEdit, ...)
  ,tp_Str_TCmbBox     //string asociado a TComboBox
  ,tp_StrList_TListBox //StringList asociado a TListBox
  ,tp_StrList_TStringGrid //StringList asociado a TStringGrid
  ,tp_Bol_TCheckBox   //booleano asociado a CheckBox
  ,tp_TCol_TColBut    //TColor asociado a TColorButton
  ,tp_Enum_TRadBut    //Enumerado asociado a TRadioButton
  ,tp_Enum_TRadGroup  //Enumerado asociado a TRadioGroup
  ,tp_Bol_TRadBut     //Booleano asociado a TRadioButton
  ,tp_Int             //Entero sin asociación
  ,tp_Bol             //Boleano sin asociación
  ,tp_Str             //String sin asociación
  ,tp_StrList         //TStringList sin asociación
  );

  //Registro de asociación variable-control
  TParElem = record
    pVar: pointer;     //referencia a la variable
    lVar: integer;     //tamaño de variable. (Cuando no sea conocido)
    pCtl: TComponent;  //referencia al control
    radButs: array of TRadioButton;  //referencia a controles TRadioButton (se usan en conjunto)
    tipPar: TTipPar;   //tipo de par agregado
    etiqVar: string;   //etiqueta usada para grabar la variable en archivo INI
    minEnt, maxEnt: integer;  //valores máximos y mínimos para variables enteras
    minDbl, maxDbl: Double;  //valores máximos y mínimos para variables Double
    //Campos para configurar la grilla,cuando se use
    HasHeader  : boolean;  //Si incluye encabezado
    HasFixedCol: boolean;   //Si tiene una columna fija
    ColCount   : byte;     //Cantidad de columnas para la grilla
    //valores por defecto
    defEnt: integer;   //valor entero por defecto al leer de archivo INI
    defDbl: Double;    //valor double por defecto al leer de archivo INI
    defStr: string;    //valor string por defecto al leer de archivo INI
    defBol: boolean;   //valor booleano por defecto al leer de archivo INI
    defCol: TColor;    //valor TColor por defecto al leer de archivo INI
  end;

  { TCfgFrame }

  TCfgFrame = class(TFrame)
  private
    listParElem : array of TParElem;
    procedure AgregAsoc(r: TParElem);
  protected
    valInt: integer;  //valor entero de salida
    valDbl: Double;  //valor double de salida
  public
    secINI: string;   //sección donde se guardaran los datos en un archivo INI
    MsjErr: string;   //mensaje de error
    OnUpdateChanges: procedure of object;  {Se genera cuando se modifica alguna de las
                                            propiedades, del frame. Sea por que se lee de
                                            archivo o se cambia con controles.}
    procedure ShowPos(x, y: integer); virtual;
    function EditValidateInt(edit: TEdit; min: integer=MaxInt; max: integer=-MaxInt): boolean;
    function EditValidateDbl(edit: TEdit; min: Double=0; max: Double=1e6): boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure PropToWindow; virtual;
    procedure WindowToProp; virtual;
    procedure ReadFileToProp(var arcINI: TIniFile); virtual;
    procedure SavePropToFile(var arcINI: TIniFile); virtual;
    //Métodos para asociar pares: variable-control
    procedure Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
                             defVal: integer; minVal, maxVal: integer);
    procedure Asoc_Int_TSpinEdit(ptrInt: pointer; spEdit: TSpinEdit; etiq: string;
                             defVal: integer);
    procedure Asoc_Dbl_TEdit(ptrDbl: pointer; edit: TEdit; etiq: string;
                             defVal: double; minVal, maxVal: double);
    procedure Asoc_Dbl_TFloatSpinEdit(ptrDbl: pointer; spEdit: TFloatSpinEdit; etiq: string;
                             defVal: double);
    procedure Asoc_Str_TEdit(ptrStr: pointer; edit: TCustomEdit; etiq: string;
                             defVal: string);
    procedure Asoc_Str_TEditButton(ptrStr: pointer; edit: TCustomEditButton; etiq: string;
                             defVal: string);
    procedure Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
                             defVal: string);
    procedure Asoc_StrList_TListBox(ptrStrList: pointer; lstBox: TlistBox; etiq: string);
    procedure Asoc_Bol_TChkBox(ptrBol: pointer; chk: TCheckBox; etiq: string;
                             defVal: boolean);
    procedure Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
                             defVal: TColor);
    procedure Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
                    radButs: array of TRadioButton; etiq: string; defVal: integer);
    procedure Asoc_Enum_TRadGroup(ptrEnum: pointer; EnumSize: integer;
                    radGroup: TRadioGroup; etiq: string; defVal: integer);
    procedure Asoc_Bol_TRadBut(ptrBol: pointer;
                    radButs: array of TRadioButton; etiq: string; defVal: boolean);
    //métodos para agregar valores sin asociación a controles
    procedure Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
    procedure Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
    procedure Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
    procedure Asoc_StrList(ptrStrList: pointer; etiq: string);
  end;

  TlistFrames = array of TCfgFrame;

  //Utilidades para el formulario de configuración
  function IsFrameProperty(c: TComponent): boolean;
  function ListOfFrames(form: TForm): TlistFrames;
  function GetIniName(ext: string = 'ini'): string;
  procedure Free_AllConfigFrames(form: TForm);
  procedure Hide_AllConfigFrames(form: TForm);
  function ReadFileToProp_AllFrames(form: TForm; arIni: string): string;
  function SavePropToFile_AllFrames(form: TForm; arIni: string): string;
  function WindowToProp_AllFrames(form: TForm): TCfgFrame;
  function PropToWindow_AllFrames(form: TForm): TCfgFrame;
  function IdFromTTreeNode(node: TTreeNode): string;
  function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;

implementation
{$R *.lfm}
//Funciones de uso interno
function CodeStr(s:string): string;
{Protege a una cadena para que no pierda los espacios laterales si es que los tiene,
porque el el archivo INI se pierden. Además codifica el caracter "=", porque es
reservado en el archvio INI}
begin
  Result := '.'+s+'.';
  Result := StringReplace(Result, '=', #25, [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, LineEnding, #26, [rfReplaceAll]);  //protege caracter
end;
function DecodeStr(s:string): string;
{Quita la protección a una cadena que ha sido guardada en un archivo INI}
begin
  Result:=copy(s,2,length(s)-2);
  Result := StringReplace(Result, #25, '=', [rfReplaceAll]);  //protege caracter
  Result := StringReplace(Result, #26, LineEnding, [rfReplaceAll]);  //protege caracter
end;
//Utilidades para el formulario de configuración
function IsFrameProperty(c: TComponent): boolean;
//Permite identificar si un componente es un Frame creado a partir de
//esta unidad.
begin
  if (c.ClassParent.ClassName='TCfgFrame') then
     Result := true
  else
     Result := false;
end;
function ListOfFrames(form: TForm): TlistFrames;
//Devuelve la lista de frames del tipo TCfgFrame declarado aquí
var
  i: Integer;
  n : integer;
  f: TCfgFrame;
begin
  SetLength(Result,0);
  for i:= 0 to form.ComponentCount-1 do begin
    if IsFrameProperty(form.Components[i]) then begin
      f:=TCfgFrame(form.Components[i]);  //obtiene referencia
      n := high(Result)+1;    //número de elementos
      setlength(Result, n+1);  //hace espacio
      Result[n] := f;          //agrega
    end;
  end;
end;
function GetIniName(ext: string = 'ini'): string;
//Devuelve el nombre del archivo INI, creándolo si no existiera
var F:textfile;
begin
  Result := ChangeFileExt(Application.ExeName,'.'+ext);
  if not FileExists(Result) then begin
    ShowMessage(MSG_NO_INI_FOUND +Result);
    //crea uno vacío para leer las opciones por defecto
    AssignFile(F, Result);
    Rewrite(F);
    CloseFile(F);
  end;
end;
procedure Free_AllConfigFrames(form: TForm);
//Libera los frames de configuración
var
  f: TCfgFrame;
begin
  for f in ListOfFrames(form) do f.Free;
end;
procedure Hide_AllConfigFrames(form: TForm);
//oculta todos los frames de configuración
var
  f: TCfgFrame;
begin
  for f in ListOfFrames(form) do
    f.visible := false;
end;
function ReadFileToProp_AllFrames(form: TForm; arIni: string): string;
//Lee de disco, todas las propiedades de todos los frames de configuración.
//Si encuentra error devuelve el mensaje.
var
  appINI : TIniFile;
  f: TCfgFrame;
begin
  Result := '';
  if not FileExists(arIni) then exit;  //para que no intente leer
  Result := MSG_ERR_READ_INI + arIni;  //valor por defecto
  try
     appINI := TIniFile.Create(arIni);
     //lee propiedades de los Frame de configuración
     for f in ListOfFrames(form) do begin
       f.ReadFileToProp(appINI);
     end;
     Result := '';  //Limpia
  finally
     appIni.Free;                   //libera
  end;
end;
function SavePropToFile_AllFrames(form: TForm; arIni: string): string;
//Escribe a disco, todas las propiedades de todos los frames de configuración.
//Si encuentra error devuelve el mensaje.
var
   appINI : TIniFile;
   f: TCfgFrame;
begin
  Result := MSG_ERR_WRIT_INI + arIni;  //valor por defecto
  try
    If FileExists(arIni)  Then  begin  //ve si existe
       If FileIsReadOnly(arIni) Then begin
          Result := MSG_INI_ONLY_READ;
          Exit;
       End;
    End;
    appINI := TIniFile.Create(arIni);
    //escribe propiedades de los Frame de configuración
    for f in ListOfFrames(form) do begin
      f.SavePropToFile(appINI);
    end;
    Result := '';  //Limpia
  finally
    appIni.Free;                   //libera
  end;
end;
function WindowToProp_AllFrames(form: TForm): TCfgFrame;
//Llama al método WindowToProp de todos los frames de configuración.
//Si encuentra error devuelve el Frame que produjo el error.
var
  f: TCfgFrame;
begin
  Result := nil;
  //Fija propiedades de los controles
  for f in ListOfFrames(form) do begin
    f.WindowToProp;
    if f.MsjErr<>'' then exit(f);
  end;
end;
function PropToWindow_AllFrames(form: TForm): TCfgFrame;
//Llama al método PropToWindow de todos los frames de configuración.
//Si encuentra error devuelve el Frame que produjo el error.
var
  f: TCfgFrame;
begin
  Result := nil;
  //llama a PropToWindow() de todos los PropertyFrame.Frames
  for f in ListOfFrames(form) do begin
    f.PropToWindow;
    if f.MsjErr<>'' then exit(f);
  end;
end;
function IdFromTTreeNode(node: TTreeNode): string;
//Returns an ID with indication of the position of a TTreeNode'.
//It has the form: 1, 1.1, 1.2. Only works for two levels.
var
  nivel: Integer;
begin
  nivel := node.Level;
  if nivel = 1 then  //de dos niveles
    Result := IntToStr(node.Parent.Index+1) + '.' +
             IntToStr(node.Index+1)
  else  //de un nivel
    Result := IntToStr(node.Index+1);
end;
function TTreeNodeFromId(Id: string; tree: TTreeView): TTreeNode;
//Returns a TreeNode, given the ID position. If not found, returns NIL.
//Only works for two levels.
var
  list: TStringList;
  it: TTreeNode;
  Padre: TTreeNode;
  i: Integer;
begin
  Result := nil;  //por defecto
  if Id='' then exit;
  list := TStringList.Create;
  list.Delimiter:='.';
  list.DelimitedText:=Id;
  if list.Count = 1 then begin  //de un solo nivel
    //ubica el nodo
    for it in Tree.Items do if it.Level=0 then begin
        if IntToStr(it.Index+1) = list[0] then Result := it;
    end;
  end else begin  //de dos o más niveles
    //ubica al nodo padre
    Padre := nil;
    for it in Tree.Items do begin
      if it.Level=0 then begin
        if IntToStr(it.Index+1) = list[0] then Padre := it;
      end;
    end;
    if Padre = nil then exit;  //no lo ubica
    //ubica al nodo hijo
    for i := 0 to Padre.Count-1 do begin
      it := Padre.Items[i];
      if it.Level=1 then begin
        if IntToStr(it.Index+1) = list[1] then Result := it;
      end;
    end;
  end;
  list.Destroy;
end;

procedure TCfgFrame.AgregAsoc(r: TParElem);
//Agrega una asociación a listParElem[]
var n: integer;
begin
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
constructor TCfgFrame.Create(TheOwner: TComponent);
begin
  inherited;
  setlength(listParElem, 0)
end;
destructor TCfgFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TCfgFrame.PropToWindow;
//Muestra en los controles, las variables asociadas
var
  i,j:integer;
  r: TParElem;
  n: integer;
  d: Double;
  b: boolean;
  s: string;
  c: TColor;
  list: TStringList;
  gr: TStringGrid;
begin
  msjErr := '';
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //entero en TEdit
          //carga entero
          n:= Integer(r.Pvar^);
          TEdit(r.pCtl).Text:=IntToStr(n);
       end;
    tp_Int_TSpinEdit: begin  //entero en TSpinEdit
          //carga entero
          n:= Integer(r.Pvar^);
          TSpinEdit(r.pCtl).Value:=n;
       end;
    tp_Dbl_TEdit: begin
          //carga double
          d:= Double(r.Pvar^);
          TEdit(r.pCtl).Text:=FloatToStr(d);
      end;
    tp_Dbl_TFloatSpinEdit: begin
          //carga double
          d:=Double(r.pVar^);
          TFloatSpinEdit(r.pCtl).Value:=d;
      end;
    tp_Str_TEdit:  begin  //cadena en TEdit
          //carga cadena
          s:= String(r.Pvar^);
          TEdit(r.pCtl).Text:=s;
       end;
    tp_Str_TEditButton: begin
        //carga cadena
        s:= String(r.Pvar^);
        TEditButton(r.pCtl).Text:=s;
      end;
    tp_Str_TCmbBox: begin  //cadena en TComboBox
          //carga cadena
          s:= String(r.Pvar^);
          TComboBox(r.pCtl).Text:=s;
       end;
    tp_StrList_TListBox: begin  //lista en TlistBox
         //carga lista
         list := TStringList(r.Pvar^);
         TListBox(r.pCtl).Clear;
         for j:=0 to list.Count-1 do
           TListBox(r.pCtl).AddItem(list[j],nil);
      end;
    tp_StrList_TStringGrid: begin  //lista en TStringGrid
         //carga lista
         list := TStringList(r.Pvar^);
         gr := TStringGrid(r.pCtl);
         gr.Clear;
         gr.BeginUpdate;
         if r.HasFixedCol then gr.FixedCols:=1 else gr.FixedCols:=0;
         gr.ColCount:=r.ColCount;  //fija número de columnas
         if r.HasHeader then begin
           //Hay encabezado
           gr.RowCount:=list.Count+1;  //deja espacio para encabezado
           for j:=0 to list.Count-1 do begin
             gr.Cells[0,j+1] := list[j];
           end;
         end else  begin
           //No hay encabezado
           gr.RowCount:=list.Count;
           for j:=0 to list.Count-1 do begin
             gr.Cells[0,j] := list[j];
           end;
         end;
         gr.EndUpdate();
      end;
    tp_Bol_TCheckBox: begin //boolean a TCheckBox
          b := boolean(r.Pvar^);
          TCheckBox(r.pCtl).Checked := b;
       end;
    tp_TCol_TColBut: begin //Tcolor a TColorButton
          c := Tcolor(r.Pvar^);
          TColorButton(r.pCtl).ButtonColor := c;
       end;
    tp_Enum_TRadBut: begin //Enumerado a TRadioButtons
          if r.lVar = 4 then begin  //enumerado de 4 bytes
            n:= Int32(r.Pvar^);  //convierte a entero
            if n<=High(r.radButs) then
              r.radButs[n].checked := true;  //lo activa
          end else begin  //tamño no implementado
            msjErr := MSG_NO_IMP_ENUM_T;
            exit;
          end;
       end;
    tp_Enum_TRadGroup: begin
          if r.lVar = 4 then begin  //enumerado de 4 bytes
            n:= Int32(r.Pvar^);  //convierte a entero
            if n<TRadioGroup(r.pCtl).Items.Count then
              TRadioGroup(r.pCtl).ItemIndex:=n; //activa
          end else begin  //tamño no implementado
            msjErr := MSG_NO_IMP_ENUM_T;
            exit;
          end;
       end;
    tp_Bol_TRadBut: begin //Enumerado a TRadioButtons
          b:= boolean(r.Pvar^);  //convierte a entero
          if 1<=High(r.radButs) then
            if b then r.radButs[1].checked := true  //activa primero
            else r.radButs[0].checked := true  //activa segundo
       end;
    tp_Int:; //no tiene control asociado
    tp_Bol:; //no tiene control asociado
    tp_Str:; //no tiene control asociado
    tp_StrList:; //no tiene control asociado
    else  //no se ha implementado bien
      msjErr := MSG_DESIGN_ERROR;
      exit;
    end;
  end;
end;
procedure TCfgFrame.WindowToProp;
//Lee en las variables asociadas, los valores de loc controles
var
  i,j: integer;
  spEd: TSpinEdit;
  r: TParElem;
  list: TStringList;
  spFloatEd: TFloatSpinEdit;
begin
  msjErr := '';
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //entero de TEdit
          if not EditValidateInt(TEdit(r.pCtl),r.minEnt, r.MaxEnt) then
            exit;   //hubo error. con mensaje en "msjErr"
          Integer(r.Pvar^) := valInt;  //guarda
       end;
    tp_Int_TSpinEdit: begin   //entero de TSpinEdit
          spEd := TSpinEdit(r.pCtl);
          Integer(r.Pvar^) := spEd.Value;
       end;
    tp_Dbl_TEdit: begin  //double a TEdit
          if not EditValidateDbl(TEdit(r.pCtl),r.minDbl, r.MaxDbl) then
            exit;   //hubo error. con mensaje en "msjErr"
          Double(r.Pvar^) := valDbl;  //guarda
       end;
    tp_Dbl_TFloatSpinEdit: begin  //double a TFloatSpinEdit
          spFloatEd := TFloatSpinEdit(r.pCtl);
          //las validaciones de rango las hace el mismo control
          Double(r.pVar^) := spFloatEd.Value;
       end;
    tp_Str_TEdit: begin  //cadena de TEdit
          String(r.Pvar^) := TEdit(r.pCtl).Text;
       end;
    tp_Str_TEditButton: begin  //cadena de TEditButton;
          String(r.Pvar^) := TEditButton(r.pCtl).Text;
       end;
    tp_Str_TCmbBox: begin //cadena de TComboBox
          String(r.Pvar^) := TComboBox(r.pCtl).Text;
       end;
    tp_StrList_TListBox: begin //carga a TStringList
          list := TStringList(r.Pvar^);
          list.Clear;
          for j:= 0 to TListBox(r.pCtl).Count-1 do
            list.Add(TListBox(r.pCtl).Items[j]);
       end;
    tp_Bol_TCheckBox: begin  //boolean de  CheckBox
          boolean(r.Pvar^) := TCheckBox(r.pCtl).Checked;
       end;
    tp_TCol_TColBut: begin //TColor a TColorButton
          TColor(r.Pvar^) := TColorButton(r.pCtl).ButtonColor;
       end;
    tp_Enum_TRadBut: begin //TRadioButtons a Enumerado
          //busca el que está marcado
          for j:=0 to high(r.radButs) do begin
             if r.radButs[j].checked then begin
               //debe fijar el valor del enumerado
               if r.lVar = 4 then begin  //se puede manejar como entero
                 Int32(r.Pvar^) := j;  //guarda
                 break;
               end else begin  //tamaño no implementado
                 msjErr := MSG_NO_IMP_ENUM_T;
                 exit;
               end;
             end;
          end;
       end;
    tp_Enum_TRadGroup: begin //TRadioButtons a Enumerado
          //debe fijar el valor del enumerado
          if r.lVar = 4 then begin  //se puede manejar como entero
            Int32(r.Pvar^) := TRadioGroup(r.pCtl).ItemIndex;  //lee
            break;
          end else begin  //tamaño no implementado
            msjErr := MSG_NO_IMP_ENUM_T;
            exit;
          end;
       end;
    tp_Bol_TRadBut: begin //TRadioButtons a Enumerado
          //busca el que está marcado
          if high(r.radButs)>=1 then begin
             if r.radButs[1].checked then boolean(r.Pvar^) := true
             else boolean(r.Pvar^) := false;
          end;
       end;
    tp_Int:; //no tiene control asociado
    tp_Bol:; //no tiene control asociado
    tp_Str:; //no tiene control asociado
    tp_StrList:; //no tiene control asociado
    else  //no se ha implementado bien
      msjErr := MSG_DESIGN_ERROR;
      exit;
    end;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnUpdateChanges<>nil then OnUpdateChanges;
end;
procedure TCfgFrame.ReadFileToProp(var arcINI: TIniFile);
//Lee de disco las variables registradas
var
  i, n: integer;
  r   : TParElem;
  list: TStringList;
begin
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Int_TSpinEdit: begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Dbl_TEdit: begin
         Double(r.Pvar^) := arcINI.ReadFloat(secINI, r.etiqVar, r.defDbl);
       end;
    tp_Dbl_TFloatSpinEdit: begin
         Double(r.pVar^) := arcINI.ReadFloat(secINI, r.etiqVar, r.defDbl);
       end;
    tp_Str_TEdit: begin  //lee cadena
         String(r.Pvar^) := DecodeStr(arcINI.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
       end;
    tp_Str_TEditButton: begin  //lee cadena
         String(r.Pvar^) := DecodeStr(arcINI.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
       end;
    tp_Str_TCmbBox: begin  //lee cadena
         String(r.Pvar^) := DecodeStr(arcINI.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
       end;
    tp_StrList_TListBox: begin //lee TStringList
         list := TStringList(r.Pvar^);
         arcINI.ReadSection(secINI+'_'+r.etiqVar, list);
         //decodifica cadena
         for n:=0 to list.Count-1 do list[n] := DecodeStr(list[n]);
       end;
    tp_Bol_TCheckBox: begin  //lee booleano
         boolean(r.Pvar^) := arcINI.ReadBool(secINI, r.etiqVar, r.defBol);
       end;
    tp_TCol_TColBut: begin  //lee TColor
         TColor(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defCol);
       end;
    tp_Enum_TRadBut: begin  //lee enumerado como entero
         if r.lVar = 4 then begin
           Int32(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
         end else begin  //tamaño no implementado
           msjErr := MSG_NO_IMP_ENUM_T;
           exit;
         end;
       end;
    tp_Enum_TRadGroup: begin  //lee enumerado como entero
         if r.lVar = 4 then begin
           Int32(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
         end else begin  //tamaño no implementado
           msjErr := MSG_NO_IMP_ENUM_T;
           exit;
         end;
       end;
    tp_Bol_TRadBut: begin  //lee booleano
         boolean(r.Pvar^) := arcINI.ReadBool(secINI, r.etiqVar, r.defBol);
       end;
    tp_Int: begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Bol: begin  //lee booleano
         boolean(r.Pvar^) := arcINI.ReadBool(secINI, r.etiqVar, r.defBol);
       end;
    tp_Str: begin  //lee cadena
         String(r.Pvar^) := DecodeStr(arcINI.ReadString(secINI, r.etiqVar, '.'+r.defStr+'.'));
       end;
    tp_StrList: begin //lee TStringList
         list := TStringList(r.Pvar^);
         arcINI.ReadSection(secINI+'_'+r.etiqVar, list);
         //decodifica cadena
         for n:=0 to list.Count-1 do list[n] := DecodeStr(list[n]);
       end;
    else  //no se ha implementado bien
      msjErr := MSG_DESIGN_ERROR;
      exit;
    end;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnUpdateChanges<>nil then OnUpdateChanges;
end;
procedure TCfgFrame.SavePropToFile(var arcINI: TIniFile);
//Guarda en disco las variables registradas
var
  i,j: integer;
  r: TParElem;
  n: integer;
  b: boolean;
  s: string;
  c: TColor;
  strlst: TStringList;
  d: Double;
begin
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Int_TSpinEdit: begin //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Dbl_TEdit: begin  //escribe double
         d := Double(r.Pvar^);
         arcINI.WriteFloat(secINI, r.etiqVar, d);
    end;
    tp_Dbl_TFloatSpinEdit: begin
         d := Double(r.Pvar^);
         arcINI.WriteFloat(secINI, r.etiqVar, d);
    end;
    tp_Str_TEdit: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
       end;
    tp_Str_TEditButton: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
       end;
    tp_Str_TCmbBox: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
       end;
    tp_StrList_TListBox: begin  //escribe TStringList
          strlst := TStringList(r.Pvar^);
          arcINI.EraseSection(secINI+'_'+r.etiqVar);
          for j:= 0 to strlst.Count-1 do begin
            arcINI.WriteString(secINI+'_'+r.etiqVar,
                               CodeStr(strlst[j]),'');
          end;
       end;
    tp_Bol_TCheckBox: begin  //escribe booleano
         b := boolean(r.Pvar^);
         arcINI.WriteBool(secINI, r.etiqVar, b);
       end;
    tp_TCol_TColBut: begin  //escribe TColor
         c := Tcolor(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, c);
       end;
    tp_Enum_TRadBut: begin  //escribe enumerado
       if r.lVar = 4 then begin
         n := Int32(r.Pvar^);   //lo guarda como entero
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end else begin  //tamaño no implementado
         msjErr := MSG_NO_IMP_ENUM_T;
         exit;
       end;
    end;
    tp_Enum_TRadGroup: begin  //escribe enumerado
       if r.lVar = 4 then begin
         n := Int32(r.Pvar^);   //lo guarda como entero
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end else begin  //tamaño no implementado
         msjErr := MSG_NO_IMP_ENUM_T;
         exit;
       end;
    end;
    tp_Bol_TRadBut: begin  //escribe booleano
         b := boolean(r.Pvar^);
         arcINI.WriteBool(secINI, r.etiqVar, b);
       end;
    tp_Int: begin //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Bol: begin  //escribe booleano
         b := boolean(r.Pvar^);
         arcINI.WriteBool(secINI, r.etiqVar, b);
       end;
    tp_Str: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar,CodeStr(s));
       end;
    tp_StrList: begin  //escribe TStringList
          strlst := TStringList(r.Pvar^);
          arcINI.EraseSection(secINI+'_'+r.etiqVar);
          for j:= 0 to strlst.Count-1 do begin
            arcINI.WriteString(secINI+'_'+r.etiqVar,
                               CodeStr(strlst[j]),'');
          end;
       end;
    else  //no se ha implementado bien
      msjErr := MSG_DESIGN_ERROR;
      exit;
    end;
  end;
end;
//Métodos de asociación
procedure TCfgFrame.Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
  defVal: integer; minVal, maxVal: integer);
//Agrega un par variable entera - Control TEdit
var
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Int_TEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  r.minEnt := minVal;    //protección de rango
  r.maxEnt := maxVal;    //protección de rango
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Int_TSpinEdit(ptrInt: pointer; spEdit: TSpinEdit;
  etiq: string; defVal: integer);
//Agrega un par variable entera - Control TSpinEdit
var
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
  r.pCtl   := spEdit;    //toma referencia
  r.tipPar := tp_Int_TSpinEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Dbl_TEdit(ptrDbl: pointer; edit: TEdit; etiq: string;
  defVal: double; minVal, maxVal: double);
//Agrega un par variable double - Control TEdit
var
  r: TParElem;
begin
  r.pVar   := ptrDbl;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Dbl_TEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defDbl := defVal;
  r.minDbl := minVal;    //protección de rango
  r.maxDbl := maxVal;    //protección de rango
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Dbl_TFloatSpinEdit(ptrDbl: pointer;
  spEdit: TFloatSpinEdit; etiq: string; defVal: double);
var
  r: TParElem;
begin
  r.pVar   := ptrDbl;  //toma referencia
  r.pCtl   := spEdit;    //toma referencia
  r.tipPar := tp_Dbl_TFloatSpinEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defDbl := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Str_TEdit(ptrStr: pointer; edit: TCustomEdit;
  etiq: string; defVal: string);
//Agrega un par variable string - Control TEdit
var
  r: TParElem;
begin
  r.pVar   := ptrStr;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Str_TEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Str_TEditButton(ptrStr: pointer; edit: TCustomEditButton;
  etiq: string; defVal: string);
//Agrega un par variable string - Control TEditButton
var
  r: TParElem;
begin
  r.pVar   := ptrStr;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Str_TEditButton;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
  defVal: string);
//Agrega un par variable string - Control TEdit
var
  r: TParElem;
begin
  r.pVar   := ptrStr;     //toma referencia
  r.pCtl   := cmbBox;   //toma referencia
  r.tipPar := tp_Str_TCmbBox;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_StrList_TListBox(ptrStrList: pointer; lstBox: TlistBox;
  etiq: string);
var
  r: TParElem;
begin
  r.pVar   := ptrStrList;  //toma referencia
  r.pCtl   := lstBox;    //toma referencia
  r.tipPar := tp_StrList_TlistBox;  //tipo de par
  r.etiqVar:= etiq;
//  r.defCol := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Bol_TChkBox(ptrBol: pointer; chk: TCheckBox; etiq: string;
  defVal: boolean);
//Agrega un para variable booleana - Control TCheckBox
var
  r: TParElem;
begin
  r.pVar   := ptrBol;  //toma referencia
  r.pCtl   := chk;    //toma referencia
  r.tipPar := tp_Bol_TCheckBox;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
  defVal: TColor);
//Agrega un par variable TColor - Control TColorButton
var
  r: TParElem;
begin
  r.pVar   := ptrInt;    //toma referencia
  r.pCtl   := colBut;    //toma referencia a control
  r.tipPar := tp_TCol_TColBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defCol := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
  radButs: array of TRadioButton; etiq: string; defVal: integer);
//Agrega un par variable Enumerated - Controles TRadioButton
//Solo se permiten enumerados de hasta 32 bits de tamaño
var
  r: TParElem;
  i: Integer;
begin
  r.pVar   := ptrEnum;  //toma referencia
  r.lVar   :=EnumSize;  //necesita el tamaño para modificarlo luego
//  r.pCtl   := ;    //toma referencia
  r.tipPar := tp_Enum_TRadBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;   //se maneja como entero
  //guarda lista de controles
  setlength(r.radButs,high(radButs)+1);  //hace espacio
  for i:=0 to high(radButs) do
    r.radButs[i]:= radButs[i];

  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Enum_TRadGroup(ptrEnum: pointer; EnumSize: integer;
  radGroup: TRadioGroup; etiq: string; defVal: integer);
//Agrega un par variable Enumerated - Control TRadioGroup
//Solo se permiten enumerados de hasta 32 bits de tamaño
var
  r: TParElem;
begin
  r.pVar   := ptrEnum;   //toma referencia
  r.lVar   :=EnumSize;   //necesita el tamaño para modificarlo luego
  r.pCtl   := radGroup;  //toma referencia a control
  r.tipPar := tp_Enum_TRadGroup;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;   //se maneja como entero

  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Bol_TRadBut(ptrBol: pointer;
  radButs: array of TRadioButton; etiq: string; defVal: boolean);
//Agrega un par variable Enumerated - Controles TRadioButton
//Solo se permiten enumerados de hasta 32 bits de tamaño
var
  r: TParElem;
  i: Integer;
begin
  r.pVar   := ptrBol;  //toma referencia
//  r.pCtl   := ;    //toma referencia
  r.tipPar := tp_Bol_TRadBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;   //se maneja como entero
  //guarda lista de controles
  setlength(r.radButs,high(radButs)+1);  //hace espacio
  for i:=0 to high(radButs) do
    r.radButs[i]:= radButs[i];
  AgregAsoc(r);          //agrega
end;

procedure TCfgFrame.Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
//Agrega una variable Entera para guardarla en el archivo INI.
var
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Int;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
//Agrega una variable String para guardarla en el archivo INI.
var
  r: TParElem;
begin
  r.pVar   := ptrBol;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Bol;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
//Agrega una variable String para guardarla en el archivo INI.
var
  r: TParElem;
begin
  r.pVar   := ptrStr;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Str;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  AgregAsoc(r);          //agrega
end;
procedure TCfgFrame.Asoc_StrList(ptrStrList: pointer; etiq: string);
//Agrega una variable TStringList para guardarla en el archivo INI. El StrinList, debe estar
//ya creado, sino dará error.
var
  r: TParElem;
begin
  r.pVar   := ptrStrList;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_StrList;  //tipo de par
  r.etiqVar:= etiq;
//  r.defCol := defVal;
  AgregAsoc(r);          //agrega
end;

procedure TCfgFrame.ShowPos(x, y: integer);
//Muestra el frame en la posición indicada
begin
  Self.left:= x;
  Self.Top := y;
  Self.Visible:=true;
end;
function TCfgFrame.EditValidateInt(edit: TEdit; min: integer; max: integer): boolean;
{Valida el contenido de un TEdit, para ver si se puede convertir a un valor entero.
Si no se puede convertir, devuelve FALSE, devuelve el mensaje de error en "MsjErr", y
pone el TEdit con enfoque.
Si se puede convertir, devuelve TRUE, y el valor convertido en "valInt".}
var
  tmp : string;
  c : char;
  signo: string;
  larMaxInt: Integer;
  n: Int64;
begin
  Result := false;
  //validaciones previas
  larMaxInt := length(IntToStr(MaxInt));
  tmp := trim(edit.Text);
  if tmp = '' then begin
    MsjErr:= MSG_FLD_HAV_VAL;
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  if tmp[1] = '-' then begin  //es negativo
    signo := '-';  //guarda signo
    tmp := copy(tmp, 2, length(tmp));   //quita signo
  end;
  for c in tmp do begin
    if not (c in ['0'..'9']) then begin
      MsjErr:= MSG_ONLY_NUM_VAL;
      if edit.visible and edit.enabled and self.visible then edit.SetFocus;
      exit;
    end;
  end;
  if length(tmp) > larMaxInt then begin
    MsjErr:= MSG_NUM_TOO_LONG;
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  //lo leemos en Int64 por seguridad y validamos
  n := StrToInt64(signo + tmp);
  if n>max then begin
    MsjErr:= MSG_MAX_VAL_IS + IntToStr(max);
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  if n<min then begin
    MsjErr:= MSG_MIN_VAL_IS + IntToStr(min);
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  //pasó las validaciones
  valInt:=n;  //actualiza valor
  Result := true;   //tuvo éxito
end;

function TCfgFrame.EditValidateDbl(edit: TEdit; min: Double; max: Double): boolean;
{Valida el contenido de un TEdit, para ver si se puede convertir a un valor Double.
Si no se puede convertir, devuelve FALSE, devuelve el mensaje de error en "MsjErr", y
pone el TEdit con enfoque.
Si se puede convertir, devuelve TRUE, y el valor convertido en "valDbl".}
var
  d: double;
begin
  Result := false;
  //intenta convertir
  if not TryStrToFloat(edit.Text, d) then begin
    MsjErr:= MSG_WRG_FLT_NUMB;
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  //validamos
  if d>max then begin
    MsjErr:= MSG_MAX_VAL_IS + FloatToStr(max);
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  if d<min then begin
    MsjErr:= MSG_MIN_VAL_IS + FloatToStr(min);
    if edit.visible and edit.enabled and self.visible then edit.SetFocus;
    exit;
  end;
  //pasó las validaciones
  valDbl:=d;  //actualiza valor
  Result := true;   //tuvo éxito
end;

end.

