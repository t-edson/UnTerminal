UnTerminal 0.6
===============

Unidad en Lazarus, para el control de procesos tipo consola, con detección de "prompt".

Esta unidad permite procesar la entrada y salida de procesos que manejen entrada y salida estándar de texto. Permite además detectar el prompt y el estado de "ocupado" y "listo".

Los procesos a controlar con esta unidad deben cumplir las siguientes condiciones:

1. Que se puedan lanzar como procesos de tipo consola.
2. Que su entrada y salida estándar sea de tipo texto.
3. Que tengan prompt. (No necesario si no importa detectar el prompt)
4. Que acepten comandos.

Los procesos que se pueden controlar con esta unidad son diversos, como clientes de telnet, ftp, o el mismo shell del sistema operativo, como se muestar en el ejemplo incluido.

En esta versión no permite leer el flujo de salida de errores.

Para conectarse mediante un proceso, se debe crear una instancia de TConsoleProc, y seguir la secuencia de conexión:

```
  p := TConsoleProc.Create(StatusBar1.Panels[1]);  //Crea conexión
  ...
  p.Free.
```

Opcionalmente se le puede pasar la referencia a un panel de una barra de estado, para
que se muestren íconos que indiquen el estado de la conexión (poner en NIL si no se
usará). En este caso se debe manejar el evento OnDrawPanel() de la Barra de estado:

```
 StatusBar1.OnDrawPanel:=@SBDrawPanel;
 ...
 procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
 begin
  if panel.Index = 1 then q.DrawStatePanel(StatusBar.Canvas, Rect);
 end;
```

 La conexión se maneja por estados. Una secuencia típica de estados al iniciar una
 conexión exitosa es:
 
 ECO_STOPPED -> ECO_CONNECTING -> ECO_READY

El estado de ECO_READY indica que el proceso está listo para recibir comandos y, se detecta cuando se recibe el prompt. Por lo tanto para que se pueda considerar a un proceso como ECO_READY, se debe cumplir que tenga un prompt y que el terminal esté configurado para detectarlo.

La secuencia de una conexión con error sería:
 
 ECO_STOPPED -> ECO_CONNECTING -> ...

Es decir, que no se detecta el prompt, sino que probablemente, el proceso se detiene con un mensaje de error. La detección de los mensajes de error, no se hace en esta unidad, porque pueden ser muy diversos. Se recomienda implementarlos en otra unidad o derivando una clase adicional.
 
Una vez el proceso se encuentre disponible. Un comando cualquiera, tendría el siguiente flujo:

ECO_READY -> ECO_BUSY -> ECO_READY

Está unidad está pensada para ser usada en conexiones lentas, y con volúmenes
considerables de datos, por ello se maneja la llegada de datos completamente por eventos.

Para el manejo de la pantalla usa un terminal VT100 sencillo, de la unidad TermVT. Sin
embargo, esta unidad ha sido diseñada para poder recibir el texto del VT100, en un
TStringList, y poder registrar permanentemente el contenido.

El proceso a lanzar, se inicia con el método Open() y se termina con el método Close().

Por defecto, los saltos de línea se enviarán al proceso como el caracter LF (que es usual en conexiones de tipo Telnet). Para cambiar a CR+LF (que es usual en el entorno Windows), se debe fijar la propiedad 'sendCRLF' en TRUE. 

Esta unidad solo ha sido probada en Windows, pero podría funcionar también en Linux.

# Configurando un Editor para mostrar la salida

Para configurar un editor para mostrar la salida del proceso, se debe usar los eventos:
OnInitScreen(), OnRefreshLine(), OnRefreshLines() y OnAddLine():

```
  proc.OnInitScreen:=@procInitScreen;
  proc.OnRefreshLine:=@procRefreshLine;
  proc.OnRefreshLines:=@procRefreshLines;
  proc.OnAddLine:=@procAddLine;
...

procedure TfrmPrincipal.procInitScreen(const grilla: TtsGrid; fIni, fFin: integer);
begin
  for i:=fIni to fFin do SynEdit1.Lines.Add(grilla[i]);
end;
procedure TfrmPrincipal.procRefreshLine(const grilla: TtsGrid; fIni, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;
  SynEdit1.Lines[yvt+fIni] := grilla[fIni];
end;
procedure TfrmPrincipal.procRefreshLines(const grilla: TtsGrid; fIni, fFin, HeightScr: integer);
begin
  yvt := SynEdit1.Lines.Count-HeightScr-1;  //calcula fila equivalente a inicio de VT100
  for f:=fIni to fFin do SynEdit1.Lines[yvt+ f] := grilla[f];
end;
procedure TfrmPrincipal.procAddLine;
begin
  SynEdit1.Lines.Add('');
end;
```

El evento OnInitScreen(), es llamado solo una vez al inicio para dimensionar el StringList, de modo que pueda contener a todas las líneas del terminal VT100.

Para limpiar el contenido del terminal, se debe llamar al método ClearTerminal(), que además, reinicia la posición del cursor, poniéndolo en (1,1).

Llamar a ClearTerminal(), no tendrá efecto, sobre el estado de la conexión o del proceso en curso, sino solamente en el contenido del terminal.

# Manejo del Terminal

Los comandos, se envían al proceso con los método Send(), SendLn() o SendFile(). SendLn() es similar a Send(), pero envía adicionalmente un salto de línea al final. El salto de línea es necesario para que el proceso del terminal reconozca que se le ha enviado un comando.

El salto de línea a enviar se configura con la propiedad 'sendCRLF'. Por defecto el salto de línea es el caracter LF, pero si 'sendCRLF' se pone a TRUE, el salto de línea enviado con SendLn(), es CR+LF. 'sendCRLF', solo tiene efecto cuando se usa SendLn() o SendFile(). 
Desde luego, que se puede enviar cualquier tipo de salto de línea o caracter si es que se usa el método Send().

El método SendFile(), envía el contendio completo de un archivo al terminal. El salto de línea enviado depende del valor de 'sendCRLF'.

Para enviar caracteres de control, se debe usar el método SendVT100Key(), porque los caracteres de control, deben convertirse primero en secuencias de escape, al menos para los aplicativos que reconozcan las secuencias de escape. Por ejemplo si se envía la tecla direccional derecha, con SendVT100Key(), primero se transformará en la secuencia ESC+[C.

Para enviar simples comandos (que son secuencias de caracteres imprimibles), basta con usar SenLn().

En principio, solo debería mandarse un solo comando por SendLn() porque, enviar más de un comando podría hacer que el proceso pase repetidamente por  las fases:

ECO_BUSY -> ECO_READY -> ECO_BUSY ... -> ECO_READY.

Haciendo que se pase de un estado de libre a ocupado de forma automática, y pudiendo complicar a alguna rutina que esté programada para enviar más comandos cuando detecte que el proceso se encuentre libre.

Los datos de salida que van llegando del proceso, no se guardan completamente en la
clase. Solo se mantienen las líneas de trabajo del terminal VT100 en el objeto "term".
Así se ahorra memoria, porque por lo general, el texto de salida está destinado a ser
almacenado en algún otro control como un editor de texto o una grilla.
Es responsabilidad del programador, limitar el tamaño de los datos almacenados.

El evento OnLineCompleted(), se genera cuando se detecta la llegada del caracter Chr(13) que es el salto de línea. Además pasa como parámetro a la línea actual. Este evento puede servir para detectar cuando ha llegado una línea completa.

Los datos de salida que llegan por el terminal, se recuperan por sondeo del flujo de salida. Por defecto, se explora la salida en un intervalo de 50 milisegundos, y usando un "buffer" interno de 2048 bytes.

Para cambiar la frecuencia de sondeo de la salida del proceso, se debe cambiar el valor de la propiedad 'clock.interval'. Pero no es recomendable.

# Detección del Prompt

La detección del Prompt se hace explorando las cadenas de texto que van llegando del proceso, para ver si coinciden con los parámetros de la detección. Esta forma puede no ser muy confiable si el Prompt del proceso es muy cambiante.

Para configurar la detección del Prompt se debe poner la propiedad 'detecPrompt' en TRUE y fijar valores para 'promptIni' y 'promptFin'. Estas cadevas determinan la parte inicial y final del prompt.

Si el prompt es fijo y no cambia, se puede poner su valor directamente en 'promptIni' y se deja 'promptFin' vacío. Pero si el prompt es variable, se puede poner la parte inicial en 'promptIni' y la parte final en 'promptFin', pero eligiendo valores que no den lugar a confusión con líneas de datos.

Así, por ejemplo para detectar un prompt de tipo:

```[usuario@localhost ~]$ ```

Se puede configurar: promptIni='[' y promptFin = '$ '. Los espacios son también parte del texto que se debe configurar.

Por defecto se espera que el prompt encontrado sea exactamnente igual a la última línea del texto que llega por el terminal. Pero existen opciones adicionales. El tipo de coincidencia se puede configurar en la variable 'promptMatch'. Puede tener los siguientes valores:

   prmExactly,   //prompt es la línea entera
   prmAtBegin,   //prompt aparece al inicio de la línea
   prmAtEnd,     //prompt aparece al final de la línea
   prmAtAnyPos   //prompt aparece en cualquier parte de la línea

Por defecto, 'promptMatch' está en 'prmExactly'.

Tambíen se puede usar la función de configuración automática del prompt, que se ejecuta llamando al método AutoConfigPrompt. Pero este método debe ser llamado cuando se tenga el Pprompt visible en la última línea del terminal.

'AutoConfigPrompt' lee la última línea asumiendo que es el pprompt y fija valores automáticamente para 'promptIni' y 'promptFin'.

Se puede usar también una rutina personalizada para la detección deñ prompt. Esta se debe enganchar al evento OnChkForPrompt() que tiene la forma:

function(lin: string): boolean;

El evento recibe la línea a explorar y debe devolver TRUE, si es que se determina que la línea actual contiene al prompt. Al activar este evento, se desactiva la detección interna de la unidad.