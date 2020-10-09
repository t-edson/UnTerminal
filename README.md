UnTerminal 1.0
==============

By Tito Hinostroza

Lazarus unit for control external programs or console processes, with prompt detection. Tested in Linux and DOS consoles.

This unit allows to launch a process and interact with it trough the standard input and output.

Features:

* Works as a wrapper for TProcess.
* Includes detection of prompt.
* Assigns states for a process, like "Ready" or "Busy".
* Is event driven.
* Recognize some VT100 sequences.

Programs to be controled with this unit should comply:

1. That can be launched as a console command.
2. That input and output are text format.

Optionally they can:

3. Have a prompt.
4. Be controled by commands (Si se quiere enviar información al proceso).

The processes that can be controlled with this unit are diverse, such as telnet clients, ftp, or the operating system shell itself, as shown in the included example.

In the current version, the error output stream cannot be read independently. Only two flows are controlled:

1. Input
2. Output (stdout and stderr).

![Tito's Terminal](https://github.com/t-edson/UnTerminal/blob/1.0/screen1.png "Título de la imagen")

## Hello world

The simplest code to launch a command/process/program and capture the output can be:

```
var outputText: string;

  proc:= TConsoleProc.Create(nil);
  proc.RunInLoop('bash -c "ls" ','', -1, outputText);
```

This code executes the command 'bash -c "ls" ' and captures the output (and the errors) in the variable "outputText".

This code works for linux but can be used in windows too, usign DOS commands.


## Executing commands

To execute a command, first you need to create an instance of the class TConsoleProc:

```
  p := TConsoleProc.Create(nil);  //Create conecction
  ...

```

Then you need to execute the command, choosing the appropriate way.

There are several ways to execute commands using UnTerminal. They can be clasified in two types:

A) Running in a finite loop (or until the process finishes).
B) Running in a infinite loop (or until the program finishes).

Both methods can use events to work.

The option A, is implemented using the method RunInLoop(). There are several versions of this method:
 
```
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer = -1): boolean;
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer;
    var progOut: TStringList): boolean;
  function RunInLoop(progPath, progParam: string; TimeoutSegs: integer; out
    outText: String): boolean;
```

The idea of this way is to send a command and get the output/error text in a variable, generally without interacting with the process.

The parameter "progPath" is the name of the program or commnand to be executed. Consider that popular commands of DOS or Linux, are not programs, so we cannot launch "dir" or "ls" in "progPath". Instead we need to call the corresponding command processor and pass the commands, like "cmd /c dir" or 'bash -c "ls"'.

The parameter "progParam" is a string containing the paremeters of the program or commnand to be executed. They can include in "progaPath" too, but it's advisable to include them separately.

As an example the next commands are similar:

```
  proc.RunInLoop('cmd','/c dir', -1, outText);
```

```
  proc.RunInLoop('cmd /c dir','', -1, outText);
```

If the program or command doesn't exist, the instruction RunInLoop() will raise an exception in runtime.

The parameters "TimeoutSegs" is the numbers of seconds to wait the command finishes, before continue executing the next instructions. The value -1 indicates, the instruction will wait until the command finishes.

The option B is described in the next sections of this document.

## States of the process

The connection is managed by states. The possible states of the process are:

* ECO_STOPPED .- The Process has not started. This condition always occurs after creating the connection, or after closing an open connection. There may be data pending in the "buffer"

* ECO_CONNECTING .- This condition occurs after opening the connection with the indicated process. Terminates when the prompt is detected. If the prompt is not detected (whether it does not arrive, or it has not been configured correctly), the process will always remain in this state.

* ECO_ERROR_CON .- Occurs when after trying to open the process, an error occurs, for example, when referring to a file that does not exist.

* ECO_READY .- This process starts when the prompt is detected (if prompt detection is used). Indicates that the process is ready to accept commands. It remains in this state until a command is received to send to the process.

* ECO_BUSY .- This state starts after sending a command, when the process was in ECO_READY state. It remains in this state, until the prompt is detected and the ECO_READY state is fired.

The process can only be in one of these states at a time. The transition between states has a logical sequence, but can have changes.

A typical sequence of states when initiating a successful connection is:
 
 ECO_STOPPED -> ECO_CONNECTING -> ECO_READY

The ECO_READY status indicates that the process is ready to receive commands and is detected when the prompt is received. Therefore, for a process to be considered as ECO_READY, it must have a prompt and that the terminal is configured to detect it.

The sequence of a failed connection would be:
 
 ECO_STOPPED -> ECO_CONNECTING -> ...

That is, the prompt is not detected, but the process probably stops with an error message. Error messages detection is not done on this unit, because they can be very diverse. It is recommended to implement them in another unit or by deriving an additional class.
 
Once the process is available. Any command would have the following flow:

ECO_READY -> ECO_BUSY -> ECO_READY

This unit is thought to be used in slow connections, and with long data responses, therefore the arrival of data is handled completely by events.

To operate the display, use a simple VT100 terminal from the TermVT unit. However, this unit has been designed to be able to receive the text from the VT100, in a TStringList, and to be able to permanently record the content.

The process to be launched starts with the Open() method and ends with the Close() method.

## Configurando un Editor para mostrar la salida

El objetivo del uso de UnTerminal es poder mostrar la salida de forma cómoda para el usuario.

"UnTerminal", ha sido diseñado para manejar procesos lentos y con salida masiva de datos. Por ello la salida de datos se maneja enteramente por eventos. Además se supone que se requiere guardar un registro en pantalla, de la información que se va campturando. Por ello los eventos están orientado a manejar un objeto "TStrings", que permite ir agregando líneas nuevas e ir guardando las líneas anteriores (registro de salida).

Existen diversos grupos de eventos para manejar la salida de datos. Se definen 4 métodos para el control de la salida de datos:

1. Salida como terminal VT100.
2. Salida línea por línea.
3. Salida línea por línea con línea parcial.
4. Salida línea por línea con línea parcial en prompt.

Cada uno de estos métodos tiene sus ventajas y desventajas. Y se debe usar el que se adecue mejor a las necesidades de la aplicación. Así para implementar un cliente de Telnet, se debería usar el método 1 (el más completo), pero para otros procesos bastará usar métodos más simples.

Podría implementarse un método adicional que consiste en interceptar directamente los eventos del terminal (propiedad term: TTermVT100) de TConsoleProc, para mostrar la información en pantalla. Sin embargo este método de trabajo, es de nivel más bajo, pero puede servir para capturar el contenido completo del terminal VT100, cada vez que se produzca un cambio (lo cual no sería muy eficiente, desde luego). 

Implementar la salida de datos, no afecta en la detección del prompt. Las rutinas de detección trabajan directamente sobre los bloques de datos que van llegando del proceso, sin necesidad de que estos datos se muestren.

### Salida como Terminal VT100

Este es el método más completo de manejo de salida del terminal. Permite reconocer las secuencias de escape que controlan el cursor, o manipulan el texto del terminal. 

Se puede decir que este es el método oficial para manejo de cualquier proceso, inclusive si genera secuencias de escape ANSI.

Para configurar un editor en esta forma, se deben usar los eventos: OnInitScreen(), OnRefreshLine(), OnRefreshLines() y OnAddLine().

En el siguiente código, se usa un editor SynEdit, como salida para un proceso cualquiera. 

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

El evento OnInitScreen(), es llamado solo una vez al inicio para dimensionar el StringList, de modo que pueda contener a todas las líneas del terminal VT100. Es necesario que el editor de salida tenga al menos la cantidad de líneas que tiene el terminal VT100 (25 por defecto), de otra forma se geenrará un error en tiempo de ejecución. Esta condición es compresible, si tenemos en cuenta que se trata de "encajar" una pantalla virtual de un termninal (que puede ser de 80 * 25 líneas) en un contenedor TString.

Para posicionar el cursor virtual del terminal en alguna línea del TString, se debe hace primero el cálculo[1]:

  yvt := SynEdit1.Lines.Count-HeightScr-1;

donde 'HeightScr' es la altura actual del terminal VT100, que es porporcionado por loe eventos de  TConsoleProc.

[1] Desde luego si no se quisiera guardar el registro de las líneas anteriores, no sería necesario complicarnos con los cálculos y solamente necesitaríamos tener un TString con la misma cantidad de líneas del VT100, y evitaríamos calcular la posición vertical porque sería la misma en el terminal y en el TString.

Para limpiar el contenido del terminal, se debe llamar al método ClearTerminal(), que además, reinicia la posición del cursor, poniéndolo en (1,1).

Llamar a ClearTerminal(), no tendrá efecto, sobre el estado de la conexión o del proceso en curso, sino solamente en el contenido del terminal.

### Salida Línea por Línea

El método descrito en la sección anterior (usando los eventos OnInitScreen(), OnRefreshLine(), OnRefreshLines() y OnAddLine()), es por así decirlo, la manera formal de controlar la salida. Este método considera que el proceso puede manejar secuencias ANSI de escape para el control de la pantalla del terminal virtual. 

Sin embargo, este método puede resultar complicado al momento de manejar la posición del cursor o cuando se quiere conmutar desde un editor de salida a otro.

Si nuestro proceso a controlar, no manejará secuencias ANSI, sino que simplemente enviará texto plano sin comandos de borrado o manejo de cursor, entonces podemos usar formas alternativas más simples de mostrar la información en pantalla.

El método más simple sería usar solamente el evento OnLineCompleted():

OnLineCompleted:TEvLinCompleted;

Que se genera cada vez que se detecta un salto de línea en la llegada de datos. Así podríamos usar este método para ir agregando líneas a nuestro editor de salida, sin preocuparnos en iniciar la pantalla, o de la información anterior que pueda contener.

Nuestro código sería algo como esto:

```
  proc.OnLineCompleted:=@procOnLineCompleted;
...

procedure TForm1.procOnLineCompleted(const lin: string);
begin
  SynEdit1.Lines.Add(lin);
end;
```

Esta forma minimalista funcionará bien, mostrando los datos de llegada, pero con un aparente retraso, porque para que se muestre una línea, esta debe estar completa. De modo que cuando llegue el prompt (o una línea incompleta), esta línea no se mostrará en el editor (a pesar de que el proceso podría pasar al estado ECO_READY). 

El hecho de que no se muestre la última línea, no implica que no se mostrará más. Esta se hará visible al recibir el salto de línea correspondiente, a menos que se  cierre la conexión antes.

Este método de captura de salida es útil cuando se requiere capturar la salida masiva de datos sin que sea muy importante la correcta visualización del prompt o líneas intermedias.

Para determinar cuando se termina de recibir los datos, se puede usar la detección del prompt, si es que el proceso lo permite.

### Salida línea por línea con línea parcial

El método de línea por línea es simple y seguro. Pero no muestra la última línea. Para que se muestre la última línea completa, es necesario que se reciba el salto de línea correspondiente. Así podemos decir que en este modo de trabajo, no se mostrará nunca la última línea recibida (a menos claro, que el último caracter recibido sea el salto de línea y consideremos que la última línea es la anterior al salto).

Para salvar este inconveniente, se puede usar la conbinación de eventos:

  OnLineCompleted: TEvLinCompleted; 
  OnReadData     : TEvReadData;   
 	
El evento OnReadData(), se genera cuando se termina de leer un bloque de datos y por lo general, se termina con una línea final sin salto (puede ser el prompt). Entonces, lo que deberíamos hacer es completar reemplazar esta línea, si es que luego recibimos el evento OnLineCompleted().

El código de trabajo, se parecerá a este:

```
var LinPartial: boolean = false;

  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnReadData:=@procReadData;
...
  
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
```

La variable 'LinPartial' nos sirve para saber cuando se ha recibido una línea parcial.

Este método de trabajo funcionará bastante bien y es simple, pero podría darse el caso (poco común), en el que se reciban dos veces seguidas el evento OnReadData(), haciendo que se genere una línea adicional en el editor. Este caso se podría dar cuando, se genera una línea muy larga que llegue en más de un bloque, o en conexiones muy lentas.

Para evitar este efecto, se podría incluir una rutina de protección adicional que verificara que el evento OnReadData(), se está produciendo dos veces seguidas y de ser así, evitar agregar una línea más.

El uso de los eventos OnLineCompleted()-OnReadData() es una forma sencilla de recbir los datos "linea por línea", pero no debemos olvidar que solo debe aplicarse a procesos con salida simple de texto sin saltos de línea o borrado de texto. La mayoría de procesos caen en esta categoría (como clientes de FTP o hasta el mismo shell de DOS), pero procesos como un cliente de Telnet o SSH, usan secuencias de escape que pueden requerir posicionar el cursor para sobreescribir bloques. 

En estos casos no se debería implementar este método de captura de salida; pero inclusive si se implementara aquí, podríamos tarbajar bien, mientras no se generen saltos del cursor o borrado de texto. Las secuencias de escape que cambian atributos del texto no afectan en la salida del texto, porque son procesadas (o mejor dicho ignoradas) por TConsoleProc, de modo que los eventos de salida de texto no contienen secuencias ANSI de ningún tipo, en sus parámetros.

### Salida línea por línea con línea parcial en Prompt

Como una variación del método anterior, se presenta este método que es muy similar, pero con la diferencia que la detección de líneas parciales se hace solo para cuando se detecte el promtp.
 
De esta forma, se evita generar muchas sobre-escrituras de líneas, ya que solo se hará en la línea del prompt. Este método es más eficiente cuando se tiene varios paquetes de datos entre prompt y prompt.

Una desventaja de este método es que, es necesario que se tenga configurado el prompt, y este aparezca siempre al final de un bloque de datos, para que se refresque la última línea, en caso contrario no se refrescará de ninguna forma.

Por lo tanto solo es recomendable para procesos que siempre muestran el prompt. No es recomendable por ejemplo, en un proceso que el algún momento mostrará un mensaje como  "Ingrese un valor: ", esperando una entrada del usuario. Por lo tanto hay que ser cauteloso cuando se use este método de captura de salida. 

La implementación de la captura, es igual al caso anterior, solo que se usa otro par de eventos:

  OnLineCompleted: TEvLinCompleted; 
  OnLinePrompt: TEvLinCompleted; 

El evento OnLinePrompt(), se genera cuando se recibe la línea que contiene al prompt, aunque no esté completa. Entonces, lo que deberíamos hacer es completar reemplazar esta línea, si es que luego recibimos el evento OnLineCompleted().

El código de trabajo, se parecerá a este:

```
var LinPrompt: boolean = false;

  proc.OnLineCompleted:=@procLineCompleted;
  proc.OnLinePrompt:=@procLinePrompt;
...
  
procedure TForm1.procLineCompleted(const lin: string);
begin
  if LinPrompt then begin
    //Estamos en la línea del prompt
    SynEdit1.Lines[SynEdit1.Lines.Count-1] := lin;  //reemplaza última línea
    LinPrompt := false;
  end else begin  //caso común
    SynEdit1.Lines.Add(lin);
  end;
end;

procedure TForm1.procLinePrompt(const lin: string);
begin
  LinPrompt := true;   //marca bandera
  SynEdit1.Lines.Add(lin);   //agrega la línea que contiene al prompt
end;
```

La variable 'LinPrompt', nos sirve para detectar cuando se ha recibido el prompt en una línea.

Este método de trabajo también también puede sufrir del problema del método anterior si, es que se recibe una línea larga (donde se detecte el prompt) que viene en más de un bloque. La solución a aplicar es la misma.

Aunque el efecto de este caso extremo puede no ser muy crítico, ya que solo generará una línea adicional con el prompt en el editor de salida. Esta línea adicional no quitará la legibilidad del código pero de todas formas, es una falta de fidelidad en la salida del proceso.
  
## Terminal use

### Sending commands

The commands (or any string in general), are sent to the process with the Send(), SendLn() or SendFile() methods.

SendLn() is similar to Send(), but additionally sends a newline character(s) at the end. The newline character(s) is necessary, in most processes, so that the sent string is recognized, as a command.

The SendFile() method sends the complete content of a file to the program.

The newline character(s) to send depends on the value of "LineDelimSend".

To send control characters, the SendVT100Key() method must be used, because control characters must first be converted into escape sequences, at least for applications that recognize escape sequences. For example, if the right directional key is sent, with SendVT100Key(), it will first be transformed into the sequence: ESC + \[C.

To send simple commands (which are printable character sequences), just use SenLn().

In principle, only a single command should be sent by SendLn() because, sending more than one command could make the process go through the phases repeatedly:

ECO_BUSY -> ECO_READY -> ECO_BUSY ... -> ECO_READY.

Making it go from a free to busy state automatically, and being able to complicate a routine that is programmed to send more commands when it detects that the process is ready.

The output data that comes from the process are not completely saved in the class. Only the working lines of a VT100 terminal are kept in the "term" object. This saves memory, because generally the output text is intended to be stored in some other control such as a text editor or grid.

It is the responsibility of the programmer to limit the size of the stored data.

The process output data, arriving through the terminal, is retrieved by polling the output stream. By default, the output is scanned in intervals of 50 milliseconds (20 times per second), and using an internal 2048-byte "buffer".

To change the polling period of the process output, you must change the value of the 'clock.interval' property (in milliseconds). You can work well with periods of 100 msec or even more, if the amount of information is low. But it is not recommended to go below 50 msec, because an additional CPU load is generated (even when no data arrives). This period should only be lowered when it is required to process the arrival data, immediately.

### Line-ending in sending

Inside UnTerminal, you can configure the line break characters that will be used, both for sending and receiving.

To set the line-ending characters in sending, the "LineDelimSend" property must be used, which can take the following values:

* LDS_CRLF   //Sends CR and LF characters
* LDS_CR     //Send only CR (character #13)
* LDS_LF     //Send only LF (character #10) 

Setting the line-ending for sending implies that the configured characters will be added, each time the TConsoleProc.SendLn() or TConsoleProc.SendFile() methods are used, regardless of the type of delimiter that has been included in the string. The following examples will illustrate the behavior of SendLn(), when "LineDelimSend" is set.

If LineDelimSend has been defined in LDS_LF, then:

SendLn('aaa');             //Will actually send 'aaa'#10
SendLn('aaa'#10);          //Will actually send 'aaa'#10
SendLn('aaa'#13);          //Will actually send 'aaa'#10
SendLn('aaa'#13#10);       //Will actually send 'aaa'#10
SendLn('aaa'#13#10'bbb');  //Will actually send 'aaa'#10'bbb'#10

As you can see, when a newline character is set, all the newlines are changed so that only the configured newline is used.

In general, "LineDelimSend" should be set to LDS_LF, for Linux / UNIX processes, and should be left at LDS_CRLF, for Windows processes.

The "LineDelimSend" property has no effect on the TConsoleProc.Send() method, which will always send the indicated characters.

### Line-ending in receiving

To set the line-ending characters, at the reception, you must use the "LineDelimRecv" property, which can take the following values:

* LDR_CRLF  //The line break is CR-LF (or LF-CR).
* LDR_CR    //The line break is this character. LF is ignored.
* LDR_LF    //The line break is this character. CR is ignored.
* LDR_CR_LF //The line break is CR or LF
   
When "LineDelimRecv" is set, the reception routines are being told how they should interpret the CR and LF characters. If for example "LineDelimRecv" is configured in LDR_LF, then every time the LF character is received, it will be interpreted as a line break, ignoring the CR character.

In general, "LineDelimRecv" should be set to LDR_LF, for Linux / UNIX processes, and it should be left at LDR_CRLF, for Windows processes.

The TTermVT100.OnLineCompleted() event is generated when the arrival of the line-ending character (or characters) is detected. This event will contain the current line as parameter.

## Prompt Detection

Prompt detection is done by exploring the text strings that are arriving from the process, to see if they match the detection parameters. This method may not be very reliable if the Prompt of the process changes too much.

The output data generated by the process is received in blocks that can have a variable size, but that do not exceed the constant UBLOCK_SIZE. The search for the prompt is always done on the last line of each block of data that is received from the process (in the ReadData() method). If the process must send too much information, it usually arrives in several blocks, but the prompt (at least the final part) is always expected to arrive in the last block received. Not every line received is scanned, to reduce the processing load.

To configure the detection of the Prompt, set the 'detecPrompt' property to TRUE and set values ​​for 'promptIni' and 'promptFin'. These strings determine the beginning and ending part of the prompt.

If the prompt does not change, you can put its value directly in 'promptIni' and leave 'promptFin' empty. But if the prompt is variable, you can put the initial part in 'promptIni' and the final part in 'promptFin', but choosing values ​​that do not lead to confusion with data lines.

So, for example to detect a prompt of type:

```[usuario@localhost ~]$ ```

It can be set: promptIni = '\[' and promptFin = '$ '. Spaces are also part of the text that must be configured.

By default, the prompt found is expected to be exactly the same as the last line of the text that arrives from the terminal. But there are additional options. The type of match can be configured in the variable 'promptMatch'. It can have the following values:

*   prmExactly,   //prompt is the whole line.
*   prmAtBegin,   //prompt appears at the beginning of the line
*   prmAtEnd,     //prompt appears at the end of the line
*   prmAtAnyPos   //prompt appears anywhere on the line

By default, 'promptMatch' is set to 'prmExactly'.

You can also use the automatic prompt configuration function, which is executed by calling the AutoConfigPrompt() method. But this method must be called when the Pprompt is visible in the last line of the terminal.

'AutoConfigPrompt' reads the last line assuming it is the prompt and automatically sets values for 'promptIni' and 'promptFin'.

You can also use a custom routine for prompt detection. This must be hooked to the OnChkForPrompt() event that has the form:

function (lin: string): boolean;

The event receives the line to be scanned and must return TRUE, if it is determined that the current line contains the prompt. Activating this event disables the internal detection of the unit.

## Using a Status bar

Optionally, when creating the process, you can pass the reference to a panel of a Statusbar, so animated icons are displayed to indicates the states of the connection. In this case, the OnDrawPanel () event of the Statusbar must be handled.

The next code use the Statusbar StatusBar1 to show animated icons representing the states of the connection.

```
  p := TConsoleProc.Create(StatusBar1.Panels[1]);  //Crea conexión
  StatusBar1.OnDrawPanel:=@SBDrawPanel;
  
  ...
  
  procedure Form1.SBDrawPanel(StatusBar:TStatusBar; Panel:TStatusPanel; const Rect:TRect);
  begin
   if panel.Index = 1 then p.DrawStatePanel(StatusBar.Canvas, Rect);
  end;

```
