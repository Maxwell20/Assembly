;------------------------------------------------------------------------------
; Author:  Maxwell Twente
; CS308 hw2
;------------------------------------------------------------------------------
title prog2_Twente.asm                   ;DOS file name of program

.586                                    ;enable all pentium instructions
.model flat, stdcall                    ;memory model & calling convention
.stack 8192                             ;allocate 8k for stack

INCLUDELIB kernel32.lib                 ;Include the kernel 32 library

;----------------------------------------------------------
; Constant Definitions
;----------------------------------------------------------

MAX_LINE  equ 80
BUF_SIZE equ 240

STD_INPUT  equ -10d                     ;Function number for keyboard input
STD_OUTPUT equ -11d                     ;Function number for monitor output

LF equ 10d                              ;Line feed ascii constant
CR equ 13d                              ;Carriage return constant
NEWLINE equ CR,LF                       ;Combine CR and LF for carriage return

ENABLE_procESSED_INPUT  equ 1           ;Flag to turn off line buffering
ENABLE_procESSED_OUTPUT equ 1           ;Flag to turn off line bufferin
ENABLE_LINE_WRAP        equ 3           ;Flag to trun line wrap on
DISABLE_procESSED_INPUT equ 7           ;Flag to turn on line buffering

CREATE_NEW    EQU  1                    ;Parameter for creating a new file
CREATE_ALWAYS EQU  2                    ;Always create (overwrite existing)
OPEN_EXISTING EQU  3                    ;Parameter for opening an existing file
GENERIC_READ  EQU  80000000h            ;Parameter for reading a file
GENERIC_WRITE EQU  40000000h            ;Parameter for writing a file

FILE_SHARE_READ   equ 1
FILE_SHARE_WRITE  equ 2
FILE_SHARE_DELETE equ 4

FILE_ATTRIBUTE_NORMAL equ 80h
INVALID_HANDLE_VALUE equ 0FFFFFFFFh
HANDLE equ dword

;----------------------------------------------------------
; prototype Declarations for libarary imports
;----------------------------------------------------------

ExitProcess proto,
    dwExitCode:dword				   ;The exit code for the process 

GetStdHandle proto, 
	nStdHandle: dword                  ;The standard device. -10=INPUT, -11=OUTPUT, -13=ERROR

SetConsoleMode proto,                  
    hConsoleHandle:dword,              ;A handle to the console input buffer or a console screen buffer
	dwMode:dword                       ;The input or output mode to be set. 

ReadFile proto,	
    hFile:dword,                       ;A handle to the device
	lpBuffer:near32,                   ;A pointer to the buffer that receives the data read 
    nNumberOfCharsToRead:dword,        ;The maximum number of bytes to be read.
    lpNumberOfbytesRead:near32,        ;A pointer to the variable that receives the number of bytes read
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

WriteFile proto,                  
    hFile:dword, lpBuffer:near32,      ;A handle to the device
    nNumberOfCharsToWrite:dword,       ;The maximum number of bytes to be written.
    lpNumberOfbytesWritten:near32,     ;A pointer to the variable that receives the number of bytes written
    lpOverlapped:near32                ;A pointer to an OVERLAPPED structure is required if the hFile parameter 
	                                   ;was opened with FILE_FLAG_OVERLAPPED, otherwise it can be NULL.

CloseHandle proto,                     ;Prototype for closing a file
    fHandle:dword

GetLastError proto                     ;Prototype for getting specific error


CreateFileA proto,                     ;Prototype for CreateFile, used for getting handle to new or existin file
    lpFileName:near32,
	dwDesiredAccess:dword,
	dwShareMode:dword,
	lpSecurityAttributes:near32,
	dwCreationDisposition:dword,
	dwFlagsAndAttributes:dword,
	hTemplateFile:dword



	SetConsoleCursorPosition proto,        ;Prototype for setting cursor position
	hConsoleOutput:dword,
	dwCursorPosition:dword

GetConsoleScreenBufferInfo proto,      ;Prototype for getting console info
	hConsoleOutput:dword,
	lpConsoleScreenBufferInfo:near32

 FillConsoleOutputCharacterA proto,    ;Prototype for filling screen with character (used for clear screen)
	hConsoleOutput:dword,
	cCharacter:byte,
	nLength:dword,
	dwWriteCoord:dword,
	lpNumberOfCharsWritten:near32



;----------------------------------------------------------
; Data Segment -- Global Variables
;----------------------------------------------------------

.data
	strAddr			dd  ?
	strLength		dd  ?
	hStdOut			dd  ?
	hStdIn			dd  ?
	hFileOut        dd  ?
	hFileIn         dd  ?
	read			dd  ?
	written			dd  ?	
	inFilename      db 256 dup(0)
	outFilename     db 256 dup(0)
	fdata           db BUF_SIZE dup(0)
	numBytes		dd ?
	options         db 256 dup(0)

    newlineStr      db NEWLINE, 0       ;string for printing newline
	filePrompt      db "Enter filename: ",0
	fileError       db "Error reading file!",NEWLINE,0
	bytesReadStr    db " bytes read.",NEWLINE,0
	bytesWriteStr   db " bytes written.",NEWLINE,0
	LFst			    db " 1)LOAD FILE ",NEWLINE,0
	LFstprompt			db "ENTER FILE NAME: ",0
	SOst			    db " 2)SORT ",NEWLINE,0
	SRst			    db " 3)SORT REVERSE ",NEWLINE,0
	TUst			    db " 4)TO UPPER CASE ",NEWLINE,0
	TLst			    db " 5)TO LOWER CASE ",NEWLINE,0
	RLst			    db " 6)REVERSE EACH LINE ",NEWLINE,0
	SRstr			    db " 7)SEARCH AND REPLACE ",NEWLINE,0
	SRstrprompt			db "ENTER WORD TO REPLACE: ",0
	SRstrprompt2		db "REPLACE WITH:  ",0
	SFst			    db " 8)SAVE TO FILE ",NEWLINE,0
	QQst			    db " 9)QUIT ",NEWLINE,0

	output1         db MAX_LINE dup(0)       ;buffer for output strings
	input1			db MAX_LINE dup(0)		 ;buffer for input strings
	input2			db MAX_LINE dup(0)		 ;buffer for input strings
	mystring		db "Aye yo dude",NEWLINE,0
	
	
    coord           dd  0
	scrnBufInfo     db 22 DUP(0)


	;do i need?
	prompt1 BYTE    "String to Search: ", 0
prompt2 BYTE    "Word to Search For: ", 0
prompt3 BYTE    "Word to replace with: ", 0
target  BYTE    80 DUP (?)
key     BYTE    80 DUP (?)
strSub  BYTE    80 DUP (?)
trgtLength  DWORD   ?
keyLength   DWORD   ?
lastPosn    DWORD   ?
strSubLen   DWORD   ?
resultLbl BYTE  "The new sentence is: ", 0
	

;----------------------------------------------------------
; Code Segment
;----------------------------------------------------------

.code
main proc
	
	
	MainLoop:
	
		; zero out registers
		xor eax, eax                    ; zero out eax
		xor ebx, ebx                    ; zero out ebx
		xor ecx, ecx                    ; zero out ecx
		xor edx, edx                    ; zero out edx

		;call clearscreen
		;call ClearScreen
		;display menue

		call menue



		;;testing toUpper
		;lea edi, inFilename
		;call GetString

		;Call ReadFileContents

		;lea esi, fdata
		;call PrintString
			
		;lea esi, fdata
		;call toUpper
		;call revStr
		;call toLower
		


		;lea esi, newlineStr
		;call PrintString
		;lea esi, fdata
		;call PrintString
		;lea esi, newlineStr
		;call PrintString


		;get outfile name
		;lea edi, outFilename
		;call GetString
		;call WriteFileContents

	

		;lea edi, inFilename
		;call GetString

		;;load data
		;call readFileContents

		;;display file data
		;lea esi,fdata





		;;;;;;for testing 
		;lea esi, mystring
		;call PrintString

		;lea esi, mystring
		;call toUpper
		;call PrintString
	 


	Exit:
 		invoke ExitProcess, 0			;exit process with no error

main endp

;-----------------------------------------------------------------------------
;menue proc
;----------------------------------------------------------------------------
menue proc

		pushad                        ; save registers
		pushfd                        ; save flags
		
		
		;clears screen
		call ClearScreen

		menueLoop:
		
		lea esi, newlineStr
		call PrintString
		;printing options for the user
		lea esi, LFst
		call PrintString

		lea esi, SOst
		call PrintString

		lea esi, SRst
		call PrintString

		lea esi, TUst
		call PrintString

		lea esi, TLst
		call PrintString

		lea esi, RLst
		call PrintString

		lea esi, SRstr
		call PrintString

		lea esi, SFst
		call PrintString

		lea esi, QQst
		call PrintString

		lea esi, newlineStr
		call PrintString
		;getting menue option from user
		lea edi, options
		call GetString

		lea esi, newlineStr
		call PrintString

		cmp options, '1'
		je op1
		
		cmp options, '2'
		je op2
		
		cmp options, '3'
		je op3

		cmp options, '4'
		je op4

		cmp options, '5'
		je op5

		cmp options, '6'
		je op6

		cmp options, '7'
		je op7

		cmp options, '8'
		je op8
		
		cmp options, '9'
		je op9

		; option 1 for loading a fle
		;op1:
		; option 1 for loading a fle
		op1:;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;clears screen
		call ClearScreen


		lea esi, LFstprompt
		call PrintString
		lea edi, inFilename
		call GetString
		Call ReadFileContents
		lea esi, fdata
		call PrintString

		lea esi, newlineStr
		call PrintString
		jmp menueLoop
		;;;end op1;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		op2:

		op3:


		;; begin op4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		op4:			; option 4 for to upper case

		;clears screen
		call ClearScreen


		lea esi, newlineStr
		call PrintString
		lea esi, fdata
		call toUpper
		
		call PrintString
		lea esi, newlineStr
		call PrintString
		jmp menueLoop
		;; end op4;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		;; begin op5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		op5:				;option 5 for to lower case

		;clears screen
		call ClearScreen


		lea esi, newlineStr
		call PrintString
		lea esi, fdata
		call toLower
		lea esi, fdata
		call PrintString
		lea esi, newlineStr
		call PrintString
		jmp menueLoop
		;; end op5;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; begin op6;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		op6:
		call ClearScreen

		call revStr

		lea esi, fdata
		call PrintString

		jmp menueLoop
		;; end op6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; begin op7;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		op7:;;search and replace
		call ClearScreen


		call searchR
		lea esi, newlineStr
		call PrintString
		jmp menueLoop
		;; end op7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;; begin op 8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		op8:		;;option 8 for saving to a file..

		;clears screen
		call ClearScreen

		;get outfile name
		lea esi, LFstprompt
		call PrintString
		lea edi, outFilename
		call GetString
		call WriteFileContents
		lea esi, newlineStr
		call PrintString
		jmp menueLoop
		;; end op 8;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
		
		op9:							;option 9 for quiting the program		
		invoke ExitProcess, 0			;exit process with no error

		
		popfd                         ; restore registers
		popad                         ; restore flags
		ret                           ; return to calling procedure (pops IP)

menue endp



;-----------------------------------------------------------------------------
;2}sort proc
;-----------------------------------------------------------------------------

		;pushad                        ; save registers
		;pushfd                        ; save flags
		



	

		;popfd                         ; restore registers
		;popad                         ; restore flags
		;ret                           ; return to calling procedure (pops IP)
;-----------------------------------------------------------------------------
;3}reverse sort proc
;-----------------------------------------------------------------------------
		;pushad                        ; save registers
		;pushfd                        ; save flags
		

	

		;popfd                         ; restore registers
		;popad                         ; restore flags
		;ret                           ; return to calling procedure (pops IP)
;-----------------------------------------------------------------------------
;7) search and replace proc
;-----------------------------------------------------------------------------
searchR proc
		pushad                        ; save registers
		pushfd                        ; save flags
		
		lea esi, fdata				  ;printing file back before manupulations
		call PrintString

		lea esi, newlineStr
		call PrintString

		lea esi, SRstrprompt		  ;word to get
		call PrintString

		lea edi, input1				  ;stores word to get
		call GetString

		lea esi, SRstrprompt2		  ;word to replace
		call PrintString

		lea ebx, input2				 ;stores word to replace
		call GetString




	
 
    lea eax, fdata             ;address of target
    push eax                    ;parameter
    call strlen                 ;strlen(target)
    add esp, 4                  ;remove parameter
    mov trgtLength, eax         ;save length of target
   
    lea eax, input1                ;address of key
    push eax                    ;parameter
    call strlen                 ;strlen(key)
    add esp, 4                  ;remove parameter
    mov keyLength, eax          ;save length of key
   
    lea eax, input2             ;address of key
    push eax                    ;parameter
    call strlen                 ;strlen(strSub)
    add esp, 4                  ;remove parameter
    mov strSubLen, eax          ;save length of key

    mov eax, trgtLength
    sub eax, keyLength
    inc eax                     ;trgtLength - keyLength +1
    mov lastPosn, eax
    cld                         ;Left to Right comparison
    mov eax, 1                  ;starting position

    whilePosn:
        cmp eax, lastPosn       ;position <= last_posn?
        jnle endWhilePosn       ;exit if past last position

        lea esi, fdata         ;address of target string
        add esi, eax            ;add position
        dec esi                 ;address of position to check
        lea edi, input1            ;address of key
        mov ecx, keyLength      ;number of position to check
        repe cmpsb              ;check
        jz found                ;exit of success
        inc eax                 ;increment position
        jmp whilePosn           ;repeat

    endWhilePosn:
  
        jmp quit

    found:
        sub edi, keyLength
        mov ecx, strSubLen
        lea esi, input2
        cld
        rep movsb
        inc eax
        jmp whilePosn

    quit:



		lea esi, fdata				;printing file back aftermanupulations
		call PrintString



	

		popfd                         ; restore registers
		popad                         ; restore flags
		ret                           ; return to calling procedure (pops IP)
searchR endp
;-----------------------------------------------------------------------------
;6)proc for reversing a string
;-----------------------------------------------------------------------------
revStr proc
		pushad                        ; save registers
		pushfd                        ; save flags


		mov ecx, numBytes
		lea eax, fdata
		mov esi, eax  ; esi points to start of string
		add eax, ecx
		mov edi, eax
		dec edi       ; edi points to end of string
		shr ecx, 1    ; ecx is count (length/2)
		jz done       ; if string is 0 or 1 characters long, done
	reverseLoop:
		mov al, [esi] ; load characters
		mov bl, [edi]
		mov [esi], bl ; and swap
		mov [edi], al
		inc esi       ; adjust pointers
		dec edi
		dec ecx       ; and loop
		jnz reverseLoop


	done:

;lea esi,fdata
;call PrintString

lea esi, newlineStr
call PrintString
		popfd                         ; restore registers
		popad                         ; restore flags
		ret                           ; return to calling procedure (pops IP)
revStr endp

;------------------------------------------------------------------------------
;5)proc for changing to lower case-done
;
;------------------------------------------------------------------------------
toLower proc
		pushad                        ; save registers
		pushfd                        ; save flags



		
			mov bl, 20h
			lea esi, fdata
			mov ecx,numBytes
			jcxz Exit3
		foreachChar:
			mov al,[esi]
			or al,bl;;subtracting 20h from each digit
			mov[esi],al
			inc esi
			loop foreachChar 

		
			Exit3:


		popfd                         ; restore registers
		popad                         ; restore flags
		ret                           ; return to calling procedure (pops IP)
toLower endp
;------------------------------------------------------------------------------
;4)proc for changing toUPPER--done
;
;------------------------------------------------------------------------------
	
			
toUpper proc	
		pushad                        ; save registers
		pushfd                        ; save flags

	
			mov bl, 20h
			not bl
			lea esi, fdata
			mov ecx,numBytes
			jcxz Exit2
		foreachChar:
			mov al,[esi]
			and al,bl;;adding 20h to each digit
			mov[esi],al
			inc esi
			loop foreachChar 

		
			
			Exit2:

			

			popfd                         ; restore registers
		    popad                         ; restore flags
		    ret                           ; return to calling procedure (pops IP)

toUpper endp
;------------------------------------------------------------------------------
; Procedure to clear the console in win32
;------------------------------------------------------------------------------
ClearScreen proc
          pushad                        ; save registers
		  pushfd                        ; save flags

          ; get handle to console
		  invoke GetStdHandle,STD_OUTPUT; get handle for console output
		  mov    hStdOut, eax           ; copy file handle for screen

		  ; get console dimensions
		  invoke GetConsoleScreenBufferInfo,hStdOut,near32 ptr scrnBufInfo
		  mov ax, word ptr scrnBufInfo  ; get the console width
		  mov bx, word ptr scrnBufInfo+2; get the console height
		  imul bx                       ; calculate size = width * height

		  ; clear console
		  invoke FillConsoleOutputCharacterA,hStdOut,' ',ax,coord,near32 ptr written

		  ; reset cursor
		  invoke SetConsoleCursorPosition,hStdOut,coord

		  popfd                         ; restore registers
		  popad                         ; restore flags
		  ret                           ; return to calling procedure (pops IP)
ClearScreen endp






;------------------------------------------------------------------------------
; 8)Procedure to write data to file
; [IN] file contents in fdata
; [IN] outFilename
;------------------------------------------------------------------------------
WriteFileContents  proc                ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke CreateFileA, near32 ptr outFilename, GENERIC_WRITE, FILE_SHARE_WRITE,
			   0, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
			mov hFileOut, eax

            invoke WriteFile,          ; invoke standard WriteFile with
              hFileOut,                ;   file handle for screen
              near32 ptr fdata,         ;changed from esi to fdata ;   address of string
              numBytes,                ;   length of string
              near32 ptr written,      ;   variable for # bytes written
              0                        ;   overlapped mode			

			invoke CloseHandle, hFileOut; close file handle

			; display number of bytes written
			lea esi, numBytes          ; point esi at num bytes read for converting to string
			mov ebx, 10                ; set our number base to base 10
			lea edi, output1           ; point edit at location to store string
			call Num2Str			   ; now convert number to a string
			mov esi, edi               ; now point esi at that string to display
			call PrintString           ; display the string
			lea esi, bytesWriteStr     ; point esi at " bytes written." string
			call PrintString           ; display it

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
WriteFileContents   endp


;------------------------------------------------------------------------------
; 1)Procedure to read file
; [IN] inFilename
; [OUT] file contents in fdata
;------------------------------------------------------------------------------
ReadFileContents  proc                 ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

			; zero buffer out
			lea edi, fdata             ; point edi at buffer to be zero'd out
			xor eax, eax               ; put zero into accumulator to write to buffer
			mov cx, BUF_SIZE           ; put buf size into counter
			rep stosb                  ; now fill buffer

			; open file for reading
            invoke CreateFileA, near32 ptr inFilename, GENERIC_READ, FILE_SHARE_READ,
			   0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0
			mov hFileIn, eax           ; save file handle

			; see if we got an error opening file
			cmp eax, INVALID_HANDLE_VALUE
			je error_ReadFileContents

            mov ecx, BUF_SIZE          ; string length
			lea edi, fdata             ; load address of fdata into edi

            invoke ReadFile,           ; invoke standard ReadFile with
              eax,                     ;   file handle for keyboard
              edi,                     ;   address of location to write file contents
              ecx,                     ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode

		    invoke CloseHandle, hFileIn; close file handle

			; display # bytes read
			mov eax, read				; move num bytes read into eax, so we can save to variable
			mov numBytes, eax			; now save value from eax into permanent variable
			lea esi, read              ; point esi at num bytes read for converting to string
			mov ebx, 10                ; set our number base to base 10
			lea edi, output1           ; point edit at location to store string
			call Num2Str               ; now convert number to a string
			mov esi, edi               ; now point esi at that string to display
			call PrintString           ; display the string
			lea esi, bytesReadStr      ; point esi at " bytes read." string
			call PrintString           ; display it
			jmp done_ReadFileContents  ; jump over error handling section

	error_ReadFileContents:
			lea esi, fileError         ; point esi at file read error message
			call PrintString           ; now display the message

	done_ReadFileContents:
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
ReadFileContents   endp



;------------------------------------------------------------------------------
; Procedure to print a string to stdout
;
; Given   :  The Address of Null (0) terminated String to print in ESI register
; process :  Print the String using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
PrintString proc                       ; Define procedure
            pushad                     ; save registers
            pushfd                     ; save flags
            mov    strAddr, esi        ; copy string address
                                       ; find string length
            mov    strLength, 0        ; initialize string length
WhileChar:  cmp    byte ptr [esi], 0   ; character = null?
            jz     EndWhileChar        ; exit if so
            inc    strLength           ; increment character count
            inc    esi                 ; point at next character
            jmp    WhileChar           ; while more characters exist
EndWhileChar:
            invoke GetStdHandle,STD_OUTPUT ; get handle for console output
            mov    hStdOut, eax        ; copy file handle for screen
            invoke WriteFile,          ; invoke standard WriteFile with
              hStdOut,                 ;   file handle for screen
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr written,      ;   variable for # bytes written
              0                        ;   overlapped mode
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
PrintString endp

;------------------------------------------------------------------------------
; Procedure to get a string from stdin
;
; Given   :  The Address of the String to fill in EDI register
; process :  Input the String using the kernel32.lib ReadFile from the
;         :  Standard_Input function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  The input string in the data segment
;------------------------------------------------------------------------------
GetString proc                         ; Define procedure
            pushad                     ; save all registers
            pushfd                     ; save flags

            invoke GetStdHandle,STD_INPUT  ; get handle for console
            mov    hStdIn, eax         ; save the handle
            invoke SetConsoleMode,     ; invoke standard console with
              hStdIn,                  ;   file handle for keyboard
              DISABLE_procESSED_INPUT  ;   turn line buffering on

            mov    ecx, 255d;MAXSTR    ; string length
            mov    strLength, ecx      ; maximum string to accept
            mov    strAddr, edi        ; save pointer to input string
            invoke ReadFile,           ; invoke standard ReadFile with
              hStdIn,                  ;   file handle for keyboard
              strAddr,                 ;   address of string
              strLength,               ;   length of string
              near32 ptr read,         ;   variable for # bytes read
              0                        ;   overlapped mode
			
			mov ecx, read            
			mov byte ptr [edi+ecx-2],0 ; replace CR/LF by trailing null

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
GetString   endp



;------------------------------------------------------------------------------
; Procedure to print a character to the console
;
; Given   :  The Address of the Character to print in ESI register
; Process :  Print the Character using the kernel32.lib WriteFile to
;         :  Standard_Output function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  Nothing
;------------------------------------------------------------------------------
PutChar proc                         ; Define procedure
            pushad                          ; save registers
            pushfd                          ; save flags
            invoke GetStdHandle,STD_OUTPUT  ; get handle for console output
            mov hStdOut, eax                ; copy file handle for screen
	    invoke SetConsoleMode,          ; invoke standard console with
            hStdOut,                        ;   file handle for screen
            ENABLE_procESSED_OUTPUT	    ;   turn line buffering off
	    invoke WriteFile,               ; invoke standard WriteFile with
            hStdOut,                        ;   file handle for screen
            esi,                            ;   address of character
            1,                              ;   length of one byte
            near32 ptr written,             ;   variable for # bytes written
            0                               ;   overlapped mode
            popfd                           ; restore flags
            popad                           ; restore registers
            ret                             ; return to caller
PutChar endp



;------------------------------------------------------------------------------
; Procedure to get a character from the console
;
; Given   :  The Address of the Character to get in ESI register
; Process :  Input the Character using the kernel32.lib ReadFile from the
;         :  Standard_Input function call.  No registers are changed and the
;         :  flags are not affected.
; Return  :  The input character in the data segment
;------------------------------------------------------------------------------
GetChar proc                         ; Define procedure
            pushad                          ; save all registers
            pushfd                          ; save flags
            invoke GetStdHandle,STD_INPUT   ; get handle for keyboard
            mov hStdIn, eax                 ; save the handle
            invoke SetConsoleMode,          ; invoke standard console with
            hStdIn,                         ;   file handle for keyboard
            ENABLE_procESSED_INPUT          ;   turn line buffering off
            invoke ReadFile,                ; invoke standard ReadFile with
              hStdIn,                       ;   file handle for keyboard
              esi,                          ;   address of character
              1,                            ;   length of one byte
              near32 ptr read,              ;   variable for # bytes read
              0                             ;   overlapped mode
            call PutChar                    ; echo the character on screen
            popfd                           ; restore flags
            popad                           ; restore registers
            ret                             ; return to caller
GetChar   endp

;------------------------------------------------------------------------------
; Procedure to convert a number to a null-terminated ascii string
;
; (input) The Address of number to convert in ESI register
; (input) number base to convert to in ebx
; (output) null-terminated string written to address pointed to by edi
;------------------------------------------------------------------------------
Num2Str proc
            pushad						; save registers
            pushfd						; save flags

			mov ax, [esi]				; move the number to convert into accumulator
			push word ptr 0				; push null terminator onto the stack
		
		divLoop:						; start body of our divide-remainder loop
		    cmp ax, bx					; compare our number to the base we are converting to
			
			; if less than the base, no reason to divide, this number is last part,		
			jl lessThanBase				; so just jump to convert number to ascii
			
			; wasn't less than base, so we need to divide by base
			xor edx,edx					; zero edx for remainder
			div bx                      ; divide by our base

			; if remainder is 0-9, just convert to ascii '0'-'9'
			; otherwise use alphabet with 10='A', 11='B', 12='C', ...
			cmp dx, 9
			jg alphaChar_divLoop

			add dx, '0'					; convert remainder to character
			push dx						; push the character onto the stack
			jmp divLoop					; continue with our loop

		alphaChar_divLoop:
			sub dx, 10					; subtract 10 from remainder to align with alphabet
			add dx, 'A'					; now add 'A' to convert to character
			push dx						; push character onto the stack
			jmp divLoop					; continue with our loop

			; number was less than base, so now we just need to convert this final
			; number to ascii character
		lessThanBase:
			cmp ax, 9
			jg alphaChar_final

			add ax, '0'					; convert number to character
			push ax						; push character onto the stack
			jmp popCharacters			; now we are ready to pop all characters off into string

		alphaChar_final:
			sub ax, 10					; subtract 10 from number to align with alphabet
			add ax, 'A'					; now add 'A' to convert to character
			push ax						; push character onto the stack
			
			; once we get here, we have converted the number to a string, and each character is
			; pushed onto the stack in reverse order, starting with the null-terminator
			; all we need to do is pop them off into the string
		popCharacters:
			pop ax						; pop character off stack
			mov [edi], al				; mov character into destination string
			inc edi						; update our pointer in destination string to next byte
			cmp al, 0					; see if the character was null-terminator
			jz done						; if it was, jump to done
			jmp popCharacters			; otherwise, continue loop until we get null-terminator

		done:
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
Num2Str endp

;------------------------------------------------------------------------------
; Procedure to convert a null-terminated base-10 ascii string to a number
;
; (input) The Address of string to convert in ESI register
; (output) number written to address pointed to by edi
;------------------------------------------------------------------------------
Str2Num proc
            pushad						; save registers
            pushfd						; save flags

			xor ax, ax					; zx = zero
			xor cx, cx					; cx = zero			
			mov bx, 10					; mov number base into bx
		
		; for each character in string, perform weighted positional notation
		; ax = (ax * base) + next number
		charLoop:
			cmp byte ptr [esi], 0		; see if we have reached null terminator
			jz done						; if so, jump to done
			
			; not null terminator
			mov cl, [esi]				; move character into cl		
			sub cl, '0'					; convert character into number
			mul bx						; multiply by our number base
			add ax, cx					; add the 
			inc esi						; increment our pointer in string to next character
			jmp charLoop				; next iteration of loop
		
		; once we get here, we have reached the end of string, and ax should
		; contain the number
		done:
			mov [edi], ax			; move ax into destination storage location

            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller
Str2Num endp





strlen  PROC
		pushad                        ; save registers
		pushfd                        ; save flags
		


push ebp                    ;establish stack frame
mov ebp, esp
push ebx                    ;save EBX
sub eax, eax                ;length := 0
mov ebx, [ebp+8]            ;address of string

whileChar:
cmp BYTE PTR [ebx], 0       ;null byte?
je endWhileChar             ;exit if so
inc eax                     ;increment length
inc ebx                     ;point at next character
jmp whileChar               ;repeat

endWhileChar:

pop ebx                     ;restore registers
pop ebp
            popfd                      ; restore flags
            popad                      ; restore registers
            ret                        ; return to caller

strlen  ENDP













end  ; end directive to compiler

