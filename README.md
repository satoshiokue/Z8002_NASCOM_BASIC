# Z8002_NASCOM_BASIC

Nascom BASICはマイクロソフトが作成し、Grant Searleさんがサブセットに改編し、鈴木哲哉さんが移植されたコードを元に、奥江聡が8080/Z80からZ8002にコンバートしました。  

ファイルは原作者の宣言にしたがってご利用ください。  

https://github.com/vintagechips/emuz80/tree/main  
こちらのemuz80/examples/EMUBASIC/にあるEMUBASIC.ASM ソースコードを元にしてコード変換を行いました。

ターゲット SBCZ8002  
アセンブラ Macro Assembler 1.42  

BASICのMONITOR命令で3000hへジャンプします。

# メモリーマップ
0000H ROM  
8000H RAM  

I/O  
0005H SCC Control Registor  
0007H SCC Data Registor  

# 8080/Z80 から Z8002 への変換ルール

SCC通信のルーチンはUniversal MonitorのSBZ8002用を流用しました。  
https://electrelic.com/electrelic/node/1317  

ARITHMETIC PRECEDENCE TABLEの算術演算ルーチンアドレスが奇数バイトに格納されないようにPrecedence valueの後ろに1バイト追加してテーブルの単位を3バイトから4バイトに変更しました。  

FOR命令、GOSUB命令でスタック上の変数領域を節約するためPUSHしたあとにINC SPを実行して1バイトのブロックマーカーを格納していました。ブロックマーカーを2バイトにすることでスタックポインタが奇数バイトにならないように変更しました。    

CHKSYNルーチンを呼ぶ際、CALL命令直後の1バイトを引数としていました。このままでは次の命令が奇数バイトに配置されて実行できません。これをRH4レジスタにトークンを格納してからCALLするように変更しました。  

AMD TRANZ Translatorを参考にしてソースコードの変換を行いました。  
```
——————————————————
レジスタ対応
R0   AF
RH0  A
R1   BC
RH1  B
RL1  C
R2   DE
RH2  D
RL2  E
R3   HL
RH3  H
RL3  L
R15  SP

RL0  PUSH/POP命令時のフラグ一時保存
RH4  CHKSYNルーチンの引数
RL4  演算命令前後のフラグ一時保護

RH6  DIV3
RL6  DIV4
RH7  DIV2
RL7  DIV1
——————————————————
条件ジャンプ
JP C	  JP C		Carry
JP NC	  JP NC		Not Carry
JP Z	  JP Z		Zero
JP NZ	  JP NZ		Not Zero
JP PE	  JP PE		Parity Even
JP PO	  JP PO		Parity Odd
JP P	  JP PL		Plus
JP M	  JP MI		Minus
——————————————————
CCF  COMFLG C
CPL  COMB RH0

SCF  SETFLG C
——————————————————
PUSH AF

LDCTLB	RL0,FLAGS
PUSH	@R15,R0	
——————————————————
POP AF

POP	R0,@R15
LDCTLB	FLAGS,RL0
——————————————————
INC HL,DE,BC

LDCTLB	RL4,FLAGS
INC R3,R2,R1
LDCTLB	FLAGS,RL4
——————————————————
DEC HL,DE,BC

LDCTLB	RL4,FLAGS
DEC R3,R2,R1
LDCTLB	FLAGS,RL4
——————————————————


```
