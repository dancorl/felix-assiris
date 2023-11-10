-- FELIX -- an approximate emulator for Felix-C-256 assembly (ASSIRIS) jobs
-- Copyright (c) 2023 Alexandru Dan Corlan, MD, PhD
--
-- This code is released under the GNU General Public Licence version 2
-- 
-- INSTALLATION. 
-- On LINUX. Install gnat, the GNU Ada Translator. On debian: apt install gnat
-- Copy this file in a terminal and run: gnatchop -w -r felix.ada ; gnatmake felix
-- The resulting binary, felix, can be run locally or copied to /usr/local/bin
-- for general availability on your system.
--
-- On other systems, there are two variants:
--   (1) A. Get linux. B. Do the above
--   (2) Experiment on your own with gnat installation and felix compilation
--       then add a paragraph to the above on how to do it, and release
--
-- RUNNING.

-- The simulator is a linux executable, felix, that takes cards on
-- stdin and prints on stdout. Try: ./felix <hello.assiris
-- for an example.
-- 

with Text_Io; use Text_Io;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Calendar; use Ada.Calendar;

procedure Felix is
   
   function Heading(N: Natural; Title: String; Pre: Natural:= 0; Post: Natural := 0) return String;
   
   -- CONFIGURATION VARIABLES
   
   Verbosity: Integer:= 0;
   
   -- BASIC TYPES
   
   type Bit is mod 2;
   type Dbit is mod 4;
   type Tbit is mod 8;
   type Nibble is mod 16;
   type Bit7 is mod 2**7;
   type Byte is mod 2**8;
   type Half is mod 2**16;
   type Word is mod 2**32;
   type Doub is mod 2**64;

   type Opcod is mod 2**7;
   type Iflag is mod 2;
   type Xflag is mod 2;
   type Bfld is mod 2**3;
   type Qfld is mod 2**4;
   
   type Instr is record
      I: Iflag;
      B: Bfld;
      Q: Qfld;
      Op: Opcod;
      X: Xflag;
      D: Half;
   end record;
   pragma Pack(Instr);
   for Instr'Size use 32;
   
   type Memory_Bytes is array(Half range <>) of Byte;
   
   type Psw_Type is record
      Unused1: Half;
      Ai: Half;
      Ds, Di, Dd, Db: Bit; -- masti de depasire
      Z,S,D,C: Bit; -- indicatori de conditie
      Iis,Iic,Ice: Bit; -- masti de intrerupere
      Np: Bit; -- indicator de mod
      Ca: Nibble; 
      Unused2: Byte;
      Pm: Bit;
      Nit: Bit7;
   end record;
   pragma Pack(Psw_Type);
   for Psw_Type'Size use 64;
   
   -- THE MEMORY
   
   M: Memory_Bytes(Half);
   for M'Alignment use 8;
   
   -- THE REGISTERS; there could be more register banks, currently only one
   --                the registers are addressed like memory and corespond
   --                to the first 64 bytes of memory. 
   
   type Bytes_Register is new Memory_Bytes(0..3);
--   for Bytes_Register'Alignment use 8;
   type    Halfs_Register is array(0..1) of Half;
--   for Halfs_Register'Alignment use 8;
   type Words_Register is new Word;
--   for Words_Register'Alignment use 8;
   
   Max_Register: constant Half := 16;
   
   subtype Register is Half range 0..Max_Register-1;
   subtype Dregister is Half range 0..(Max_Register/2)-1;
   
   type Register_Bank is array(Register) of Words_Register;
   
   Rw: Register_Bank;
   for Rw'Address use M'Address;
   
   Rh: array(Register) of Halfs_Register;
   for Rh'Address use M'Address;
   
   Rb: array(Register) of Bytes_Register;
   for Rb'Address use M'Address;
   
   Ri: array(Register) of Integer;
   for Ri'Address use M'Address;
   
   Rf: array(Register) of Float;
   for Rf'Address use M'Address;
   
   Rg: array(Dregister) of Long_Float; -- this is disputable, addressing must be revised
   for Rg'Address use M'Address;
   
   Rl: array(Dregister) of Long_Integer; -- also disputable must check on double register use for longs
   for Rl'Address use M'Address;
   
   Halted: Boolean:= False;
   In_Error: Boolean:= False;
   Psw: Psw_Type;
   Pc: Half  renames Psw.Ai;
   
   Pswbytes: Memory_Bytes(0..7);
   for Pswbytes'Address use Psw'Address;
      
   -- INS -- a register where we read the nextr instruction to execute or assemble
   
   Inw: Word;
   Inb: Memory_Bytes(0..3);
   for Inb'Address use Inw'Address;
   Ins: Instr;
   for Ins'Address use Inw'Address;
   
   Asm: Half:= 64; -- after the registers..
   Card_Addr: Half:= Asm; -- the asm before this card was assembled
   Run_Entry: Half:= 64; -- where the program will run from, given by the END directive
   
   -- JOB CONTROL CONDITIONS
   
   Injob: Boolean:= False;
   Incompile: Boolean:= False;
   
   -- some utilities
   
      
   Ai: constant Integer:= Character'Pos('a');
   Amai: constant Integer:= Character'Pos('A');
   Zeroi: constant Integer:= Character'Pos('0');
   

      function To_Hex_Digit(X: Natural) return Character is
	 
      begin
	 if X<10 then
	    return Character'Val(X + Zeroi);
	 else
	    return Character'Val(X - 10 + Amai);
	 end if;
      end To_Hex_Digit;
      
      function To_Hex_Half(X: Half) return String is
	 
	 Rv: String(1..4);
	 
      begin
	 Rv(1) := To_Hex_Digit(Natural(X / 2**12));
	 Rv(2) := To_Hex_Digit(Natural((X / 2**8) mod 2**4));
	 Rv(3) := To_Hex_Digit(Natural((X / 2**4) mod 2**4));
	 Rv(4) := To_Hex_Digit(Natural(X mod 2**4));
	 return Rv;
      end To_Hex_Half;
       
      function To_Hex_byte(X: Byte) return String is
	 
	 Rv: String(1..2);
	 
      begin
	 Rv(1) := To_Hex_Digit(Natural(X / 2**4));
	 Rv(2) := To_Hex_Digit(Natural(X mod 2**4));
	 return Rv;
      end To_Hex_Byte;
     
      function To_Upper(X: String) return String is
	 
	 Rv: String(1..X'Length);
	 
      begin
	 for I in X'Range loop
	    if X(I) <= 'z' and X(I) >= 'a' then
	       Rv(I-X'First+1) := Character'Val(Character'Pos(X(I)) + Amai - Ai);
	    else
	       Rv(I-X'First+1) := X(I);
	    end if;
	 end loop;
	 return Rv;
      end To_Upper;
      
      function Is_White(C: Character) return Boolean is
	 
      begin
	 return C=' ' or C=Ascii.Ht;
      end Is_White;
      
      function Is_Alpha(C: Character) return Boolean is
	 
      begin
	 return (C >= 'a' and C <= 'z') or (C >='A' and C <= 'Z');
      end Is_Alpha;
      
      function Is_Decimal(C: Character) return Boolean is
	 
      begin
	 return C >= '0' and then C<='9';
      end Is_Decimal;
      
      function Is_Alphanum(C: Character) return Boolean is
	 
      begin
	 return Is_Alpha(C) or Is_Decimal(C);
      end Is_Alphanum;
      
   -- The SYMBOL TABLE
      
   Labelmax: constant Natural := 8;
   Symbol_Table_Max: constant Natural := 10_000;
   Symbol_Not_Found: constant Natural := Symbol_Table_Max + 1;
   subtype Label_String is String(1..Labelmax);
   
   type Symbol_Table_Entry is record
      Label: Label_String;
      Value: Word;
   end record;
   
   subtype St_Cursor is Natural range 1..Symbol_Table_Max;

   St: array(St_Cursor) of Symbol_Table_Entry;
   St_Next: St_Cursor:= 1;
   
   -- The LINK TABLE
   
   subtype Addr is Half;
   
   Link_Table_Max: constant Natural := 20_000;
   
   type Link_Table_Entry is record
      Label: Label_String;
      Location: Addr;
   end record;
   
   subtype Lt_Cursor is Natural range 1..Link_Table_Max;
   Lt: array(Lt_Cursor) of Link_Table_Entry;
   Lt_Next: Lt_Cursor:= 1;
   
   procedure Add_To_Lt(Lb: String; La: Addr) is
      
      Ls: Label_String := ( others => ' ');
      
   begin
      Ls(Ls'First..Ls'First+Lb'Length-1) := Lb;
      Lt(Lt_Next).Label := Ls;
      Lt(Lt_Next).Location := La;
      Lt_Next := Lt_Next+1;
   end Add_To_Lt;
   
   procedure Print_Symbol_Table is
      
   begin
      Put(Heading(1, "SYMBOL TABLE", 1, 1));
      for K in 1..St_Next-1 loop
	 Put_Line(St(K).Label & " " & To_Hex_Half(Half(St(K).Value)) & " " & Word'Image(St(K).Value));
      end loop;
   end Print_Symbol_Table;
   
   function String_To_Label(Ll: String) return Label_String is
     
     Ls: Label_String := (others => ' ');
     L: String:= To_Upper(Ll);

   begin 
      if L'Length > Labelmax then
	 Ls(1..Labelmax) := L(L'First..L'First+Labelmax-1);
      else
	 Ls(1..L'Length) := "" & L;
      end if;
      return Ls;
   end String_To_Label;
   
   function Find_In_Table(L: string) return Natural is
     -- return cursor in table where the label is or
     -- symbol_table_max+1 if not found
      Ls: Label_String:= String_To_Label(L);
   begin
      for K in 1..St_Next-1 loop
	 if Ls=St(K).Label then
	    return K;
	 end if;
      end loop;
      return Symbol_Not_Found;
   end Find_In_Table;
   
   procedure Add_To_Table(L: String; V: Word) is
      
      Ls: Label_String := String_To_Label(L);
      Ni: Natural:= Find_In_Table(L);
      
   begin
      if Ni=Symbol_Not_Found then
	 St(St_Next).Label := Ls;
	 St(St_Next).Value := V;
	 St_Next := St_Next+1;
      else
	 St(Ni).Value := V; -- we just replace the value, but this should be reported
      end if;
   end Add_To_Table;
      
   -- THE CARD READER (reads from standard_input).
   
   subtype Card is String(1..80); -- a line
   
   Incard: Card;
   Inlen: Natural;
   No_More_Cards: Boolean:= False;
   
   Cardno: Natural:= 0;
   
   procedure Error(Msg: String) is
	 
   begin
      Put_Line(Standard_Error, "ERR: " & Msg & " in: " & Integer'Image(Cardno) & " : " & Incard(1..Inlen) & Ascii.Lf);
      In_Error := True;
   end Error;
      
   
   procedure Read_Next_Card(Fin: File_Type) is
      
   begin
      Incard := ( others => ' '); -- this is card semantics..
      Cardno := Cardno + 1;
      Get_Line(Fin, Incard, Inlen);
   exception
      when End_Error => No_More_Cards := True;
   end Read_Next_Card;
   
   -- THE CARD PARSER AND PRINTER
   
   type Span is array(0..1) of Natural;

   Card_Type: Character; -- ' ' unlabeled instruction, '.' control card, '*' comment, 'A' labeled instruction, '$' error
   Label, Mnemo, R, Arg: Span := (others => 0);
   
   Maxarg: constant Natural:= 1000; -- we have enough memory
   
   type Arg_Type is ( Fx, Dx, Int, Str, Ch, Reg, Longstr, Lbl, Unknown );
   
   type Arg_Component is record
      T: Arg_Type;
      Val:  Span;
      Name: Span;
      Len:  Natural;
      Sval: Card; -- another card
      Hval: Half;
      Ival: Integer;
      Fval: Float;
      Dval: Long_Float;
   end record;
   
   subtype Argn is Natural range 1..Maxarg;
   
   type Args_Type is array(Argn) of Arg_Component;
   
   Args: Args_Type;
   Nextarg: Argn:= 1;
   
      
      -- the following will treat all expressions
      -- that are expected to be integers
      
	 
   function Is_Declit(X: String) return Boolean is
	    
   begin
      for K in X'Range loop
	 if K=X'First and then X(K)='-' then
	    null;
	 else
	    if X(K)<'0' or else X(K)>'9' then
	       return False;
	    end if;
	 end if;
      end loop;
      return True;
   end Is_Declit;
   
   function Is_Starred(X: String) return Boolean is
	    
   begin
      for K in X'Range loop
	 if K=X'First then 
	    if X(K)/='*' then
	       return False;
	    end if;
	 else
	    if X(K)<'0' or else X(K)>'9' then
	       return False;
	    end if;
	 end if;
      end loop;
      return True;
   end Is_Starred;
   
   
   function Is_Charlit(X:String) return Boolean is
      
   begin
      return X(X'First)='C' and then X(X'First+1)='''
	and then X(X'Last)=''';
   end Is_Charlit;
   
   function Is_Hexlit(X:String) return Boolean is
      
   begin
      if X(X'First)='X' and then X(X'First+1)='''
	and then X(X'Last)=''' then
	 for K in X'First+2..X'Last-1 loop
	    if not ((X(K)>='A' and X(K)<='F') or
		      (X(K)>='0' and X(K)<='9')) then
	       return False;
	    end if;
	 end loop;
	 return True;
      else
	 return False;
      end if;
   end Is_Hexlit;
   
   function Is_Label(X: String) return Boolean is
      
   begin
      for K in X'Range loop
	 if K=X'First and then not Is_Alpha(X(K)) then
	    return False;
	 end if;
	 if not Is_Alphanum(X(K)) then
	    return False;
	 end if;
      end loop;
      return True;
--      return Find_In_Table(X) /= Symbol_Not_Found;
   end Is_Label;
    
   function To_Half(X: String) return Half is
      
      Rv: Half:= 0;
      
   begin
      if Is_Declit(X) then
	 return Half'Value(X);
      elsif Is_Label(X) then
	 return Half(St(Find_In_Table(X)).Value);
      elsif Is_Charlit(X) then
	 for K in X'First+2..X'Last-1 loop
	    Rv := Rv * 256 + Character'Pos(X(K));
	 end loop;
	 return Rv;
      elsif Is_Hexlit(X) then
	 for K in X'First+2..X'Last-1 loop
	    if X(K)>='0' and X(K)<='9' then
	       Rv := Rv * 16 + Character'Pos(X(K)) - Half(Zeroi);
	    else
	       Rv := Rv * 16 + Character'Pos(X(K)) - Half(Amai) + 10;
	    end if;
	 end loop;
	 return Rv;	    
      else
	 null;
--	 Put_Line(Standard_Error, "I do not understand: " & X);
      end if;
      return 0;
   end To_Half;
   
   procedure Parse_Arg is
      
      procedure Clear_Arg(A: Argn) is
	 
      begin
	 Args(A).Val := ( others => 0);
	 Args(A).T := Unknown;
	 Args(A).Name := (others => 0);
	 Args(A).Ival := 0;
	 Args(A).Fval := 0.0;
	 Args(A).Dval := 0.0;
      end Clear_Arg;
      
      N: Natural;
      
   begin
      Nextarg:= 1;
      if Arg(0)=0 then
	 return ;
      end if;
      N := Arg(0);
      
      loop
	 Clear_Arg(Nextarg);
	 Args(Nextarg).Val(0) := N;
	 while N<=Arg(1) and then Incard(N)/=',' loop
	    if Incard(N)=''' then 
	       N := N+1;
	       while N<= Arg(1) and then Incard(N)/=''' loop
		  N := N+1;
	       end loop;
	       if N>Arg(1) then -- we have a tick problem
		  Error("Syntax error");
	       end if;
	    end if;
	    N := N+1;
	 end loop;
	 Args(Nextarg).Val(1) := N-1;
	 Nextarg := Nextarg+1;
	 if N>Arg(1) then
	    exit;
	 end if;
	 N := N+1; -- it is ','
      end loop;
      -- parse each arg in turn
      for A in 1..Nextarg-1 loop
	 -- does it have a name?
	 for C in Args(A).Val(0)..Args(A).Val(1) loop
	    if not Is_Alphanum(Incard(C)) then
	       if Incard(C)=':' then -- it has an alphanum name
		  Args(A).Name(0) := Args(A).Val(0);
		  Args(A).Name(1) := C-1;
		  Args(A).Val(0) := C+1;
	       end if;
	       exit;
	    end if;
	 end loop;
	 -- now, parse the value
	 declare
	    X: String:= Incard(Args(A).Val(0)..Args(A).Val(1));
	    Chi: Integer;
	 begin
	    if Is_Declit(X) then
	       Args(A).Ival := Integer'Value(X);
	       Args(A).Hval := Half(Args(A).Ival mod 2**16);
	       Args(A).T := Int;
	    elsif Is_Starred(X) then
	       Args(A).T := Reg;
	       Args(A).Ival := Integer'Value(Incard(Args(A).Val(0)+1 .. Args(A).Val(1)));
	       Args(A).Hval := Half(Args(A).Ival mod 2**16);
	    elsif Is_Label(X) then
	       if Find_In_Table(X) = Symbol_Not_Found then
		  Args(A).Hval := 0;
	       else
		  Args(A).Hval := Half(St(Find_In_Table(X)).Value);
	       end if;
	       Args(A).Ival := Integer(Args(A).Hval);
	       Args(A).T := Lbl;
	    elsif X(X'First)=''' and X(X'Last)=''' then
	       Args(A).T := Str;
	       Args(A).Sval(1..X'Length-2) := "" & X(X'First+1..X'Last-1);
	       Args(A).Len := X'Length-2;
	    elsif Is_Charlit(X) then
	       Chi := 0;
	       for K in X'First+2..X'Last-1 loop
		  Chi := Chi * 256 + Character'Pos(X(K));
	       end loop;
	       Args(A).Ival := Chi;
	       Args(A).Hval := Half(Chi mod 2**16);
	       Args(A).T := Ch;
	    elsif Is_Hexlit(X) then
	       Chi := 0;
	       for K in X'First+2..X'Last-1 loop
		  if X(K)>='0' and X(K)<='9' then
		     Chi := Chi * 16 + Character'Pos(X(K)) - Zeroi;
		  else
		     Chi := Chi * 16 + Character'Pos(X(K)) - Amai + 10;
		  end if;
	       end loop;
	       Args(A).Ival := Chi;
	       Args(A).Hval := Half(Chi mod 2**16);
	       Args(A).T := Int;
	    else
	       --	       Put_Line(Standard_Error, "I do not understand: " & X);
	       null;
	    end if;
	 end;
	 
      end loop;
   end Parse_Arg;
      

   procedure Parse_Card is
      
      Instring: Boolean:= False;
      I,J: Natural;

   begin
      Card_Type := '*';
      Label := (others => 0);
      Mnemo := (others => 0);
      R := (others => 0);
      Arg := (others => 0);
      
      Nextarg := 1;
      
      if Inlen < 2 then
	 return;
      end if;     
            
      case Incard(1) is
	 when '*' =>
	    Card_Type := '*';
	    return;
	 when '.' =>
	    Card_Type := '.';
	 when ' ' | Ascii.Ht =>
	    for K in 2..Inlen loop
	       if Is_Alpha(Incard(K)) then
		  Card_Type := ' ';
		  exit ;
	       end if;
	    end loop;
	    -- a space containing card, thus a comment; ignore
	 when 'A'..'Z'|'a'..'z' =>
	    Card_Type := 'A'; -- labeled 
	 when others =>
	    null; -- ignore, treat as comment
      end case;
      
      if Card_Type = '.' then
	 I := 2;
	 while I<70 and then Is_White(Incard(I)) loop
	    I := I+1;
	 end loop;
	 if I>=70 then -- error in fact
	    Error("Unrecognised control card.");
	    Card_Type := '*';
	    return; -- ignore, perhaps not best idea
	 end if;
	 J := I;
	 while J<I+10 and then Is_Alpha(Incard(J)) loop
	    J := J+1;
	 end loop;
	 if J>=I+10 then
	    Error("Unrecognised control card.");
	    Card_Type := '*';
	    return; -- ignore, perhaps not best idea
	 end if;
	 Mnemo(0) := I;
	 Mnemo(1) := J-1;
	 while J<72 and then Is_White(Incard(J)) loop
	    J := J+1; -- could be white in rest, that's ok
	 end loop;
	 if J<72 then
	    I := J;
	    Instring := False;
	    while J<72 and then not (Is_White(Incard(J)) and then not Instring) loop
	       if Incard(J)=''' then
		  Instring := not Instring;
	       end if;
	       J := J+1;
	    end loop;
	    if Is_White(Incard(J)) then
	       Arg(0) := I;
	       Arg(1) := J-1;
	    else
	       Error("Unrecognised control card.");
	       Card_Type := '*';
	       return; -- ignore, perhaps not best idea
	    end if;
	 end if;
	 Parse_Arg;
	 return;
      end if;
      
      if Card_Type = 'A' or else Card_Type = ' ' then
	 
 	 if Card_Type = 'A' then
	    Label(0) := 1;
	    for K in 1..9 loop -- maximum label size
	       if Is_White(Incard(K)) then
		  Label(1) := K-1;
		  exit;
	       end if;
	    end loop;
	    
	    if Label(1) = 0 then
	       -- something is wrong, raise an error
	       Error("Label too long");
	       -- ignored
	       Card_Type := '$';
	       return;
	    end if;
	 end if;
	 
	 for K in Label(1)+1 .. 16 loop -- the opcode, directive etc must start within first 16 characters
	    if Is_Alpha(Incard(K)) then -- mnemo must be here
	       Mnemo(0) := K;
	       for M in K..K+10 loop
		  if not Is_Alphanum(Incard(M)) then
		     Mnemo(1) := M-1;
		     exit;
		  end if;
	       end loop;
	       if Mnemo(1) = 0 then	
		  -- something is wrong, raise an error
		  Error("Mnemonic too long");
		  -- ignored
		  Card_Type := '$';
		  return;
	       end if;
	       if Incard(Mnemo(1)+1)=',' then
		  R(0) := Mnemo(1)+2;
		  J := Mnemo(1) + 3;
		  loop
		     if Is_White(Incard(J)) or J=Inlen then
			R(1) := J-1;
			exit;
		     end if;
		     J := J+1;
		     if J-R(0)>3 then
			Error("Reg too long");
			-- ignored
			Card_Type := '$';
			return;
		     end if;
		  end loop; 
	       end if;
	       
	       if R(0) > 0 then
		  J := R(1) + 1;
	       else
		  J := Mnemo(1)+1;
	       end if;
		 
	       while Is_White(Incard(J)) and J<=Inlen loop
		  J := J+1;
	       end loop;
	       
	       if J<=Inlen then
		  Arg(0) := J;
		  while J<=Inlen and then (not Is_White(Incard(J)) or Instring)  loop
		     if Incard(J)=''' then
			Instring := not Instring;
		     end if;
		     J := J+1;
		  end loop;
		  Arg(1) := J-1;
		  Parse_Arg;
	       end if;
	    end if;
	    if Mnemo(0)>0 then
	       exit;
	    end if;
	 end loop;
      end if;
   end Parse_Card;
   
   procedure Print_Asm_Header is
      
   begin
      Put(Heading(1, "ASSEMBLING LISTING", 1, 1));
      Put_Line("ADDR I B  Q OP X  D   LINE NR: LABEL     INSTRUCTION");
      New_Line;
   end Print_Asm_Header;
   
   procedure Print_Card(With_Assembly: Boolean) is
      Nr: String:= Integer'Image(Cardno);
      Snr: String(1..6) := ( others => ' ' );
      Slabel: String(1..10) := ( others => ' ' );
      Smnemor: String(1..10) := ( others => ' ');
      Sarg: String(1..64) := ( others => ' ');
      Sasm: String(1..18) := ( others => ' ');
      Sd: String := Half'Image(Ins.D);
      Si: Card;
      
   begin
      Snr(Snr'Last-Nr'Length+1..Snr'Last) := Nr;
      
      if Verbosity > 2 then
	 Put(Nr & "/" & Card_Type & "/");
	 if Label(0)>0 then
	    Put(Incard(Label(0)..Label(1)));
	 end if;
	 Put("/");
	 if Mnemo(0)>0 then
	    Put(Incard(Mnemo(0)..Mnemo(1)));
	 end if;
	 Put("/");
	 if R(0)>0 then
	    Put(Incard(R(0)..R(1)));
	 end if;
	 Put("/");
	 if Arg(0)>0 then
	    Put(Incard(Arg(0)..Arg(1)));
	 end if;
	 New_Line;
      end if;
      
      if With_Assembly then
	 Sasm(1) := Iflag'Image(Ins.I)(2);
	 Sasm(3) := Bfld'Image(Ins.B)(2);
	 if Ins.Q > 9 then
	    Sasm(5..6) := Qfld'Image(Ins.Q);
	 else
	    Sasm(6) := Qfld'Image(Ins.Q)(2);
	    Sasm(5) := '0';
	 end if;
	 Sasm(8) := To_Hex_Digit(Natural(Ins.Op) / 16);
	 Sasm(9) := To_Hex_Digit(Natural(Ins.Op) mod 16);
	 Sasm(11) := Xflag'Image(Ins.X)(2);
	 Sasm(13..12+Sd'Length) := Sd;
      else
	 null;
      end if;
      
      if Verbosity>2 and Nextarg>1 then
	 -- print the parsed arguments
	 for K in 1..Nextarg-1 loop
	    if Args(K).Name(0)>0 then
	       Put(Incard(Args(K).Name(0)..Args(K).Name(1)) & ": ");
	    end if;
	    case Args(K).T is
	       when Longstr =>
		  Put_Line("S/" & Args(K).Sval(1..Args(K).Len) & "/");
	       when Ch =>
		  Si(1) := Character'Val(Args(K).Ival / 256);
		  Si(2) := Character'Val(Args(K).Ival mod 256);
		  if Si(1)<' ' or Si(1)>'~'then
		     Si(1) := '?';
		  end if;
		  if Si(2)<' ' or Si(2)>'~' then
		     Si(2) := '?';
		  end if;
		  Put_Line("C/" & Integer'Image(Args(K).Ival) & 
			     "/" & Si(1..2));
	       when Int =>
		    Put_Line("I/" & Integer'Image(Args(K).Ival));
	       when others =>
		  Put_Line("???");
	    end case;
	 end loop;
      end if;
      
      Put(To_Hex_Half(Card_Addr));
      Put(" ");
      Put(Sasm);
      
      if Label(0) > 0 then
	 Slabel(Slabel'First..Slabel'First+Label(1)-Label(0)) := "" & Incard(Label(0)..Label(1));
      end if;
      if Mnemo(0)>0 then
	 if R(0)>0 then
	    declare
	       Mnemor: String:= "" & Incard(Mnemo(0)..Mnemo(1)) & "," & Incard(R(0)..R(1));
	    begin
	       Smnemor(Smnemor'First..Smnemor'First+Mnemor'Last-Mnemor'First) := Mnemor;
	    end;
	 else
	    Smnemor(Smnemor'First..Smnemor'First+Mnemo(1)-Mnemo(0)) := Incard(Mnemo(0)..Mnemo(1));
	 end if;
      end if;
      if Arg(0)>0 then
	 Sarg(Sarg'First..Sarg'First+Arg(1)-Arg(0)) := Incard(Arg(0)..Arg(1));
      end if;
      Put_Line(Snr & ": " & Slabel & Smnemor & Sarg);
   end Print_Card;
   
   function Heading(N: Natural; Title: String; Pre: Natural:= 0; Post: Natural := 0) return String is
      
      function Nchars(N: Natural; Char: Character) return String is
	 
      begin
	 if N=0 then
	    return "";
	 else
	    declare 
	       Rv: String(1..N) := ( others => Char );
	    begin
	       return Rv;
	    end;
	 end if;
      end Nchars;
      
   begin
      case N is
	 when 1 => 
	    return Nchars(Pre, Ascii.Lf) & Nchars(Title'Length+16, '*') & Ascii.Lf & "*** *** " & Title &
	      " *** ***" & Ascii.Lf & Nchars(Title'Length+16, '*') & Ascii.Lf & Nchars(Post, Ascii.Lf);
	 when 2 =>
	    return Nchars(Pre, Ascii.Lf) & Nchars(Title'Length+4, '=') & 
	      Ascii.Lf & "= " & Title & " =" & Ascii.Lf & 
	      Nchars(Title'Length+4, '=') & Ascii.Lf & Nchars(Post, Ascii.Lf);
	 when 3 =>
	    return Nchars(Pre, Ascii.Lf) &  Title & Ascii.Lf & 
	      Nchars(Title'Length, '-') & Ascii.Lf & Nchars(Post, Ascii.Lf);
	 when others =>
	    return Title;
      end case;
      
   end Heading;
   
   procedure Print_About is
      
   begin 
      Put(Heading(1, "About FELIX/ASSIRIS",3,1));
      Put_Line("FELIX-C was a series of computers produced in Romania in the 1970-s");
      Put_Line("that were clones, more or less, of the french IRIS-50, that were");
      Put_Line("modified clones of the SDS Sigma series computers.");
      Put_Line("SIRIS was the operating system and ASSIRIS the assembly language.");
      New_Line;
      Put_Line("This is an approximate simulator for ASSIRIS jobs on the Felix.");
      Put_Line("It was written by Alexandru Dan Corlan and made available under");      
      Put_Line("the GPL v2 licence. ");      
      New_Line;
      Put_Line("Only a subset of the instructions are currently implemented. ");
      Put_Line("A few instructions are added to connect to the modern operating");
      Put_Line("system (Linux) under which the simulator runs. Some control cards and"); 
      Put_Line("directives are implemented, a few more are added.");
      New_Line;
      Put_Line("The felix command accepts no arguments, all instructions must be");
      Put_Line("given on standard input, that represents the card reader. Results");
      Put_Line("will be printed on the standard output.");
      New_Line;
      Put_Line("More help is available by submitting (on stdin) a simple job, listed below:");
      New_Line;
      Put_Line(". LIST 'HELP'");      
      New_Line;
      Put(Heading(2, "Code of Conduct",1,1));
      Put_Line("  1.- Nothing may work, or work as expected, there is no warranty whatsoever, stay calm.");
      Put_Line("  2.- Any resemblance to an existing or historical computer system is purely coincidental");
      Put_Line("      (althought we tried, a little); don't hope.");
      Put_Line("  3.- Doing this is a complete, 100%, waste of time (we can proove that); relax.");
      Put_Line("  4.- Total utility of this project and of what you are doing right now is −273.15 Celsius (0K, Absolute Zero).");
   end Print_About;
   
   procedure Print_Help is
      
   begin
      Put(Heading(1, "FELIX/ASSIRIS VIRTUAL PROCESSOR HELP", 3, 1));
      Put(Heading(2, "GENERALITIES", 0, 1));
      Put_Line("This version only has 64K of memory and only knows some of the instructions:");
      Put_Line("AD4I BRU BCF BCT BAL CP1I CP1 CP2 CP4 EO2 EO4 EX2 EX4 LDC2 LDC4 LD1I LD2I LD1");
      Put_Line("LDL2 LDH2 LD4 LDM LD4I MG2 MG4 ST1 ST4 STH2 STM SB4I SB4");
      Put_Line("to which we added a couple more (see below): PRINT HALT");
      Put_Line("Only the direct and indirect addressing modes are implemented in this version.");
      Put_Line("The indirect addressing mode works for exactly one indirection.");
      Put(Heading(2, "CONTROL CARDS", 1, 1));
      Put_Line("The following cards (LIST and CONF) can appear anywhere in the input stream:");
      Put_Line(". LIST opt{,opt}* where opt can be:");
      Put_Line("       'HELP'     -- include this help text in the listing");
      Put_Line("       'ABOUT'    -- introduction to the FELIX/ASSIRIS system");
      Put_Line("       'SYMS'     -- the symbols table");
      Put_Line("       'LINKS'    -- the linkings performed by the link editor up to that point");
      Put_Line("       'DUMP'     -- `VIDAGE MEMOIRE' at that point");
      Put_Line("        MSG:'x'    -- display the message at that point");
      Put_Line(". CONF opt{,opt}* -- currently is ignored.");
      New_Line;
      Put_Line("The following cards can only appear in a specifc sequence.");
      Put_Line("The sequence is: JOB, COMPILE, LINK, RUN, EOJ; then, you may repeat.");
      Put_Line("COMPILE must be follwed by ASSIRIS (. COMPILE ASSIRIS)");
      Put_Line("RUN will admit one option, KINS:n where n is the number of thousands of");
      Put_Line("  instructions to run. Without it, the simulator only runs 500 instructions");
      Put_Line("  then stops (as was necessary in early tests).");
      Put_Line("Otherwise, you may add any options to the cards, but they are currently ignored.");
      Put(Heading(2, "DIRECTIVES", 1, 1));
      Put_Line("Directives are cards that can occur only between '. COMPILE ASSIRIS' and 'END'");
      Put_Line("The directives: ORG, EQU, DS, DB, ALIGN, work as expected. EQU defines a symbol.");
      Put_Line("ORG changes the address (that must be its argument) where the assembler generates code");
      Put_Line("DS is followed by a '-delimited string, the ASCII characters of which it puts into memory");
      Put_Line("  in succesive locations");
      Put_Line("DB is followed by a sequence of byte-sized numbers (0..255), comma separated, that ar put into memory in sequence.");
      Put_Line("  the number can be: [-]ddd, decimal numbers; X'xx' hexadecimal, C'c' characters.");
      Put_Line("ALIGN is essential. Before assemblying machine code, the assembly address must be aligned to 4 bytes.");
      Put_Line("  After DSs and DBs, always use ALIGN to synchronise the address to a 4-byte alignment, if code follows.");
      Put_Line("  ALIGN take an optional argument about at what pace to align, in bytes. The default is 4.");
      Put_Line("END X must include this X which is the address, usually a label, where RUN will start execution.");
      Put_Line("CSECT is ignored.");
      Put_Line("Label expressions are not implemented, you can't say 'BRU ADDR+8' or something");
      Put(Heading(2, "NEW INSTRUCTIONS", 1, 1));
      Put_Line("HALT will finish running");
      Put_Line("PRINT,r will print the LSB of register r as an ascii character on stdout");
      Put_Line("PRINT,r C'd' will print the register r as a decimal on stdout");
      Put_Line("PRINT,r C'xy' will print the two ASCII characters x and y on stdout");
   end Print_Help;
   
   -- THE ASSEMBLER
   
   
   Dir: Boolean:= False; -- if it was identified as a directive card
   
   Type Ops is (Ad4, Ad4i, 
		AD8, ADD, ADF8, ADF4, 
		Adh2, Adl2, 
		AIO, ANALD, ANALN,
		Bal, Bcf, Bct, Bdf, Bdt, Bru, Cp1, Cp1i, Cp2, Cp4, 
		CP8, CPD, CPSL, CPSR, CYBL, CYBR,
		Dv2, Dv4,
		DVD, DVF8, DVF4, DVU2, EO2, EO4, EX2, EX4, EXU, HIO,
		Ic2, Ic4, 
		LAS,
		Ld1, Ld1i, Ld2i, Ldh2, Ldl2, Ld4, 
		LD4I, LD8, LDC, LDC2, LDC4, LDC8, LDDA, LDM, LDS2, LDTM, LPK, LPS, 
		Mp2, Mp4, 
		MG2, MG4, MPD, MPF4, MPF8, MPU2, MVSR, MVSL,
		NF4, PACK, 
		Rd, 
		Sb4, Sb4i,
		SB8, SBD, SBF4, SBF8, SBH2,
		Sh2, Sh4, 
		SH8, SHD, SIO,
		St1, St4, Sth2, Stpa, 
		ST8, STC, STM, STS2, STTM,
		TDV, TIO, TRTL, TRTR, UNPK,
		Wd, 
		WT, ZAD,
		
		Print, Halt,
		
	       UNASSIGNED);
   
   Byopcod: array(Opcod) of Ops := (others => UNASSIGNED);
   
   Byops: array(Ops) of Opcod := (others => 0); -- impossible opcode
   
   procedure Setops(Op: Ops; Cod: Opcod) is
      
   begin
      Byopcod(Cod) := Op;
      if Byops(Op) /= 0 then
	 Put_Line("Standard_Error: Code already exists: " & To_Hex_Half(Half(Cod)));
      else
	 Byops(Op) := Cod;
      end if;
   end Setops;
   
   procedure Set_All_Ops is
      
   begin
    Setops(Ad4, 16#1E#);
    Setops(Ad4i, 16#27#);
    Setops(AD8, 16#1F#);
    Setops(ADD, 16#61#);
    Setops(ADF8, 16#7C#);
    Setops(ADF4, 16#6C#);
    Setops(Adh2, 16#1D#);
    Setops(Adl2, 16#1C#);
    Setops(AIO, 16#56#);
    Setops(ANALD, 16#3D#);
    Setops(ANALN, 16#3C#);
    Setops(Bal, 16#39#);
    Setops(Bcf, 16#34#);
    Setops(Bct, 16#35#);
    Setops(Bdf, 16#36#);
    Setops(Bdt, 16#37#);
    Setops(Bru, 16#38#);
    Setops(Cp1, 16#10#);
    Setops(Cp1i, 16#24#);
    Setops(Cp2, 16#11#);
    Setops(Cp4, 16#12#);
    Setops(CP8, 16#13#);
    Setops(CPD, 16#63#);
    Setops(CPSL, 16#4C#);
    Setops(CPSR, 16#4D#);
    Setops(CYBL, 16#4A#);
    Setops(CYBR, 16#4B#);
    Setops(Dv2, 16#32#);
    Setops(Dv4, 16#68#);
    Setops(DVD, 16#65#);
    Setops(DVF8, 16#7F#);
    Setops(DVF4, 16#6F#);
    Setops(DVU2, 16#33#);
    Setops(EO2, 16#0D#);
    Setops(EO4, 16#0E#);
    Setops(EX2, 16#01#);
    Setops(EX4, 16#02#);
    Setops(EXU, 16#3A#);
    Setops(HIO, 16#53#);
    Setops(Ic2, 16#46#);
    Setops(Ic4, 16#47#);
    Setops(Ld1, 16#18#);
    Setops(Ld1i, 16#28#);
    Setops(Ld2i, 16#29#);
    Setops(Ldh2, 16#08#);
    Setops(Ldl2, 16#09#);
    Setops(Ld4, 16#0A#);
    Setops(LD4I, 16#2A#);
    Setops(LD8, 16#0B#);
    Setops(LDC, 16#43#);
    Setops(LDC2, 16#19#);
    Setops(LDC4, 16#1A#);
    Setops(LDC8, 16#1B#);
    Setops(LDDA, 16#2B#);
    Setops(LDM, 16#2C#);
    Setops(LDS2, 16#45#);
    Setops(LDTM, 16#41#);
    Setops(LPK, 16#5D#);
    Setops(LPS, 16#5E#);
    Setops(Mp2, 16#30#);
    Setops(Mp4, 16#69#);
    Setops(MG2, 16#05#);
    Setops(MG4, 16#06#);
    Setops(MPD, 16#64#);
    Setops(MPF4, 16#6E#);
    Setops(MPF8, 16#7E#);
    Setops(MPU2, 16#31#);
    Setops(MVSR, 16#49#);
    Setops(MVSL, 16#48#);
    Setops(NF4, 16#22#);
    Setops(PACK, 16#66#);
    Setops(Sb4, 16#16#);
    Setops(Sb4i, 16#26#);
    Setops(SB8, 16#17#);
    Setops(SBD, 16#62#);
    Setops(SBF4, 16#6D#);
    Setops(SBF8, 16#6F#);
    Setops(SBH2, 16#15#);
    Setops(Sh2, 16#20#);
    Setops(Sh4, 16#21#);
    Setops(SH8, 16#6B#);
    Setops(SHD, 16#5F#);
    Setops(SIO, 16#52#);
    Setops(St1, 16#58#); -- stl2?
    Setops(St4, 16#5A#);
    Setops(Sth2, 16#59#);
    Setops(Stpa, 16#5C#); 
    Setops(ST8, 16#5B#);
    Setops(STC, 16#42#);
    Setops(STM, 16#2D#);
    Setops(STS2, 16#44#);
    Setops(STTM, 16#40#);
    Setops(TDV, 16#55#);
    Setops(TIO, 16#54#);
    Setops(TRTL, 16#4E#);
    Setops(TRTR, 16#4F#);
    Setops(UNPK, 16#67#);
    Setops(Rd, 16#50#);
    Setops(Wd, 16#51#);
    Setops(WT, 16#57#);
    Setops(ZAD, 16#60#);
    
--    Put_Line(Standard_Error, "Unassigned opcodes: ");
--    for Op in Opcod loop
--       if Byopcod(Op)=Unassigned then
--	  Put(To_Hex_Half(Half(Op)) & ", ");
--       end if;
--    end loop;
    
    --    Unused codes:
    
    --    0000, 
    --    0003, 0004, 0007, 000C, 000F, 
    --    0014, 001B, 0023, 0025, 002E, 
    --    002F, 003B, 003E, 003F, 006A, 
    --    0070, 0071, 0072, 0073, 0074, 
    --    0075, 0076, 0077, 0078, 0079, 
    --    007A, 007B, 007D
    
    
   -- codes 70..77 are unused, we use them for our special instructions
     Setops(Print,16#70#);
     Setops(Halt,16#77#);
   end Set_All_Ops;
   
   
   procedure Asminc(Op: Opcod; Q: Qfld; B: Bfld; D: Half) is
      
   begin
      while Asm mod 4 /= 0 loop
	 Asm := Asm+1;
      end loop;
      Ins.Op := Op;
      Ins.D := D;
      Ins.Q := Q;
      Ins.X := 0; -- we only do direct addressing, currently
      if B>0 then
	 Ins.I := 1;
	 Ins.B := B;
      else
	 Ins.I := 0;
	 Ins.B := 0;
      end if;
      M(Asm..Asm+3) := Inb;
      Asm := Asm+4;
   end Asminc;
   
   
   procedure Assemble_Ins(Opid: String; Rarg0: Natural; Barg0: Bfld; Disp0: Natural) is
      
      Rarg: Qfld:= Qfld(Rarg0);
      Disp: Half:= Half(Disp0);

   begin
      for K in Ops loop
	 if Opid=Ops'Image(K) then
	    Asminc(Byops(K),Rarg,Barg0,Disp);
	    return;
	 end if;
      end loop;
      Put_Line("Invalid opcode: " & Opid);
   end Assemble_Ins;
   
   procedure Assemble_Card is 
      -- card that has just been read and parsed, found in incard
      -- and parsed in mnemo & folks
      
      
      function Reg_Arg return Bfld is
	 
      begin
	 if Nextarg < 2 then
	    return 0;
	 end if;
	 if Args(1).T=Reg then
	    if Args(1).Ival > 0 and then Args(1).Ival<9 then
	       return Bfld(Args(1).Ival);
	    elsif
	      Args(1).Ival > 8 and then Args(1).Ival < 16 then
	       return Bfld(Args(1).Ival - 8);
	    else
	       Error("Indirect, B register must be in 1..8 or 9..16, 1..8 meaning 9..16.");
	    end if;
	 end if;
	 return 0;
      end Reg_Arg;
      
      function Half_Arg return Half is
	 
      begin
	 if Nextarg < 2 then
	    return 0;
	 end if;
	 if Args(1).T = Reg then
	    if Nextarg=3 then
	       return Args(2).Hval;
	    else
	       return 0;
	    end if;
	 else
	    return Args(1).Hval;
	 end if;
      end Half_Arg;
      
      Alignat: Half := 4;
      
   begin
      Card_Addr := Asm;
      
      -- NEW LABELS
      if Label(0) > 0 then
	 if Mnemo(0) > 0 and then "" & Incard(Mnemo(0)..Mnemo(1)) = "EQU" then
	    Add_To_Table(Incard(Label(0)..Label(1)), Word(Half_Arg)); -- but must check for the sign!
	    return;
	 else
	    Add_To_Table(Incard(Label(0)..Label(1)), Word(Asm));
	 end if;
      end if;
      
      -- DIRECTIVES
      Dir := False;
      if Mnemo(0) > 0 then
	 if "RES" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    if Nextarg/=2 or else Args(1).T /= Int then
	       Error("RES needs a number of bytes to reserve.");
	    end if;
	    Asm := Asm + Half(Args(1).Ival);
	    Dir := True;
	 elsif "ALIGN" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    if Nextarg=2 and Args(1).T = Int then
	       Alignat := Half(Args(1).Ival);
	    end if;
	    while (Asm mod Alignat) /= 0 loop
	      Asm := Asm + 1;
	    end loop;
	    Dir := True;
	 elsif "DS" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    if Nextarg/=2 or else Args(1).T /= Str then
	       Error("DS needs a string to store.");
	    end if;
	    for K in 1..Args(1).Len loop
	       M(Asm) := Byte(Character'Pos(Args(1).Sval(K)));
	       Asm := Asm+1;
	    end loop;
	    Dir := True;
	 elsif "ORG" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    if Nextarg/=2 or else Args(1).T /= Int then
	       Error("ORG needs an address from where to assemble.");
	    end if;
	    Asm := Half(Args(1).Ival);
	    Dir := True;
	 elsif "LIST" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    for K in 1..Nextarg-1 loop
	       if Args(K).Name(0) > 0 and then
		 Incard(Args(K).Name(0)..Args(K).Name(1)) = "VERB"
		 and then Args(K).T=Int  then
		  Verbosity := Args(K).Ival;
	       elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "HELP" then
		  Print_Help;
	       elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "ABOUT" then
		  Print_Help;
	       end if;
	    end loop;
	    Dir := True;
	 elsif "END" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    if Nextarg/=2 or else Args(1).T /= Lbl then
	       Error("END needs an address from where the program will run.");
	    end if;
	    Run_Entry := Args(1).Hval;
	    Dir := True;
	    Incompile := False;
	 elsif "DB" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    for K in 1..Nextarg-1 loop
	       if Args(K).T/=Int and Args(K).T/=Ch then
		  Error("DB needs decimal or hex numbers or character to put into memory.");
	       else
		  M(Asm) := Byte(Args(K).Hval mod 256);
		  Asm := Asm + 1;
	       end if;
	    end loop;
	    Dir := True;
	 elsif "CSECT" = "" & Incard(Mnemo(0)..Mnemo(1)) then
	    Dir := True;
	    null; -- nothing to do, but should enable start of code generation
	 end if;
      end if;
      
      -- INSTRUCTIONS
      if not Dir and Mnemo(0) > 0 then
	 if Nextarg>1 and then Args(1).T = Lbl then
	    Add_To_Lt(Incard(Args(1).Val(0)..Args(1).Val(1)),Asm+2);
--	    Put_Line("Added to LT " & Incard(Args(1).Val(0)..Args(1).Val(1)) & " @ " & To_Hex_Half(Asm+2));
	 end if;
	 if Nextarg>2 and then Args(2).T = Lbl then
	    Add_To_Lt(Incard(Args(2).Val(0)..Args(2).Val(1)),Asm+2);
--	    Put_Line("Added to LT " & Incard(Args(1).Val(0)..Args(1).Val(1)) & " @ " & To_Hex_Half(Asm+2));
	 end if;
	 if R(0) > 0 then
	    Assemble_Ins( Opid => Incard(Mnemo(0)..Mnemo(1)),
			  Barg0 => Reg_Arg,
			  Rarg0 => Natural(To_Half(Incard(R(0)..R(1)))),
			  Disp0 => Natural(Half_Arg));
	 else
	    Assemble_Ins( Opid => Incard(Mnemo(0)..Mnemo(1)),
			  Barg0 => Reg_Arg,
			  Rarg0 => 0,
			  Disp0 => Natural(Half_Arg));
	 end if;
      else
	 null; -- it is just a label, do not assemble anything
      end if;
   end Assemble_Card;
   
   -- THE SIMULATOR
   
   procedure Clear_Memory is
      
   begin
      M := (others => 0);
   end Clear_Memory;
   
   procedure Vidage is
      
      procedure Put_Mem_Line(A: Half) is
	 
      begin
	 for K in 0..7 loop
 	    Put(To_Hex_Byte(M(4*Half(K)+A)));
	    Put(To_Hex_Byte(M(4*Half(K)+A+1)));
	    Put(To_Hex_Byte(M(4*Half(K)+A+2)));
	    Put(To_Hex_Byte(M(4*Half(K)+A+3)));
	    Put(' ');
	 end loop;
	 New_Line;
      end Put_Mem_Line;
      
      Adr: Half;
      
   begin
      Put("ETAT PROGRAMME         ");
      for K in 0..7 loop
	 Put(To_Hex_Byte(Pswbytes(Half(K))));
	 if K=3 then
	    Put(' ');
	 end if;
      end loop;
      
      New_Line(2);
      Put("REGISTRES      ");
      for K in 0..7 loop
	 Put('R' & Integer'Image(K) & "      ");
      end loop;
      New_Line;
      Put("            ");
      Put_Mem_Line(0);
      New_Line;	
      Put("               ");
      for K in 8..15 loop
	 Put('R' & Integer'Image(K) & "     ");
	 if K<10 then
	    Put(' ');
	 end if;
      end loop; 
      New_Line;
      Put("            ");
      Put_Mem_Line(32);
      New_Line(2);	
      Put_Line("VIDAGE MEMOIRE");
      New_Line;
      Adr := 64;
      
      for J in 1..20 loop
	 Put("  ");
	 Put('0');
	 Put(To_Hex_Half(Adr));
	 Put("     ");
	 Put_Mem_Line(Adr);
	 Adr := Adr + 32;
	 if J mod 5 = 0 then
	    New_Line;
	 end if;
      end loop;
      
   end Vidage;
   
   procedure Link_Edit is
      
      Symfound: Natural;
      
   begin
      if Lt_Next < 2 then
	 return; -- nothing to do
      end if;
      for L in 1..Lt_Next-1 loop
	 Symfound := Find_In_Table(Lt(L).Label);
	 if Symfound = 0 then
	    Error("Symbol " & Lt(L).Label & " not found at link time.");
	 end if;
	 M(Lt(L).Location) := Byte(St(Symfound).Value mod 256);
	 M(Lt(L).Location + 1) := Byte((St(Symfound).Value mod 2**16) / 2**8);
	 New_Line;
--	 Put(To_Hex_Half(Lt(L).Location) & " => " & Lt(L).Label  & "(" & To_Hex_Half(Half(St(Symfound).Value mod 2**16)));
	 -- link edited
      end loop;
      
   end Link_Edit;
   
   
   procedure Print_Links_Table is
      
      Symfound: Natural;
      
   begin
      if Lt_Next < 2 then
	 return; -- nothing to do
      end if;
      Put(Heading(1, "LINKS TABLE", 1, 1));
      Put_Line("AT      LABEL     LINK");
      for L in 1..Lt_Next-1 loop
	 Symfound := Find_In_Table(Lt(L).Label);
	 if Symfound = 0 then
	    Put_Line(To_Hex_Half(Lt(L).Location) & " => " & Lt(L).Label  & " (" & "NOT FOUND" & ")");
	 else
	    Put_Line(To_Hex_Half(Lt(L).Location) & " => " & Lt(L).Label  & " (" & To_Hex_Half(Half(St(Symfound).Value mod 2**16)) & ")");
	 end if;
      end loop;
      New_Line(2);
   end Print_Links_Table;

   
   procedure Step is
      
      A: Half; -- the computed address
      
      procedure Error(M: String) is
	 
      begin
	 Put_Line("Execution error at address " & To_Hex_Half(pc-4) & " : " & M);
	 In_Error := True;
	 return ;
      end Error;
      
      Format: String(1..2);
      Zsdc: Qfld:= Qfld(Natural(Psw.Z)*8 + Natural(Psw.S)*4 + Natural(Psw.D)*2 + Natural(Psw.C));
      
      Ij: Integer;
      
      procedure Setsz is
	 
      begin
	    if Ri(Half(Ins.Q)) < 0 then Psw.S := 1; else Psw.S := 0; end if;
	    if Ri(Half(Ins.Q)) = 0 then Psw.Z := 1; else Psw.Z := 0; end if;	 
      end;
      
   begin
--      Put(To_Hex_Half(Pc) & " ");
      Inb := M(Pc..Pc+3);
      Pc := Pc+4;
      
      if Ins.X=0 and Ins.I=0 then
	 A := Ins.D;
      elsif Ins.X=0 and Ins.I=1 then
	 A := Ins.D + Rh(Half(Ins.B)+8)(0);
      else
	 Error("Only in/direct addressing implemented so far.");
	 return;
      end if;
      
      case Byopcod(Ins.Op) is
	 when Ad4 =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) + 
	      Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Ad4i =>
	    Rw(Half(Ins.Q)) := Words_Register(Word(Rw(Half(Ins.Q))) + Word(A));
	    Setsz;
	 when Bru =>
	    Pc := A;
	 when Bcf =>
	    if (Ins.Q and Zsdc) = 0 then
	       Pc := A;
	    end if;
	 when Bct =>
	    if (Ins.Q and Zsdc) /= 0 then
	       Pc := A;
	    end if;
	 when Bal =>
	    Rw(Half(Ins.Q)) := Words_Register(Pc);
	    Pc := A;
	 when Cp1i =>
	    if Rb(Half(Ins.Q))(0) = Byte(A mod 256) then
	       Psw.Z := 1;
	    else
	       Psw.Z := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	    if Rb(Half(Ins.Q))(0) < Byte(A mod 256) then
	       Psw.S := 1;
	    else
	       Psw.S := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	 when Cp1 =>
	    if Rb(Half(Ins.Q))(0) = M(A) then
	       Psw.Z := 1;
	    else
	       Psw.Z := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	    if Rb(Half(Ins.Q))(0) < M(A) Then
	       Psw.S := 1;
	    else
	       Psw.S := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	 when Cp2 =>
	    if Rh(Half(Ins.Q))(0) = Half(M(A)) + 256 * Half(M(A+1)) then
	       Psw.Z := 1;
	    else
	       Psw.Z := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	    if Rh(Half(Ins.Q))(0) < Half(M(A)) + 256 * Half(M(A+1)) then
	       Psw.S := 1;
	    else
	       Psw.S := 0;
	    end if; -- ar trebui si altele dar sunt signed?
	 when Cp4 =>
	    Ij := Integer(Rw(Half(Ins.Q)) - Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3))));
	    if Ij=0 then
	       Psw.Z := 1;
	    else
	       Psw.Z := 0;
	    end if;
	    if Ij<0 then
	       Psw.S := 1;
	    else
	       Psw.S := 0;
	    end if;
	 when Eo2 =>
	    Rh(Half(Ins.Q))(0) := Rh(Half(Ins.Q))(0) xor (Half(M(A)) + 256 * Half(M(A+1)));
	 when Eo4 =>
	      Rw(Half(Ins.Q)) :=  Rw(Half(Ins.Q)) xor 
		Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Ex2 =>
	    Rh(Half(Ins.Q))(0) := Rh(Half(Ins.Q))(0) and (Half(M(A)) + 256 * Half(M(A+1)));
	 when Ex4 =>
	      Rw(Half(Ins.Q)) :=  Rw(Half(Ins.Q)) and
		Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Ldc2 =>
	    Rh(Half(Ins.Q))(0) := not (Half(M(A)) + 256 * Half(M(A+1)));
	 when Ldc4 =>
	      Rw(Half(Ins.Q)) :=  not Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Ld1i =>
	    Rb(Half(Ins.Q))(0) := Byte(Half(A) mod 256);
	 when Ld2i =>
	    Rh(Half(Ins.Q))(0) := Half(A);
	 when Ld1 =>
	    Rb(Half(Ins.Q))(0) := M(A);
	 when Ldl2 =>
	    Rb(Half(Ins.Q))(0) := M(A);
	    Rb(Half(Ins.Q))(1) := M(A+1);
	 when Ldh2 =>
	    Rb(Half(Ins.Q))(2) := M(A);
	    Rb(Half(Ins.Q))(3) := M(A+1);
	 when Ld4 =>
	    Rb(Half(Ins.Q)) := Bytes_Register(M(A..A+3));
	 when Ldm =>
	    for Y in 0..Ins.Q loop
	       Rb(Half(Y)) := Bytes_Register(M((A+Half(4*Y))..(A+Half(4*Y+3))));
	    end loop;
	 when Ld4i =>
	    Ri(Half(Ins.Q)) := Integer(Ins.D);
	 when Mg2 =>
	    Rh(Half(Ins.Q))(0) := Rh(Half(Ins.Q))(0) or (Half(M(A)) + 256 * Half(M(A+1)));
	 when Mg4 =>
	      Rw(Half(Ins.Q)) :=  Rw(Half(Ins.Q)) or 
		Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when St1 => 
	    M(A) := Rb(Half(Ins.Q))(0);
	 when St4 =>
	    M(A..A+3) := Memory_Bytes(Rb(Half(Ins.Q)));
	 when Sth2 =>
	    M(A) := Rb(Half(Ins.Q))(2);
	    M(A+1) := Rb(Half(Ins.Q))(3);
	 when Stm =>
	    for Y in 0..Ins.Q loop
	       M((A+Half(4*Y))..(A+Half(4*Y+3))) := Memory_Bytes(Rb(Half(Y)));
	    end loop;
	 when Sb4i =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) - Words_Register(A);
	    Setsz;
	 when Sb4 =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) - Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Halt =>
	    Halted := True;
	 when Print => -- this should be a case..
	    Format(1) := Character'Val(Natural(Ins.D/256));
	    Format(2) := Character'Val(Natural(Ins.D mod 256));
	    if Ins.D=0  then
	       Put(Character'Val(Rw(Half(Ins.Q)) and 16#0ff#));
	    elsif Ins.D<255 and then Format(2)='d' then
	       Put(Integer'Image(Ri(Half(Ins.Q))));
	    else
	       if Format(1)<' ' or Format(1)>'~'then
	       Format(1) := '?';
	      end if;
	      if Format(2)<' ' or Format(2)>'~' then
	       Format(2) := '?';
	      end if;
	      Put(Format);
	    end if;
	 when others =>
	    Error("BAD OP, not implemented (yet)." & Opcod'Image(Ins.Op));
      end case;
      return;
   end Step;
   
   procedure Run(Maxtime: Long_Integer) is
      
      Steps: Long_Integer:= 0;
      Start, Stop: Time;
      
   begin
      Put_Line("RUNNING FROM " & To_Hex_Half(Run_Entry) & " ===>");
      Halted := False;
      Pc := Run_Entry;
      Start := Clock;
      for K in 1..Maxtime loop
	 Step;
	 Steps := K;
	 if Halted or In_Error Then
	    exit;
	 end if;
      end loop;
      Stop := Clock;
      Put_Line("<=== FINISHED ");
      if Halted then
	 Put_Line("HALT instruction encountered.");
      elsif In_Error then
	 Put_Line("Stopped because of error.");
      else
	 Put_Line("Finished by the system after the " & Long_Integer'Image(Maxtime) & " steps.");
      end if;
      Put_Line("Executed " & Long_Integer'Image(Steps) & " instructions " & " in " & Duration'Image(Stop-Start)
		 & " seconds, " & Ascii.Lf & " at a speed of " & 
		 Integer'Image(Integer(Long_Float(Steps) / Long_Float(Stop-Start) / 1_000_000.0))
	         & " BogoMIPS");
--      Vidage;
   end Run;
   
   -- PROCESS JOB SET
   
   procedure Process_Control_Card is
      
      Cmd: String := "" & Incard(Mnemo(0)..Mnemo(1));
      Cc_Done: Boolean:= False;
      Maxtime: Long_Integer := 500;
      
   begin
--      Put_Line("cmd = " & Cmd);
      if Cmd="JOB" then
	 if Injob then
	    Error("Already in job.");
	    return;
	 else
	    Injob := True;
	    Incompile := False;
	    Cc_Done := True;
	 end if;
      end if;
      if Cmd="CONF" then
	 for K in 1..Nextarg-1 loop
	    if Args(K).Name(0) > 0 and then
	      Incard(Args(K).Name(0)..Args(K).Name(1)) = "VERB"
	      and then Args(K).T=Int  then
	       Verbosity := Args(K).Ival;
	    end if;
	 end loop;
	 Cc_Done := True;
      end if;
      if Cmd="SAY" or else Cmd="LIST" then
	 for K in 1..Nextarg-1 loop
	    if Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "HELP" then
	       Print_Help;
	    elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "ABOUT" then
	       Print_About;
	    elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "SYMS" then
	       Print_Symbol_Table;
	    elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "LINKS" then
	       Print_Links_Table;
	    elsif Args(K).T=Str and then Args(K).Sval(1..Args(K).Len) = "DUMP" then
	       Vidage;
	    elsif Args(K).Name(0) > 0 and then
	      Incard(Args(K).Name(0)..Args(K).Name(1)) = "MSG"
	      and then Args(K).T=Str  then
	       Put_Line(Args(K).Sval(1..Args(K).Len));
	    end if;
	 end loop;
	 Cc_Done := True;
      end if;
      if Injob then -- only considered inside a job, ignored otherwise
	 if Cmd="EOJ" then
	    Injob := False;
	    Incompile := False;
	    Cc_Done := True;
	 elsif Cmd="COMPILE" then
	    Incompile := True;
	    Cc_Done := True;
	    Print_Asm_Header;
	 elsif Cmd="LINK" then
--	    Put("LINKING " & Integer'Image(Lt_Next-1) & " LOCATIONS ... ");
	    Incompile := False;
	    Link_Edit;
--	    Put_Line("DONE");
	    Cc_Done := True;
	 elsif Cmd="RUN" then
	    for K in 1..Nextarg-1 loop
	       if Args(K).Name(0) > 0 and then
		 Incard(Args(K).Name(0)..Args(K).Name(1)) = "KINS"
		 and then Args(K).T=Int  then
		  Maxtime := Long_Integer(Args(K).Ival) * 1_000;
	       end if;
	    end loop;
	    Run(Maxtime);
	    Cc_Done := True;
	 end if;
      end if;
      if not Cc_Done then
	 Error("Control card could not be processed.");
      end if;
   end;

   
   procedure Process_Job_Set(From: File_Type) is
      
   begin
      Set_All_Ops;
      Clear_Memory;
      -- Print_Asm_Header;
      
      -- read and assemble
      
      Asm := 64;
      
      loop
	 Read_Next_Card(From);
	 if No_More_Cards then
	    exit;
	 end if;
	 Parse_Card;
	 if Card_Type='.' then
	    Process_Control_Card;
	 end if;
	 if Incompile then
	    if (Card_Type=' ' or Card_Type='A') and then Incompile then
	       Assemble_Card;
	       Print_Card (not Dir);
	    else
	       Print_Card (False);
	    end if;
	 end if;
      end loop;
      
   end Process_Job_Set;
   
   -- SOME EARLY TESTS
   
   procedure Test_Asm_1 is
      
   begin
      Assemble_Ins("LD1I", 1, 0, Character'Pos('H'));
      Assemble_Ins("LD1I", 2, 0, Character'Pos('e'));
      Assemble_Ins("LD1I", 3, 0, Character'Pos('l'));
      Assemble_Ins("LD1I", 4, 0, Character'Pos('l'));
      Assemble_Ins("LD1I", 5, 0, Character'Pos('o'));
      Assemble_Ins("LD1I", 6, 0, Character'Pos(Ascii.Lf));
      Assemble_Ins("PRINT",1,0,0);
      Assemble_Ins("PRINT",2,0,0);
      Assemble_Ins("PRINT",3,0,0);
      Assemble_Ins("PRINT",4,0,0);
      Assemble_Ins("PRINT",5,0,0);
      Assemble_Ins("PRINT",6,0,0);
      Assemble_Ins("HALT",0,0,0);
   end Test_Asm_1;
   
   procedure Test_Ins_Mem is
      
   begin
      Asm := 213;
      Inw := 16#01020304#;
      M(Asm..Asm+3) := Inb; 
      Put_Line(Half'Image(INS.D));
      Inw := 0;
      Put_Line(Half'Image(INS.D));
      Inb := M(Asm..Asm+3);
      Put_Line(Half'Image(INS.D));
   end Test_Ins_Mem;
   
   -- THE MAIN LOOP
   
begin
   
   if Argument_Count /= 0 then
      Print_About;
      return;
   end if;
   
   Process_Job_Set(Standard_Input);
   
end Felix;
