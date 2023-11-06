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

procedure Felix is
   
   -- CONFIGURATION VARIABLES
   
   Verbosity: Integer:= 2;
   
   -- BASIC TYPES
   
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
   Pc: Half:= 64;
      
   -- INS -- a register where we read the nextr instruction to execute or assemble
   
   Inw: Word;
   Inb: Memory_Bytes(0..3);
   for Inb'Address use Inw'Address;
   Ins: Instr;
   for Ins'Address use Inw'Address;
   
   Asm: Half:= 64; -- after the registers..
   
   
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
   
   -- THE CARD READER (reads from standard_input).
   
   subtype Card is String(1..80); -- a line
   
   Incard: Card;
   Inlen: Natural;
   No_More_Cards: Boolean:= False;
   
   Cardno: Natural:= 0;
   
   procedure Read_Next_Card is
      
   begin
      Incard := ( others => ' '); -- this is card semantics..
      Cardno := Cardno + 1;
      Get_Line(Standard_Input, Incard, Inlen);
   exception
      when End_Error => No_More_Cards := True;
   end Read_Next_Card;
   
   -- THE CARD PARSER AND PRINTER
   
   type Span is array(0..1) of Natural;

   Card_Type: Character; -- ' ' unlabeled instruction, '.' control card, '*' comment, 'A' labeled instruction, '$' error
   Label, Mnemo, R, Arg: Span := (others => 0);
   
   procedure Parse_Card is
      
      
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
      
      J: Natural;
      
      procedure Error(Msg: String) is
	 
      begin
	 Put_Line(Standard_Error, "ERR: " & Msg & " in: " & Integer'Image(Cardno) & " : " & Incard(1..Inlen) & Ascii.Lf);
      end Error;
   begin
      Card_Type := '*';
      Label := (others => 0);
      Mnemo := (others => 0);
      R := (others => 0);
      Arg := (others => 0);
      
      if Inlen < 2 then
	 return;
      end if;
      
      case Incard(1) is
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
      
      if Card_Type = '*' or else Card_Type = '.' then
	 return ; -- we ignore control cards for now
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
		  while not Is_White(Incard(J)) and J<=Inlen loop
		     J := J+1;
		  end loop;
		  Arg(1) := J;
		  if J>Inlen then
		     Arg(1) := J-1;
		  end if;
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
      Put_Line("*** *** ASSEMBLY LISTING *** ***");
      New_Line;
      Put_Line("ADDR I B  Q OP X  D   LINE NR: LABEL    INSTRUCTION");
      New_Line;
   end Print_Asm_Header;
   
   procedure Print_Card(With_Assembly: Boolean) is
      Nr: String:= Integer'Image(Cardno);
      Snr: String(1..6) := ( others => ' ' );
      Slabel: String(1..10) := ( others => ' ' );
      Smnemor: String(1..10) := ( others => ' ');
      Sarg: String(1..18) := ( others => ' ');
      Sasm: String(1..18) := ( others => ' ');
      Sd: String := Half'Image(Ins.D);
      
      
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
      
      Put(To_Hex_Half(Asm-4));
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
      
   -- THE ASSEMBLER
   
   
   
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
    Setops(LDC8, 16#18#);
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
     Setops(St1, 16#58#);
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
   
   procedure Asminc(Op: Opcod; Q: Qfld; D: Half) is
      
   begin
      Ins.Op := Op;
      Ins.D := D;
      Ins.Q := Q;
      Ins.X := 0; -- we only do direct addressing, currently
      Ins.I := 0;
      M(Asm..Asm+3) := Inb;
      Asm := Asm+4;
   end Asminc;
   
   
   procedure Assemble_Ins(Opid: String; Rarg0: Natural; Disp0: Natural) is
      
      Rarg: Qfld:= Qfld(Rarg0);
      Disp: Half:= Half(Disp0);

   begin
      for K in Ops loop
	 if Opid=Ops'Image(K) then
	    Asminc(Byops(K),Rarg,Disp);
	    return;
	 end if;
      end loop;
      Put_Line("Invalid opcode: " & Opid);
   end Assemble_Ins;
   
   procedure Assemble_Card is 
      -- card that has just been read and parsed, found in incard
      -- and parsed in mnemo & folks
      
      
      -- the following will treat all expressions
      -- that are expected to be integers
      
      function To_Half(X: String) return Half is
	 
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
	    return Find_In_Table(X) /= Symbol_Not_Found;
	 end Is_Label;
	 
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
		  Rv := Rv * 16 + Character'Pos(X(K)) - Half(Amai);
	       end if;
	    end loop;
	    return Rv;	    
	 else
	    Put_Line(Standard_Error, "I do not understand: " & X);
	 end if;
	 return 0;
      end To_Half;
      
      function Half_Arg return Half is
	 
      begin
	 if Arg(0) = 0 then
	    return 0;
	 else
	    return To_Half(Incard(Arg(0)..Arg(1)));
	 end if;
      end Half_Arg;
      
   begin
      if Label(0) > 0 then
	 if Mnemo(0) > 0 and then "" & Incard(Mnemo(0)..Mnemo(1)) = "EQU" then
	    Add_To_Table(Incard(Label(0)..Label(1)), Word(Half_Arg)); -- but must check for the sign!
	    return;
	 else
	    Add_To_Table(Incard(Label(0)..Label(1)), Word(Asm));
	 end if;
      end if;
      if Mnemo(0) > 0 then
	 if R(0) > 0 then
	    Assemble_Ins( Opid => Incard(Mnemo(0)..Mnemo(1)),
			  Rarg0 => Natural(To_Half(Incard(R(0)..R(1)))),
			  Disp0 => Natural(Half_Arg));
	 else
	    Assemble_Ins( Opid => Incard(Mnemo(0)..Mnemo(1)),
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
   
   procedure Step is
      
      A: Half; -- the computed address
      
      procedure Error(M: String) is
	 
      begin
	 Put_Line("Execution error at address " & half'image(pc-4) & " : " & M);
	 return ;
      end Error;
      
      Format: String(1..2);
      
   begin
      Inb := M(Pc..Pc+3);
      Pc := Pc+4;
      
      if Ins.X=0 and Ins.I=0 then
	 A := Ins.D;
      else
	 Error("Only direct addressing implemented so far.");
	 return;
      end if;
      
      case Byopcod(Ins.Op) is
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
	 when Ld1i =>
	    Rb(Half(Ins.Q))(0) := Byte(Half(Ins.D) mod 256);
	 when Ld2i =>
	    Rh(Half(Ins.Q))(0) := Half(Ins.D);
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
	 when St1 => 
	    M(A) := Rb(Half(Ins.Q))(0);
	 when St4 =>
	    M(A..A+3) := Memory_Bytes(Rb(Half(Ins.Q)));
	 when Sth2 =>
	    M(A) := Rb(Half(Ins.Q))(2);
	    M(A+1) := Rb(Half(Ins.Q))(3);
	 when Ad4i =>
	    Rw(Half(Ins.Q)) := Words_Register(Word(Rw(Half(Ins.Q))) + Word(Ins.D));
	 when Sb4i =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) - Words_Register(Ins.D);
	 when Ad4 =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) + 
	      Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Sb4 =>
	    Rw(Half(Ins.Q)) := Rw(Half(Ins.Q)) - Words_Register(Word(M(A)) + 2**8 * Word(M(A+1)) + 2**16 * Word(M(A+2)) + 2**24 * Word(M(A+3)));
	 when Halt =>
	    Halted := True;
	 when others =>
	    Error("BAD OP, not implemented (yet)." & Opcod'Image(Ins.Op));
      end case;
      return;
   end Step;
   
   -- TESTS
   
   procedure Test_Asm_1 is
      
   begin
      Assemble_Ins("LD1I", 1, Character'Pos('H'));
      Assemble_Ins("LD1I", 2, Character'Pos('e'));
      Assemble_Ins("LD1I", 3, Character'Pos('l'));
      Assemble_Ins("LD1I", 4, Character'Pos('l'));
      Assemble_Ins("LD1I", 5, Character'Pos('o'));
      Assemble_Ins("LD1I", 6, Character'Pos(Ascii.Lf));
      Assemble_Ins("PRINT",1,0);
      Assemble_Ins("PRINT",2,0);
      Assemble_Ins("PRINT",3,0);
      Assemble_Ins("PRINT",4,0);
      Assemble_Ins("PRINT",5,0);
      Assemble_Ins("PRINT",6,0);
      Assemble_Ins("HALT",0,0);
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
   
   Set_All_Ops;
   Clear_Memory;
   Print_Asm_Header;
   
   -- read and assemble
    
   loop
      Read_Next_Card;
      if No_More_Cards then
	 exit;
      end if;
      Parse_Card;
      if Card_Type=' ' or Card_Type='A' then
	 Assemble_Card;
	 Print_Card (True);
      else
	 Print_Card (False);
      end if;
   end loop;
   
   --   Asm := 64;
   --   Test_Asm_1;
   --   Put_Line("Asm pointer " & Half'Image(Asm));
   
   -- execute the code
   
   New_Line;
   Put_Line("*** *** RUNNING *** ***");
   
   Halted := False;
   Pc := 64;
   for K in 1..100 loop
      Step;
      if Halted then
	 exit;
      end if;
   end loop;
end Felix;
