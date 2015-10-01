-- BinToAsc.Base32
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

pragma Warnings (Off, "suspicious *mod* value, was ** intended?");

package body BinToAsc.Base32 is

   Reverse_Alphabet : constant Reverse_Alphabet_Lookup
     := Make_Reverse_Alphabet(Alphabet, True);

   --
   -- Base32_To_String
   --

   procedure Reset (C : out Base32_To_String) is
   begin
      C := (State => Ready,
            Next_Index => 0,
            Buffer => (others => 0));
   end Reset;

   procedure Process
     (C : in out Base32_To_String;
      Input : in Bin;
      Output : out String;
      Output_Length : out Natural)
   is

   begin
      C.Buffer(C.Next_Index) := Input;
      if C.Next_Index /= 4 then
         Output := (others => ' ');
         Output_Length := 0;
         C.Next_Index := C.Next_Index + 1;
      else
         C.Next_Index := 0;
         Output := ( Alphabet(Integer(C.Buffer(0) / 8)),
                     Alphabet(Integer((C.Buffer(0) mod 8) * 4 + C.Buffer(1) / 64)),
                     Alphabet(Integer((C.Buffer(1) mod 64) / 2)),
                     Alphabet(Integer((C.Buffer(1) mod 2) * 16 + C.Buffer(2) / 16)),
                     Alphabet(Integer((C.Buffer(2) mod 16) * 2 + C.Buffer(3) / 128)),
                     Alphabet(Integer((C.Buffer(3) mod 128) / 4)),
                     Alphabet(Integer((C.Buffer(3) mod 4) * 8 + C.Buffer(4) / 32)),
                     Alphabet(Integer(C.Buffer(4) mod 32)),
                     others => ' '
                    );
         Output_Length := 8;
      end if;
   end Process;

   procedure Process
     (C : in out Base32_To_String;
      Input : in Bin_Array;
      Output : out String;
      Output_Length : out Natural)
   is
      Output_Index : Integer := Output'First;
   begin
      for I in Input'Range loop
         C.Buffer(C.Next_Index) := Input(I);
         if C.Next_Index /= 4 then
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Output (Output_Index .. Output_Index + 7) :=
              ( Alphabet(Integer(C.Buffer(0) / 8)),
                Alphabet(Integer((C.Buffer(0) mod 8) * 4 + C.Buffer(1) / 64)),
                Alphabet(Integer((C.Buffer(1) mod 64) / 2)),
                Alphabet(Integer((C.Buffer(1) mod 2) * 16 + C.Buffer(2) / 16)),
                Alphabet(Integer((C.Buffer(2) mod 16) * 2 + C.Buffer(3) / 128)),
                Alphabet(Integer((C.Buffer(3) mod 128) / 4)),
                Alphabet(Integer((C.Buffer(3) mod 4) * 8 + C.Buffer(4) / 32)),
                Alphabet(Integer(C.Buffer(4) mod 32))
               );
            Output_Index := Output_Index + 8;
         end if;
      end loop;
      Output_Length := Output_Index - Output'First;
   end Process;

   procedure Complete
     (C : in out Base32_To_String;
      Output : out String;
      Output_Length : out Natural)
   is
   begin
      C.State := Completed;
      case C.Next_Index is
         when 0 =>
            Output := (others => ' ');
            Output_Length := 0;
         when 1 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 8)),
                        Alphabet(Integer((C.Buffer(0) mod 8) * 4 + 0)),
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 2 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 8)),
                        Alphabet(Integer((C.Buffer(0) mod 8) * 4 + C.Buffer(1) / 64)),
                        Alphabet(Integer((C.Buffer(1) mod 64) / 2)),
                        Alphabet(Integer((C.Buffer(1) mod 2) * 16 + 0)),
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 3 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 8)),
                        Alphabet(Integer((C.Buffer(0) mod 8) * 4 + C.Buffer(1) / 64)),
                        Alphabet(Integer((C.Buffer(1) mod 64) / 2)),
                        Alphabet(Integer((C.Buffer(1) mod 2) * 16 + C.Buffer(2) / 16)),
                        Alphabet(Integer((C.Buffer(2) mod 16) * 2 + 0)),
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 4 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 8)),
                        Alphabet(Integer((C.Buffer(0) mod 8) * 4 + C.Buffer(1) / 64)),
                        Alphabet(Integer((C.Buffer(1) mod 64) / 2)),
                        Alphabet(Integer((C.Buffer(1) mod 2) * 16 + C.Buffer(2) / 16)),
                        Alphabet(Integer((C.Buffer(2) mod 16) * 2 + C.Buffer(3) / 128)),
                        Alphabet(Integer((C.Buffer(3) mod 128) / 4)),
                        Alphabet(Integer((C.Buffer(3) mod 4) * 8 + 0)),
                        Padding,
                        others => ' ');
            Output_Length := 8;
      end case;
   end Complete;

   function To_String_Private is
     new BinToAsc.To_String(Codec => Base32_To_String);

   function To_String (Input : in Bin_Array) return String
                       renames To_String_Private;

   --
   -- Base32_To_Bin
   --

   function Padding_Characters_Effect(X : Bin_Array_Index)
                                      return Bin_Array_Index is
     (case X is
         when 0 => 0,
         when 1 => 1,
         when 3 => 2,
         when 4 => 3,
         when 6 => 4,
         when others => 0);

   procedure Reset (C : out Base32_To_Bin) is
   begin
      C := (State => Ready,
            Next_Index => 0,
            Buffer => (others => 0),
            Padding_Length => 0);
   end Reset;

   procedure Process (C : in out Base32_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
   begin

      if Input = Padding then
         Input_Bin := 0;
         C.Padding_Length := C.Padding_Length + 1;
         if C.Padding_Length > 6 then
            -- No reason to ever have more than six padding characters in Base32
            -- input
            C.State := Failed;
         end if;
      elsif C.Padding_Length > 0 then
         -- After the first padding character, only a second padding character
         -- can be valid
         C.State := Failed;
      else
         Input_Bin := Reverse_Alphabet(Input);
         if Input_Bin = Invalid_Character_Input then
            C.State := Failed;
         end if;
      end if;

      if not (C.State = Failed) then
         C.Buffer(C.Next_Index) := Input_Bin;

         if C.Next_Index /= 7 then
            Output := (others => 0);
            Output_Length := 0;
            C.Next_Index := C.Next_Index + 1;
         elsif C.Padding_Length = 2 or C.Padding_Length = 5 then
            Output := (others => 0);
            Output_Length := 0;
            C.State := Failed;
         else
            C.Next_Index := 0;
            Output := ( C.Buffer(0) * 8 + C.Buffer(1) / 4,
                        (C.Buffer(1) mod 4) * 64 + C.Buffer(2) * 2 + C.Buffer(3) / 16,
                        (C.Buffer(3) mod 16) * 16 + C.Buffer(4) / 2,
                        (C.Buffer(4) mod 2) * 128 + C.Buffer(5) * 4 + C.Buffer(6) / 8,
                        (C.Buffer(6) mod 8) * 32 + C.Buffer(7),
                        others => 0);
            Output_Length := 5 - Padding_Characters_Effect(C.Padding_Length);
         end if;
      else
         Output := (others => 0);
         Output_Length := 0;
      end if;
   end Process;

   procedure Process (C : in out Base32_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
      Output_Index : Bin_Array_Index := Output'First;
   begin
      for I in Input'Range loop

         if Input(I) = Padding then
            Input_Bin := 0;
            C.Padding_Length := C.Padding_Length + 1;
            if C.Padding_Length > 6 then
               -- No reason to ever have more than six padding characters in
               -- Base64 input
               C.State := Failed;
               exit;
            end if;
         elsif C.Padding_Length > 0 then
            -- After the first padding character, only a second padding
            -- character can be valid
            C.State := Failed;
            exit;
         else
            Input_Bin := Reverse_Alphabet(Input(I));
            if Input_Bin = Invalid_Character_Input then
               C.State := Failed;
               exit;
            end if;
         end if;

         C.Buffer(C.Next_Index) := Input_Bin;

         if C.Next_Index /= 7 then
            C.Next_Index := C.Next_Index + 1;
         elsif C.Padding_Length = 2 or C.Padding_Length = 5 then
            C.State := Failed;
         else
            C.Next_Index := 0;
            Output(Output_Index .. Output_Index + 4) :=
              ( C.Buffer(0) * 8 + C.Buffer(1) / 4,
                (C.Buffer(1) mod 4) * 64 + C.Buffer(2) * 2 + C.Buffer(3) / 16,
                (C.Buffer(3) mod 16) * 16 + C.Buffer(4) / 2,
                (C.Buffer(4) mod 2) * 128 + C.Buffer(5) * 4 + C.Buffer(6) / 8,
                (C.Buffer(6) mod 8) * 32 + C.Buffer(7));
            Output_Index := Output_Index + 5;
         end if;

      end loop;

      if C.State = Failed then
         Output := (others => 0);
         Output_Length := 0;
      else
         Output(Output_Index .. Output'Last) := (others => 0);
         Output_Length := Output_Index - Output'First -
           Padding_Characters_Effect(C.Padding_Length);
      end if;

   end Process;

   procedure Completed (C : in out Base32_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
   is
   begin
      if C.Next_Index /= 0 then
         C.State := Failed;
      else
         C.State := Completed;
      end if;
      Output := (others => 0);
      Output_Length := 0;
   end Completed;

   function To_Bin_Private is new BinToAsc.To_Bin(Codec => Base32_To_Bin);

   function To_Bin (Input : in String) return Bin_Array renames To_Bin_Private;

begin

   -- The following Compile_Time_Error test is silently ignored by GNAT GPL 2015,
   -- although it does appear to be a static boolean expression as required by
   -- the user guide. It works if converted to a run-time test so it has been
   -- left in, in the hope that in a future version of GNAT it will actually be
   -- tested.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((for some X in 1..Alphabet'Last =>
                                 (for some Y in 0..X-1 =>
                                    Alphabet(Y) = Alphabet(X)
                                 )
                               ),
                              "Duplicate letter in alphabet for Base64 codec.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc.Base32;
