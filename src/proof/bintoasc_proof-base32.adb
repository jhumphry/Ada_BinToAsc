-- BinToAsc.Base32
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc_Proof.Base32
with SPARK_Mode is

   use Ada.Characters.Handling;

   function Make_Base32_Reverse_Alphabet (Alphabet : in Alphabet_32;
                                          Case_Sensitive : in Boolean;
                                          Allow_Homoglyphs : in Boolean)
                                          return Reverse_Alphabet_Lookup
     with Pre => (Alphabet'First = 0 and then
                    Alphabet'Last > Alphabet'First and then
                      Valid_Alphabet(Alphabet, Case_Sensitive)),
     Post => (for all I in Make_Base32_Reverse_Alphabet'Result'Range =>
                (Make_Base32_Reverse_Alphabet'Result(I) <= Alphabet'Last and
                     Make_Base32_Reverse_Alphabet'Result(I) >= Alphabet'First) or
                  Make_Base32_Reverse_Alphabet'Result(I) = Invalid_Character_Input);

   function Make_Base32_Reverse_Alphabet (Alphabet : in Alphabet_32;
                                          Case_Sensitive : in Boolean;
                                          Allow_Homoglyphs : in Boolean)
                                          return Reverse_Alphabet_Lookup is
      R : Reverse_Alphabet_Lookup
        := Make_Reverse_Alphabet(Alphabet, Case_Sensitive);
   begin
      if Allow_Homoglyphs then
         if R('1') = Invalid_Character_Input then
            if R('l') /= Invalid_Character_Input then
               R('1') := R('l');
            elsif R('I') /= Invalid_Character_Input then
               R('1') := R('I');
            end if;
         end if;
         if R('0') = Invalid_Character_Input then
            if R('O') /= Invalid_Character_Input then
               R('0') := R('O');
            elsif R('o') /= Invalid_Character_Input then
               R('0') := R('o');
            end if;
         end if;
      end if;
      return R;
   end Make_Base32_Reverse_Alphabet;

   Reverse_Alphabet : constant Reverse_Alphabet_Lookup
     := Make_Base32_Reverse_Alphabet(Alphabet         => Alphabet,
                                     Case_Sensitive   => Case_Sensitive,
                                     Allow_Homoglyphs => Allow_Homoglyphs);
   pragma Annotate (GNATprove, Intentional,
                    "precondition might fail",
                    "It is part of the ""package precondition"" to supply valid alphabets.");

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
         Output := ( Alphabet(C.Buffer(0) / 8),
                     Alphabet((C.Buffer(0) and 2#00000111#) * 4 or C.Buffer(1) / 64),
                     Alphabet((C.Buffer(1) and 2#00111111#) / 2),
                     Alphabet((C.Buffer(1) and 2#00000001#) * 16 or C.Buffer(2) / 16),
                     Alphabet((C.Buffer(2) and 2#00001111#) * 2 or C.Buffer(3) / 128),
                     Alphabet((C.Buffer(3) and 2#01111111#) / 4),
                     Alphabet((C.Buffer(3) and 2#00000011#) * 8 or C.Buffer(4) / 32),
                     Alphabet(C.Buffer(4) and 2#00011111#),
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
         pragma Loop_Invariant (Output_Index = Output'First + (Integer(I-Input'First) / 5) * 8);
         C.Buffer(C.Next_Index) := Input(I);
         if C.Next_Index /= 4 then
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Output (Output_Index .. Output_Index + 7) :=
              ( Alphabet(C.Buffer(0) / 8),
                Alphabet((C.Buffer(0) and 2#00000111#) * 4 or C.Buffer(1) / 64),
                Alphabet((C.Buffer(1) and 2#00111111#) / 2),
                Alphabet((C.Buffer(1) and 2#00000001#) * 16 or C.Buffer(2) / 16),
                Alphabet((C.Buffer(2) and 2#00001111#) * 2 or C.Buffer(3) / 128),
                Alphabet((C.Buffer(3) and 2#01111111#) / 4),
                Alphabet((C.Buffer(3) and 2#00000011#) * 8 or C.Buffer(4) / 32),
                Alphabet(C.Buffer(4) and 2#00011111#)
               );
            Output_Index := Output_Index + 8;
         end if;
      end loop;
      Output_Length := Output_Index - Output'First;
      Output(Output_Index .. Output'Last) := (others => ' ');
   end Process;
   pragma Annotate (GNATprove, False_Positive,
                    """Output"" might not be initialized",
                    "Output_Index from Output'First to Output_Index is filled, the rest is cleared");

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
            Output := ( Alphabet(C.Buffer(0) / 8),
                        Alphabet((C.Buffer(0) and 2#00000111#) * 4),
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 2 =>
            Output := ( Alphabet(C.Buffer(0) / 8),
                        Alphabet((C.Buffer(0) and 2#00000111#) * 4 or C.Buffer(1) / 64),
                        Alphabet((C.Buffer(1) and 2#00111111#) / 2),
                        Alphabet((C.Buffer(1) and 2#00000001#) * 16),
                        Padding,
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 3 =>
            Output := ( Alphabet(C.Buffer(0) / 8),
                        Alphabet((C.Buffer(0) and 2#00000111#) * 4 or C.Buffer(1) / 64),
                        Alphabet((C.Buffer(1) and 2#00111111#) / 2),
                        Alphabet((C.Buffer(1) and 2#00000001#) * 16 or C.Buffer(2) / 16),
                        Alphabet((C.Buffer(2) and 2#00001111#) * 2),
                        Padding,
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 8;
         when 4 =>
            Output := ( Alphabet(C.Buffer(0) / 8),
                        Alphabet((C.Buffer(0) and 2#00000111#) * 4 or C.Buffer(1) / 64),
                        Alphabet((C.Buffer(1) and 2#00111111#) / 2),
                        Alphabet((C.Buffer(1) and 2#00000001#) * 16 or C.Buffer(2) / 16),
                        Alphabet((C.Buffer(2) and 2#00001111#) * 2 or C.Buffer(3) / 128),
                        Alphabet((C.Buffer(3) and 2#01111111#) / 4),
                        Alphabet((C.Buffer(3) and 2#00000011#) * 8),
                        Padding,
                        others => ' ');
            Output_Length := 8;
      end case;
   end Complete;

   function To_String_Private is
     new BinToAsc_Proof.To_String(Codec => Base32_To_String,
                            Input_Group_Size  => 5,
                            Output_Group_Size => 8);

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
   -- The Process routines should have caught invalid padding lengths and set
   -- the status to 'Failed' so the 'others' clause is not a problem here.

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
         if C.Padding_Length > 5 then
            -- No reason to ever have more than six padding characters in Base32
            -- input, so if we already have six we have failed.
            C.State := Failed;
            Output := (others => 0);
            Output_Length := 0;
            return;
         else
            C.Padding_Length := C.Padding_Length + 1;
         end if;
         Input_Bin := 0;
      elsif C.Padding_Length > 0 then
         -- After the first padding character, only a second padding character
         -- can be valid
         C.State := Failed;
         Output := (others => 0);
         Output_Length := 0;
         return;
      else
         Input_Bin := Reverse_Alphabet(Input);
         if Input_Bin = Invalid_Character_Input then
            C.State := Failed;
            Output := (others => 0);
            Output_Length := 0;
            return;
         end if;
      end if;

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
                     (C.Buffer(1) and 2#00011#) * 64 or C.Buffer(2) * 2 or C.Buffer(3) / 16,
                     (C.Buffer(3) and 2#01111#) * 16 or C.Buffer(4) / 2,
                     (C.Buffer(4) and 2#00001#) * 128 or C.Buffer(5) * 4 or C.Buffer(6) / 8,
                     (C.Buffer(6) and 2#00111#) * 32 or C.Buffer(7),
                     others => 0);
         Output_Length := 5 - Padding_Characters_Effect(C.Padding_Length);
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
            if C.Padding_Length > 5 then
            -- No reason to ever have more than six padding characters in Base32
            -- input, so if we already have six we have failed.
               C.State := Failed;
               exit;
            else
               C.Padding_Length := C.Padding_Length + 1;
            end if;
            Input_Bin := 0;
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
                (C.Buffer(1) and 2#00011#) * 64 or C.Buffer(2) * 2 or C.Buffer(3) / 16,
                (C.Buffer(3) and 2#01111#) * 16 or C.Buffer(4) / 2,
                (C.Buffer(4) and 2#00001#) * 128 or C.Buffer(5) * 4 or C.Buffer(6) / 8,
                (C.Buffer(6) and 2#00111#) * 32 or C.Buffer(7));
            Output_Index := Output_Index + 5;
         end if;

      end loop;

      if C.State = Failed then
         Output := (others => 0);
         Output_Length := 0;
      else
         Output(Output_Index .. Output'Last) := (others => 0);
         Output_Length := Bin_Array_Index'Max(0, Output_Index - Output'First -
                                                Padding_Characters_Effect(C.Padding_Length));
      end if;

   end Process;
   pragma Annotate (GNATprove, False_Positive,
                    """Output"" might not be initialized",
                    "Output_Index from Output'First to Output_Index is filled, the rest is cleared");

   procedure Complete (C : in out Base32_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
   is
   begin
      if C.Next_Index /= 0 then
         C.State := Failed;
      elsif C.State = Ready then
         C.State := Completed;
      end if;
      Output := (others => 0);
      Output_Length := 0;
   end Complete;

   function To_Bin_Private is new BinToAsc_Proof.To_Bin(Codec => Base32_To_Bin,
                                                  Input_Group_Size  => 8,
                                                  Output_Group_Size => 5);

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
                                    (Alphabet(Y) = Alphabet(X) or
                                         (not Case_Sensitive and
                                              To_Lower(Alphabet(Y)) = To_Lower(Alphabet(X)))
                                    )
                                 )
                               ),
                              "Duplicate letter in alphabet for Base32 codec.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc_Proof.Base32;
