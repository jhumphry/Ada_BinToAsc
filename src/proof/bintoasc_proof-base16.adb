-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc_Proof.Base16
with SPARK_Mode => On is

   use Ada.Characters.Handling;

   Reverse_Alphabet : constant Reverse_Alphabet_Lookup
     := Make_Reverse_Alphabet(Alphabet, Case_Sensitive);

   --
   -- Base16_To_String
   --

   procedure Reset (C : out Base16_To_String) is
   begin
      C := (State => Ready);
   end Reset;

   procedure Process
     (C : in out Base16_To_String;
      Input : in Bin;
      Output : out String;
      Output_Length : out Natural)
   is
      pragma Unreferenced (C);
      Input_Index : constant Bin := Bin'Pos(Input);
   begin
      Output_Length := 2;
      Output := (Alphabet(Input_Index / 16),
                 Alphabet(Input_Index and 2#00001111#),
                 others => ' ');
   end Process;

   procedure Process
     (C : in out Base16_To_String;
      Input : in Bin_Array;
      Output : out String;
      Output_Length : out Natural)
   is
      pragma Unreferenced (C);
      Output_Index : Integer := Output'First;
      Input_Index : Bin;
   begin
      pragma Assert (Output'Length >= 2 * Input'Length);
      Output_Length := 2 * Input'Length;
      for I in Input'Range loop
         pragma Loop_Invariant (Output_Index = Output'First + Integer(I-Input'First) * 2);
         Input_Index := Bin'Pos(Input(I));
         Output(Output_Index) := Alphabet(Input_Index / 16);
         Output(Output_Index + 1) := Alphabet(Input_Index and 2#00001111#);
         Output_Index := Output_Index + 2;
      end loop;
      pragma Assert ((Output_Index - Output'First) = Output_Length);
      Output(Output_Index .. Output'Last) := (others => ' ');
   end Process;
   pragma Annotate (GNATprove, False_Positive,
                    """Output"" might not be initialized",
                    "Output_Index from Output'First to Output_Index is filled, the rest is cleared");

   procedure Complete
     (C : in out Base16_To_String;
      Output : out String;
      Output_Length : out Natural)
   is
   begin
      C.State := Completed;
      Output := (others => ' ');
      Output_Length := 0;
   end Complete;

   function To_String_Private is
     new BinToAsc_Proof.To_String(Codec             => Base16_To_String,
                            Input_Group_Size  => 1,
                            Output_Group_Size => 2);

   function To_String (Input : in Bin_Array) return String
                       renames To_String_Private;

   --
   -- Base16_To_Bin
   --

   procedure Reset (C : out Base16_To_Bin) is
   begin
      C := (State => Ready,
            Loaded => False,
            Load => 0);
   end Reset;

   procedure Process (C : in out Base16_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
   begin
         Input_Bin := Reverse_Alphabet(Input);

      if Input_Bin = Invalid_Character_Input then
         Output := (others => 0);
         Output_Length := 0;
         C.State := Failed;
      else
         if C.Loaded then
            Output := (Bin(C.Load) * 16 or Input_Bin,
                       others => 0);
            Output_Length := 1;
            C.Loaded := False;
         else
            Output := (others => 0);
            Output_Length := 0;
            C.Loaded := True;
            C.Load := Input_Bin;
         end if;
      end if;
   end Process;

   procedure Process (C : in out Base16_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
      Output_Index : Bin_Array_Index := Output'First;
   begin
      for I in Input'Range loop

         Input_Bin := Reverse_Alphabet(Input(I));

         if Input_Bin = Invalid_Character_Input then
            C.State := Failed;
            exit;
         end if;

         if C.Loaded then
            Output(Output_Index) := Bin(C.Load) * 16 or Input_Bin;
            Output_Index := Output_Index + 1;
            C.Loaded := False;
         else
            C.Loaded := True;
            C.Load := Input_Bin;
         end if;

      end loop;

      if C.State = Failed then
         Output := (others => 0);
         Output_Length := 0;
      else
         Output(Output_Index .. Output'Last) := (others => 0);
         Output_Length := Output_Index - Output'First;
      end if;

   end Process;

   procedure Complete (C : in out Base16_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
   is
   begin
      if C.Loaded then
         C.State := Failed;
      elsif C.State = Ready then
         C.State := Completed;
      end if;
      Output := (others => 0);
      Output_Length := 0;
   end Complete;

   function To_Bin_Private is new BinToAsc_Proof.To_Bin(Codec => Base16_To_Bin,
                                                  Input_Group_Size  => 2,
                                                  Output_Group_Size => 1);

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
                              "Duplicate letter in alphabet for Base16 codec.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc_Proof.Base16;
