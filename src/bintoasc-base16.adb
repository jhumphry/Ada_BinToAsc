-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Characters.Handling;

package body BinToAsc.Base16 is

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
      Input_Index : constant Integer := Bin'Pos(Input);
   begin
      Output_Length := 2;
      Output := (Alphabet(Input_Index / 16),
                 Alphabet(Input_Index mod 16),
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
      Input_Index : Integer;
   begin
      Output_Length := 2 * Input'Length;
      for I in Input'Range loop
         Input_Index := Bin'Pos(Input(I));
         Output(Output_Index) := Alphabet(Input_Index / 16);
         Output(Output_Index + 1) := Alphabet(Input_Index mod 16);
         Output_Index := Output_Index + 2;
      end loop;
   end Process;

   procedure Completed
     (C : in out Base16_To_String;
      Output : out String;
      Output_Length : out Natural)
   is
   begin
      C.State := Complete;
      Output := (others => ' ');
      Output_Length := 0;
   end Completed;

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
         Input_Bin := Reverse_Alphabet(if Case_Sensitive
                                       then Input
                                       else To_Lower(Input)
                                      );

      if Input_Bin = 255 then
         Output_Length := 0;
         C.State := Failed;
      else
         if C.Loaded then
            Output(Output'First) := Bin(C.Load) * 16 + Input_Bin;
            Output(Output'First + 1 .. Output'Last) := (others => 0);
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

         Input_Bin := Reverse_Alphabet(if Case_Sensitive
                                       then Input(I)
                                       else To_Lower(Input(I))
                                      );

         if Input_Bin = 255 then
            C.State := Failed;
            exit;
         end if;

         if C.Loaded then
            Output(Output_Index) := Bin(C.Load) * 16 + Input_Bin;
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

   procedure Completed (C : in out Base16_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
   is
   begin
      if C.Loaded = False then
         C.State := Complete;
      else
         C.State := Failed;
      end if;
      Output := (others => 0);
      Output_Length := 0;
   end Completed;

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

end BinToAsc.Base16;
