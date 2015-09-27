-- BinToAsc.Base16
-- Binary data to ASCII codecs - Base16 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

package body BinToAsc.Base16 is

   procedure Reset (C : in out Base16_To_String) is
   begin
      C.State := Ready;
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
                 Alphabet(Input_Index mod 16));
      Output(Output'First + 2 .. Output'Last) := (others => ' ');
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

end BinToAsc.Base16;
