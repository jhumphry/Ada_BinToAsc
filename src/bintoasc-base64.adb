-- BinToAsc.Base64
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

package body BinToAsc.Base64 is

   Reverse_Alphabet : constant Reverse_Alphabet_Lookup
     := Make_Reverse_Alphabet(Alphabet, True);
   pragma Unreferenced (Reverse_Alphabet);

   --
   -- Base64_To_String
   --

   procedure Reset (C : out Base64_To_String) is
   begin
      C := (State => Ready,
            Next_Index => 0,
            Buffer => (others => 0));
   end Reset;

   procedure Process
     (C : in out Base64_To_String;
      Input : in Bin;
      Output : out String;
      Output_Length : out Natural)
   is

   begin
      C.Buffer(C.Next_Index) := Input;
      if C.Next_Index /= 2 then
         Output := (others => ' ');
         Output_Length := 0;
         C.Next_Index := C.Next_Index + 1;
      else
         C.Next_Index := 0;
         Output := ( Alphabet(Integer(C.Buffer(0) / 4)),
                     Alphabet(Integer((C.Buffer(0) mod 4) * 16 + C.Buffer(1) / 16)),
                     Alphabet(Integer((C.Buffer(1) mod 16) * 4 + C.Buffer(2) / 64)),
                     Alphabet(Integer(C.Buffer(2) mod 64)),
                     others => ' ');
         Output_Length := 4;
      end if;
   end Process;

   procedure Process
     (C : in out Base64_To_String;
      Input : in Bin_Array;
      Output : out String;
      Output_Length : out Natural)
   is
      Output_Index : Integer := Output'First;
   begin
      for I in Input'Range loop
         C.Buffer(C.Next_Index) := Input(I);
         if C.Next_Index /= 2 then
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Output (Output_Index .. Output_Index + 3) :=
              ( Alphabet(Integer(C.Buffer(0) / 4)),
                Alphabet(Integer((C.Buffer(0) mod 4) * 16 + C.Buffer(1) / 16)),
                Alphabet(Integer((C.Buffer(1) mod 16) * 4 + C.Buffer(2) / 64)),
                Alphabet(Integer(C.Buffer(2) mod 64))
               );
            Output_Index := Output_Index + 4;
         end if;
      end loop;
      Output_Length := Output_Index - Output'First;
   end Process;

   procedure Completed
     (C : in out Base64_To_String;
      Output : out String;
      Output_Length : out Natural)
   is
   begin
      C.State := Complete;
      case C.Next_Index is
         when 0 =>
            Output := (others => ' ');
            Output_Length := 0;
         when 1 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 4)),
                        Alphabet(Integer((C.Buffer(0) mod 4) * 16 + 0)),
                        Padding,
                        Padding,
                        others => ' ');
            Output_Length := 4;
         when 2 =>
            Output := ( Alphabet(Integer(C.Buffer(0) / 4)),
                        Alphabet(Integer((C.Buffer(0) mod 4) * 16 + C.Buffer(1) / 16)),
                        Alphabet(Integer((C.Buffer(1) mod 16) * 4 + 0)),
                        Padding,
                        others => ' ');
            Output_Length := 4;
      end case;
   end Completed;

   --
   -- Base64_To_Bin
   --

--     procedure Reset (C : out Base64_To_Bin) is
--     begin
--        C := (State => Ready,
--              Loaded => False,
--              Load => 0);
--     end Reset;
--
--     procedure Process (C : in out Base64_To_Bin;
--                        Input : in Character;
--                        Output : out Bin_Array;
--                        Output_Length : out Bin_Array_Index)
--     is
--        Input_Bin : Bin;
--     begin
--           Input_Bin := Reverse_Alphabet(if Case_Sensitive
--                                         then Input
--                                         else To_Lower(Input)
--                                        );
--
--        if Input_Bin = 255 then
--           Output_Length := 0;
--           C.State := Failed;
--        else
--           if C.Loaded then
--              Output(Output'First) := Bin(C.Load) * 16 + Input_Bin;
--              Output(Output'First + 1 .. Output'Last) := (others => 0);
--              Output_Length := 1;
--              C.Loaded := False;
--           else
--              Output := (others => 0);
--              Output_Length := 0;
--              C.Loaded := True;
--              C.Load := Input_Bin;
--           end if;
--        end if;
--     end Process;
--
--     procedure Process (C : in out Base64_To_Bin;
--                        Input : in String;
--                        Output : out Bin_Array;
--                        Output_Length : out Bin_Array_Index)
--     is
--        Input_Bin : Bin;
--        Conversion_Error : Boolean := False;
--        Output_Index : Bin_Array_Index := Output'First;
--     begin
--        for I in Input'Range loop
--
--           Input_Bin := Reverse_Alphabet(if Case_Sensitive
--                                         then Input(I)
--                                         else To_Lower(Input(I))
--                                        );
--
--           if Input_Bin = 255 then
--              Conversion_Error := True;
--              exit;
--           end if;
--
--           if C.Loaded then
--              Output(Output_Index) := Bin(C.Load) * 16 + Input_Bin;
--              Output_Index := Output_Index + 1;
--              C.Loaded := False;
--           else
--              C.Loaded := True;
--              C.Load := Input_Bin;
--           end if;
--
--        end loop;
--
--        if Conversion_Error then
--           Output := (others => 0);
--           Output_Length := 0;
--           C.State := Failed;
--        else
--           Output(Output_Index .. Output'Last) := (others => 0);
--           Output_Length := Output_Index - Output'First;
--        end if;
--
--     end Process;
--
--     procedure Completed (C : in out Base64_To_Bin;
--                          Output : out Bin_Array;
--                          Output_Length : out Bin_Array_Index)
--     is
--     begin
--        if C.Loaded = False then
--           C.State := Complete;
--        else
--           C.State := Failed;
--        end if;
--        Output := (others => 0);
--        Output_Length := 0;
--     end Completed;

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

end BinToAsc.Base64;
