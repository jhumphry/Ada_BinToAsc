-- BinToAsc.Base85
-- Various binary data to ASCII codecs known as Base85, ASCII85 etc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

package body BinToAsc.Base85 is

   Reverse_Alphabet : constant Reverse_Alphabet_Lookup
     := Make_Reverse_Alphabet(Alphabet, True);

   type Bin_Frame is mod 2**32;
   subtype String_Frame is String(1..5);

   --
   -- Base85_To_String
   --

   procedure Reset (C : out Base85_To_String) is
   begin
      C := (State => Ready,
            Next_Index => 0,
            Buffer => (others => 0));
   end Reset;

   procedure Process
     (C : in out Base85_To_String;
      Input : in Bin;
      Output : out String;
      Output_Length : out Natural)
   is
      Input_Frame : Bin_Frame;
      Output_Frame : String_Frame;
   begin
      C.Buffer(C.Next_Index) := Input;
      if C.Next_Index /= 3 then
         Output := (others => ' ');
         Output_Length := 0;
         C.Next_Index := C.Next_Index + 1;
      else
         C.Next_Index := 0;
         Input_Frame := Bin_Frame(C.Buffer(0)) * 2**24 or
           Bin_Frame(C.Buffer(1)) * 2**16 or
           Bin_Frame(C.Buffer(2)) * 2**8 or
           Bin_Frame(C.Buffer(3));
         for I in reverse 1..5 loop
            Output_Frame(I) := Alphabet(Bin(Input_Frame mod 85));
            Input_Frame := Input_Frame / 85;
         end loop;
         Output(Output'First..Output'First + 4) := Output_Frame;
         Output_Length := 5;
      end if;
   end Process;

   procedure Process
     (C : in out Base85_To_String;
      Input : in Bin_Array;
      Output : out String;
      Output_Length : out Natural)
   is
      Input_Frame : Bin_Frame;
      Output_Frame : String_Frame;
      Output_Index : Integer := Output'First;
   begin
      for I in Input'Range loop
         C.Buffer(C.Next_Index) := Input(I);
         if C.Next_Index /= 3 then
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Input_Frame := Bin_Frame(C.Buffer(0)) * 2**24 or
              Bin_Frame(C.Buffer(1)) * 2**16 or
              Bin_Frame(C.Buffer(2)) * 2**8 or
              Bin_Frame(C.Buffer(3));
            for I in reverse 1..5 loop
               Output_Frame(I) := Alphabet(Bin(Input_Frame mod 85));
               Input_Frame := Input_Frame / 85;
            end loop;
            Output (Output_Index .. Output_Index + 4) := Output_Frame;
            Output_Index := Output_Index + 5;
         end if;
      end loop;
      Output_Length := Output_Index - Output'First;
   end Process;

   procedure Complete
     (C : in out Base85_To_String;
      Output : out String;
      Output_Length : out Natural)
   is
      Input_Frame : Bin_Frame;
      Output_Frame : String_Frame;
   begin
      C.State := Completed;
      if C.Next_Index = 0 then
         Output_Length := 0;
         Output := (others => ' ');
      else
         C.Buffer(C.Next_Index .. 3) := (others => 0);
         Input_Frame := Bin_Frame(C.Buffer(0)) * 2**24 or
           Bin_Frame(C.Buffer(1)) * 2**16 or
           Bin_Frame(C.Buffer(2)) * 2**8 or
           Bin_Frame(C.Buffer(3));
         for I in reverse 1..5 loop
            Output_Frame(I) := Alphabet(Bin(Input_Frame mod 85));
            Input_Frame := Input_Frame / 85;
         end loop;
         Output(Output'First..Output'First + 4) := Output_Frame;
         Output_Length := 5 - (4 - Integer(C.Next_Index));
      end if;
   end Complete;

   function To_String_Private is
     new BinToAsc.To_String(Codec => Base85_To_String);

   function To_String (Input : in Bin_Array) return String
                       renames To_String_Private;

   --
   -- Base85_To_Bin
   --

   procedure Reset (C : out Base85_To_Bin) is
   begin
      C := (State => Ready,
            Next_Index => 0,
            Buffer => (others => 0));
   end Reset;

   procedure Process (C : in out Base85_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
      Output_Frame : Bin_Frame;
      MSB_Limit : Bin_Frame;
   begin

      Input_Bin := Reverse_Alphabet(Input);
      if Input_Bin = Invalid_Character_Input then
         C.State := Failed;
         Output := (others => 0);
         Output_Length := 0;
      else
         C.Buffer(C.Next_Index) := Input_Bin;
         if C.Next_Index /= 4 then
            Output := (others => 0);
            Output_Length := 0;
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Output_Frame := Bin_Frame(C.Buffer(4)) +
              Bin_Frame(C.Buffer(3)) * 85 +
              Bin_Frame(C.Buffer(2)) * 85 * 85 +
              Bin_Frame(C.Buffer(1)) * 85 * 85 * 85;
            MSB_Limit := (not Output_Frame) / (85*85*85*85);
            if Bin_Frame(C.Buffer(0)) > MSB_Limit then
               C.State := Failed;
               Output := (others => 0);
               Output_Length := 0;
            else
               Output_Frame := Output_Frame +
                 Bin_Frame(C.Buffer(0))*85*85*85*85;
               Output(Output'First..Output'First+3) :=
                 (Bin((Output_Frame / 2**24)),
                  Bin((Output_Frame / 2**16) and 255),
                  Bin((Output_Frame / 2**8) and 255),
                  Bin((Output_Frame) and 255),
                  others => 0);
               Output_Length := 4;
            end if;
         end if;
      end if;
   end Process;

   procedure Process (C : in out Base85_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index)
   is
      Input_Bin : Bin;
      Output_Frame : Bin_Frame;
      MSB_Limit : Bin_Frame;
      Output_Index : Bin_Array_Index := Output'First;
   begin
      for I in Input'Range loop

         Input_Bin := Reverse_Alphabet(Input(I));
         if Input_Bin = Invalid_Character_Input then
            C.State := Failed;
            exit;
         end if;

         C.Buffer(C.Next_Index) := Input_Bin;
         if C.Next_Index /= 4 then
            C.Next_Index := C.Next_Index + 1;
         else
            C.Next_Index := 0;
            Output_Frame := Bin_Frame(C.Buffer(4)) +
              Bin_Frame(C.Buffer(3)) * 85 +
              Bin_Frame(C.Buffer(2)) * 85 * 85 +
              Bin_Frame(C.Buffer(1)) * 85 * 85 * 85;
            MSB_Limit := (not Output_Frame) / (85*85*85*85);
            if Bin_Frame(C.Buffer(0)) > MSB_Limit then
               C.State := Failed;
               exit;
            else
               Output_Frame := Output_Frame +
                 Bin_Frame(C.Buffer(0))*85*85*85*85;
               Output(Output_Index..Output_Index+3) :=
                 (
                  Bin((Output_Frame / 2**24)),
                  Bin((Output_Frame / 2**16) and 255),
                  Bin((Output_Frame / 2**8) and 255),
                  Bin((Output_Frame) and 255)
                 );
               Output_Index := Output_Index + 4;
            end if;
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

   procedure Completed (C : in out Base85_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index)
   is
      Output_Frame : Bin_Frame;
      MSB_Limit : Bin_Frame;
   begin
      if C.State = Ready then
         if C.Next_Index = 0 then
            Output := (others => 0);
            Output_Length := 0;
            C.State := Completed;
         elsif C.Next_Index = 1 then
            -- This is an invalid state as one Base85 character is not enough
            -- to represent a single byte.
            Output := (others => 0);
            Output_Length := 0;
            C.State := Failed;
         else
            C.Buffer(C.Next_Index .. 4) := (others => 84);
            -- Note padding must be all ones for decoding even though it
            -- was all zeros for encoding. Base85 <-> Binary is not a
            -- simple rearrangement of bits so high and low bits can interact.
            Output_Frame := Bin_Frame(C.Buffer(4)) +
              Bin_Frame(C.Buffer(3)) * 85 +
              Bin_Frame(C.Buffer(2)) * 85 * 85 +
              Bin_Frame(C.Buffer(1)) * 85 * 85 * 85;
            MSB_Limit := (not Output_Frame) / (85*85*85*85);
            if Bin_Frame(C.Buffer(0)) > MSB_Limit then
               Output := (others => 0);
               Output_Length := 0;
               C.State := Failed;
            else
               Output_Frame := Output_Frame +
                 Bin_Frame(C.Buffer(0))*85*85*85*85;
               Output :=
                 (Bin((Output_Frame / 2**24)),
                  Bin((Output_Frame / 2**16) and 255),
                  Bin((Output_Frame / 2**8) and 255),
                  Bin((Output_Frame) and 255),
                  others => 0);
               Output_Length := Bin_Array_Index(C.Next_Index - 1);
               C.State := Completed;
            end if;
         end if;
      else
         Output := (others => 0);
         Output_Length := 0;
      end if;
   end Completed;

   function To_Bin_Private is new BinToAsc.To_Bin(Codec => Base85_To_Bin);

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
                              "Duplicate letter in alphabet for Base85 codec.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc.Base85;
