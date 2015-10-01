-- BinToAsc_Suite.Utils
-- Unit test utilities for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with AUnit.Assertions; use AUnit.Assertions;

with String_To_Storage_Array, Storage_Array_To_String;

package body BinToAsc_Suite.Utils is

   use RFC4648;
   use type RFC4648.Codec_State;
   use System.Storage_Elements;

   function STSA (X : String) return Storage_Array
                  renames String_To_Storage_Array;

   function SATS (X : Storage_Array) return String
                  renames Storage_Array_To_String;

   --------------------
   -- Check_Symmetry --
   --------------------

   procedure Check_Symmetry (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      function Bin_To_String is
        new RFC4648.BToA.To_String(Codec => Codec_To_String);

      function String_To_Bin is
        new RFC4648.BToA.To_Bin(Codec => Codec_To_Bin);

      Binary_Input : Storage_Array(0..255);

   begin
      for I in Binary_Input'Range loop
         Binary_Input(I) := Storage_Element(I - Binary_Input'First);
      end loop;

      declare
         Encoded_Data : constant String := Bin_To_String(Binary_Input);
         Decoded_Data : constant Storage_Array := String_To_Bin(Encoded_Data);
      begin
         Assert(Decoded_Data'Length = 256,
                "Encoder / Decoder pair changes the length of the data");
         Assert((for all I in Decoded_Data'Range =>
                   Decoded_Data(I) = Storage_Element(I-Decoded_Data'First)),
                "Encoder / Decoder are not a symmetric pair");
      end;
   end Check_Symmetry;

   ----------------------------------
   -- Check_Test_Vectors_To_String --
   ----------------------------------

   procedure Check_Test_Vectors_To_String (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      function Bin_To_String is
        new RFC4648.BToA.To_String(Codec => Codec_To_String);

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);
         begin
            Assert( Bin_To_String(STSA(Unencoded)) = Encoded,
                    "BinToAsc encoder not generating correct string output " &
                      "for binary test vectors.");
         end;
      end loop;
   end Check_Test_Vectors_To_String;

   -------------------------------
   -- Check_Test_Vectors_To_Bin --
   -------------------------------

   procedure Check_Test_Vectors_To_Bin (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      function String_To_Bin is
        new RFC4648.BToA.To_Bin(Codec => Codec_To_Bin);

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);
         begin
            Assert( SATS(String_To_Bin(Encoded)) = Unencoded,
                    "BinToAsc decoder not generating correct binary output " &
                      "for string test vectors.");
         end;
      end loop;
   end Check_Test_Vectors_To_Bin;

   ------------------------------------
   -- Check_Test_Vectors_Incremental --
   ------------------------------------

   procedure Check_Test_Vectors_Incremental_To_String (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      BinToAsc_Encoder : Codec_To_String;

      Result_String : String(1..Max_Buffer_Length);
      Result_String_Length : Integer;

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);

            Buffer_String : String(1..Max_Buffer_Length);
            Buffer_String_Used : Integer;

         begin
            BinToAsc_Encoder.Reset;

            Result_String := (others => 'z');
            Result_String_Length := 0;

            for C of Unencoded loop
               BinToAsc_Encoder.Process(Input => STSA(C & ""),
                                        Output => Buffer_String,
                                        Output_Length => Buffer_String_Used);
               if Buffer_String_Used > 0 then
                  Result_String(Result_String_Length + 1 .. Result_String_Length + Buffer_String_Used) :=
                    Buffer_String(1..Buffer_String_Used);
                  Result_String_Length := Result_String_Length + Buffer_String_Used;
               end if;
            end loop;

            BinToAsc_Encoder.Complete(Output => Buffer_String,
                                       Output_Length => Buffer_String_Used);
            if Buffer_String_Used > 0 then
               Result_String(Result_String_Length + 1 .. Result_String_Length + Buffer_String_Used) :=
                 Buffer_String(1..Buffer_String_Used);
               Result_String_Length := Result_String_Length + Buffer_String_Used;
            end if;

            Assert(BinToAsc_Encoder.State = Complete,
                   "BinToAsc encoder not terminating correctly.");

            Assert(Result_String(1..Result_String_Length) = Encoded,
                   "BinToAsc encoder on: " &
                     Unencoded &
                     " gives wrong result: " &
                     Result_String(1..Result_String_Length) &
                     " instead of: " &
                     Encoded);
         end;
      end loop;
   end Check_Test_Vectors_Incremental_To_String;

   -------------------------------------------
   -- Check_Test_Vectors_Incremental_To_Bin --
   -------------------------------------------

   procedure Check_Test_Vectors_Incremental_To_Bin (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      BinToAsc_Decoder : Codec_To_Bin;

      Result_Bin : Storage_Array(1..Storage_Offset(Max_Buffer_Length));
      Result_Bin_Length : Storage_Offset;

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);

            Buffer_Bin : Storage_Array(1..Storage_Offset(Max_Buffer_Length));
            Buffer_Bin_Used : Storage_Offset;

         begin

            BinToAsc_Decoder.Reset;

            Result_Bin := (others => 0);
            Result_Bin_Length := 0;

            for C of Encoded loop
               BinToAsc_Decoder.Process(Input => C & "",
                                        Output => Buffer_Bin,
                                        Output_Length => Buffer_Bin_Used);
               if Buffer_Bin_Used > 0 then
                  Result_Bin(Result_Bin_Length + 1 .. Result_Bin_Length + Buffer_Bin_Used) :=
                    Buffer_Bin(1..Buffer_Bin_Used);
                  Result_Bin_Length := Result_Bin_Length + Buffer_Bin_Used;
               end if;
            end loop;

            BinToAsc_Decoder.Completed(Output => Buffer_Bin,
                                       Output_Length => Buffer_Bin_Used);
            Assert (Buffer_Bin_Used = 0,
                    "BinToAsc decoder should not generate output on " &
                      "completion as the binary output can never be padded.");

            Assert(BinToAsc_Decoder.State = Complete,
                   "BinToAsc decoder not terminating correctly.");

            Assert(SATS(Result_Bin(1..Result_Bin_Length)) = Unencoded,
                   "BinToAsc decoder on: " &
                     Encoded &
                     " gives wrong result: " &
                     SATS(Result_Bin(1..Result_Bin_Length)) &
                     " instead of: " &
                     Unencoded);
         end;
      end loop;
   end Check_Test_Vectors_Incremental_To_Bin;

   ------------------------------------------
   -- Check_Test_Vectors_By_Char_To_String --
   ------------------------------------------

   procedure Check_Test_Vectors_By_Char_To_String (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      BinToAsc_Encoder : Codec_To_String;

      Result_String : String(1..Max_Buffer_Length);
      Result_String_Length : Integer;

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);

            Buffer_String : String(1..Max_Buffer_Length);
            Buffer_String_Used : Integer;

         begin
            BinToAsc_Encoder.Reset;

            Result_String := (others => 'z');
            Result_String_Length := 0;

            for C of Unencoded loop
               BinToAsc_Encoder.Process(Input => Storage_Element'Val(Character'Pos(C)),
                                      Output => Buffer_String,
                                      Output_Length => Buffer_String_Used);
               if Buffer_String_Used > 0 then
                  Result_String(Result_String_Length + 1 .. Result_String_Length + Buffer_String_Used) :=
                    Buffer_String(1..Buffer_String_Used);
                  Result_String_Length := Result_String_Length + Buffer_String_Used;
               end if;
            end loop;

            BinToAsc_Encoder.Complete(Output => Buffer_String,
                                     Output_Length => Buffer_String_Used);
            if Buffer_String_Used > 0 then
               Result_String(Result_String_Length + 1 .. Result_String_Length + Buffer_String_Used) :=
                 Buffer_String(1..Buffer_String_Used);
               Result_String_Length := Result_String_Length + Buffer_String_Used;
            end if;

            Assert(BinToAsc_Encoder.State = Complete,
                   "BinToAsc encoder not terminating correctly.");

            Assert(Result_String(1..Result_String_Length) = Encoded,
                   "BinToAsc encoder on: " &
                     Unencoded &
                     " gives wrong result: " &
                     Result_String(1..Result_String_Length) &
                     " instead of: " &
                     Encoded);
         end;
      end loop;
   end Check_Test_Vectors_By_Char_To_String;

   ---------------------------------------
   -- Check_Test_Vectors_By_Char_To_Bin --
   ---------------------------------------

   procedure Check_Test_Vectors_By_Char_To_Bin (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      BinToAsc_Decoder : Codec_To_Bin;

      Result_Bin : Storage_Array(1..Storage_Offset(Max_Buffer_Length));
      Result_Bin_Length : Storage_Offset;

   begin
      for T of Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);

            Buffer_Bin : Storage_Array(1..Storage_Offset(Max_Buffer_Length));
            Buffer_Bin_Used : Storage_Offset;

         begin
            BinToAsc_Decoder.Reset;

            Result_Bin := (others => 0);
            Result_Bin_Length := 0;

            for C of Encoded loop
               BinToAsc_Decoder.Process(Input => C,
                                      Output => Buffer_Bin,
                                      Output_Length => Buffer_Bin_Used);
               if Buffer_Bin_Used > 0 then
                  Result_Bin(Result_Bin_Length + 1 .. Result_Bin_Length + Buffer_Bin_Used) :=
                    Buffer_Bin(1..Buffer_Bin_Used);
                  Result_Bin_Length := Result_Bin_Length + Buffer_Bin_Used;
               end if;
            end loop;

            BinToAsc_Decoder.Completed(Output => Buffer_Bin,
                                       Output_Length => Buffer_Bin_Used);

            Assert (Buffer_Bin_Used = 0,
                    "BinToAsc decoder should not generate output on " &
                      "completion as the binary output can never be padded.");

            Assert(BinToAsc_Decoder.State = Complete,
                   "BinToAsc decoder not terminating correctly.");

            Assert(SATS(Result_Bin(1..Result_Bin_Length)) = Unencoded,
                   "BinToAsc decoder on: " &
                     Encoded &
                     " gives wrong result: " &
                     SATS(Result_Bin(1..Result_Bin_Length)) &
                     " instead of: " &
                     Unencoded);
         end;
      end loop;
   end Check_Test_Vectors_By_Char_To_Bin;

end BinToAsc_Suite.Utils;
