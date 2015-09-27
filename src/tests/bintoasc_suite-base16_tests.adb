-- BinToAsc_Suite.Base16_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with AUnit.Assertions; use AUnit.Assertions;

with RFC4648;

with String_To_Storage_Array, Storage_Array_To_String;

package body BinToAsc_Suite.Base16_Tests is

   use System.Storage_Elements;

   use RFC4648;
   use type RFC4648.Codec_State;

   function STSA (X : String) return Storage_Array
                  renames String_To_Storage_Array;

   function SATS (X : Storage_Array) return String
                  renames Storage_Array_To_String;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Base16_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Test_Vectors'Access, "Check test vectors from RFC4648");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Base16_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of Base16 codec from RFC4648");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Base16_Test) is
   begin
      null;
   end Set_Up;

   ------------------------
   -- Check_Test_Vectors --
   ------------------------

   procedure Check_Test_Vectors (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base16_Encoder : Base16.Base16_To_String;
      Base16_Decoder : Base16.Base16_To_Bin;

      Result_String : String(1..12);
      Result_String_Length : Integer;

      Result_Bin : Storage_Array(1..12);
      Result_Bin_Length : Storage_Offset;

   begin
      for T of Base16_Test_Vectors loop
         declare
            Unencoded : constant String := To_String(T.Unencoded);
            Encoded : constant String := To_String(T.Encoded);
         begin
            Base16_Encoder.Reset;
            Base16_Encoder.Process(Input => STSA(Unencoded),
                                   Output => Result_String,
                                   Output_Length => Result_String_Length);
            Assert(Result_String(1..Result_String_Length) = Encoded,
                   "Base16 encoder on: " &
                     Unencoded &
                     " gives wrong result: " &
                     Result_String(1..Result_String_Length) &
                     " instead of: " &
                     Encoded);

            Base16_Encoder.Completed(Output => Result_String,
                                     Output_Length => Result_String_Length);
            Assert(Result_String_Length = 0 and Base16_Encoder.State = Complete,
                   "Base16 encoder not terminating correctly.");

            Base16_Decoder.Reset;
            Base16_Decoder.Process(Input => Encoded,
                                   Output => Result_Bin,
                                   Output_Length => Result_Bin_Length);
            Assert(SATS(Result_Bin(1..Result_Bin_Length)) = Unencoded,
                   "Base16 decoder on: " &
                     Encoded &
                     " gives wrong result: " &
                     SATS(Result_Bin(1..Result_Bin_Length)) &
                     " instead of: " &
                     Unencoded);

            Base16_Decoder.Completed(Output => Result_Bin,
                                     Output_Length => Result_Bin_Length);
            Assert(Result_Bin_Length = 0 and Base16_Decoder.State = Complete,
                   "Base16 decoder not terminating correctly.");
         end;
      end loop;
   end Check_Test_Vectors;

end BinToAsc_Suite.Base16_Tests;
