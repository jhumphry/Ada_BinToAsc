-- BinToAsc_Suite.Base64_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit.Assertions;

with System.Storage_Elements;

package body BinToAsc_Suite.Base64_Tests is

   use Aunit.Assertions;
   use System.Storage_Elements;

   use RFC4648;
   use type RFC4648.Codec_State;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Base64_Test) is
      use AUnit.Test_Cases.Registration;
   begin

      Register_Routine (T, Check_Test_Vectors_To_String'Access,
                        "Check test vectors from RFC4648, binary -> string");
      Register_Routine (T, Check_Test_Vectors_To_Bin'Access,
                        "Check test vectors from RFC4648, string -> binary");
      Register_Routine (T, Check_Test_Vectors_Incremental_To_String'Access,
                        "Check test vectors from RFC4648, incrementally, binary -> string");
      Register_Routine (T, Check_Test_Vectors_Incremental_To_Bin'Access,
                        "Check test vectors from RFC4648, incrementally, string -> binary");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_String'Access,
                        "Check test vectors from RFC4648, character-by-character, binary -> string");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_Bin'Access,
                        "Check test vectors from RFC4648, character-by-character, string -> binary");
      Register_Routine (T, Check_Padding'Access,
                        "Check correct Base64 padding is enforced");

   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Base64_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of Base64 codec from RFC4648");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Base64_Test) is
   begin
      null;
   end Set_Up;

   -------------------
   -- Check_Padding --
   -------------------

   procedure Check_Padding (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base64_Decoder : RFC4648.Base64.Base64_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin
      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg===",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject excessive padding");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => "=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject excessive padding when presented " &
               "as a one-char string after the initial valid input");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => "==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject excessive padding when presented " &
               "as a == after the initial valid but incompletely padded " &
               "input");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject excessive padding when presented " &
               "as a separate character after the initial valid input");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Completed(Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject inadequate padding");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Completed(Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject inadequate padding");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9v=Yg==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject non-padding characters appearing " &
               " after the first padding Character in a single input");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9v=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => "Zm9",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject non-padding input presented " &
               " after an initial input ended with padding");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9v=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => 'C',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject non-padding input char presented " &
               " after an initial input ended with padding");

      Base64_Decoder.Reset;
      Base64_Decoder.Process(Input => "Zm9vYg",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base64_Decoder.Process(Input => "Zm",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base64_Decoder.State = Failed or Result_Length /= 0,
             "Base64 decoder did not reject non-padding string presented " &
               " after a padding char presented on its own");

   end Check_Padding;

end BinToAsc_Suite.Base64_Tests;
