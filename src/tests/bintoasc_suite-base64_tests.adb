-- BinToAsc_Suite.Base64_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

package body BinToAsc_Suite.Base64_Tests is

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

end BinToAsc_Suite.Base64_Tests;
