-- BinToAsc_Suite.Base64_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with RFC4648;

with BinToAsc_Suite.Utils;

package BinToAsc_Suite.Base64_Tests is

   Base64_Test_Vectors : constant Test_Vector_Array := ((TBS(""), TBS("")),
                                                        (TBS("f"), TBS("Zg==")),
                                                        (TBS("fo"), TBS("Zm8=")),
                                                        (TBS("foo"), TBS("Zm9v")),
                                                        (TBS("foob"), TBS("Zm9vYg==")),
                                                        (TBS("fooba"), TBS("Zm9vYmE=")),
                                                        (TBS("foobar"), TBS("Zm9vYmFy")));

   type Base64_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Base64_Test);

   function Name (T : Base64_Test) return Test_String;

   procedure Set_Up (T : in out Base64_Test);

   procedure Check_Test_Vectors_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_String(Test_Vectors => Base64_Test_Vectors,
                                                           Codec_To_String => RFC4648.Base64.Base64_To_String,
                                                           Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_Bin(Test_Vectors => Base64_Test_Vectors,
                                                        Codec_To_Bin => RFC4648.Base64.Base64_To_Bin,
                                                        Max_Buffer_Length => 20);


   procedure Check_Test_Vectors_Incremental_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_String(Test_Vectors => Base64_Test_Vectors,
                                                                       Codec_To_String => RFC4648.Base64.Base64_To_String,
                                                                       Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_Incremental_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_Bin(Test_Vectors => Base64_Test_Vectors,
                                                                    Codec_To_Bin => RFC4648.Base64.Base64_To_Bin,
                                                                    Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_String(Test_Vectors => Base64_Test_Vectors,
                                                                   Codec_To_String => RFC4648.Base64.Base64_To_String,
                                                                   Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_Bin(Test_Vectors => Base64_Test_Vectors,
                                                                Codec_To_Bin => RFC4648.Base64.Base64_To_Bin,
                                                                Max_Buffer_Length => 20);

   procedure Check_Padding (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Base64_Tests;
