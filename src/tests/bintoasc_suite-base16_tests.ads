-- BinToAsc_Suite.Base16_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with RFC4648;

with BinToAsc_Suite.Utils;

package BinToAsc_Suite.Base16_Tests is

   Base16_Test_Vectors : constant Test_Vector_Array := ((TBS(""), TBS("")),
                                                        (TBS("f"), TBS("66")),
                                                        (TBS("fo"), TBS("666F")),
                                                        (TBS("foo"), TBS("666F6F")),
                                                        (TBS("foob"), TBS("666F6F62")),
                                                        (TBS("fooba"), TBS("666F6F6261")),
                                                        (TBS("foobar"), TBS("666F6F626172")));

   type Base16_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Base16_Test);

   function Name (T : Base16_Test) return Test_String;

   procedure Set_Up (T : in out Base16_Test);

   procedure Check_Test_Vectors_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_String(Test_Vectors => Base16_Test_Vectors,
                                                           Codec_To_String => RFC4648.Base16.Base16_To_String);

   procedure Check_Test_Vectors_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_Bin(Test_Vectors => Base16_Test_Vectors,
                                                        Codec_To_Bin => RFC4648.Base16.Base16_To_Bin);

   procedure Check_Test_Vectors_Incremental_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_String(Test_Vectors => Base16_Test_Vectors,
                                                                       Codec_To_String => RFC4648.Base16.Base16_To_String,
                                                                       Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_Incremental_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_Bin(Test_Vectors => Base16_Test_Vectors,
                                                                    Codec_To_Bin => RFC4648.Base16.Base16_To_Bin,
                                                                    Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_String(Test_Vectors => Base16_Test_Vectors,
                                                                   Codec_To_String => RFC4648.Base16.Base16_To_String,
                                                                   Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_Bin(Test_Vectors => Base16_Test_Vectors,
                                                                Codec_To_Bin => RFC4648.Base16.Base16_To_Bin,
                                                                Max_Buffer_Length => 20);

   procedure Check_Junk_Rejection (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Junk_Rejection_By_Char (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Incomplete_Group_Rejection (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Base16_Tests;
