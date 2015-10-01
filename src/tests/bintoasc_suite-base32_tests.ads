-- BinToAsc_Suite.Base32_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with RFC4648;

with BinToAsc_Suite.Utils;

package BinToAsc_Suite.Base32_Tests is

   Base32_Test_Vectors : constant Test_Vector_Array := ((TBS(""), TBS("")),
                                                        (TBS("f"), TBS("MY======")),
                                                        (TBS("fo"), TBS("MZXQ====")),
                                                        (TBS("foo"), TBS("MZXW6===")),
                                                        (TBS("foob"), TBS("MZXW6YQ=")),
                                                        (TBS("fooba"), TBS("MZXW6YTB")),
                                                        (TBS("foobar"), TBS("MZXW6YTBOI======")));

   Base32Hex_Test_Vectors : constant Test_Vector_Array := ((TBS(""), TBS("")),
                                                           (TBS("f"), TBS("CO======")),
                                                           (TBS("fo"), TBS("CPNG====")),
                                                           (TBS("foo"), TBS("CPNMU===")),
                                                           (TBS("foob"), TBS("CPNMUOG=")),
                                                           (TBS("fooba"), TBS("CPNMUOJ1")),
                                                           (TBS("foobar"), TBS("CPNMUOJ1E8======")));

   type Base32_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Base32_Test);

   function Name (T : Base32_Test) return Test_String;

   procedure Set_Up (T : in out Base32_Test);

   procedure Check_Symmetry is
     new BinToAsc_Suite.Utils.Check_Symmetry(Codec_To_String => RFC4648.Base32.Base32_To_String,
                                             Codec_To_Bin    => RFC4648.Base32.Base32_To_Bin);

   procedure Check_Symmetry_Hex is
     new BinToAsc_Suite.Utils.Check_Symmetry(Codec_To_String => RFC4648.Base32Hex.Base32_To_String,
                                             Codec_To_Bin    => RFC4648.Base32Hex.Base32_To_Bin);

   procedure Check_Test_Vectors_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_String(Test_Vectors => Base32_Test_Vectors,
                                                           Codec_To_String => RFC4648.Base32.Base32_To_String);

    procedure Check_Test_Vectors_To_String_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_String(Test_Vectors => Base32Hex_Test_Vectors,
                                                           Codec_To_String => RFC4648.Base32Hex.Base32_To_String);

   procedure Check_Test_Vectors_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_Bin(Test_Vectors => Base32_Test_Vectors,
                                                        Codec_To_Bin => RFC4648.Base32.Base32_To_Bin);

   procedure Check_Test_Vectors_To_Bin_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_To_Bin(Test_Vectors => Base32Hex_Test_Vectors,
                                                        Codec_To_Bin => RFC4648.Base32Hex.Base32_To_Bin);

   procedure Check_Test_Vectors_Incremental_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_String(Test_Vectors => Base32_Test_Vectors,
                                                                       Codec_To_String => RFC4648.Base32.Base32_To_String,
                                                                       Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_Incremental_To_String_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_String(Test_Vectors => Base32Hex_Test_Vectors,
                                                                       Codec_To_String => RFC4648.Base32Hex.Base32_To_String,
                                                                       Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_Incremental_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_Bin(Test_Vectors => Base32_Test_Vectors,
                                                                    Codec_To_Bin => RFC4648.Base32.Base32_To_Bin,
                                                                    Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_Incremental_To_Bin_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_Incremental_To_Bin(Test_Vectors => Base32Hex_Test_Vectors,
                                                                    Codec_To_Bin => RFC4648.Base32Hex.Base32_To_Bin,
                                                                    Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_String is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_String(Test_Vectors => Base32_Test_Vectors,
                                                                   Codec_To_String => RFC4648.Base32.Base32_To_String,
                                                                   Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_String_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_String(Test_Vectors => Base32Hex_Test_Vectors,
                                                                   Codec_To_String => RFC4648.Base32Hex.Base32_To_String,
                                                                   Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_Bin is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_Bin(Test_Vectors => Base32_Test_Vectors,
                                                                Codec_To_Bin => RFC4648.Base32.Base32_To_Bin,
                                                                Max_Buffer_Length => 20);

   procedure Check_Test_Vectors_By_Char_To_Bin_Hex is
     new BinToAsc_Suite.Utils.Check_Test_Vectors_By_Char_To_Bin(Test_Vectors => Base32Hex_Test_Vectors,
                                                                Codec_To_Bin => RFC4648.Base32Hex.Base32_To_Bin,
                                                                Max_Buffer_Length => 20);

   procedure Check_Padding (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Junk_Rejection (T : in out Test_Cases.Test_Case'Class);

   procedure Check_Junk_Rejection_By_Char (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Base32_Tests;
