-- BinToAsc_Suite.Base16_Tests
-- Unit test utilities for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with BinToAsc;
with RFC4648;

package BinToAsc_Suite.Utils is

   generic
      with package BToA is new BinToAsc(<>);
      type Codec_To_String is new BToA.Codec_To_String with private;
      type Codec_To_Bin is new BToA.Codec_To_Bin with private;
   procedure Check_Symmetry (T : in out Test_Cases.Test_Case'Class);

   generic
      with package BToA is new BinToAsc(<>);
      type Codec_To_String is new BToA.Codec_To_String with private;
      type Codec_To_Bin is new BToA.Codec_To_Bin with private;
   procedure Check_Length (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_String is new RFC4648.BToA.Codec_To_String with private;
   procedure Check_Test_Vectors_To_String (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_Bin is new RFC4648.BToA.Codec_To_Bin with private;
   procedure Check_Test_Vectors_To_Bin (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_String is new RFC4648.BToA.Codec_To_String with private;
      Max_Buffer_Length : Positive := 20;
   procedure Check_Test_Vectors_Incremental_To_String (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_Bin is new RFC4648.BToA.Codec_To_Bin with private;
      Max_Buffer_Length : Positive := 20;
   procedure Check_Test_Vectors_Incremental_To_Bin (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_String is new RFC4648.BToA.Codec_To_String with private;
      Max_Buffer_Length : Positive := 20;
   procedure Check_Test_Vectors_By_Char_To_String (T : in out Test_Cases.Test_Case'Class);

   generic
      Test_Vectors : Test_Vector_Array;
      type Codec_To_Bin is new RFC4648.BToA.Codec_To_Bin with private;
      Max_Buffer_Length : Positive := 20;
   procedure Check_Test_Vectors_By_Char_To_Bin (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Utils;
