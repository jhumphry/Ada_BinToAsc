-- BinToAsc_Suite.Base85_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with ASCII85;

with BinToAsc_Suite.Utils;

package BinToAsc_Suite.Base85_Tests is

   type Base85_Test is new Test_Cases.Test_Case with null record;

   procedure Register_Tests (T: in out Base85_Test);

   function Name (T : Base85_Test) return Test_String;

   procedure Set_Up (T : in out Base85_Test);

   procedure Check_Z85_Symmetry is
     new BinToAsc_Suite.Utils.Check_Symmetry(BToA            => ASCII85.BToA,
                                             Codec_To_String => ASCII85.Z85.Base85_To_String,
                                             Codec_To_Bin    => ASCII85.Z85.Base85_To_Bin);

   procedure Check_Z85_Length is
     new BinToAsc_Suite.Utils.Check_Length(BToA            => ASCII85.BToA,
                                           Codec_To_String => ASCII85.Z85.Base85_To_String,
                                           Codec_To_Bin    => ASCII85.Z85.Base85_To_Bin);

   procedure Check_Z85_Test_Vector (T : in out Test_Cases.Test_Case'Class);

end BinToAsc_Suite.Base85_Tests;
