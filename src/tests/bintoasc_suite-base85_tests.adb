-- BinToAsc_Suite.Base85_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit.Assertions;

with System.Storage_Elements;

with Storage_Array_To_Hex_String;

package body BinToAsc_Suite.Base85_Tests is

   use AUnit.Assertions;
   use System.Storage_Elements;

   use ASCII85;
   use type ASCII85.Codec_State;

   function SATHS (X : Storage_Array) return String
                  renames Storage_Array_To_Hex_String;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Base85_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Z85_Symmetry'Access,
                        "Check the Z85 Encoder and Decoder are a symmetrical pair");
      Register_Routine (T, Check_Z85_Length'Access,
                        "Check the Z85 Encoder and Decoder handle variable-length input successfully");
      Register_Routine (T, Check_Z85_Test_Vector'Access,
                        "Check Z85 test vector can be encoded/decoded successfully");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Base85_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of ASCII85/Base85 codecs");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Base85_Test) is
   begin
      null;
   end Set_Up;

   --------------------------
   -- Check_Junk_Rejection --
   --------------------------

   procedure Check_Z85_Test_Vector (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Test_Vector : constant Storage_Array :=
     (16#86#, 16#4F#, 16#D2#, 16#6F#,
      16#B5#, 16#59#, 16#F7#, 16#5B#);

      Z85_Encoded : constant String := "HelloWorld";

   begin
      Assert(Z85.To_String(Z85_Test_Vector) = Z85_Encoded,
             "Z85 encoded on test vector produced " &
               Z85.To_String(Z85_Test_Vector) &
               " rather than 'HelloWorld'");
      Assert(Z85.To_Bin(Z85_Encoded) = Z85_Test_Vector,
             "Z85 decoded on test vector produced " &
               SATHS(Z85.To_Bin(Z85_Encoded)) &
               " rather than " &
               SATHS(Z85_Test_Vector));
   end Check_Z85_Test_Vector;

end BinToAsc_Suite.Base85_Tests;
