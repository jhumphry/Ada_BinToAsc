-- BinToAsc_Suite.Base16_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Assertions;
with AUnit.Assertions;

with System.Storage_Elements;

with String_To_Storage_Array;

package body BinToAsc_Suite.Base16_Tests is

   use AUnit.Assertions;
   use System.Storage_Elements;

   use RFC4648;
   use type RFC4648.Codec_State;

   function STSA (X : String) return Storage_Array
                     renames String_To_Storage_Array;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Base16_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Symmetry'Access,
                        "Check the Encoder and Decoder are a symmetrical pair");
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
      Register_Routine (T, Check_Junk_Rejection'Access,
                        "Check Base16 decoder will reject junk input");
      Register_Routine (T, Check_Junk_Rejection_By_Char'Access,
                        "Check Base16 decoder will reject junk input (single character)");
      Register_Routine (T, Check_Incomplete_Group_Rejection'Access,
                        "Check Base16 decoder will reject incomplete groups in input");
      Register_Routine (T, Check_Case_Insensitive'Access,
                        "Check Base16_Case_Insensitive decoder will accept mixed-case input");
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

   --------------------------
   -- Check_Junk_Rejection --
   --------------------------

   -- This procedure cannot be nested inside Check_Junk_Rejection due to access
   -- level restrictions
   procedure Should_Raise_Exception_From_Junk is
      Discard : Storage_Array(1..6);
   begin
      Discard  := RFC4648.Base16.To_Bin("666F6F6Z6172");
   end;

   procedure Check_Junk_Rejection (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base16_Decoder : RFC4648.Base16.Base16_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin

      Assert_Exception(Should_Raise_Exception_From_Junk'Access,
                       "Base16 decoder did not reject junk input.");

      Base16_Decoder.Reset;

      Base16_Decoder.Process(Input => "666F6F6Z6172",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder did not reject junk input.");
      Assert(Result_Length = 0,
             "Base16 decoder rejected junk input but did not return 0 " &
               "length output.");

      begin
         Base16_Decoder.Process(Input => "66",
                                Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Process to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder reset its state on valid input after junk input.");
      Assert(Result_Length = 0,
             "Base16 decoder rejected input after a junk input but did " &
               "not return 0 length output.");

      begin
         Base16_Decoder.Completed(Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Completed to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder allowed successful completion after junk input.");

      Assert(Result_Length = 0,
             "Base16 decoder completed after a junk input did " &
               "not return 0 length output.");
   end Check_Junk_Rejection;

   ----------------------------------
   -- Check_Junk_Rejection_By_Char --
   ----------------------------------

   procedure Check_Junk_Rejection_By_Char (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base16_Decoder : RFC4648.Base16.Base16_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin
      Base16_Decoder.Reset;

      Base16_Decoder.Process(Input => 'Y',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder did not reject junk input character.");
      Assert(Result_Length = 0,
             "Base16 decoder rejected junk input but did not return 0 " &
               "length output.");

      begin
         Base16_Decoder.Process(Input => '6',
                                Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Process to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder reset its state on valid input after junk input " &
               "character.");
      Assert(Result_Length = 0,
             "Base16 decoder rejected input after a junk input char but did " &
               "not return 0 length output.");

      begin
         Base16_Decoder.Completed(Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Completed to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base16_Decoder.State = Failed,
             "Base16 decoder allowed successful completion after junk input " &
               "char.");

      Assert(Result_Length = 0,
             "Base16 decoder completed after a junk input char did " &
               "not return 0 length output.");
   end Check_Junk_Rejection_By_Char;

   --------------------------------------
   -- Check_Incomplete_Group_Rejection --
   --------------------------------------

   procedure Check_Incomplete_Group_Rejection
     (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

       Base16_Decoder : RFC4648.Base16.Base16_To_Bin;

      Result_Bin : Storage_Array (1..20);
      Result_Length : Storage_Offset;

   begin
      Base16_Decoder.Reset;

      Base16_Decoder.Process(Input => "666F6F6",
                             Output => Result_Bin,
                             Output_Length => Result_Length);

      Base16_Decoder.Completed(Output => Result_Bin,
                               Output_Length => Result_Length);

      Assert(Base16_Decoder.State = Failed, "Base16 decoder did not complain " &
               "about receiving an incomplete group.");
   end Check_Incomplete_Group_Rejection;

   ----------------------------
   -- Check_Case_Insensitive --
   ----------------------------

   procedure Check_Case_Insensitive (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Input : constant Storage_Array := STSA("foobar");
      Encoded : constant String := "666F6F626172";
      Encoded_Mixed_Case : constant String := "666F6f626172";

      Base16_Decoder : RFC4648.Base16.Base16_To_Bin;
      Buffer : Storage_Array(1..7);
      Buffer_Used : Storage_Offset;
   begin
      Assert(Test_Input = RFC4648.Base16.To_Bin(Encoded),
             "Base16 case-sensitive decoder not working");

      Base16_Decoder.Reset;
      Base16_Decoder.Process(Encoded_Mixed_Case,
                             Buffer,
                             Buffer_Used);
      Assert(Base16_Decoder.State = Failed and Buffer_Used = 0,
             "Base16 case-sensitive decoder did not reject mixed-case input");

      Assert(Test_Input = RFC4648.Base16_Case_Insensitive.To_Bin(Encoded_Mixed_Case),
             "Base16 case-insensitive decoder not working");

   end Check_Case_Insensitive;

end BinToAsc_Suite.Base16_Tests;
