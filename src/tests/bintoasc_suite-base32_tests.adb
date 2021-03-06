-- BinToAsc_Suite.Base32_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit.Assertions;

with System.Storage_Elements;
with Ada.Assertions;

with String_To_Storage_Array;

with BinToAsc.Base32;

package body BinToAsc_Suite.Base32_Tests is

   use AUnit.Assertions;
   use System.Storage_Elements;

   use RFC4648;
   use type RFC4648.Codec_State;

   function STSA (X : String) return Storage_Array
                     renames String_To_Storage_Array;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T: in out Base32_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Check_Symmetry'Access,
                        "Check the Base32 Encoder and Decoder are a symmetrical pair");
      Register_Routine (T, Check_Length'Access,
                        "Check the Encoder and Decoder handle variable-length input successfully");
      Register_Routine (T, Check_Symmetry_Hex'Access,
                        "Check the Base32Hex Encoder and Decoder are a symmetrical pair");
      Register_Routine (T, Check_Test_Vectors_To_String'Access,
                        "Check Base32 test vectors from RFC4648, binary -> string");
      Register_Routine (T, Check_Test_Vectors_To_String_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, binary -> string");
      Register_Routine (T, Check_Test_Vectors_To_Bin'Access,
                        "Check Base32 test vectors from RFC4648, string -> binary");
      Register_Routine (T, Check_Test_Vectors_To_Bin_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, string -> binary");
      Register_Routine (T, Check_Test_Vectors_Incremental_To_String'Access,
                        "Check Base32 test vectors from RFC4648, incrementally, binary -> string");
       Register_Routine (T, Check_Test_Vectors_Incremental_To_String_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, incrementally, binary -> string");
      Register_Routine (T, Check_Test_Vectors_Incremental_To_Bin'Access,
                        "Check Base32 test vectors from RFC4648, incrementally, string -> binary");
      Register_Routine (T, Check_Test_Vectors_Incremental_To_Bin_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, incrementally, string -> binary");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_String'Access,
                        "Check Base32 test vectors from RFC4648, character-by-character, binary -> string");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_String_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, character-by-character, binary -> string");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_Bin'Access,
                        "Check Base32 test vectors from RFC4648, character-by-character, string -> binary");
      Register_Routine (T, Check_Test_Vectors_By_Char_To_Bin_Hex'Access,
                        "Check Base32Hex test vectors from RFC4648, character-by-character, string -> binary");
      Register_Routine (T, Check_Padding'Access,
                        "Check correct Base32 padding is enforced");
      Register_Routine (T, Check_Junk_Rejection'Access,
                        "Check Base32 decoder will reject junk input");
      Register_Routine (T, Check_Junk_Rejection_By_Char'Access,
                        "Check Base32 decoder will reject junk input (single character)");
      Register_Routine (T, Check_Case_Insensitive'Access,
                        "Check Base32_Case_Insensitive decoder will accept mixed-case input");
      Register_Routine (T, Check_Homoglyph'Access,
                        "Check Base32 decoder with Homoglpyh_Allowed set tolerates homoglyphs");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : Base32_Test) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Tests of Base32 and Base32Hex codecs from RFC4648");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Base32_Test) is
   begin
      null;
   end Set_Up;

   -------------------
   -- Check_Padding --
   -------------------

   -- These procedures cannot be nested inside Check_Padding due to access
   -- level restrictions
   procedure Should_Raise_Exception_Excess_Padding is
      Discard : Storage_Array(1..6);
   begin
      Discard  := RFC4648.Base32.To_Bin("MZXW6YTBI=======");
   end;

   procedure Should_Raise_Exception_Insufficient_Padding is
      Discard : Storage_Array(1..6);
   begin
      Discard  := RFC4648.Base32.To_Bin("MZXW6YTBOI=====");
   end;

   procedure Check_Padding (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base32_Decoder : RFC4648.Base32.Base32_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin

      Assert_Exception(Should_Raise_Exception_Excess_Padding'Access,
                       "Base32 decoder did not reject excessive padding");

      Assert_Exception(Should_Raise_Exception_Insufficient_Padding'Access,
                       "Base32 decoder did not reject insufficient padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBI=======",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject excessive padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBI======",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => "=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject excessive padding when presented " &
               "as a one-char string after the initial valid input");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBI=====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => "==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject excessive padding when presented " &
               "as a == after the initial valid but incompletely padded " &
               "input");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBI======",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject excessive padding when presented " &
               "as a separate character after the initial valid input");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBOI=====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Complete(Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject inadequate padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YTBOI",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Complete(Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject inadequate padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6Y==",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject impossible length 2 padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6Y=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject impossible length 2 padding " &
               "presented via a character");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZX=====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject impossible length 5 padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZX====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject impossible length 5 padding " &
               "presented via a character");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YT=BOI=====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject non-padding characters appearing " &
               " after the first padding Character in a single input");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YT=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => "BOI=====",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject non-padding input presented " &
               " after an initial input ended with padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YT=",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => 'B',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject non-padding input char presented " &
               " after an initial input ended with padding");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Input => "MZXW6YT",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => '=',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Base32_Decoder.Process(Input => "BOI",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed or Result_Length /= 0,
             "Base32 decoder did not reject non-padding string presented " &
               " after a padding char presented on its own");

   end Check_Padding;

   --------------------------
   -- Check_Junk_Rejection --
   --------------------------

   -- This procedure cannot be nested inside Check_Junk_Rejection due to access
   -- level restrictions
   procedure Should_Raise_Exception_From_Junk is
      Discard : Storage_Array(1..6);
   begin
      Discard  := RFC4648.Base32.To_Bin("MZXW:YTB");
   end;

   procedure Check_Junk_Rejection (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base32_Decoder : RFC4648.Base32.Base32_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin

      Assert_Exception(Should_Raise_Exception_From_Junk'Access,
                       "Base32 decoder did not reject junk input.");

      Base32_Decoder.Reset;

      Base32_Decoder.Process(Input => "MZXW:YTB",
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed,
             "Base32 decoder did not reject junk input.");
      Assert(Result_Length = 0,
             "Base32 decoder rejected junk input but did not return 0 " &
               "length output.");

      begin
         Base32_Decoder.Process(Input => "MZ",
                                Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Process to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base32_Decoder.State = Failed,
             "Base32 decoder reset its state on valid input after junk input.");
      Assert(Result_Length = 0,
             "Base32 decoder rejected input after a junk input but did " &
               "not return 0 length output.");

      begin
         Base32_Decoder.Complete(Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Completed to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base32_Decoder.State = Failed,
             "Base16 decoder allowed successful completion after junk input.");

      Assert(Result_Length = 0,
             "Base32 decoder completed after a junk input did " &
               "not return 0 length output.");
   end Check_Junk_Rejection;

   ----------------------------------
   -- Check_Junk_Rejection_By_Char --
   ----------------------------------

   procedure Check_Junk_Rejection_By_Char (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Base32_Decoder : RFC4648.Base32.Base32_To_Bin;

      Result_Bin : Storage_Array(1..20);
      Result_Length : Storage_Offset;

   begin
      Base32_Decoder.Reset;

      Base32_Decoder.Process(Input => '@',
                             Output => Result_Bin,
                             Output_Length => Result_Length);
      Assert(Base32_Decoder.State = Failed,
             "Base32 decoder did not reject junk input character.");
      Assert(Result_Length = 0,
             "Base32 decoder rejected junk input but did not return 0 " &
               "length output.");

      begin
         Base32_Decoder.Process(Input => '6',
                                Output => Result_Bin,
                                Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Process to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base32_Decoder.State = Failed,
             "Base32 decoder reset its state on valid input after junk input " &
               "character.");
      Assert(Result_Length = 0,
             "Base32 decoder rejected input after a junk input char but did " &
               "not return 0 length output.");

      begin
         Base32_Decoder.Complete(Output => Result_Bin,
                                  Output_Length => Result_Length);
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Completed to be run
                  -- on a codec with state /= Ready.
      end;

      Assert(Base32_Decoder.State = Failed,
             "Base32 decoder allowed successful completion after junk input " &
               "char.");

      Assert(Result_Length = 0,
             "Base32 decoder completed after a junk input char did " &
               "not return 0 length output.");
   end Check_Junk_Rejection_By_Char;

   ----------------------------
   -- Check_Case_Insensitive --
   ----------------------------

   procedure Check_Case_Insensitive (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced(T);

      Test_Input : constant Storage_Array := STSA("foobar");
      Encoded : constant String := "MZXW6YTBOI======";
      Encoded_Mixed_Case : constant String := "MZXw6yTBoI======";

      Base32_Decoder : RFC4648.Base32.Base32_To_Bin;
      Buffer : Storage_Array(1..15);
      Buffer_Used : Storage_Offset;
   begin
      Assert(Test_Input = RFC4648.Base32.To_Bin(Encoded),
             "Base32 case-sensitive decoder not working");

      Base32_Decoder.Reset;
      Base32_Decoder.Process(Encoded_Mixed_Case,
                             Buffer,
                             Buffer_Used);
      Assert(Base32_Decoder.State = Failed and Buffer_Used = 0,
             "Base32 case-sensitive decoder did not reject mixed-case input");

      Assert(Test_Input = RFC4648.Base32_Case_Insensitive.To_Bin(Encoded_Mixed_Case),
             "Base32 case-insensitive decoder not working");

   end Check_Case_Insensitive;

   ---------------------
   -- Check_Homoglyph --
   ---------------------

   procedure Check_Homoglyph (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      package Base32_UC is new
        RFC4648.BToA.Base32(Alphabet         => Base32_Alphabet,
                            Padding          => '=',
                            Case_Sensitive   => True,
                            Allow_Homoglyphs => True);

      Base32_Alphabet_LC : constant BToA.Alphabet_32 :=
           "abcdefghijklmnopqrstuvwxyz234567";

      package Base32_LC is new
        RFC4648.BToA.Base32(Alphabet         => Base32_Alphabet_LC,
                            Padding          => '=',
                            Case_Sensitive   => True,
                            Allow_Homoglyphs => True);

      Binary_Input : Storage_Array(0..255);
      Encoded_Data : String(1..416);
      Decoded_Data : Storage_Array(0..255);

   begin
      for I in Binary_Input'Range loop
         Binary_Input(I) := Storage_Element(I - Binary_Input'First);
      end loop;

      Encoded_Data := Base32_UC.To_String(Binary_Input);
      Assert((for some I of Encoded_Data => I = 'O') and
               (for some I of Encoded_Data => I = 'I'),
             "Test data does not contain O or I so upper-case homoglyph test cannot be done");

      for I in Encoded_Data'Range loop
         case Encoded_Data(I) is
            when 'O' =>
               Encoded_Data(I) := '0';
            when 'I' =>
               Encoded_Data(I) := '1';
            when others =>
               null;
         end case;
      end loop;

      Decoded_Data := Base32_UC.To_Bin(Encoded_Data);

      Assert((for all I in Decoded_Data'Range =>
                Decoded_Data(I) = Storage_Element(I-Decoded_Data'First)),
             "Encoder / Decoder pair does not tolerate upper-case homoglyphs");

      Encoded_Data := Base32_LC.To_String(Binary_Input);
      Assert((for some I of Encoded_Data => I = 'o') and
               (for some I of Encoded_Data => I = 'l'),
             "Test data does not contain o or l so lower-case homoglyph test cannot be done");

      for I in Encoded_Data'Range loop
         case Encoded_Data(I) is
            when 'o' =>
               Encoded_Data(I) := '0';
            when 'l' =>
               Encoded_Data(I) := '1';
            when others =>
               null;
         end case;
      end loop;

      Decoded_Data := Base32_LC.To_Bin(Encoded_Data);

      Assert((for all I in Decoded_Data'Range =>
                Decoded_Data(I) = Storage_Element(I-Decoded_Data'First)),
             "Encoder / Decoder pair does not tolerate lower-case homoglyphs");

   end Check_Homoglyph;

end BinToAsc_Suite.Base32_Tests;
