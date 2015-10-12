-- BinToAsc_Suite.Base85_Tests
-- Unit tests for BinToAsc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with AUnit.Assertions;

with System.Storage_Elements;
with Ada.Assertions;

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
      Register_Routine (T, Check_Z85_Test_Vector_By_Char'Access,
                        "Check Z85 test vector can be encoded/decoded incrementally by byte/char");
      Register_Routine (T, Check_Z85_Junk_Rejection'Access,
                        "Check Z85 rejects junk chars");
      Register_Routine (T, Check_Z85_High_Group'Access,
                        "Check Z85 correctly decodes strings that represent values around 2**32");
      Register_Routine (T, Check_Z85_Length_Rejection'Access,
                        "Check Z85 decoder rejects final character group of length 1");
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

   ---------------------------
   -- Check_Z85_Test_Vector --
   ---------------------------

   procedure Check_Z85_Test_Vector (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Test_Vector : constant Storage_Array :=
     (16#86#, 16#4F#, 16#D2#, 16#6F#,
      16#B5#, 16#59#, 16#F7#, 16#5B#);

      Z85_Encoded : constant String := "HelloWorld";

   begin
      Assert(Z85.To_String(Z85_Test_Vector) = Z85_Encoded,
             "Z85 encoder on test vector produced " &
               Z85.To_String(Z85_Test_Vector) &
               " rather than 'HelloWorld'");
      Assert(Z85.To_Bin(Z85_Encoded) = Z85_Test_Vector,
             "Z85 decoder on test vector produced " &
               SATHS(Z85.To_Bin(Z85_Encoded)) &
               " rather than " &
               SATHS(Z85_Test_Vector));
   end Check_Z85_Test_Vector;

   -----------------------------------
   -- Check_Z85_Test_Vector_By_Char --
   -----------------------------------

   procedure Check_Z85_Test_Vector_By_Char (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Test_Vector : constant Storage_Array :=
     (16#86#, 16#4F#, 16#D2#, 16#6F#,
      16#B5#, 16#59#, 16#F7#, 16#5B#);

      Z85_Encoded : constant String := "HelloWorld";

   begin
      declare
         Z85_Encoder : Z85.Base85_To_String;
         Buffer_String : String(1..15);
         Buffer_Index : Integer := 1;
         Buffer_Used : Integer := 0;
      begin
         Z85_Encoder.Reset;
         for I of Z85_Test_Vector loop
            Z85_Encoder.Process(Input => I,
                                Output => Buffer_String(Buffer_Index..Buffer_String'Last),
                                Output_Length => Buffer_Used);
            Buffer_Index := Buffer_Index + Buffer_Used;
         end loop;
         Z85_Encoder.Complete(Output => Buffer_String(Buffer_Index..Buffer_String'Last),
                              Output_Length => Buffer_Used);
         Buffer_Index := Buffer_Index + Buffer_Used;
         Assert(Buffer_String(1..Buffer_Index-1) = Z85_Encoded,
                "Z85 Encoder on test vector gave the wrong result when used " &
                  "character-by-character");
      end;

      declare
         Z85_Decoder : Z85.Base85_To_Bin;
         Buffer_Bin: Storage_Array(1..12);
         Buffer_Index : Storage_Offset := 1;
         Buffer_Used : Storage_Offset := 0;
      begin
         Z85_Decoder.Reset;
         for I of Z85_Encoded loop
            Z85_Decoder.Process(Input => I,
                                Output => Buffer_Bin(Buffer_Index..Buffer_Bin'Last),
                                Output_Length => Buffer_Used);
            Buffer_Index := Buffer_Index + Buffer_Used;
         end loop;
         Z85_Decoder.Complete(Output => Buffer_Bin(Buffer_Index..Buffer_Bin'Last),
                              Output_Length => Buffer_Used);
         Buffer_Index := Buffer_Index + Buffer_Used;
         Assert(Buffer_Bin(1..Buffer_Index-1) = Z85_Test_Vector,
                "Z85 Decoder on test vector gave the wrong result when used " &
                  "byte-by-byte");
      end;

   end Check_Z85_Test_Vector_By_Char;

   ------------------------------
   -- Check_Z85_Junk_Rejection --
   ------------------------------

   procedure Should_Raise_Exception_From_Junk is
      Discard : Storage_Array(1..8);
   begin
      Discard  := Z85.To_Bin("Hel\oWorld");
   end;

   procedure Check_Z85_Junk_Rejection (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Decoder : Z85.Base85_To_Bin;
      Buffer_Bin: Storage_Array(1..12);
      Buffer_Used : Storage_Offset := 0;

   begin

      Assert_Exception(Should_Raise_Exception_From_Junk'Access,
                       "Z85 decoder did not reject junk input.");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "Hel\oWorld",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject junk input");

      begin
         Z85_Decoder.Complete(Output => Buffer_Bin,
                              Output_Length => Buffer_Used
                             );
      exception
         when Ada.Assertions.Assertion_Error =>
            null; -- Preconditions (if active) will not allow Completed to be run
                  -- on a codec with state /= Ready.
      end;
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder in failed state was reset by the Complete routine");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "Hel",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejecting valid input");
      Z85_Decoder.Process(Input => "\oWorld",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject junk input when introduced incrementally");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "Hel",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejecting valid input");
      Z85_Decoder.Process(Input => '\',
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject junk input when introduced incrementally as a character");

   end Check_Z85_Junk_Rejection;

   --------------------------
   -- Check_Z85_High_Group --
   --------------------------

   procedure Check_Z85_High_Group (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Decoder : Z85.Base85_To_Bin;
      Buffer_Bin: Storage_Array(1..12);
      Buffer_Used : Storage_Offset := 0;

   begin

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "@####",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected @#### which is a valid group");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%####",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject %#### which is a valid group (more than 2**32)");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSc0",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected @nSb0 which is the highest valid group");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSc1",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject @nSb1 which is the lowest invalid group");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSc",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Z85_Decoder.Process(Input => '0',
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected @nSb0 which is the highest valid group " &
            "when the last character was presented separately");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSc",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Z85_Decoder.Process(Input => '1',
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject @nSb1 which is the lowest invalid group " &
            "when the last character was presented separately");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSb",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected %nSb which could be the start of a valid group");
      Z85_Decoder.Complete(Output => Buffer_Bin,
                           Output_Length => Buffer_Used
                          );
      Assert(Z85_Decoder.State = Complete,
             "Z85 decoder rejected %nSb at the end of input which is a valid group");
      Assert(Buffer_Used = 3,
             "Z85 decoder returned wrong length from %nSb at the end of input " &
             "(three FF bytes");
      Assert(Buffer_Bin(1..3) = (16#FF#, 16#FF#, 16#FF#),
             "Z85 decoder did not correctly decode %nSb at the end of input " &
             "(three FF bytes");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "%nSc",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected %nSc which could be the start of a valid group");
      Z85_Decoder.Complete(Output => Buffer_Bin,
                           Output_Length => Buffer_Used
                          );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder did not reject %nSc at the end of input which is a invalid group");

   end Check_Z85_High_Group;

   --------------------------------
   -- Check_Z85_Length_Rejection --
   --------------------------------

   procedure Should_Raise_Exception_From_Length is
      Discard : Storage_Array(1..8);
   begin
      Discard  := Z85.To_Bin("HelloW");
   end;

   procedure Check_Z85_Length_Rejection (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);

      Z85_Decoder : Z85.Base85_To_Bin;
      Buffer_Bin: Storage_Array(1..12);
      Buffer_Used : Storage_Offset := 0;

   begin

      Assert_Exception(Should_Raise_Exception_From_Length'Access,
                       "Z85 decoder did not reject impossible 6-character input.");

      Z85_Decoder.Reset;
      Z85_Decoder.Process(Input => "HelloW",
                          Output => Buffer_Bin,
                          Output_Length => Buffer_Used
                         );
      Assert(Z85_Decoder.State = Ready,
             "Z85 decoder rejected 6-character input too early");
      Z85_Decoder.Complete(Output => Buffer_Bin,
                           Output_Length => Buffer_Used
                          );
      Assert(Z85_Decoder.State = Failed,
             "Z85 decoder failed to reject an impossible final input of 1 character");

   end Check_Z85_Length_Rejection;

end BinToAsc_Suite.Base85_Tests;
