-- BinToAsc_Example
-- Binary data to ASCII codecs example

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with String_To_Storage_Array;

with RFC4648;
use RFC4648;

procedure BinToAsc_Example is

   use type RFC4648.Codec_State;

   Base16_Codec : Base16.Base16_To_String;

   Result : String(1..16);
   Result_Length : Natural;

begin

   Put_Line("Examples of using the BinToAsc package.");
   New_Line;

   Put_Line("According to RFC4648 BASE16('foobar') = '666F6F626172'");
   Put("According to this package BASE16('foobar') = '");
   Base16.Process(C => Base16_Codec,
                          Input => String_To_Storage_Array("foobar"),
                          Output => Result,
                          Output_Length => Result_Length);
   Put(Result(1..Result_Length));
   Put_Line("'");
   New_Line;

   -- The following is for demonstration purposes only - as the Base16 codec
   -- produces fixed-width output there should never be an issue here.
   Base16.Completed(C => Base16_Codec,
                            Output => Result,
                            Output_Length => Result_Length);
   pragma Assert(Result_Length = 0, "Base16 codec producing unexpected output.");
   pragma Assert(Base16_Codec.State = Complete, "Base16 codec not terminated.");

end BinToAsc_Example;
