-- BinToAsc_Example
-- Binary data to ASCII codecs example

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with System.Storage_Elements;
use System.Storage_Elements;

with String_To_Storage_Array, Storage_Array_To_String;

with RFC4648;
use RFC4648;

procedure BinToAsc_Example is

   use type RFC4648.Codec_State;

   Base16_Coding : Base16.Base16_To_String;
   Base16_Decoding : Base16.Base16_To_Bin;

   Base64_Coding : Base64.Base64_To_String;

   Result_String : String(1..16);
   Result_String_Length : Natural;
   Result_Bin : System.Storage_Elements.Storage_Array(1..16);
   Result_Bin_Length : System.Storage_Elements.Storage_Offset;

begin

   Put_Line("Examples of using the BinToAsc package.");
   New_Line;

   -- Demonstrate coding into BASE16
   Put_Line("According to RFC4648 BASE16('foobar') = '666F6F626172'");
   Put("According to this package BASE16('foobar') = '");
   Base16.Process(C => Base16_Coding,
                  Input => String_To_Storage_Array("foobar"),
                  Output => Result_String,
                  Output_Length => Result_String_Length);
   Put(Result_String(1..Result_String_Length));
   Put_Line("'");
   New_Line;

   -- The following is for demonstration purposes only - as the Base16 coder
   -- produces fixed-width output there should never be an issue here.
   Base16.Completed(C => Base16_Coding,
                    Output => Result_String,
                    Output_Length => Result_String_Length);
   pragma Assert(Result_String_Length = 0, "Base16 coder producing unexpected output.");
   pragma Assert(Base16_Coding.State = Complete, "Base16 coder not terminated cleanly.");

   -- Demonstrate decoding from BASE16
   Put_Line("According to RFC4648 BASE16^{-1}('666F6F626172') = 'foobar'");
   Put("According to this package BASE16^{-1}('666F6F626172') = '");
   Base16.Process(C => Base16_Decoding,
                  Input => "666F6F626172",
                  Output => Result_Bin,
                  Output_Length => Result_Bin_Length);
   Put(Storage_Array_To_String(Result_Bin(1..Result_Bin_Length)));
   Put_Line("'");
   New_Line;

   Base16.Completed(C => Base16_Decoding,
                    Output => Result_Bin,
                    Output_Length => Result_Bin_Length);
   pragma Assert(Result_Bin_Length = 0, "Base16 decoder producing unexpected output.");
   pragma Assert(Base16_Decoding.State = Complete, "Base16 decoder not terminated cleanly.");

   -- Demonstrate coding into BASE64
   Put_Line("According to RFC4648 BASE64('foobar') = 'Zm9vYmFy'");
   Put("According to this package BASE16('foobar') = '");
   Base64.Process(C => Base64_Coding,
                  Input => String_To_Storage_Array("foobar"),
                  Output => Result_String,
                  Output_Length => Result_String_Length);
   Put(Result_String(1..Result_String_Length));
   Base64.Completed(C => Base64_Coding,
                  Output => Result_String,
                  Output_Length => Result_String_Length);
   Put(Result_String(1..Result_String_Length));
   Put_Line("'");
   New_Line;

   Base64.Reset(C => Base64_Coding);
   Put_Line("According to RFC4648 BASE64('foob') = 'Zm9vYg=='");
   Put("According to this package BASE16('foob') = '");
   Base64.Process(C => Base64_Coding,
                  Input => String_To_Storage_Array("foob"),
                  Output => Result_String,
                  Output_Length => Result_String_Length);
   Put(Result_String(1..Result_String_Length));
   Base64.Completed(C => Base64_Coding,
                  Output => Result_String,
                  Output_Length => Result_String_Length);
   Put(Result_String(1..Result_String_Length));
   Put_Line("'");
   New_Line;

end BinToAsc_Example;
